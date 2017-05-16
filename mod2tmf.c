
#include "errno.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"

/* protracker to tmf converter (c)2017 mumart@gmail.com */

#define MAX_CHANNELS 16
#define FP_SHIFT 14
#define FP_ONE   16384
#define FP_MASK  16383

struct note {
	unsigned short key;
	unsigned char instrument, effect, param;
};

struct instrument {
	unsigned char volume, fine_tune;
	unsigned long loop_start, loop_length;
	signed char *sample_data;
};

struct channel {
	struct note note;
	unsigned short period, porta_period;
	unsigned long sample_offset, sample_idx, step;
	unsigned char volume, panning, fine_tune, ampl;
	unsigned char id, instrument, assigned, porta_speed, pl_row, fx_count;
	unsigned char vibrato_type, vibrato_phase, vibrato_speed, vibrato_depth;
	unsigned char tremolo_type, tremolo_phase, tremolo_speed, tremolo_depth;
	signed char tremolo_add, vibrato_add, arpeggio_add;
	int trig_inst, swap_inst, prev_step, prev_ampl, prev_panning;
};

static const unsigned short fine_tuning[] = {
	4340, 4308, 4277, 4247, 4216, 4186, 4156, 4126,
	4096, 4067, 4037, 4008, 3979, 3951, 3922, 3894
};

static const unsigned short arp_tuning[] = {
	4096, 3866, 3649, 3444, 3251, 3069, 2896, 2734,
	2580, 2435, 2299, 2170, 2048, 1933, 1825, 1722
};

static const unsigned char sine_table[] = {
	  0,  24,  49,  74,  97, 120, 141, 161, 180, 197, 212, 224, 235, 244, 250, 253,
	255, 253, 250, 244, 235, 224, 212, 197, 180, 161, 141, 120,  97,  74,  49,  24
};

static const int freq_table[] = {
	16744, 16865, 16988, 17111, 17235, 17360, 17485, 17612,
	17740, 17868, 17998, 18128, 18260, 18392, 18525, 18659,
	18795, 18931, 19068, 19206, 19345, 19485, 19627, 19769,
	19912, 20056, 20202, 20348, 20496, 20644, 20794, 20944,
	21096, 21249, 21403, 21558, 21714, 21872, 22030, 22190,
	22351, 22513, 22676, 22840, 23006, 23172, 23340, 23509,
	23680, 23851, 24024, 24198, 24374, 24550, 24728, 24907,
	25088, 25270, 25453, 25637, 25823, 26010, 26198, 26388,
	26580, 26772, 26966, 27162, 27358, 27557, 27756, 27957,
	28160, 28364, 28570, 28777, 28985, 29195, 29407, 29620,
	29834, 30051, 30268, 30488, 30709, 30931, 31155, 31381,
	31609, 31838, 32068, 32301, 32535, 32770, 33008, 33247,
	33488
};

static signed char *module_data;
static unsigned char *pattern_data, *sequence;
static long song_length, restart, num_patterns, num_channels;
static struct instrument instruments[ 32 ];

static long sample_rate, c2_rate, tick_len, tick_offset;
static long pattern, break_pattern, row, next_row, tick;
static long speed, pl_count, pl_channel, random_seed;

static struct channel channels[ MAX_CHANNELS ];

static long calculate_num_patterns( signed char *module_header ) {
	long num_patterns, order_entry, pattern;
	num_patterns = 0;
	for( pattern = 0; pattern < 128; pattern++ ) {
		order_entry = module_header[ 952 + pattern ] & 0x7F;
		if( order_entry >= num_patterns ) num_patterns = order_entry + 1;
	}
	return num_patterns;
}

static long calculate_num_channels( signed char *module_header ) {
	long numchan;
	switch( ( module_header[ 1082 ] << 8 ) | module_header[ 1083 ] ) {
		case 0x4b2e: /* M.K. */
		case 0x4b21: /* M!K! */
		case 0x542e: /* N.T. */
		case 0x5434: /* FLT4 */
			numchan = 4;
			break;
		case 0x484e: /* xCHN */
			numchan = module_header[ 1080 ] - 48;
			break;
		case 0x4348: /* xxCH */
			numchan = ( ( module_header[ 1080 ] - 48 ) * 10 ) + ( module_header[ 1081 ] - 48 );
			break;
		default: /* Not recognised. */
			numchan = 0;
			break;
	}
	if( numchan > MAX_CHANNELS ) numchan = 0;
	return numchan;
}

static long unsigned_short_big_endian( signed char *buf, long offset ) {
	return ( ( buf[ offset ] & 0xFF ) << 8 ) | ( buf[ offset + 1 ] & 0xFF );
}

static void set_tempo( long tempo ) {
	tick_len = ( ( sample_rate << 1 ) + ( sample_rate >> 1 ) ) / tempo;
}

static void update_frequency( struct channel *chan ) {
	long period, volume;
	unsigned long freq;
	period = chan->period + chan->vibrato_add;
	period = period * arp_tuning[ chan->arpeggio_add ] >> 11;
	period = ( period >> 1 ) + ( period & 1 );
	if( period < 14 ) period = 6848;
	freq = c2_rate * 428 / period;
	chan->step = ( freq << FP_SHIFT ) / sample_rate;
	volume = chan->volume + chan->tremolo_add;
	if( volume > 64 ) volume = 64;
	if( volume < 0 ) volume = 0;
	chan->ampl = volume;
}

static void tone_portamento( struct channel *chan ) {
	long source, dest;
	source = chan->period;
	dest = chan->porta_period;
	if( source < dest ) {
		source += chan->porta_speed;
		if( source > dest ) source = dest;
	} else if( source > dest ) {
		source -= chan->porta_speed;
		if( source < dest ) source = dest;
	}
	chan->period = source;
}

static void volume_slide( struct channel *chan, long param ) {
	long volume;
	volume = chan->volume + ( param >> 4 ) - ( param & 0xF );
	if( volume < 0 ) volume = 0;
	if( volume > 64 ) volume = 64;
	chan->volume = volume;
}

static long waveform( long phase, long type ) {
	long amplitude = 0;
	switch( type & 0x3 ) {
		case 0: /* Sine. */
			amplitude = sine_table[ phase & 0x1F ];
			if( ( phase & 0x20 ) > 0 ) amplitude = -amplitude;
			break;
		case 1: /* Saw Down. */
			amplitude = 255 - ( ( ( phase + 0x20 ) & 0x3F ) << 3 );
			break;
		case 2: /* Square. */
			amplitude = 255 - ( ( phase & 0x20 ) << 4 );
			break;
		case 3: /* Random. */
			amplitude = ( random_seed >> 20 ) - 255;
			random_seed = ( random_seed * 65 + 17 ) & 0x1FFFFFFF;
			break;
	}
	return amplitude;
}

static void vibrato( struct channel *chan ) {
	chan->vibrato_add = waveform( chan->vibrato_phase, chan->vibrato_type ) * chan->vibrato_depth >> 7;
}

static void tremolo( struct channel *chan ) {
	chan->tremolo_add = waveform( chan->tremolo_phase, chan->tremolo_type ) * chan->tremolo_depth >> 6;
}

static void trigger( struct channel *channel ) {
	long period, ins;
	ins = channel->note.instrument;
	if( ins > 0 && ins < 32 ) {
		channel->assigned = ins;
		channel->sample_offset = 0;
		channel->fine_tune = instruments[ ins ].fine_tune;
		channel->volume = instruments[ ins ].volume;
		if( instruments[ ins ].loop_length > 0 && channel->instrument > 0
		&& channel->instrument != ins ) {
			channel->instrument = ins;
			channel->swap_inst = channel->instrument;
		}
	}
	if( channel->note.effect == 0x09 ) {
		channel->sample_offset = ( channel->note.param & 0xFF ) << 8;
	} else if( channel->note.effect == 0x15 ) {
		channel->fine_tune = channel->note.param;
	}
	if( channel->note.key > 0 ) {
		period = ( channel->note.key * fine_tuning[ channel->fine_tune & 0xF ] ) >> 11;
		channel->porta_period = ( period >> 1 ) + ( period & 1 );
		if( channel->note.effect != 0x3 && channel->note.effect != 0x5 ) {
			channel->instrument = channel->assigned;
			channel->period = channel->porta_period;
			channel->sample_idx = ( channel->sample_offset << FP_SHIFT );
			if( channel->vibrato_type < 4 ) channel->vibrato_phase = 0;
			if( channel->tremolo_type < 4 ) channel->tremolo_phase = 0;
			channel->trig_inst = channel->instrument;
		}
	}
}

static void channel_row( struct channel *chan ) {
	long effect, param, volume, period;
	effect = chan->note.effect;
	param = chan->note.param;
	chan->trig_inst = chan->swap_inst = 0;
	chan->prev_step = chan->step;
	chan->prev_ampl = chan->ampl;
	chan->prev_panning = chan->panning;
	chan->vibrato_add = chan->tremolo_add = chan->arpeggio_add = chan->fx_count = 0;
	if( !( effect == 0x1D && param > 0 ) ) {
		/* Not note delay. */
		trigger( chan );
	}
	switch( effect ) {
		case 0x3: /* Tone Portamento.*/
			if( param > 0 ) chan->porta_speed = param;
			break;
		case 0x4: /* Vibrato.*/
			if( ( param & 0xF0 ) > 0 ) chan->vibrato_speed = param >> 4;
			if( ( param & 0x0F ) > 0 ) chan->vibrato_depth = param & 0xF;
			vibrato( chan );
			break;
		case 0x6: /* Vibrato + Volume Slide.*/
			vibrato( chan );
			break;
		case 0x7: /* Tremolo.*/
			if( ( param & 0xF0 ) > 0 ) chan->tremolo_speed = param >> 4;
			if( ( param & 0x0F ) > 0 ) chan->tremolo_depth = param & 0xF;
			tremolo( chan );
			break;
		case 0x8: /* Set Panning. Not for 4-channel ProTracker. */
			if( num_channels != 4 ) {
				chan->panning = ( param < 128 ) ? ( param << 1 ) : 255;
			}
			break;
		case 0xB: /* Pattern Jump.*/
			if( pl_count < 0 ) {
				break_pattern = param;
				next_row = 0;
			}
			break;
		case 0xC: /* Set Volume.*/
			chan->volume = param > 64 ? 64 : param;
			break;
		case 0xD: /* Pattern Break.*/
			if( pl_count < 0 ) {
				if( break_pattern < 0 ) break_pattern = pattern + 1;
				next_row = ( param >> 4 ) * 10 + ( param & 0xF );
				if( next_row >= 64 ) next_row = 0;
			}
			break;
		case 0xF: /* Set Speed.*/
			if( param > 0 ) {
				if( param < 32 ) tick = speed = param;
				else set_tempo( param );
			}
			break;
		case 0x11: /* Fine Portamento Up.*/
			period = chan->period - param;
			chan->period = period < 0 ? 0 : period;
			break;
		case 0x12: /* Fine Portamento Down.*/
			period = chan->period + param;
			chan->period = period > 65535 ? 65535 : period;
			break;
		case 0x14: /* Set Vibrato Waveform.*/
			if( param < 8 ) chan->vibrato_type = param;
			break;
		case 0x16: /* Pattern Loop.*/
			if( param == 0 ) /* Set loop marker on this channel. */
				chan->pl_row = row;
			if( chan->pl_row < row && break_pattern < 0 ) { /* Marker valid. */
				if( pl_count < 0 ) { /* Not already looping, begin. */
					pl_count = param;
					pl_channel = chan->id;
				}
				if( pl_channel == chan->id ) { /* Next Loop.*/
					if( pl_count == 0 ) { /* Loop finished. */
						/* Invalidate current marker. */
						chan->pl_row = row + 1;
					} else { /* Loop. */
						next_row = chan->pl_row;
					}
					pl_count--;
				}
			}
			break;
		case 0x17: /* Set Tremolo Waveform.*/
			if( param < 8 ) chan->tremolo_type = param;
			break;
		case 0x1A: /* Fine Volume Up.*/
			volume = chan->volume + param;
			chan->volume = volume > 64 ? 64 : volume;
			break;
		case 0x1B: /* Fine Volume Down.*/
			volume = chan->volume - param;
			chan->volume = volume < 0 ? 0 : volume;
			break;
		case 0x1C: /* Note Cut.*/
			if( param <= 0 ) chan->volume = 0;
			break;
		case 0x1E: /* Pattern Delay.*/
			tick = speed + speed * param;
			break;
	}
	update_frequency( chan );
}

static void channel_tick( struct channel *chan ) {
	long effect, param, period;
	effect = chan->note.effect;
	param = chan->note.param;
	chan->trig_inst = chan->swap_inst = 0;
	chan->prev_step = chan->step;
	chan->prev_ampl = chan->ampl;
	chan->prev_panning = chan->panning;
	chan->fx_count++;
	switch( effect ) {
		case 0x1: /* Portamento Up.*/
			period = chan->period - param;
			chan->period = period < 0 ? 0 : period;
			break;
		case 0x2: /* Portamento Down.*/
			period = chan->period + param;
			chan->period = period > 65535 ? 65535 : period;
			break;
		case 0x3: /* Tone Portamento.*/
			tone_portamento( chan );
			break;
		case 0x4: /* Vibrato.*/
			chan->vibrato_phase += chan->vibrato_speed;
			vibrato( chan );
			break;
		case 0x5: /* Tone Porta + Volume Slide.*/
			tone_portamento( chan );
			volume_slide( chan, param );
			break;
		case 0x6: /* Vibrato + Volume Slide.*/
			chan->vibrato_phase += chan->vibrato_speed;
			vibrato( chan );
			volume_slide( chan, param );
			break;
		case 0x7: /* Tremolo.*/
			chan->tremolo_phase += chan->tremolo_speed;
			tremolo( chan );
			break;
		case 0xA: /* Volume Slide.*/
			volume_slide( chan, param );
			break;
		case 0xE: /* Arpeggio.*/
			if( chan->fx_count > 2 ) chan->fx_count = 0;
			if( chan->fx_count == 0 ) chan->arpeggio_add = 0;
			if( chan->fx_count == 1 ) chan->arpeggio_add = param >> 4;
			if( chan->fx_count == 2 ) chan->arpeggio_add = param & 0xF;
			break;
		case 0x19: /* Retrig.*/
			if( chan->fx_count >= param ) {
				chan->fx_count = 0;
				chan->sample_idx = 0;
				chan->trig_inst = chan->instrument;
			}
			break;
		case 0x1C: /* Note Cut.*/
			if( param == chan->fx_count ) chan->volume = 0;
			break;
		case 0x1D: /* Note Delay.*/
			if( param == chan->fx_count ) trigger( chan );
			break;
	}
	if( effect > 0 ) update_frequency( chan );
}

static long sequence_row() {
	long song_end, chan_idx, pat_offset;
	long effect, param;
	struct note *note;
	song_end = 0;
	if( next_row < 0 ) {
		break_pattern = pattern + 1;
		next_row = 0;
	}
	if( break_pattern >= 0 ) {
		if( break_pattern >= song_length ) break_pattern = next_row = 0;
		if( break_pattern <= pattern ) song_end = 1;
		pattern = break_pattern;
		for( chan_idx = 0; chan_idx < num_channels; chan_idx++ ) channels[ chan_idx ].pl_row = 0;
		break_pattern = -1;
	}
	row = next_row;
	next_row = row + 1;
	if( next_row >= 64 ) next_row = -1;
	pat_offset = ( sequence[ pattern ] * 64 + row ) * num_channels * 4;
	for( chan_idx = 0; chan_idx < num_channels; chan_idx++ ) {
		note = &channels[ chan_idx ].note;
		note->key  = ( pattern_data[ pat_offset ] & 0xF ) << 8;
		note->key |=   pattern_data[ pat_offset + 1 ];
		note->instrument  = pattern_data[ pat_offset + 2 ] >> 4;
		note->instrument |= pattern_data[ pat_offset ] & 0x10;
		effect = pattern_data[ pat_offset + 2 ] & 0xF;
		param = pattern_data[ pat_offset + 3 ];
		pat_offset += 4;
		if( effect == 0xE ) {
			effect = 0x10 | ( param >> 4 );
			param &= 0xF;
		}
		if( effect == 0 && param > 0 ) effect = 0xE;
		note->effect = effect;
		note->param = param;
		channel_row( &channels[ chan_idx ] );
	}
	return song_end;
}

static long sequence_tick() {
	long song_end, chan_idx;
	song_end = 0;
	if( --tick <= 0 ) {
		tick = speed;
		song_end = sequence_row();
	} else {
		for( chan_idx = 0; chan_idx < num_channels; chan_idx++ )
			channel_tick( &channels[ chan_idx ] );
	}
	return song_end;
}

static void resample( struct channel *chan, short *buf, long offset, long count ) {
	long sample, ampl, lamp, ramp;
	unsigned long buf_idx, buf_end, sidx, step, inst, llen, lep1, epos;
	signed char *sdat;
	ampl = buf ? chan->ampl : 0;
	ramp = ampl * chan->panning;
	lamp = ampl * ( 255 - chan->panning );
	sidx = chan->sample_idx;
	step = chan->step;
	inst = chan->instrument;
	llen = instruments[ inst ].loop_length;
	lep1 = instruments[ inst ].loop_start + llen;
	sdat = instruments[ inst ].sample_data;
	buf_idx = offset << 1;
	buf_end = ( offset + count ) << 1;
	while( buf_idx < buf_end ) {
		if( sidx >= lep1 ) {
			/* Handle loop. */
			if( llen <= FP_ONE ) {
				/* One-shot sample. */
				sidx = lep1;
				break;
			}
			/* Subtract loop-length until within loop points. */
			while( sidx >= lep1 ) sidx -= llen;
		}
		/* Calculate sample position at end. */
		epos = sidx + ( ( buf_end - buf_idx ) >> 1 ) * step;
		if( ampl <= 0 ) {
			/* No need to mix. */
			sidx = epos;
			break;
		}
		/* Only mix to end of current loop. */
		if( epos > lep1 ) epos = lep1;
		while( sidx < epos ) {
			/* Most of the cpu time is spent in here. */
			sample = sdat[ sidx >> FP_SHIFT ];
			buf[ buf_idx++ ] += sample * lamp >> 8;
			buf[ buf_idx++ ] += sample * ramp >> 8;
			sidx += step;
		}
	}
	chan->sample_idx = sidx;
}

/*
	Calculate the length in bytes of a module file given the 1084-byte header.
	Returns -1 if the data is not recognised as a module.
*/
long micromod_calculate_mod_file_len( signed char *module_header ) {
	long length, numchan, inst_idx;
	numchan = calculate_num_channels( module_header );
	if( numchan <= 0 ) return -1;
	length = 1084 + 4 * numchan * 64 * calculate_num_patterns( module_header );
	for( inst_idx = 1; inst_idx < 32; inst_idx++ )
		length += unsigned_short_big_endian( module_header, inst_idx * 30 + 12 ) * 2;
	return length;
}

/*
	Obtains song and instrument names from the module.
	The song name is returned as instrument 0.
	The name is copied into the location pointed to by string,
	and is at most 23 characters long, including the trailing null.
*/
void micromod_get_string( long instrument, char *string ) {
	long index, offset, length, character;
	if( num_channels <= 0 ) {
		string[ 0 ] = 0;
		return;
	}
	offset = 0;
	length = 20;
	if( instrument > 0 && instrument < 32 ) {
		offset = ( instrument - 1 ) * 30 + 20;
		length = 22;
	}
	for( index = 0; index < length; index++ ) {
		character = module_data[ offset + index ];
		if( character < 32 || character > 126 ) character = ' ';
		string[ index ] = character;
	}
	string[ length ] = 0;
}

/*
	Jump directly to a specific pattern in the sequence.
*/
void micromod_set_position( long pos ) {
	long chan_idx;
	struct channel *chan;
	if( num_channels <= 0 ) return; 
	if( pos >= song_length ) pos = 0;
	break_pattern = pos;
	next_row = 0;
	tick = 1;
	speed = 6;
	set_tempo( 125 );
	pl_count = pl_channel = -1;
	random_seed = 0xABCDEF;
	for( chan_idx = 0; chan_idx < num_channels; chan_idx++ ) {
		chan = &channels[ chan_idx ];
		memset( chan, 0, sizeof( struct channel ) );
		chan->id = chan_idx;
		switch( chan_idx & 0x3 ) {
			case 0: case 3: chan->panning =  51; break;
			case 1: case 2: chan->panning = 204; break;
		}
	}
	sequence_tick();
	tick_offset = 0;
}

/*
	Set the player to play the specified module data.
	Returns -1 if the data is not recognised as a module.
	Returns -2 if the sampling rate is less than 8000hz.
*/
long micromod_initialise( signed char *data, long sampling_rate ) {
	struct instrument *inst;
	long sample_data_offset, inst_idx;
	long sample_length, volume, fine_tune, loop_start, loop_length;
	num_channels = calculate_num_channels( data );
	if( num_channels <= 0 ) {
		num_channels = 0;
		return -1;
	}
	if( sampling_rate < 8000 ) return -2;
	module_data = data;
	sample_rate = sampling_rate;
	song_length = module_data[ 950 ] & 0x7F;
	restart = module_data[ 951 ] & 0x7F;
	if( restart >= song_length ) restart = 0;
	sequence = (unsigned char *) module_data + 952;
	pattern_data = (unsigned char *) module_data + 1084;
	num_patterns = calculate_num_patterns( module_data );
	sample_data_offset = 1084 + num_patterns * 64 * num_channels * 4;
	for( inst_idx = 1; inst_idx < 32; inst_idx++ ) {
		inst = &instruments[ inst_idx ];
		sample_length = unsigned_short_big_endian( module_data, inst_idx * 30 + 12 ) * 2;
		fine_tune = module_data[ inst_idx * 30 + 14 ] & 0xF;
		inst->fine_tune = ( fine_tune & 0x7 ) - ( fine_tune & 0x8 ) + 8;
		volume = module_data[ inst_idx * 30 + 15 ] & 0x7F;
		inst->volume = volume > 64 ? 64 : volume;
		loop_start = unsigned_short_big_endian( module_data, inst_idx * 30 + 16 ) * 2;
		loop_length = unsigned_short_big_endian( module_data, inst_idx * 30 + 18 ) * 2;
		if( loop_start + loop_length > sample_length )
			loop_length = sample_length - loop_start;
		if( loop_length < 4 ) {
			loop_start = sample_length;
			loop_length = 0;
		}
		inst->loop_start = loop_start << FP_SHIFT;
		inst->loop_length = loop_length << FP_SHIFT;
		inst->sample_data = module_data + sample_data_offset;
		sample_data_offset += sample_length;
	}
	c2_rate = ( num_channels > 4 ) ? 8363 : 8287;
	micromod_set_position( 0 );
	return 0;
}

/*
	Returns the total song duration in samples at the current sampling rate.
*/
long micromod_calculate_song_duration() {
	long duration, song_end;
	duration = 0;
	if( num_channels > 0 ) {
		micromod_set_position( 0 );
		song_end = 0;
		while( !song_end ) {
			duration += tick_len;
			song_end = sequence_tick();
		}
		micromod_set_position( 0 );
	}
	return duration;
}

/*
	Calculate the specified number of samples of audio.
	If output pointer is zero, the replay will quickly skip count samples.
	The output buffer should be cleared with zeroes.
*/
void micromod_get_audio( short *output_buffer, long count ) {
	long offset, remain, chan_idx;
	if( num_channels <= 0 ) return;
	offset = 0;
	while( count > 0 ) {
		remain = tick_len - tick_offset;
		if( remain > count ) remain = count;
		for( chan_idx = 0; chan_idx < num_channels; chan_idx++ ) {
			resample( &channels[ chan_idx ], output_buffer, offset, remain );
		}
		tick_offset += remain;
		if( tick_offset == tick_len ) {
			sequence_tick();
			tick_offset = 0;
		}
		offset += remain;
		count -= remain;
	}
}

static long read_file( char *filename, void *buffer, long length ) {
	FILE *file;
	long count;
	count = -1;
	file = fopen( filename, "rb" );
	if( file != NULL ) {
		count = fread( buffer, 1, length, file );
		if( count < length && !feof( file ) ) {
			fprintf( stderr, "Unable to read file '%s'.\n", filename );
			count = -1;
		}
		if( fclose( file ) != 0 ) {
			fprintf( stderr, "Unable to close file '%s'.\n", filename );
		}
	}
	return count;
}

static long write_file( char *filename, char *buffer, int length ) {
	long count = -1;
	char message[ 64 ];
	FILE *file = fopen( filename, "wb" );
	if( file != NULL ) {
		count = fwrite( buffer, 1, length, file );
		fclose( file );
	}
	if( count < length ) {
		strncpy( message, strerror( errno ), 63 );
		message[ 63 ] = 0;
		fputs( message, stderr );
		fputs( "\n", stderr );
		count = -1;
	}
	return count;
}

static long read_module_length( char *filename ) {
	long length;
	signed char header[ 1084 ];
	length = read_file( filename, header, 1084 );
	if( length == 1084 ) {
		length = micromod_calculate_mod_file_len( header );
		if( length < 0 ) {
			fprintf( stderr, "Module file type not recognised.\n");
		}
	} else {
		fprintf( stderr, "Unable to read module file '%s'.\n", filename );
		length = -1;
	}
	return length;
}

static void write_int16be( int value, char *dest ) {
	dest[ 0 ] = value >> 8;
	dest[ 1 ] = value;
}

static void write_int32be( int value, char *dest ) {
	dest[ 0 ] = value >> 24;
	dest[ 1 ] = value >> 16;
	dest[ 2 ] = value >> 8;
	dest[ 3 ] = value;
}

static int get_tmf_key( int freq ) {
	int octave = 0, tone = 0;
	freq = freq << 5;
	while( freq >= freq_table[ 96 ] ) {
		octave++;
		freq = freq >> 1;
	}
	while( freq_table[ tone + 1 ] < freq ) {
		tone++;
	}
	if( freq_table[ tone + 1 ] - freq <= freq - freq_table[ tone ] ) {
		tone++;
	}
	return octave * 96 + tone;
}

static int sqr_rt( int y ) {
	int n, x = 256;
	for( n = 0; n < 8; n++ ) {
		x = ( x + y / x );
		x = ( x >> 1 ) + ( x & 1 );
	}
	return x;
}

static int write_sequence( char *dest ) {
	int chn, idx = 0, song_end = 0, tick = 0, wait = 0;
	int inst, swap, sidx, step, d_step, freq, ampl, d_ampl, pann, d_pann, pan;
	micromod_set_position( 0 );
	for( chn = 0; chn < num_channels; chn++ ) {
		channels[ chn ].prev_panning = -1;
	}
	while( !song_end ) {
		if( tick != tick_len ) {
			if( dest ) {
				write_int16be( 0xE000 + ( ( tick_len >> 1 ) & 0xFFF ), &dest[ idx ] );
			}
			tick = tick_len;
			idx += 2;
		}
		for( chn = 0; chn < num_channels; chn++ ) {
			inst = channels[ chn ].trig_inst;
			swap = channels[ chn ].swap_inst;
			sidx = channels[ chn ].sample_idx;
			step = channels[ chn ].step;
			d_step = step - channels[ chn ].prev_step;
			freq = ( step * sample_rate ) >> FP_SHIFT;
			ampl = sqr_rt( channels[ chn ].ampl << 6 );
			d_ampl = ampl - sqr_rt( channels[ chn ].prev_ampl << 6 );
			pann = channels[ chn ].panning;
			d_pann = pann - channels[ chn ].prev_panning;
			if( inst || swap || d_step || d_ampl || d_pann ) {
				if( wait > 0 ) {
					if( wait > 0xFFF ) {
						wait = 0xFFF;
					}
					if( dest ) {
						write_int16be( 0xF000 + wait, &dest[ idx ] );
					}
					idx += 2;
					wait = 0;
				}
				if( inst ) {
					/* Trigger Instrument.*/
					if( dest ) {
						write_int32be( 0x10000000
							+ ( get_tmf_key( freq ) << 16 )
							+ ( inst << 8 ) + chn, &dest[ idx ] );
					}
					idx += 4;
					if( sidx ) {
						/* Set Sample Offset.*/
						if( dest ) {
							write_int32be( 0x30000000
								+ ( ( sidx >> FP_SHIFT ) << 8 )
								+ chn, &dest[ idx ] );
						}
						idx += 4;
					}
				} else if( swap ) {
					/* Switch Instrument.*/
					if( dest ) {
						write_int32be( 0x20000000
							+ ( get_tmf_key( freq ) << 16 )
							+ ( swap << 8 ) + chn, &dest[ idx ] );
					}
					idx += 4;
				} else if( d_step ) {
					/* Modulate Pitch.*/
					pan = 0;
					if( d_ampl ) {
						pan = 0x40 + ampl;
						d_ampl = 0;
					} else if( d_pann ) {
						pan = 0x80 + ( ( pann > 4 ) ? ( pann >> 2 ) : 1 );
						d_pann = 0;
					}
					if( dest ) {
						write_int32be( 0x20000000
							+ ( get_tmf_key( freq ) << 16 )
							+ ( pan << 8 ) + chn, &dest[ idx ] );
					}
					idx += 4;
				}
				if( d_ampl ) {
					/* Modulate volume.*/
					if( dest ) {
						write_int16be( ( ( 0x40 + ampl ) << 8 ) + chn, &dest[ idx ] );
					}
					idx += 2;
				}
				if( d_pann ) {
					/* Modulate panning.*/
					if( dest ) {
						pan = ( pann > 4 ) ? ( pann >> 2 ) : 1;
						write_int16be( ( ( 0x80 + pan ) << 8 ) + chn, &dest[ idx ] );
					}
					idx += 2;
				}
			}
		}
		wait++;
		song_end = sequence_tick();
	}
	return idx;
}

static int mod_to_tmf( signed char *mod, char *tmf ) {
	int seqlen, idx, loop_start, loop_length, length = -1;
	if( micromod_initialise( mod, 48000 ) >= 0 ) {
		length = 32 * 64;
		seqlen = write_sequence( NULL );
		if( tmf ) {
			printf( "Sequence length: %d bytes.\n", seqlen );
			memset( tmf, 0, length );
			strcpy( tmf, "TMF0" );
			write_int32be( seqlen, &tmf[ 4 ] );
			micromod_get_string( 0, &tmf[ 8 ] );
			write_sequence( &tmf[ length ] );
		}
		length = length + seqlen;
		idx = 1;
		while( idx < 32 ) {
			loop_start = instruments[ idx ].loop_start >> FP_SHIFT;
			loop_length = instruments[ idx ].loop_length >> FP_SHIFT;
			if( tmf ) {
				write_int32be( loop_start, &tmf[ idx * 32 ] );
				write_int32be( loop_length, &tmf[ idx * 32 + 4 ] );
				micromod_get_string( idx, &tmf[ idx * 32 + 8 ] );
				memcpy( &tmf[ length ], instruments[ idx ].sample_data, loop_start );
				length = length + loop_start;
				memcpy( &tmf[ length ], &instruments[ idx ].sample_data[ loop_start ], loop_length );
				length = length + loop_length;
			} else {
				length = length + loop_start + loop_length;
			}
			idx++;
		}
	}
	return length;
}

int main( int argc, char **argv ) {
	int result;
	long count, length;
	signed char *mod;
	char *tmf;
	result = EXIT_FAILURE;
	if( argc != 3 ) {
		fprintf( stderr, "Usage: %s input.mod output.tmf\n", argv[ 0 ] );
	} else {
		/* Read module file.*/
		length = read_module_length( argv[ 1 ] );
		if( length > 0 ) {
			printf( "Module Data Length: %li bytes.\n", length );
			mod = calloc( length, 1 );
			if( mod != NULL ) {
				count = read_file( argv[ 1 ], mod, length );
				if( count < length ) {
					fprintf( stderr, "Module file is truncated. %li bytes missing.\n", length - count );
				}
				/* Perform conversion. */
				length = mod_to_tmf( mod, NULL );
				if( length > 0 ) {
					tmf = calloc( length, 1 );
					if( tmf != NULL ) {
						mod_to_tmf( mod, tmf );
						if( write_file( argv[ 2 ], tmf, length ) > 0 ) {
							result = EXIT_SUCCESS;
						}
						free( tmf );
					}
				}
				free( mod );
			}
		}
	}
	return result;
}
