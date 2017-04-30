
#include "errno.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"

static const char *VERSION = "MOD/S3M/XM to TMF converter (c)2017 mumart@gmail.com";

static const int FP_SHIFT = 15;

static const int tmf_freq_table[] = {
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

struct data {
	signed char *buffer;
	int length;
};

struct sample {
	char name[ 24 ];
	int loop_start, loop_length;
	short volume, panning, rel_note, fine_tune, *data;
};

struct envelope {
	char enabled, sustain, looped, num_points;
	short sustain_tick, loop_start_tick, loop_end_tick;
	short points_tick[ 16 ], points_ampl[ 16 ];
};

struct instrument {
	int num_samples, vol_fadeout;
	char name[ 24 ], key_to_sample[ 97 ];
	char vib_type, vib_sweep, vib_depth, vib_rate;
	struct envelope vol_env, pan_env;
	struct sample *samples;
};

struct pattern {
	int num_rows;
	char *data;
};

struct module {
	char name[ 24 ];
	int num_channels, num_instruments;
	int num_patterns, sequence_len, restart_pos;
	int default_gvol, default_speed, default_tempo, c2_rate, gain;
	int linear_periods, fast_vol_slides;
	unsigned char default_panning[ 32 ], sequence[ 256 ];
	struct pattern *patterns;
	struct instrument *instruments;
};

struct channel {
	int sample_idx, step, ampl, pann;
	int trig_inst, swap_inst, prev_step, prev_ampl, prev_pann;
};

struct replay {
	struct channel channels[ 32 ];
	int sample_rate, num_channels;
	int seq_pos, break_pos, row, next_row, tick;
	int speed, tempo, pl_count, pl_chan;
};

static char* data_ascii( struct data *data, int offset, int length, char *dest ) {
	if( offset > data->length ) {
		offset = data->length;
	}
	if( offset + length > data->length ) {
		length = data->length - offset;
	}
	memcpy( dest, &data->buffer[ offset ], length );
	return dest;
}

static int data_s8( struct data *data, int offset ) {
	int value = 0;
	if( offset < data->length ) {
		value = data->buffer[ offset ];
	}
	return value;
}

static int data_u8( struct data *data, int offset ) {
	int value = 0;
	if( offset < data->length ) {
		value = data->buffer[ offset ] & 0xFF;
	}
	return value;
}

static int data_u16le( struct data *data, int offset ) {
	int value = 0;
	if( offset + 1 < data->length ) {
		value = ( data->buffer[ offset ] & 0xFF )
			| ( ( data->buffer[ offset + 1 ] & 0xFF ) << 8 );
	}
	return value;
}

static unsigned int data_u32le( struct data *data, int offset ) {
	unsigned int value = 0;
	if( offset + 3 < data->length ) {
		value = ( data->buffer[ offset ] & 0xFF )
			| ( ( data->buffer[ offset + 1 ] & 0xFF ) << 8 )
			| ( ( data->buffer[ offset + 2 ] & 0xFF ) << 16 )
			| ( ( data->buffer[ offset + 3 ] & 0xFF ) << 24 );
	}
	return value;
}

static void data_sam_s8d( struct data *data, int offset, int count, short *dest ) {
	int idx, length = data->length, sam = 0;
	signed char *buffer = data->buffer;
	if( offset > length ) {
		offset = length;
	}
	if( offset + count > length ) {
		count = length - offset;
	}
	for( idx = 0; idx < count; idx++ ) {
		sam += buffer[ offset + idx ];
		dest[ idx ] = sam << 8;
	}
}

static void data_sam_s16d( struct data *data, int offset, int count, short *dest ) {
	int idx, length = data->length >> 1, sam = 0;
	signed char *buffer = data->buffer;
	if( offset > length ) {
		offset = length;
	}
	if( offset + count > length ) {
		count = length - offset;
	}
	for( idx = 0; idx < count; idx++ ) {
		sam += ( buffer[ offset + idx * 2 ] & 0xFF ) | ( buffer[ offset + idx * 2 + 1 ] << 8 );
		dest[ idx ] = sam;
	}
}

static void sample_ping_pong( struct sample *sample ) {
	int idx;
	int loop_start = sample->loop_start;
	int loop_length = sample->loop_length;
	int loop_end = loop_start + loop_length;
	short *sample_data = sample->data;
	short *new_data = calloc( loop_end + loop_length, sizeof( short ) );
	if( new_data ) {
		memcpy( new_data, sample_data, loop_end * sizeof( short ) );
		for( idx = 0; idx < loop_length; idx++ ) {
			new_data[ loop_end + idx ] = sample_data[ loop_end - idx - 1 ];
		}
		free( sample->data );
		sample->data = new_data;
		sample->loop_length *= 2;
	}
}

static void dispose_module( struct module *module ) {
	int idx, sam;
	struct instrument *instrument;
	for( idx = 0; idx < module->num_patterns; idx++ ) {
		free( module->patterns[ idx ].data );
	}
	free( module->patterns );
	for( idx = 0; idx <= module->num_instruments; idx++ ) {
		instrument = &module->instruments[ idx ];
		for( sam = 0; sam < instrument->num_samples; sam++ ) {
			free( instrument->samples[ sam ].data );
		}
		free( instrument->samples );
	}
	free( module->instruments );
	free( module );
}

static struct module* module_load_xm( struct data *data ) {
	int delta_env, offset, next_offset, idx, entry;
	int num_rows, num_notes, pat_data_len, pat_data_offset;
	int sam, sam_head_offset, sam_data_bytes, sam_data_samples;
	int sam_loop_start, sam_loop_length;
	int note, flags, key, ins, vol, fxc, fxp;
	int point, point_tick, point_offset;
	int looped, ping_pong, sixteen_bit;
	char ascii[ 16 ], *pattern_data;
	struct instrument *instrument;
	struct sample *sample;
	struct module *module = calloc( 1, sizeof( struct module ) );
	if( module ) {
		if( data_u16le( data, 58 ) != 0x0104 ) {
			fputs( "XM format version must be 0x0104!", stderr );
			dispose_module( module );
			return NULL;
		}
		data_ascii( data, 17, 20, module->name );
		delta_env = !memcmp( data_ascii( data, 38, 15, ascii ), "DigiBooster Pro", 15 );
		offset = 60 + data_u32le( data, 60 );
		module->sequence_len = data_u16le( data, 64 );
		if( module->sequence_len > 256 ) {
			module->sequence_len = 256;
		}
		module->restart_pos = data_u16le( data, 66 );
		module->num_channels = data_u16le( data, 68 );
		module->num_patterns = data_u16le( data, 70 );
		module->num_instruments = data_u16le( data, 72 );
		module->linear_periods = data_u16le( data, 74 ) & 0x1;
		module->default_gvol = 64;
		module->default_speed = data_u16le( data, 76 );
		module->default_tempo = data_u16le( data, 78 );
		module->c2_rate = 8363;
		module->gain = 64;
		for( idx = 0; idx < 32; idx++ ) {
			module->default_panning[ idx ] = 128;
		}
		for( idx = 0; idx < module->sequence_len; idx++ ) {
			entry = data_u8( data, 80 + idx );
			module->sequence[ idx ] = entry < module->num_patterns ? entry : 0;
		}
		module->patterns = calloc( module->num_patterns, sizeof( struct pattern ) );
		if( !module->patterns ) {
			dispose_module( module );
			return NULL;
		}
		for( idx = 0; idx < module->num_patterns; idx++ ) {
			if( data_u8( data, offset + 4 ) ) {
				fputs( "Unknown pattern packing type!", stderr );
				dispose_module( module );
				return NULL;
			}
			num_rows = data_u16le( data, offset + 5 );
			pat_data_len = data_u16le( data, offset + 7 );
			offset += data_u32le( data, offset );
			next_offset = offset + pat_data_len;
			num_notes = num_rows * module->num_channels;
			pattern_data = calloc( num_notes, 5 );
			if( !pattern_data ) {
				dispose_module( module );
				return NULL;
			}
			module->patterns[ idx ].num_rows = num_rows;
			module->patterns[ idx ].data = pattern_data;
			if( pat_data_len > 0 ) {
				pat_data_offset = 0;
				for( note = 0; note < num_notes; note++ ) {
					flags = data_u8( data, offset );
					if( ( flags & 0x80 ) == 0 ) flags = 0x1F; else offset++;
					key = ( flags & 0x01 ) > 0 ? data_u8( data, offset++ ) : 0;
					pattern_data[ pat_data_offset++ ] = key;
					ins = ( flags & 0x02 ) > 0 ? data_u8( data, offset++ ) : 0;
					pattern_data[ pat_data_offset++ ] = ins;
					vol = ( flags & 0x04 ) > 0 ? data_u8( data, offset++ ) : 0;
					pattern_data[ pat_data_offset++ ] = vol;
					fxc = ( flags & 0x08 ) > 0 ? data_u8( data, offset++ ) : 0;
					fxp = ( flags & 0x10 ) > 0 ? data_u8( data, offset++ ) : 0;
					if( fxc >= 0x40 ) fxc = fxp = 0;
					pattern_data[ pat_data_offset++ ] = fxc;
					pattern_data[ pat_data_offset++ ] = fxp;
				}
			}
			offset = next_offset;
		}
		module->instruments = calloc( module->num_instruments + 1, sizeof( struct instrument ) );
		if( !module->instruments ) {
			dispose_module( module );
			return NULL;
		}
		for( idx = 1; idx <= module->num_instruments; idx++ ) {
			instrument = &module->instruments[ idx ];
			data_ascii( data, offset + 4, 22, instrument->name );
			instrument->num_samples = data_u16le( data, offset + 27 );
			if( instrument->num_samples ) {
				instrument->samples = calloc( instrument->num_samples, sizeof( struct sample ) );
				if( !instrument->samples ) {
					dispose_module( module );
					return NULL;
				}
				for( key = 0; key < 96; key++ ) {
					instrument->key_to_sample[ key + 1 ] = data_u8( data, offset + 33 + key );
				}
				point_tick = 0;
				for( point = 0; point < 12; point++ ) {
					point_offset = offset + 129 + ( point * 4 );
					point_tick = ( delta_env ? point_tick : 0 ) + data_u16le( data, point_offset );
					instrument->vol_env.points_tick[ point ] = point_tick;
					instrument->vol_env.points_ampl[ point ] = data_u16le( data, point_offset + 2 );
				}
				point_tick = 0;
				for( point = 0; point < 12; point++ ) {
					point_offset = offset + 177 + ( point * 4 );
					point_tick = ( delta_env ? point_tick : 0 ) + data_u16le( data, point_offset );
					instrument->pan_env.points_tick[ point ] = point_tick;
					instrument->pan_env.points_ampl[ point ] = data_u16le( data, point_offset + 2 );
				}
				instrument->vol_env.num_points = data_u8( data, offset + 225 );
				if( instrument->vol_env.num_points > 12 ) {
					instrument->vol_env.num_points = 0;
				}
				instrument->pan_env.num_points = data_u8( data, offset + 226 );
				if( instrument->pan_env.num_points > 12 ) {
					instrument->pan_env.num_points = 0;
				}
				instrument->vol_env.sustain_tick = instrument->vol_env.points_tick[ data_u8( data, offset + 227 ) & 0xF ];
				instrument->vol_env.loop_start_tick = instrument->vol_env.points_tick[ data_u8( data, offset + 228 ) & 0xF ];
				instrument->vol_env.loop_end_tick = instrument->vol_env.points_tick[ data_u8( data, offset + 229 ) & 0xF ];
				instrument->pan_env.sustain_tick = instrument->pan_env.points_tick[ data_u8( data, offset + 230 ) & 0xF ];
				instrument->pan_env.loop_start_tick = instrument->pan_env.points_tick[ data_u8( data, offset + 231 ) & 0xF ];
				instrument->pan_env.loop_end_tick = instrument->pan_env.points_tick[ data_u8( data, offset + 232 ) & 0xF ];
				instrument->vol_env.enabled = instrument->vol_env.num_points > 0 && ( data_u8( data, offset + 233 ) & 0x1 );
				instrument->vol_env.sustain = ( data_u8( data, offset + 233 ) & 0x2 ) > 0;
				instrument->vol_env.looped = ( data_u8( data, offset + 233 ) & 0x4 ) > 0;
				instrument->pan_env.enabled = instrument->pan_env.num_points > 0 && ( data_u8( data, offset + 234 ) & 0x1 );
				instrument->pan_env.sustain = ( data_u8( data, offset + 234 ) & 0x2 ) > 0;
				instrument->pan_env.looped = ( data_u8( data, offset + 234 ) & 0x4 ) > 0;
				instrument->vib_type = data_u8( data, offset + 235 );
				instrument->vib_sweep = data_u8( data, offset + 236 );
				instrument->vib_depth = data_u8( data, offset + 237 );
				instrument->vib_rate = data_u8( data, offset + 238 );
				instrument->vol_fadeout = data_u16le( data, offset + 239 );
			}
			offset += data_u32le( data, offset );
			sam_head_offset = offset;
			offset += instrument->num_samples * 40;
			for( sam = 0; sam < instrument->num_samples; sam++ ) {
				sample = &instrument->samples[ sam ];
				sam_data_bytes = data_u32le( data, sam_head_offset );
				sam_loop_start = data_u32le( data, sam_head_offset + 4 );
				sam_loop_length = data_u32le( data, sam_head_offset + 8 );
				sample->volume = data_u8( data, sam_head_offset + 12 );
				sample->fine_tune = data_s8( data, sam_head_offset + 13 );
				looped = ( data_u8( data, sam_head_offset + 14 ) & 0x3 ) > 0;
				ping_pong = ( data_u8( data, sam_head_offset + 14 ) & 0x2 ) > 0;
				sixteen_bit = ( data_u8( data, sam_head_offset + 14 ) & 0x10 ) > 0;
				sample->panning = data_u8( data, sam_head_offset + 15 );
				sample->rel_note = data_s8( data, sam_head_offset + 16 );
				data_ascii( data, sam_head_offset + 18, 22, sample->name );
				sam_head_offset += 40;
				sam_data_samples = sam_data_bytes;
				if( sixteen_bit ) {
					sam_data_samples = sam_data_samples >> 1;
					sam_loop_start = sam_loop_start >> 1;
					sam_loop_length = sam_loop_length >> 1;
				}
				if( !looped || ( sam_loop_start + sam_loop_length ) > sam_data_samples ) {
					sam_loop_start = sam_data_samples;
					sam_loop_length = 0;
				}
				sample->loop_start = sam_loop_start;
				sample->loop_length = sam_loop_length;
				sample->data = calloc( sam_data_samples, sizeof( short ) );
				if( sample->data ) {
					if( sixteen_bit ) {
						data_sam_s16d( data, offset, sam_data_samples, sample->data );
					} else {
						data_sam_s8d( data, offset, sam_data_samples, sample->data );
					}
					if( ping_pong ) {
						sample_ping_pong( sample );
					}
				} else {
					dispose_module( module );
					return NULL;
				}
				offset += sam_data_bytes;
			}
		}
	}
	return module;
}

static struct module* module_load_s3m( struct data *data ) {
	return NULL;
}

static struct module* module_load_mod( struct data *data ) {
	return NULL;
}

static struct module* module_load( struct data *data ) {
	char ascii[ 16 ];
	struct module* module;
	if( !memcmp( data_ascii( data, 0, 16, ascii ), "Extended Module:", 16 ) ) {
		module = module_load_xm( data );
	} else if( !memcmp( data_ascii( data, 44, 4, ascii ), "SCRM", 4 ) ) {
		module = module_load_s3m( data );
	} else {
		module = module_load_mod( data );
	}
	return module;
}

static struct replay* new_replay( struct module *module, int sample_rate ) {
	struct replay *replay = calloc( 1, sizeof( struct replay ) );
	if( replay ) {
		replay->sample_rate = sample_rate;
	}
	return replay;
}

static void dispose_replay( struct replay *replay ) {
	free( replay );
}

static void replay_set_sequence_pos( struct replay *replay, int pos ) {
}

static int replay_tick( struct replay *replay ) {
	return 1;
}

static int replay_calculate_tick_len( struct replay *replay ) {
	return ( replay->sample_rate * 5 ) / ( replay->tempo * 2 );
}

static void sample_quantize( struct sample *sample, char *output ) {
	int sam, offset = 0;
	int idx = 0, end = sample->loop_start + sample->loop_length;
	short *sample_data = sample->data;
	while( idx < end ) {
		sam = ( sample_data[ idx++ ] + 32768 ) >> 7;
		if( sam < 510 ) {
			sam = ( sam >> 1 ) + ( sam & 1 );
		} else {
			sam = 255;
		}
		output[ offset++ ] = sam - 128;
	}
}

static long read_file( char *file_name, void *buffer ) {
	long file_length = -1, bytes_read;
	FILE *input_file = fopen( file_name, "rb" );
	if( input_file != NULL ) {
		if( fseek( input_file, 0L, SEEK_END ) == 0 ) {
			file_length = ftell( input_file );
			if( file_length >= 0 && buffer ) {
				if( fseek( input_file, 0L, SEEK_SET ) == 0 ) {
					bytes_read = fread( buffer, 1, file_length, input_file ); 
					if( bytes_read != file_length ) {
						file_length = -1;
					}
				} else {
					file_length = -1;
				}
			}
		}
		fclose( input_file );
	}
	if( file_length < 0 ) {
		fputs( strerror( errno ), stderr );
		fputs( "\n", stderr );
	}
	return file_length;
}

static long write_file( char *filename, char *buffer, int length ) {
	long count = -1;
	FILE *file = fopen( filename, "wb" );
	if( file != NULL ) {
		count = fwrite( buffer, 1, length, file );
		fclose( file );
	}
	if( count < length ) {
		fputs( strerror( errno ), stderr );
		fputs( "\n", stderr );
		count = -1;
	}
	return count;
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
	while( freq >= tmf_freq_table[ 96 ] ) {
		octave++;
		freq = freq >> 1;
	}
	while( tmf_freq_table[ tone + 1 ] < freq ) {
		tone++;
	}
	if( tmf_freq_table[ tone + 1 ] - freq <= freq - tmf_freq_table[ tone ] ) {
		tone++;
	}
	return octave * 96 + tone;
}

static int write_sequence( struct replay *replay, char *dest ) {
	int chn, idx = 0, song_end = 0, bpm = 0, wait = 0;
	int inst, swap, sidx, step, d_step, freq;
	int vol, ampl, d_ampl, pan, pann, d_pann;
	replay_set_sequence_pos( replay, 0 );
	while( !song_end ) {
		if( bpm != replay->tempo ) {
			if( dest ) {
				write_int32be( ( replay_calculate_tick_len( replay ) << 15 )
					+ 040000, &dest[ idx ] );
			}
			bpm = replay->tempo;
			idx += 4;
		}
		chn = 0;
		while( chn < replay->num_channels ) {
			inst = replay->channels[ chn ].trig_inst;
			swap = replay->channels[ chn ].swap_inst;
			sidx = replay->channels[ chn ].sample_idx;
			step = replay->channels[ chn ].step;
			d_step = step - replay->channels[ chn ].prev_step;
			freq = ( step * replay->sample_rate ) >> FP_SHIFT;
			ampl = replay->channels[ chn ].ampl;
			d_ampl = ampl - replay->channels[ chn ].prev_ampl;
			pann = replay->channels[ chn ].pann;
			d_pann = pann - replay->channels[ chn ].prev_pann;
			if( inst || swap || d_step || d_ampl || d_pann ) {
				if( wait > 0 ) {
					if( wait > 037777 ) {
						wait = 037777;
					}
					if( dest ) {
						write_int16be( 0140000 + wait, &dest[ idx ] );
					}
					idx += 2;
					wait = 0;
				}
				vol = ampl >> ( FP_SHIFT - 6 );
				pan = 0100 + ( ( pann < 4 ) ? 1 : ( pann >> 2 ) );
				if( d_pann && !d_ampl ) {
					vol = pan;
					d_ampl = 1;
					d_pann = 0;
				}
				if( inst ) {
					/* Trigger Instrument.*/
					if( dest ) {
						write_int32be( ( get_tmf_key( freq ) << 21 ) + ( inst << 15 )
							+ ( vol << 6 ) + chn, &dest[ idx ] );
					}
					idx += 4;
					if( sidx ) {
						/* Set Sample Offset.*/
						if( dest ) {
							write_int32be( ( ( ( sidx >> FP_SHIFT ) & 0177777 ) << 15 )
								+ 020000 + ( vol << 6 ) + chn, &dest[ idx ] );
						}
						idx += 4;
					}
				} else if( swap ) {
					/* Switch Instrument.*/
					if( dest ) {
						write_int32be( ( swap << 15 )
							+ ( vol << 6 ) + chn, &dest[ idx ] );
					}
					idx += 4;
					if( d_step ) {
						/* Modulate Pitch.*/
						if( dest ) {
							write_int32be( ( get_tmf_key( freq ) << 21 )
								+ ( vol << 6 ) + chn, &dest[ idx ] );
						}
						idx += 4;
					}
				} else if( d_step ) {
					/* Modulate Pitch.*/
					if( dest ) {
						write_int32be( ( get_tmf_key( freq ) << 21 )
							+ ( vol << 6 ) + chn, &dest[ idx ] );
					}
					idx += 4;
				} else if( d_ampl ) {
					/* Modulate Vol.*/
					if( dest ) {
						write_int16be( 0100000 + ( vol << 6 ) + chn, &dest[ idx ] );
					}
					idx += 2;
				}
				if( d_pann ) {
					/* Modulate Pan.*/
					if( dest ) {
						write_int16be( 0100000 + ( pan << 6 ) + chn, &dest[ idx ] );
					}
					idx += 2;
				}
			}
			chn++;
		}
		wait++;
		song_end = replay_tick( replay );
	}
	return idx;
}

static int xm_to_tmf( struct module *module, char *tmf ) {
	int seqlen, idx, loop_start, loop_length, length = -1;
	struct instrument *instrument;
	struct sample *sample;
	if( module->num_instruments > 63 ) {
		fputs( "Module has too many instruments.", stderr );
	} else {
		struct replay *replay = new_replay( module, 48000 );
		if( replay ) {
			length = 32 * 64;
			seqlen = write_sequence( replay, NULL );
			if( tmf ) {
				printf( "Sequence length: %d bytes.\n", seqlen );
				memset( tmf, 0, length );
				strcpy( tmf, "TMF0" );
				write_int32be( seqlen, &tmf[ 4 ] );
				memcpy( &tmf[ 8 ], module->name, 24 );
				write_sequence( replay, &tmf[ length ] );
			}
			length = length + seqlen;
			idx = 1;
			while( idx <= module->num_instruments ) {
				instrument = &module->instruments[ idx ];
				sample = &instrument->samples[ 0 ];
				loop_start = sample->loop_start;
				loop_length = sample->loop_length;
				if( tmf ) {
					write_int32be( loop_start, &tmf[ idx * 32 ] );
					write_int32be( loop_length, &tmf[ idx * 32 + 4 ] );
					memcpy( &tmf[ idx * 32 + 8 ], instrument->name, 24 );
					sample_quantize( sample, &tmf[ length ] );
				}
				length = length + loop_start + loop_length;
				idx++;
			}
			dispose_replay( replay );
		}
	}
	return length;
}

int main( int argc, char **argv ) {
	int result, length;
	char *input, *output;
	struct data data;
	struct module *module;
	result = EXIT_FAILURE;
	if( argc != 3 ) {
		fprintf( stderr, "%s\nUsage: %s input.xm output.tmf\n", VERSION, argv[ 0 ] );
	} else {
		/* Read module file.*/
		length = read_file( argv[ 1 ], NULL );
		if( length >= 0 ) {
			printf( "Module Data Length: %i bytes.\n", length );
			input = calloc( length, 1 );
			if( input != NULL ) {
				if( read_file( argv[ 1 ], input ) >= 0 ) {
					data.buffer = ( signed char * ) input;
					data.length = length;
					module = module_load( &data );
					if( module ) {
						/* Perform conversion. */
						length = xm_to_tmf( module, NULL );
						if( length > 0 ) {
							output = calloc( length, 1 );
							if( output != NULL ) {
								xm_to_tmf( module, output );
								if( write_file( argv[ 2 ], output, length ) > 0 ) {
									result = EXIT_SUCCESS;
								}
								free( output );
							}
						}
						dispose_module( module );
					}
				}
				free( input );
			}
		}
	}
	return result;
}
