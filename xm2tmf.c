
#include "errno.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"

static const char *VERSION = "MOD/S3M/XM to TMF converter (c)2017 mumart@gmail.com";

static const int FP_SHIFT = 15, FP_ONE = 32768, FP_MASK = 32767;

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

static const int exp2_table[] = {
	32768, 32946, 33125, 33305, 33486, 33667, 33850, 34034,
	34219, 34405, 34591, 34779, 34968, 35158, 35349, 35541,
	35734, 35928, 36123, 36319, 36516, 36715, 36914, 37114,
	37316, 37518, 37722, 37927, 38133, 38340, 38548, 38757,
	38968, 39180, 39392, 39606, 39821, 40037, 40255, 40473,
	40693, 40914, 41136, 41360, 41584, 41810, 42037, 42265,
	42495, 42726, 42958, 43191, 43425, 43661, 43898, 44137,
	44376, 44617, 44859, 45103, 45348, 45594, 45842, 46091,
	46341, 46593, 46846, 47100, 47356, 47613, 47871, 48131,
	48393, 48655, 48920, 49185, 49452, 49721, 49991, 50262,
	50535, 50810, 51085, 51363, 51642, 51922, 52204, 52488,
	52773, 53059, 53347, 53637, 53928, 54221, 54515, 54811,
	55109, 55408, 55709, 56012, 56316, 56622, 56929, 57238,
	57549, 57861, 58176, 58491, 58809, 59128, 59449, 59772,
	60097, 60423, 60751, 61081, 61413, 61746, 62081, 62419,
	62757, 63098, 63441, 63785, 64132, 64480, 64830, 65182,
	65536
};

static const short sine_table[] = {
	  0,  24,  49,  74,  97, 120, 141, 161, 180, 197, 212, 224, 235, 244, 250, 253,
	255, 253, 250, 244, 235, 224, 212, 197, 180, 161, 141, 120,  97,  74,  49,  24
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
	int num_channels, num_rows;
	char *data;
};

struct module {
	char name[ 24 ];
	int num_channels, num_instruments;
	int num_patterns, sequence_len, restart_pos;
	int default_gvol, default_speed, default_tempo, c2_rate, gain;
	int linear_periods, fast_vol_slides;
	unsigned char *default_panning, *sequence;
	struct pattern *patterns;
	struct instrument *instruments;
};

struct note {
	unsigned char key, instrument, volume, effect, param;
};

struct channel {
	struct replay *replay;
	struct instrument *instrument;
	struct sample *sample;
	struct note note;
	int id, key_on, random_seed, pl_row;
	int sample_off, sample_idx, sample_fra, freq, ampl, pann;
	int volume, panning, fadeout_vol, vol_env_tick, pan_env_tick;
	int period, porta_period, retrig_count, fx_count, av_count;
	int porta_up_param, porta_down_param, tone_porta_param, offset_param;
	int fine_porta_up_param, fine_porta_down_param, xfine_porta_param;
	int arpeggio_param, vol_slide_param, gvol_slide_param, pan_slide_param;
	int fine_vslide_up_param, fine_vslide_down_param;
	int retrig_volume, retrig_ticks, tremor_on_ticks, tremor_off_ticks;
	int vibrato_type, vibrato_phase, vibrato_speed, vibrato_depth;
	int tremolo_type, tremolo_phase, tremolo_speed, tremolo_depth;
	int tremolo_add, vibrato_add, arpeggio_add;
	int inst_idx, trig_inst, swap_inst, prev_freq, prev_ampl, prev_pann;
};

struct replay {
	int sample_rate, global_vol;
	int seq_pos, break_pos, row, next_row, tick;
	int speed, tempo, pl_count, pl_chan;
	struct channel *channels;
	struct module *module;
};

static int exp2( int x ) {
	int c, m, y;
	int x0 = ( x & FP_MASK ) >> ( FP_SHIFT - 7 );
	c = exp2_table[ x0 ];
	m = exp2_table[ x0 + 1 ] - c;
	y = ( m * ( x & ( FP_MASK >> 7 ) ) >> 8 ) + c;
	return ( y << FP_SHIFT ) >> ( FP_SHIFT - ( x >> FP_SHIFT ) );
}

static int log2( int x ) {
	int step;
	int y = 16 << FP_SHIFT;
	for( step = y; step > 0; step >>= 1 ) {
		if( exp2( y - step ) >= x ) {
			y -= step;
		}
	}
	return y;
}

static char* data_ascii( struct data *data, int offset, int length, char *dest ) {
	int idx, chr;
	if( offset > data->length ) {
		offset = data->length;
	}
	if( offset + length > data->length ) {
		length = data->length - offset;
	}
	for( idx = 0; idx < length; idx++ ) {
		chr = data->buffer[ offset + idx ];
		if( chr < 32 ) {
			dest[ idx ] = 32;
		} else {
			dest[ idx ] = chr;
		}
	}
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
	int idx, length = data->length, sam = 0;
	signed char *buffer = data->buffer;
	if( offset > length ) {
		offset = length;
	}
	if( offset + count * 2 > length ) {
		count = ( length - offset ) / 2;
	}
	for( idx = 0; idx < count; idx++ ) {
		sam += ( buffer[ offset + idx * 2 ] & 0xFF ) | ( buffer[ offset + idx * 2 + 1 ] << 8 );
		dest[ idx ] = sam;
	}
}

static int envelope_next_tick( struct envelope *envelope, int tick, int key_on ) {
	tick++;
	if( envelope->looped && tick >= envelope->loop_end_tick ) {
		tick = envelope->loop_start_tick;
	}
	if( envelope->sustain && key_on && tick >= envelope->sustain_tick ) {
		tick = envelope->sustain_tick;
	}
	return tick;
}
	
static int envelope_calculate_ampl( struct envelope *envelope, int tick ) {
	int idx, point, dt, da;
	int ampl = envelope->points_ampl[ envelope->num_points - 1 ];
	if( tick < envelope->points_tick[ envelope->num_points - 1 ] ) {
		point = 0;
		for( idx = 1; idx < envelope->num_points; idx++ ) {
			if( envelope->points_tick[ idx ] <= tick ) {
				point = idx;
			}
		}
		dt = envelope->points_tick[ point + 1 ] - envelope->points_tick[ point ];
		da = envelope->points_ampl[ point + 1 ] - envelope->points_ampl[ point ];
		ampl = envelope->points_ampl[ point ];
		ampl += ( ( da << 24 ) / dt ) * ( tick - envelope->points_tick[ point ] ) >> 24;
	}
	return ampl;
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
	free( module->default_panning );
	free( module->sequence );
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
		module->default_panning = calloc( module->num_channels, sizeof( unsigned char ) );
		if( !module->default_panning ) {
			dispose_module( module );
			return NULL;
		}
		for( idx = 0; idx < module->num_channels; idx++ ) {
			module->default_panning[ idx ] = 128;
		}
		module->sequence = calloc( module->sequence_len, sizeof( unsigned char ) );
		if( !module->sequence ) {
			dispose_module( module );
			return NULL;
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
			module->patterns[ idx ].num_channels = module->num_channels;
			module->patterns[ idx ].num_rows = num_rows;
			module->patterns[ idx ].data = pattern_data;
			if( pat_data_len > 0 ) {
				pat_data_offset = 0;
				for( note = 0; note < num_notes; note++ ) {
					flags = data_u8( data, offset );
					if( ( flags & 0x80 ) == 0 ) {
						flags = 0x1F;
					} else {
						offset++;
					}
					key = ( flags & 0x01 ) > 0 ? data_u8( data, offset++ ) : 0;
					pattern_data[ pat_data_offset++ ] = key;
					ins = ( flags & 0x02 ) > 0 ? data_u8( data, offset++ ) : 0;
					pattern_data[ pat_data_offset++ ] = ins;
					vol = ( flags & 0x04 ) > 0 ? data_u8( data, offset++ ) : 0;
					pattern_data[ pat_data_offset++ ] = vol;
					fxc = ( flags & 0x08 ) > 0 ? data_u8( data, offset++ ) : 0;
					fxp = ( flags & 0x10 ) > 0 ? data_u8( data, offset++ ) : 0;
					if( fxc >= 0x40 ) {
						fxc = fxp = 0;
					}
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
		instrument = &module->instruments[ 0 ];
		instrument->samples = calloc( 1, sizeof( struct sample ) );
		if( !instrument->samples ) {
			dispose_module( module );
			return NULL;
		}
		for( idx = 1; idx <= module->num_instruments; idx++ ) {
			instrument = &module->instruments[ idx ];
			data_ascii( data, offset + 4, 22, instrument->name );
			instrument->num_samples = data_u16le( data, offset + 27 );
			if( instrument->num_samples ) {
				instrument->samples = calloc( instrument->num_samples, sizeof( struct sample ) );
			} else {
				instrument->samples = calloc( 1, sizeof( struct sample ) );
			}
			if( !instrument->samples ) {
				dispose_module( module );
				return NULL;
			}
			instrument->samples[ 0 ].panning = -1;
			if( instrument->num_samples ) {
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

static void pattern_get_note( struct pattern *pattern, int row, int chan, struct note *dest ) {
	int offset = ( row * pattern->num_channels + chan ) * 5;
	if( offset >= 0 && row < pattern->num_rows && chan < pattern->num_channels ) {
		dest->key = pattern->data[ offset ];
		dest->instrument = pattern->data[ offset + 1 ];
		dest->volume = pattern->data[ offset + 2 ];
		dest->effect = pattern->data[ offset + 3 ];
		dest->param = pattern->data[ offset + 4 ];
	} else {
		memset( dest, 0, sizeof( struct note ) );
	}
}

static void channel_init( struct channel *channel, struct replay *replay, int idx ) {
	memset( channel, 0, sizeof( struct channel ) );
	channel->replay = replay;
	channel->id = idx;
	channel->panning = replay->module->default_panning[ idx ];
	channel->instrument = &replay->module->instruments[ 0 ];
	channel->sample = &channel->instrument->samples[ 0 ];
	channel->random_seed = ( idx + 1 ) * 0xABCDEF;
}

static void channel_volume_slide( struct channel *channel ) {
	int up = channel->vol_slide_param >> 4;
	int down = channel->vol_slide_param & 0xF;
	if( down == 0xF && up > 0 ) {
		/* Fine slide up.*/
		if( channel->fx_count == 0 ) {
			channel->volume += up;
		}
	} else if( up == 0xF && down > 0 ) {
		/* Fine slide down.*/
		if( channel->fx_count == 0 ) {
			channel->volume -= down;
		}
	} else if( channel->fx_count > 0 || channel->replay->module->fast_vol_slides ) {
		/* Normal.*/
		channel->volume += up - down;
	}
	if( channel->volume > 64 ) {
		channel->volume = 64;
	}
	if( channel->volume < 0 ) {
		channel->volume = 0;
	}
}

static void channel_porta_up( struct channel *channel, int param ) {
	switch( param & 0xF0 ) {
		case 0xE0: /* Extra-fine porta.*/
			if( channel->fx_count == 0 ) {
				channel->period -= param & 0xF;
			}
			break;
		case 0xF0: /* Fine porta.*/
			if( channel->fx_count == 0 ) {
				channel->period -= ( param & 0xF ) << 2;
			}
			break;
		default:/* Normal porta.*/
			if( channel->fx_count > 0 ) {
				channel->period -= param << 2;
			}
			break;
	}
	if( channel->period < 0 ) {
		channel->period = 0;
	}
}

static void channel_porta_down( struct channel *channel, int param ) {
	if( channel->period > 0 ) {
		switch( param & 0xF0 ) {
			case 0xE0: /* Extra-fine porta.*/
				if( channel->fx_count == 0 ) {
					channel->period += param & 0xF;
				}
				break;
			case 0xF0: /* Fine porta.*/
				if( channel->fx_count == 0 ) {
					channel->period += ( param & 0xF ) << 2;
				}
				break;
			default:/* Normal porta.*/
				if( channel->fx_count > 0 ) {
					channel->period += param << 2;
				}
				break;
		}
		if( channel->period > 65535 ) {
			channel->period = 65535;
		}
	}
}

static void channel_tone_porta( struct channel *channel ) {
	if( channel->period > 0 ) {
		if( channel->period < channel->porta_period ) {
			channel->period += channel->tone_porta_param << 2;
			if( channel->period > channel->porta_period ) {
				channel->period = channel->porta_period;
			}
		} else {
			channel->period -= channel->tone_porta_param << 2;
			if( channel->period < channel->porta_period ) {
				channel->period = channel->porta_period;
			}
		}
	}
}

static int channel_waveform( struct channel *channel, int phase, int type ) {
	int amplitude = 0;
	switch( type ) {
		default: /* Sine. */
			amplitude = sine_table[ phase & 0x1F ];
			if( ( phase & 0x20 ) > 0 ) {
				amplitude = -amplitude;
			}
			break;
		case 6: /* Saw Up.*/
			amplitude = ( ( ( phase + 0x20 ) & 0x3F ) << 3 ) - 255;
			break;
		case 1: case 7: /* Saw Down. */
			amplitude = 255 - ( ( ( phase + 0x20 ) & 0x3F ) << 3 );
			break;
		case 2: case 5: /* Square. */
			amplitude = ( phase & 0x20 ) > 0 ? 255 : -255;
			break;
		case 3: case 8: /* Random. */
			amplitude = ( channel->random_seed >> 20 ) - 255;
			channel->random_seed = ( channel->random_seed * 65 + 17 ) & 0x1FFFFFFF;
			break;
	}
	return amplitude;
}

static void channel_vibrato( struct channel *channel, int fine ) {
	int wave = channel_waveform( channel, channel->vibrato_phase, channel->vibrato_type & 0x3 );
	channel->vibrato_add = wave * channel->vibrato_depth >> ( fine ? 7 : 5 );
}

static void channel_tremolo( struct channel *channel ) {
	int wave = channel_waveform( channel, channel->tremolo_phase, channel->tremolo_type & 0x3 );
	channel->tremolo_add = wave * channel->tremolo_depth >> 6;
}

static void channel_tremor( struct channel *channel ) {
	if( channel->retrig_count >= channel->tremor_on_ticks ) {
		channel->tremolo_add = -64;
	}
	if( channel->retrig_count >= ( channel->tremor_on_ticks + channel->tremor_off_ticks ) ) {
		channel->tremolo_add = channel->retrig_count = 0;
	}
}

static void channel_retrig_vol_slide( struct channel *channel ) {
	if( channel->retrig_count >= channel->retrig_ticks ) {
		channel->trig_inst = channel->inst_idx;
		channel->retrig_count = channel->sample_idx = channel->sample_fra = 0;
		switch( channel->retrig_volume ) {
			case 0x1: channel->volume = channel->volume -  1; break;
			case 0x2: channel->volume = channel->volume -  2; break;
			case 0x3: channel->volume = channel->volume -  4; break;
			case 0x4: channel->volume = channel->volume -  8; break;
			case 0x5: channel->volume = channel->volume - 16; break;
			case 0x6: channel->volume = channel->volume * 2 / 3; break;
			case 0x7: channel->volume = channel->volume >> 1; break;
			case 0x8: /* ? */ break;
			case 0x9: channel->volume = channel->volume +  1; break;
			case 0xA: channel->volume = channel->volume +  2; break;
			case 0xB: channel->volume = channel->volume +  4; break;
			case 0xC: channel->volume = channel->volume +  8; break;
			case 0xD: channel->volume = channel->volume + 16; break;
			case 0xE: channel->volume = channel->volume * 3 / 2; break;
			case 0xF: channel->volume = channel->volume << 1; break;
		}
		if( channel->volume <  0 ) {
			channel->volume = 0;
		}
		if( channel->volume > 64 ) {
			channel->volume = 64;
		}
	}
}

static void channel_trigger( struct channel *channel ) {
	int key, sam, porta, period, fine_tune, ins = channel->note.instrument;
	struct sample *sample;
	if( ins > 0 && ins <= channel->replay->module->num_instruments ) {
		channel->inst_idx = ins;
		channel->instrument = &channel->replay->module->instruments[ ins ];
		key = channel->note.key < 97 ? channel->note.key : 0;
		sam = channel->instrument->key_to_sample[ key ];
		sample = &channel->instrument->samples[ sam ];
		channel->volume = sample->volume >= 64 ? 64 : sample->volume & 0x3F;
		if( sample->panning >= 0 ) {
			channel->panning = sample->panning & 0xFF;
		}
		if( channel->period > 0 && sample->loop_length > 1 ) {
			/* Amiga trigger.*/
			channel->sample = sample;
			channel->swap_inst = ins;
		}
		channel->sample_off = 0;
		channel->vol_env_tick = channel->pan_env_tick = 0;
		channel->fadeout_vol = 32768;
		channel->key_on = 1;
	}
	if( channel->note.effect == 0x09 || channel->note.effect == 0x8F ) {
		/* Set Sample Offset. */
		if( channel->note.param > 0 ) {
			channel->offset_param = channel->note.param;
		}
		channel->sample_off = channel->offset_param << 8;
	}
	if( channel->note.volume >= 0x10 && channel->note.volume < 0x60 ) {
		channel->volume = channel->note.volume < 0x50 ? channel->note.volume - 0x10 : 64;
	}
	switch( channel->note.volume & 0xF0 ) {
		case 0x80: /* Fine Vol Down.*/
			channel->volume -= channel->note.volume & 0xF;
			if( channel->volume < 0 ) {
				channel->volume = 0;
			}
			break;
		case 0x90: /* Fine Vol Up.*/
			channel->volume += channel->note.volume & 0xF;
			if( channel->volume > 64 ) {
				channel->volume = 64;
			}
			break;
		case 0xA0: /* Set Vibrato Speed.*/
			if( ( channel->note.volume & 0xF ) > 0 ) {
				channel->vibrato_speed = channel->note.volume & 0xF;
			}
			break;
		case 0xB0: /* Vibrato.*/
			if( ( channel->note.volume & 0xF ) > 0 ) {
				channel->vibrato_depth = channel->note.volume & 0xF;
			}
			channel_vibrato( channel, 0 );
			break;
		case 0xC0: /* Set Panning.*/
			channel->panning = ( channel->note.volume & 0xF ) * 17;
			break;
		case 0xF0: /* Tone Porta.*/
			if( ( channel->note.volume & 0xF ) > 0 ) {
				channel->tone_porta_param = channel->note.volume & 0xF;
			}
			break;
	}
	if( channel->note.key > 0 ) {
		if( channel->note.key > 96 ) {
			channel->key_on = 0;
		} else {
			porta = ( channel->note.volume & 0xF0 ) == 0xF0 ||
				channel->note.effect == 0x03 || channel->note.effect == 0x05 ||
				channel->note.effect == 0x87 || channel->note.effect == 0x8C;
			if( !porta ) {
				ins = channel->instrument->key_to_sample[ channel->note.key ];
				channel->sample = &channel->instrument->samples[ ins ];
			}
			fine_tune = channel->sample->fine_tune;
			if( channel->note.effect == 0x75 || channel->note.effect == 0xF2 ) {
				/* Set Fine Tune. */
				fine_tune = ( ( channel->note.param & 0xF ) << 4 ) - 128;
			}
			key = channel->note.key + channel->sample->rel_note;
			if( key < 1 ) {
				key = 1;
			}
			if( key > 120 ) {
				key = 120;
			}
			period = ( key << 6 ) + ( fine_tune >> 1 );
			if( channel->replay->module->linear_periods ) {
				channel->porta_period = 7744 - period;
			} else {
				channel->porta_period = 29021 * exp2( ( period << FP_SHIFT ) / -768 ) >> FP_SHIFT;
			}
			if( !porta ) {
				channel->period = channel->porta_period;
				channel->sample_idx = channel->sample_off;
				channel->sample_fra = 0;
				if( channel->vibrato_type < 4 ) {
					channel->vibrato_phase = 0;
				}
				if( channel->tremolo_type < 4 ) {
					channel->tremolo_phase = 0;
				}
				channel->retrig_count = channel->av_count = 0;
				channel->trig_inst = channel->inst_idx;
			}
		}
	}
}

static void channel_update_envelopes( struct channel *channel ) {
	if( channel->instrument->vol_env.enabled ) {
		if( !channel->key_on ) {
			channel->fadeout_vol -= channel->instrument->vol_fadeout;
			if( channel->fadeout_vol < 0 ) {
				channel->fadeout_vol = 0;
			}
		}
		channel->vol_env_tick = envelope_next_tick( &channel->instrument->vol_env,
			channel->vol_env_tick, channel->key_on );
	}
	if( channel->instrument->pan_env.enabled ) {
		channel->pan_env_tick = envelope_next_tick( &channel->instrument->pan_env,
			channel->pan_env_tick, channel->key_on );
	}
}

static void channel_auto_vibrato( struct channel *channel ) {
	int sweep, rate, type, wave;
	int depth = channel->instrument->vib_depth & 0x7F;
	if( depth > 0 ) {
		sweep = channel->instrument->vib_sweep & 0x7F;
		rate = channel->instrument->vib_rate & 0x7F;
		type = channel->instrument->vib_type;
		if( channel->av_count < sweep ) {
			depth = depth * channel->av_count / sweep;
		}
		wave = channel_waveform( channel, channel->av_count * rate >> 2, type + 4 );
		channel->vibrato_add += wave * depth >> 8;
		channel->av_count++;
	}
}

static void channel_calculate_freq( struct channel *channel ) {
	int per = channel->period + channel->vibrato_add;
	if( channel->replay->module->linear_periods ) {
		per = per - ( channel->arpeggio_add << 6 );
		if( per < 28 || per > 7680 ) {
			per = 7680;
		}
		channel->freq = ( ( channel->replay->module->c2_rate >> 4 )
			* exp2( ( ( 4608 - per ) << FP_SHIFT ) / 768 ) ) >> ( FP_SHIFT - 4 );
	} else {
		if( per > 29021 ) {
			per = 29021;
		}
		per = ( per << FP_SHIFT ) / exp2( ( channel->arpeggio_add << FP_SHIFT ) / 12 );
		if( per < 28 ) {
			per = 29021;
		}
		channel->freq = channel->replay->module->c2_rate * 1712 / per;
	}
}

static void channel_calculate_ampl( struct channel *channel ) {
	int vol, range, env_pan = 32, env_vol = channel->key_on ? 64 : 0;
	if( channel->instrument->vol_env.enabled ) {
		env_vol = envelope_calculate_ampl( &channel->instrument->vol_env, channel->vol_env_tick );
	}
	vol = channel->volume + channel->tremolo_add;
	if( vol > 64 ) {
		vol = 64;
	}
	if( vol < 0 ) {
		vol = 0;
	}
	vol = ( vol * channel->replay->module->gain * FP_ONE ) >> 13;
	vol = ( vol * channel->fadeout_vol ) >> 15;
	channel->ampl = ( vol * channel->replay->global_vol * env_vol ) >> 12;
	if( channel->instrument->pan_env.enabled ) {
		env_pan = envelope_calculate_ampl( &channel->instrument->pan_env, channel->pan_env_tick );
	}
	range = ( channel->panning < 128 ) ? channel->panning : ( 255 - channel->panning );
	channel->pann = channel->panning + ( range * ( env_pan - 32 ) >> 5 );
}

static void channel_tick( struct channel *channel ) {
	channel->trig_inst = channel->swap_inst = 0;
	channel->prev_freq = channel->freq;
	channel->prev_ampl = channel->ampl;
	channel->prev_pann = channel->pann;
	channel->vibrato_add = 0;
	channel->fx_count++;
	channel->retrig_count++;
	if( !( channel->note.effect == 0x7D && channel->fx_count <= channel->note.param ) ) {
		switch( channel->note.volume & 0xF0 ) {
			case 0x60: /* Vol Slide Down.*/
				channel->volume -= channel->note.volume & 0xF;
				if( channel->volume < 0 ) {
					channel->volume = 0;
				}
				break;
			case 0x70: /* Vol Slide Up.*/
				channel->volume += channel->note.volume & 0xF;
				if( channel->volume > 64 ) {
					channel->volume = 64;
				}
				break;
			case 0xB0: /* Vibrato.*/
				channel->vibrato_phase += channel->vibrato_speed;
				channel_vibrato( channel, 0 );
				break;
			case 0xD0: /* Pan Slide Left.*/
				channel->panning -= channel->note.volume & 0xF;
				if( channel->panning < 0 ) {
					channel->panning = 0;
				}
				break;
			case 0xE0: /* Pan Slide Right.*/
				channel->panning += channel->note.volume & 0xF;
				if( channel->panning > 255 ) {
					channel->panning = 255;
				}
				break;
			case 0xF0: /* Tone Porta.*/
				channel_tone_porta( channel );
				break;
		}
	}
	switch( channel->note.effect ) {
		case 0x01: case 0x86: /* Porta Up. */
			channel_porta_up( channel, channel->porta_up_param );
			break;
		case 0x02: case 0x85: /* Porta Down. */
			channel_porta_down( channel, channel->porta_down_param );
			break;
		case 0x03: case 0x87: /* Tone Porta. */
			channel_tone_porta( channel );
			break;
		case 0x04: case 0x88: /* Vibrato. */
			channel->vibrato_phase += channel->vibrato_speed;
			channel_vibrato( channel, 0 );
			break;
		case 0x05: case 0x8C: /* Tone Porta + Vol Slide. */
			channel_tone_porta( channel );
			channel_volume_slide( channel );
			break;
		case 0x06: case 0x8B: /* Vibrato + Vol Slide. */
			channel->vibrato_phase += channel->vibrato_speed;
			channel_vibrato( channel, 0 );
			channel_volume_slide( channel );
			break;
		case 0x07: case 0x92: /* Tremolo. */
			channel->tremolo_phase += channel->tremolo_speed;
			channel_tremolo( channel );
			break;
		case 0x0A: case 0x84: /* Vol Slide. */
			channel_volume_slide( channel );
			break;
		case 0x11: /* Global Volume Slide. */
			channel->replay->global_vol = channel->replay->global_vol
				+ ( channel->gvol_slide_param >> 4 )
				- ( channel->gvol_slide_param & 0xF );
			if( channel->replay->global_vol < 0 ) {
				channel->replay->global_vol = 0;
			}
			if( channel->replay->global_vol > 64 ) {
				channel->replay->global_vol = 64;
			}
			break;
		case 0x19: /* Panning Slide. */
			channel->panning = channel->panning
				+ ( channel->pan_slide_param >> 4 )
				- ( channel->pan_slide_param & 0xF );
			if( channel->panning < 0 ) {
				channel->panning = 0;
			}
			if( channel->panning > 255 ) {
				channel->panning = 255;
			}
			break;
		case 0x1B: case 0x91: /* Retrig + Vol Slide. */
			channel_retrig_vol_slide( channel );
			break;
		case 0x1D: case 0x89: /* Tremor. */
			channel_tremor( channel );
			break;
		case 0x79: /* Retrig. */
			if( channel->fx_count >= channel->note.param ) {
				channel->fx_count = 0;
				channel->sample_idx = channel->sample_fra = 0;
				channel->trig_inst = channel->inst_idx;
			}
			break;
		case 0x7C: case 0xFC: /* Note Cut. */
			if( channel->note.param == channel->fx_count ) {
				channel->volume = 0;
			}
			break;
		case 0x7D: case 0xFD: /* Note Delay. */
			if( channel->note.param == channel->fx_count ) {
				channel_trigger( channel );
			}
			break;
		case 0x8A: /* Arpeggio. */
			if( channel->fx_count == 1 ) {
				channel->arpeggio_add = channel->arpeggio_param >> 4;
			} else if( channel->fx_count == 2 ) {
				channel->arpeggio_add = channel->arpeggio_param & 0xF;
			} else {
				channel->arpeggio_add = channel->fx_count = 0;
			}
			break;
		case 0x95: /* Fine Vibrato. */
			channel->vibrato_phase += channel->vibrato_speed;
			channel_vibrato( channel, 1 );
			break;
	}
	channel_auto_vibrato( channel );
	channel_calculate_freq( channel );
	channel_calculate_ampl( channel );
	channel_update_envelopes( channel );
}

static void channel_row( struct channel *channel, struct note *note ) {
	channel->note = *note;
	channel->trig_inst = channel->swap_inst = 0;
	channel->prev_freq = channel->freq;
	channel->prev_ampl = channel->ampl;
	channel->prev_pann = channel->pann;
	channel->retrig_count++;
	channel->vibrato_add = channel->tremolo_add = channel->arpeggio_add = channel->fx_count = 0;
	if( !( ( note->effect == 0x7D || note->effect == 0xFD ) && note->param > 0 ) ) {
		/* Not note delay.*/
		channel_trigger( channel );
	}
	switch( channel->note.effect ) {
		case 0x01: case 0x86: /* Porta Up. */
			if( channel->note.param > 0 ) {
				channel->porta_up_param = channel->note.param;
			}
			channel_porta_up( channel, channel->porta_up_param );
			break;
		case 0x02: case 0x85: /* Porta Down. */
			if( channel->note.param > 0 ) {
				channel->porta_down_param = channel->note.param;
			}
			channel_porta_down( channel, channel->porta_down_param );
			break;
		case 0x03: case 0x87: /* Tone Porta. */
			if( channel->note.param > 0 ) {
				channel->tone_porta_param = channel->note.param;
			}
			break;
		case 0x04: case 0x88: /* Vibrato. */
			if( ( channel->note.param >> 4 ) > 0 ) {
				channel->vibrato_speed = channel->note.param >> 4;
			}
			if( ( channel->note.param & 0xF ) > 0 ) {
				channel->vibrato_depth = channel->note.param & 0xF;
			}
			channel_vibrato( channel, 0 );
			break;
		case 0x05: case 0x8C: /* Tone Porta + Vol Slide. */
			if( channel->note.param > 0 ) {
				channel->vol_slide_param = channel->note.param;
			}
			channel_volume_slide( channel );
			break;
		case 0x06: case 0x8B: /* Vibrato + Vol Slide. */
			if( channel->note.param > 0 ) {
				channel->vol_slide_param = channel->note.param;
			}
			channel_vibrato( channel, 0 );
			channel_volume_slide( channel );
			break;
		case 0x07: case 0x92: /* Tremolo. */
			if( ( channel->note.param >> 4 ) > 0 ) {
				channel->tremolo_speed = channel->note.param >> 4;
			}
			if( ( channel->note.param & 0xF ) > 0 ) {
				channel->tremolo_depth = channel->note.param & 0xF;
			}
			channel_tremolo( channel );
			break;
		case 0x08: /* Set Panning.*/
			channel->panning = ( channel->note.param < 128 ) ? ( channel->note.param << 1 ) : 255;
			break;
		case 0x0A: case 0x84: /* Vol Slide. */
			if( channel->note.param > 0 ) {
				channel->vol_slide_param = channel->note.param;
			}
			channel_volume_slide( channel );
			break;
		case 0x0C: /* Set Volume. */
			channel->volume = channel->note.param >= 64 ? 64 : channel->note.param & 0x3F;
			break;
		case 0x10: case 0x96: /* Set Global Volume. */
			channel->replay->global_vol = channel->note.param >= 64 ? 64 : channel->note.param & 0x3F;
			break;
		case 0x11: /* Global Volume Slide. */
			if( channel->note.param > 0 ) {
				channel->gvol_slide_param = channel->note.param;
			}
			break;
		case 0x14: /* Key Off. */
			channel->key_on = 0;
			break;
		case 0x15: /* Set Envelope Tick. */
			channel->vol_env_tick = channel->pan_env_tick = channel->note.param & 0xFF;
			break;
		case 0x19: /* Panning Slide. */
			if( channel->note.param > 0 ) {
				channel->pan_slide_param = channel->note.param;
			}
			break;
		case 0x1B: case 0x91: /* Retrig + Vol Slide. */
			if( ( channel->note.param >> 4 ) > 0 ) {
				channel->retrig_volume = channel->note.param >> 4;
			}
			if( ( channel->note.param & 0xF ) > 0 ) {
				channel->retrig_ticks = channel->note.param & 0xF;
			}
			channel_retrig_vol_slide( channel );
			break;
		case 0x1D: case 0x89: /* Tremor. */
			if( ( channel->note.param >> 4 ) > 0 ) {
				channel->tremor_on_ticks = channel->note.param >> 4;
			}
			if( ( channel->note.param & 0xF ) > 0 ) {
				channel->tremor_off_ticks = channel->note.param & 0xF;
			}
			channel_tremor( channel );
			break;
		case 0x21: /* Extra Fine Porta. */
			if( channel->note.param > 0 ) {
				channel->xfine_porta_param = channel->note.param;
			}
			switch( channel->xfine_porta_param & 0xF0 ) {
				case 0x10:
					channel_porta_up( channel, 0xE0 | ( channel->xfine_porta_param & 0xF ) );
					break;
				case 0x20:
					channel_porta_down( channel, 0xE0 | ( channel->xfine_porta_param & 0xF ) );
					break;
			}
			break;
		case 0x71: /* Fine Porta Up. */
			if( channel->note.param > 0 ) {
				channel->fine_porta_up_param = channel->note.param;
			}
			channel_porta_up( channel, 0xF0 | ( channel->fine_porta_up_param & 0xF ) );
			break;
		case 0x72: /* Fine Porta Down. */
			if( channel->note.param > 0 ) {
				channel->fine_porta_down_param = channel->note.param;
			}
			channel_porta_down( channel, 0xF0 | ( channel->fine_porta_down_param & 0xF ) );
			break;
		case 0x74: case 0xF3: /* Set Vibrato Waveform. */
			if( channel->note.param < 8 ) {
				channel->vibrato_type = channel->note.param;
			}
			break;
		case 0x77: case 0xF4: /* Set Tremolo Waveform. */
			if( channel->note.param < 8 ) {
				channel->tremolo_type = channel->note.param;
			}
			break;
		case 0x7A: /* Fine Vol Slide Up. */
			if( channel->note.param > 0 ) {
				channel->fine_vslide_up_param = channel->note.param;
			}
			channel->volume += channel->fine_vslide_up_param;
			if( channel->volume > 64 ) {
				channel->volume = 64;
			}
			break;
		case 0x7B: /* Fine Vol Slide Down. */
			if( channel->note.param > 0 ) {
				channel->fine_vslide_down_param = channel->note.param;
			}
			channel->volume -= channel->fine_vslide_down_param;
			if( channel->volume < 0 ) {
				channel->volume = 0;
			}
			break;
		case 0x7C: case 0xFC: /* Note Cut. */
			if( channel->note.param <= 0 ) {
				channel->volume = 0;
			}
			break;
		case 0x8A: /* Arpeggio. */
			if( channel->note.param > 0 ) {
				channel->arpeggio_param = channel->note.param;
			}
			break;
		case 0x95: /* Fine Vibrato.*/
			if( ( channel->note.param >> 4 ) > 0 ) {
				channel->vibrato_speed = channel->note.param >> 4;
			}
			if( ( channel->note.param & 0xF ) > 0 ) {
				channel->vibrato_depth = channel->note.param & 0xF;
			}
			channel_vibrato( channel, 1 );
			break;
		case 0xF8: /* Set Panning. */
			channel->panning = channel->note.param * 17;
			break;
	}
	channel_auto_vibrato( channel );
	channel_calculate_freq( channel );
	channel_calculate_ampl( channel );
	channel_update_envelopes( channel );
}

static int replay_row( struct replay *replay ) {
	int idx, song_end = 0;
	struct note note;
	struct pattern *pattern;
	struct channel *channel;
	struct module *module = replay->module;
	if( replay->next_row < 0 ) {
		replay->break_pos = replay->seq_pos + 1;
		replay->next_row = 0;
	}
	if( replay->break_pos >= 0 ) {
		if( replay->break_pos >= module->sequence_len ) {
			replay->break_pos = replay->next_row = 0;
		}
		while( module->sequence[ replay->break_pos ] >= module->num_patterns ) {
			replay->break_pos++;
			if( replay->break_pos >= module->sequence_len ) {
				replay->break_pos = replay->next_row = 0;
			}
		}
		if( replay->break_pos <= replay->seq_pos ) {
			song_end = 1;
		}
		replay->seq_pos = replay->break_pos;
		for( idx = 0; idx < module->num_channels; idx++ ) {
			replay->channels[ idx ].pl_row = 0;
		}
		replay->break_pos = -1;
	}
	pattern = &module->patterns[ module->sequence[ replay->seq_pos ] ];
	replay->row = replay->next_row;
	if( replay->row >= pattern->num_rows ) {
		replay->row = 0;
	}
	replay->next_row = replay->row + 1;
	if( replay->next_row >= pattern->num_rows ) {
		replay->next_row = -1;
	}
	for( idx = 0; idx < module->num_channels; idx++ ) {
		channel = &replay->channels[ idx ];
		pattern_get_note( pattern, replay->row, idx, &note );
		if( note.effect == 0xE ) {
			note.effect = 0x70 | ( note.param >> 4 );
			note.param &= 0xF;
		}
		if( note.effect == 0x93 ) {
			note.effect = 0xF0 | ( note.param >> 4 );
			note.param &= 0xF;
		}
		if( note.effect == 0 && note.param > 0 ) {
			note.effect = 0x8A;
		}
		channel_row( channel, &note );
		switch( note.effect ) {
			case 0x81: /* Set Speed. */
				if( note.param > 0 ) {
					replay->tick = replay->speed = note.param;
				}
				break;
			case 0xB: case 0x82: /* Pattern Jump.*/
				if( replay->pl_count < 0 ) {
					replay->break_pos = note.param;
					replay->next_row = 0;
				}
				break;
			case 0xD: case 0x83: /* Pattern Break.*/
				if( replay->pl_count < 0 ) {
					if( replay->break_pos < 0 ) {
						replay->break_pos = replay->seq_pos + 1;
					}
					replay->next_row = ( note.param >> 4 ) * 10 + ( note.param & 0xF );
				}
				break;
			case 0xF: /* Set Speed/Tempo.*/
				if( note.param > 0 ) {
					if( note.param < 32 ) {
						replay->tick = replay->speed = note.param;
					} else {
						replay->tempo = note.param;
					}
				}
				break;
			case 0x94: /* Set Tempo.*/
				if( note.param > 32 ) {
					replay->tempo = note.param;
				}
				break;
			case 0x76: case 0xFB : /* Pattern Loop.*/
				if( note.param == 0 ) {
					/* Set loop marker on this channel. */
					channel->pl_row = replay->row;
				}
				if( channel->pl_row < replay->row && replay->break_pos < 0 ) {
					/* Marker valid. */
					if( replay->pl_count < 0 ) {
						/* Not already looping, begin. */
						replay->pl_count = note.param;
						replay->pl_chan = idx;
					}
					if( replay->pl_chan == idx ) {
						/* Next Loop.*/
						if( replay->pl_count == 0 ) {
							/* Loop finished. Invalidate current marker. */
							channel->pl_row = replay->row + 1;
						} else {
							/* Loop. */
							replay->next_row = channel->pl_row;
						}
						replay->pl_count--;
					}
				}
				break;
			case 0x7E: case 0xFE: /* Pattern Delay.*/
				replay->tick = replay->speed + replay->speed * note.param;
				break;
		}
	}
	return song_end;
}

static int replay_tick( struct replay *replay ) {
	int idx, num_channels, song_end = 0;
	if( --replay->tick <= 0 ) {
		replay->tick = replay->speed;
		song_end = replay_row( replay );
	} else {
		num_channels = replay->module->num_channels;
		for( idx = 0; idx < num_channels; idx++ ) {
			channel_tick( &replay->channels[ idx ] );
		}
	}
	return song_end;
}

static void replay_set_sequence_pos( struct replay *replay, int pos ) {
	int idx;
	struct module *module = replay->module;
	if( pos >= module->sequence_len ) {
		pos = 0;
	}
	replay->break_pos = pos;
	replay->next_row = 0;
	replay->tick = 1;
	replay->global_vol = module->default_gvol;
	replay->speed = module->default_speed > 0 ? module->default_speed : 6;
	replay->tempo = module->default_tempo > 0 ? module->default_tempo : 125;
	replay->pl_count = replay->pl_chan = -1;
	for( idx = 0; idx < module->num_channels; idx++ ) {
		channel_init( &replay->channels[ idx ], replay, idx );
	}
	replay_tick( replay );
}

static void dispose_replay( struct replay *replay ) {
	free( replay->channels );
	free( replay );
}

static struct replay* new_replay( struct module *module, int sample_rate ) {
	struct replay *replay = calloc( 1, sizeof( struct replay ) );
	if( replay ) {
		replay->module = module;
		replay->channels = calloc( module->num_channels, sizeof( struct channel ) );
		if( !replay->channels ) {
			dispose_replay( replay );
			replay = NULL;
		}
		replay->sample_rate = sample_rate;
		replay_set_sequence_pos( replay, 0 );
	}
	return replay;
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
	int inst, swap, sidx, freq, d_freq;
	int vol, ampl, d_ampl, pan, pann, d_pann;
	int num_chn = replay->module->num_channels;
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
		while( chn < num_chn ) {
			inst = replay->channels[ chn ].trig_inst;
			swap = replay->channels[ chn ].swap_inst;
			sidx = replay->channels[ chn ].sample_idx;
			freq = replay->channels[ chn ].freq;
			d_freq = freq - replay->channels[ chn ].prev_freq;
			ampl = replay->channels[ chn ].ampl;
			d_ampl = ampl - replay->channels[ chn ].prev_ampl;
			pann = replay->channels[ chn ].pann;
			d_pann = pann - replay->channels[ chn ].prev_pann;
			if( inst || swap || d_freq || d_ampl || d_pann ) {
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
					if( d_freq ) {
						/* Modulate Pitch.*/
						if( dest ) {
							write_int32be( ( get_tmf_key( freq ) << 21 )
								+ ( vol << 6 ) + chn, &dest[ idx ] );
						}
						idx += 4;
					}
				} else if( d_freq ) {
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
