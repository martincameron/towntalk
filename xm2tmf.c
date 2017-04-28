
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
	char *buffer;
	int length;
};

struct sample {
	int loop_start, loop_length;
};

struct instrument {
	char *name;
	int num_samples;
	struct sample samples[ 96 ];
};

struct module {
	char *name;
	int num_instruments;
	struct instrument instruments[ 256 ];
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

static struct module* new_module( struct data module_data ) {
	return calloc( 1, sizeof( struct module ) );
}

static void dispose_module( struct module *module ) {
	free( module );
}

static struct replay* new_replay( struct module *module, int sample_rate ) {
	struct replay *replay = calloc( 1, sizeof( struct replay ) );
	if( replay ) {
		replay->sample_rate = sample_rate;
	}
	return replay;
}

static int replay_set_position( struct replay *replay, int pos ) {
}

static int replay_tick( struct replay *replay ) {
}

static int replay_calculate_tick_len( struct replay *replay ) {
	return ( replay->sample_rate * 5 ) / ( replay->tempo * 2 );
}

static void sample_quantize( struct sample *sample, char *output, int length ) {
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
	replay_set_position( replay, 0 );
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

static int xm_to_tmf( struct data module_data, char *tmf ) {
	int seqlen, idx, loop_start, loop_length, length = -1;
	struct module *module = new_module( module_data );
	struct sample *sample;
	if( module ) {
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
					/*get_string( 0, &tmf[ 8 ] );*/
					write_sequence( replay, &tmf[ length ] );
				}
				length = length + seqlen;
				idx = 1;
				while( idx < 32 ) {
					sample = &module->instruments[ idx ].samples[ 0 ];
					loop_start = sample->loop_start;
					loop_length = sample->loop_length;
					if( tmf ) {
						write_int32be( loop_start, &tmf[ idx * 32 ] );
						write_int32be( loop_length, &tmf[ idx * 32 + 4 ] );
						/*get_string( idx, &tmf[ idx * 32 + 8 ] );*/
						sample_quantize( sample, &tmf[ length ], loop_start + loop_length );
					}
					length = length + loop_start + loop_length;
					idx++;
				}
			}
		}
		dispose_module( module );
	}
	return length;
}

int main( int argc, char **argv ) {
	int result;
	long count, length;
	char *mod, *tmf;
	struct data data;
	result = EXIT_FAILURE;
	if( argc != 3 ) {
		fprintf( stderr, "%s\nUsage: %s input.xm output.tmf\n", VERSION, argv[ 0 ] );
	} else {
		/* Read module file.*/
		length = read_file( argv[ 1 ], NULL );
		if( length >= 0 ) {
			printf( "Module Data Length: %li bytes.\n", length );
			mod = calloc( length, 1 );
			if( mod != NULL ) {
				if( read_file( argv[ 1 ], mod ) >= 0 ) {
					/* Perform conversion. */
					data.buffer = mod;
					data.length = length;
					length = xm_to_tmf( data, NULL );
					if( length > 0 ) {
						tmf = calloc( length, 1 );
						if( tmf != NULL ) {
							xm_to_tmf( data, tmf );
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
	}
	return result;
}
