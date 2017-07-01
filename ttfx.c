
#include "towntalk.c"

#include "SDL.h"
#include "dirent.h"

#define NUM_SURFACES 16
#define NUM_CHANNELS 32
#define NUM_SAMPLES 64
#define MIN_TICK_LEN 512
#define MAX_TICK_LEN 8192
#define MAX_SAMPLE_LEN 524200
#define SAMPLE_RATE 48000

static const int FREQ_TABLE[] = {
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
	31609, 31838, 32068, 32301, 32535, 32770, 33008, 33247
};

struct fxsample {
	int loop_start, loop_length;
	struct variable sample_data;
};

struct fxchannel {
	struct fxsample *sample;
	int sample_idx, sample_fra;
	int frequency, volume, panning, gain, transpose;
	int sequence_offset, sequence_wait;
	struct variable sequence, next_sequence;
};

struct fxenvironment {
	struct environment env;
	struct SDL_Surface *surfaces[ NUM_SURFACES ];
	SDL_TimerID timer;
	SDLKey key;
	struct fxsample samples[ NUM_SAMPLES ];
	struct fxchannel channels[ NUM_CHANNELS ];
	int tick, tick_len, audio_idx, audio_end;
	int audio[ ( MAX_TICK_LEN + 33 ) * 4 ], ramp_buf[ 64 ];
};

static Uint32 timer_callback( Uint32 interval, void *param ) {
	SDL_Event event;
	event.type = SDL_USEREVENT;
	event.user.code = 0;
	event.user.data1 = event.user.data2 = NULL;
	SDL_PushEvent( &event );
	return interval;
}

static void volume_ramp( int *mix_buf, int *ramp_buf, int tick_len ) {
	int idx, a1, a2;
	for( idx = 0, a1 = 0; a1 < 32; idx += 2, a1++ ) {
		a2 = 32 - a1;
		mix_buf[ idx     ] = ( mix_buf[ idx     ] * a1 + ramp_buf[ idx     ] * a2 ) >> 5;
		mix_buf[ idx + 1 ] = ( mix_buf[ idx + 1 ] * a1 + ramp_buf[ idx + 1 ] * a2 ) >> 5;
	}
	memcpy( ramp_buf, &mix_buf[ tick_len * 2 ], 64 * sizeof( int ) );
}

/* 2:1 downsampling with simple but effective anti-aliasing.
   Buf must contain count * 2 + 1 stereo samples. */
static void downsample( int *buf, int count ) {
	int idx, out_idx, out_len = count * 2;
	for( idx = 0, out_idx = 0; out_idx < out_len; idx += 4, out_idx += 2 ) {
		buf[ out_idx     ] = ( buf[ idx     ] >> 2 ) + ( buf[ idx + 2 ] >> 1 ) + ( buf[ idx + 4 ] >> 2 );
		buf[ out_idx + 1 ] = ( buf[ idx + 1 ] >> 2 ) + ( buf[ idx + 3 ] >> 1 ) + ( buf[ idx + 5 ] >> 2 );
	}
}

static void mix_channel( struct fxchannel *channel, int *output, int count ) {
	int idx, end, loop, llen, lend, sidx, sfra, lamp, ramp, step, sam;
	char *data;
	if( channel->volume > 0 && channel->sample
	&& channel->sample->sample_data.string_value ) {
		loop = channel->sample->loop_start;
		llen = channel->sample->loop_length;
		lend = loop + llen;
		sidx = channel->sample_idx;
		sfra = channel->sample_fra;
		if( sidx < lend || llen > 1 ) {
			lamp = channel->volume * ( 32 - channel->panning );
			ramp = channel->volume * ( 32 + channel->panning );
			step = ( channel->frequency << 12 ) / ( SAMPLE_RATE >> 2 );
			data = channel->sample->sample_data.string_value->string;
			idx = 0;
			end = count << 1;
			while( idx < end ) {
				if( sidx >= lend ) {
					if( llen > 1 ) {
						sidx = loop + ( ( sidx - loop ) % llen );
					} else {
						sidx = lend;
						break;
					}
				}
				sam = data[ sidx ];
				output[ idx++ ] += ( sam * lamp ) >> 11;
				output[ idx++ ] += ( sam * ramp ) >> 11;
				sfra += step;
				sidx += sfra >> 15;
				sfra &= 0x7FFF;
			}
		}
	}
}

static void update_channel( struct fxchannel *channel, int *output, int count ) {
	int step;
	struct fxsample *sample = channel->sample;
	if( sample && sample->sample_data.string_value ) {
		step = ( channel->frequency << 12 ) / ( SAMPLE_RATE >> 2 );
		channel->sample_fra += step * count;
		channel->sample_idx += channel->sample_fra >> 15;
		if( channel->sample_idx > sample->loop_start ) {
			if( sample->loop_length > 1 ) {
				channel->sample_idx = sample->loop_start
					+ ( channel->sample_idx - sample->loop_start ) % sample->loop_length;
			} else {
				channel->sample_idx = sample->loop_start;
			}
		}
		channel->sample_fra &= 0x7FFF;
	}
}

static void process_sequence( struct fxenvironment *fxenv, int channel_idx ) {
	int off, len, cmd, oper, tick, chan, key, ins, vol, gain;
	struct fxchannel *channel, *cmdchan;
	char *seq;
	channel = &fxenv->channels[ channel_idx ];
	if( channel->sequence.string_value ) {
		off = channel->sequence_offset;
		len = channel->sequence.string_value->length;
		seq = channel->sequence.string_value->string;
		while( channel->sequence_wait < 1 && off < len ) {
			cmd = seq[ off ] & 0xFF;
			oper = cmd >> 4;
			if( ( oper == 0 || oper >= 0x4 ) && ( off + 1 < len ) ) {
				cmd = ( cmd << 8 ) | ( seq[ off + 1 ] & 0xFF );
				off += 2;
			} else if( off + 3 < len ) {
				cmd = ( cmd << 24 )
					| ( ( seq[ off + 1 ] & 0xFF ) << 16 )
					| ( ( seq[ off + 2 ] & 0xFF ) << 8 )
					| ( seq[ off + 3 ] & 0xFF );
				off += 4;
			} else {
				oper = cmd = 0;
				off = len;
			}
			if( oper == 0xF ) {
				/* 0xFwww wait w ticks. */
				channel->sequence_wait = cmd & 0xFFF;
			} else if( oper == 0xE ) {
				/* 0xEttt set tempo. */
				tick = ( cmd & 0xFFF ) << 1;
				if( tick >= MIN_TICK_LEN && tick <= MAX_TICK_LEN ) {
					fxenv->tick_len = tick;
				}
			} else if( oper == 0xC ) {
				/* 0xCxxx set gain. */
				gain = cmd & 0xFFF;
				fxenv->channels[ channel_idx ].gain = gain * gain;
			} else if( oper == 0xD ) {
				/* 0xDxxx set transpose. */
				fxenv->channels[ channel_idx ].transpose = ( cmd & 0xFFF ) - 0x800;
			} else if( oper < 0xC ) {
				chan = cmd & 0xFF;
				if( chan + channel_idx < NUM_CHANNELS ) {
					cmdchan = &fxenv->channels[ chan + channel_idx ];
					if( oper >= 0x4 ) {
						if( cmd >= 0x8100 ) {
							/* 0xppcc set panning. */
							cmdchan->panning = ( ( cmd >> 8 ) - 0x80 ) - 32;
						} else if( cmd >= 0x4000 ) {
							/* 0xvvcc set volume */
							vol = ( cmd >> 8 ) - 0x40;
							gain = fxenv->channels[ channel_idx ].gain;
							cmdchan->volume = ( gain * vol * vol ) >> 12;
						}
					} else if( oper == 0x3 ) {
						/* 0x3ssssscc sample offset + channel */
						cmdchan->sample_idx = ( ( cmd & 0xFFFFF00 ) >> 8 );
						cmdchan->sample_fra = 0;
					} else if( oper > 0 ) {
						/* 0x1kkkiicc / 0x2kkkiicc key + instrument + channel */
						key = ( cmd >> 16 ) & 0xFFF;
						key = key + fxenv->channels[ channel_idx ].transpose;
						if( key > 0 && key < 957 ) {
							cmdchan->frequency = ( FREQ_TABLE[ key % 96 ] << 4 ) >> ( 9 - key / 96 );
						}
						ins = ( cmd >> 8 ) & 0xFF;
						if( ins > 0 && ins < 0xC0 ) {
							if( ins >= 0x81 ) {
								cmdchan->panning = ( ins - 0x80 ) - 32;
							} else if( ins >= 0x40 ) {
								vol = ins - 0x40;
								gain = fxenv->channels[ channel_idx ].gain;
								cmdchan->volume = ( gain * vol * vol ) >> 12;
							} else {
								cmdchan->sample = &fxenv->samples[ ins - 1 ];
							}
						}
						if( oper == 1 ) {
							cmdchan->sample_idx = cmdchan->sample_fra = 0;
						}
					}
				}
			}
		}
		channel->sequence_offset = off;
	}
}

static void audio_callback( void *userdata, Uint8 *stream, int len ) {
	struct fxenvironment *fxenv = ( struct fxenvironment * ) userdata;
	SDL_Event event = { SDL_USEREVENT + 1 };
	Sint16 *output = ( Sint16 * ) stream;
	struct fxchannel *channel;
	int samples = len >> 2;
	int *audio = fxenv->audio;
	int out_idx, out_end, aud_idx, ampl;
	int chan_idx, count, offset = 0;
	while( offset < samples ) {
		count = samples - offset;
		if( fxenv->audio_idx + count > fxenv->audio_end ) {
			count = fxenv->audio_end - fxenv->audio_idx;
		}
		out_idx = offset << 1;
		out_end = ( offset + count ) << 1;
		aud_idx = fxenv->audio_idx << 1;
		while( out_idx < out_end ) {
			ampl = audio[ aud_idx++ ];
			if( ampl > 32767 ) {
				ampl = 32767;
			}
			if( ampl < -32768 ) {
				ampl = -32768;
			}
			output[ out_idx++ ] = ampl;
		}
		offset += count;
		fxenv->audio_idx += count;
		if( fxenv->audio_idx >= fxenv->audio_end ) {
			chan_idx = 0;
			while( chan_idx < NUM_CHANNELS ) {
				channel = &fxenv->channels[ chan_idx ];
				if( channel->sequence.string_value ) {
					channel->sequence_wait--;
					if( channel->sequence_wait < 1 ) {
						channel->sequence_wait = 0;
						process_sequence( fxenv, chan_idx );
						while( channel->sequence.string_value && channel->sequence_wait == 0 ) {
							/* Signal sequence end. */
							SDL_PushEvent( &event );
							assign_variable( &channel->next_sequence, &channel->sequence );
							dispose_variable( &channel->next_sequence );
							channel->next_sequence.string_value = NULL;
							channel->sequence_offset = 0;
							process_sequence( fxenv, chan_idx );
						}
					}
				}
				chan_idx++;
			}
			fxenv->audio_idx = 0;
			fxenv->audio_end = fxenv->tick_len;
			memset( fxenv->audio, 0, sizeof( int ) * ( fxenv->tick_len + 33 ) * 4 );
			chan_idx = 0;
			while( chan_idx < NUM_CHANNELS ) {
				channel = &fxenv->channels[ chan_idx++ ];
				mix_channel( channel, fxenv->audio, ( fxenv->tick_len + 33 ) * 2 );
				update_channel( channel, fxenv->audio, fxenv->tick_len * 2 );
			}
			downsample( fxenv->audio, fxenv->tick_len + 32 );
			volume_ramp( fxenv->audio, fxenv->ramp_buf, fxenv->tick_len );
			fxenv->tick++;
		}
	}
}

static struct fxenvironment* new_fxenvironment() {
	int idx;
	struct fxenvironment *fxenv = calloc( 1, sizeof( struct fxenvironment ) );
	for( idx = 0; idx < NUM_CHANNELS; idx++ ) {
		fxenv->channels[ idx ].gain = 4096;
	}
	return fxenv;
}

static void dispose_fxenvironment( struct fxenvironment *fxenv ) {
	int idx;
	if( fxenv ) {
		for( idx = 0; idx < NUM_SURFACES; idx++ ) {
			if( fxenv->surfaces[ idx ] ) {
				SDL_FreeSurface( fxenv->surfaces[ idx ] );
			}
		}
		for( idx = 0; idx < NUM_SAMPLES; idx++ ) {
			dispose_variable( &fxenv->samples[ idx ].sample_data );
		}
		for( idx = 0; idx < NUM_CHANNELS; idx++ ) {
			dispose_variable( &fxenv->channels[ idx ].sequence );
			dispose_variable( &fxenv->channels[ idx ].next_sequence );
		}
		if( fxenv->timer ) {
			SDL_RemoveTimer( fxenv->timer );
		}
		dispose_environment( ( struct environment * ) fxenv );
	}
}

static int execute_fxopen_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int ret;
	struct variable width = { 0, NULL }, height = { 0, NULL }, caption = { 0, NULL };
	struct expression *expr = this->source;
	ret = expr->evaluate( expr, variables, &width, exception );
	if( ret ) {
		expr = expr->next;
		ret = expr->evaluate( expr, variables, &height, exception );
		if( ret ) {
			expr = expr->next;
			ret = expr->evaluate( expr, variables, &caption, exception );
			if( ret ) {
				if( caption.string_value->string ) {
					SDL_WM_SetCaption( caption.string_value->string, "" );
				}
				if( SDL_SetVideoMode( width.integer_value, height.integer_value, 32, SDL_HWSURFACE ) == NULL ) {
					ret = throw( exception, this->source, 0, SDL_GetError() );
				}
				dispose_variable( &caption );
			}
			dispose_variable( &height );
		}
		dispose_variable( &width );
	}
	return ret;
}

static int execute_fxsurface_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int ret, surf, width, height, len, idx = 0;
	struct variable params[ 4 ], *values;
	struct array *arr;
	Uint32 *pixels;
	struct SDL_Surface *surface = NULL;
	struct expression *expr = this->source;
	struct fxenvironment *fxenv = ( struct fxenvironment * ) expr->function->env;
	memset( params, 0, 4 * sizeof( struct variable ) );
	ret = expr->evaluate( expr, variables, &params[ idx++ ], exception );
	expr = expr->next;
	while( ret && expr ) {
		ret = expr->evaluate( expr, variables, &params[ idx++ ], exception );
		expr = expr->next;
	}
	if( ret ) {
		surf = params[ 0 ].integer_value;
		if( surf >= 0 && surf < NUM_SURFACES ) {
			width = params[ 1 ].integer_value;
			height = params[ 2 ].integer_value;
			if( width > 0 && height > 0 ) {
				surface = SDL_CreateRGBSurface( SDL_HWSURFACE, width, height,
					32, 0xFF000000, 0x00FF0000, 0x0000FF00, 0x000000FF );
				if( surface ) {
					if( params[ 3 ].string_value ) {
						if( params[ 3 ].string_value->line < 0 ) {
							arr = ( struct array * ) params[ 3 ].string_value;
							values = arr->array;
							if( SDL_LockSurface( surface ) == 0 ) {
								idx = 0;
								len = arr->length;
								if( len > width * height ) {
									len = width * height;
								}
								pixels = ( Uint32 * ) surface->pixels;
								while( idx < len ) {
									pixels[ idx ] = values[ idx ].integer_value;
									idx++;
								}
								SDL_UnlockSurface( surface );
							} else {
								ret = throw( exception, this->source, 0, SDL_GetError() );
							}
						} else {
							ret = throw( exception, this->source, 0, "Not an array." );
						}
					}
					if( ret ) {
						if( fxenv->surfaces[ surf ] ) {
							SDL_FreeSurface( fxenv->surfaces[ surf ] );
						}
						fxenv->surfaces[ surf ] = surface;
					} else {
						SDL_FreeSurface( surface );
					}
				} else {
					ret = throw( exception, this->source, 0, SDL_GetError() );
				}
			} else {
				ret = throw( exception, this->source, 0, "Invalid surface dimensions." );
			}
		} else {
			ret = throw( exception, this->source, surf, "Surface index out of bounds." );
		}
	}
	idx = 0;
	while( idx < 4 ) {
		dispose_variable( &params[ idx++ ] );
	}
	return ret;
}

static int execute_fxblit_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int ret, idx = 0;
	struct SDL_Rect src, dest;
	struct variable params[ 7 ];
	struct expression *expr = this->source;
	struct fxenvironment *fxenv = ( struct fxenvironment * ) expr->function->env;
	memset( params, 0, 7 * sizeof( struct variable ) );
	ret = expr->evaluate( expr, variables, &params[ idx++ ], exception );
	expr = expr->next;
	while( ret && expr ) {
		ret = expr->evaluate( expr, variables, &params[ idx++ ], exception );
		expr = expr->next;
	}
	if( ret ) {
		idx = params[ 0 ].integer_value;
		if( idx >= 0 && idx < NUM_SURFACES ) {
			src.x = params[ 1 ].integer_value;
			src.y = params[ 2 ].integer_value;
			src.w = params[ 3 ].integer_value;
			src.h = params[ 4 ].integer_value;
			dest.x = params[ 5 ].integer_value;
			dest.y = params[ 6 ].integer_value;
			if( SDL_BlitSurface( fxenv->surfaces[ idx ], &src, SDL_GetVideoSurface(), &dest ) ) {
				ret = throw( exception, this->source, 0, SDL_GetError() );
			}
		} else {
			ret = throw( exception, this->source, idx, "Surface index out of bounds." );
		}
	}
	idx = 0;
	while( idx < 7 ) {
		dispose_variable( &params[ idx++ ] );
	}
	return ret;
}

static int execute_fxrect_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int ret, idx = 0;
	struct SDL_Rect rect;
	struct variable params[ 5 ];
	struct expression *expr = this->source;
	memset( params, 0, 5 * sizeof( struct variable ) );
	ret = expr->evaluate( expr, variables, &params[ idx++ ], exception );
	expr = expr->next;
	while( ret && expr ) {
		ret = expr->evaluate( expr, variables, &params[ idx++ ], exception );
		expr = expr->next;
	}
	if( ret ) {
		rect.x = params[ 0 ].integer_value;
		rect.y = params[ 1 ].integer_value;
		rect.w = params[ 2 ].integer_value;
		rect.h = params[ 3 ].integer_value;
		if( SDL_FillRect( SDL_GetVideoSurface(), &rect, params[ 4 ].integer_value ) ) {
			ret = throw( exception, this->source, 0, SDL_GetError() );
		}
	}
	idx = 0;
	while( idx < 5 ) {
		dispose_variable( &params[ idx++ ] );
	}
	return ret;
}

static int execute_fxshow_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	SDL_UpdateRect( SDL_GetVideoSurface(), 0, 0, 0, 0 );
	return 1;
}

static int execute_fxsleep_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct variable millis = { 0, NULL };
	int ret = this->source->evaluate( this->source, variables, &millis, exception );
	if( ret && millis.integer_value > 0 ) {
		SDL_Delay( millis.integer_value );
	}
	return ret;
}

static int execute_fxtimer_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct variable millis = { 0, NULL };
	struct fxenvironment *fxenv = ( struct fxenvironment * ) this->source->function->env;
	int ret = this->source->evaluate( this->source, variables, &millis, exception );
	if( ret ) {
		if( millis.integer_value > 0 ) {
			fxenv->timer = SDL_AddTimer( millis.integer_value, timer_callback, fxenv );
			if( fxenv->timer == NULL ) {
				ret = throw( exception, this->source, millis.integer_value, "Unable to start timer." );
			}
		} else {
			if( SDL_RemoveTimer( fxenv->timer ) == 0 ) {
				ret = throw( exception, this->source, millis.integer_value, "Unable to stop timer." );
			}
		}
	}
	return ret;
}

static int execute_fxaudio_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct variable ticklen = { 0, NULL };
	struct fxenvironment *fxenv = ( struct fxenvironment * ) this->source->function->env;
	SDL_AudioSpec audiospec = { 0 };
	int ret = this->source->evaluate( this->source, variables, &ticklen, exception );
	if( ret ) {
		/* fxaudio ticklen; */
		if( ticklen.integer_value > 0 ) {
			if( ticklen.integer_value >= MIN_TICK_LEN
			&&  ticklen.integer_value <= MAX_TICK_LEN ) {
				SDL_LockAudio();
				fxenv->tick_len = ticklen.integer_value;
				SDL_UnlockAudio();
				if( SDL_GetAudioStatus() == SDL_AUDIO_STOPPED ) {
					audiospec.freq = SAMPLE_RATE;
					audiospec.format = AUDIO_S16SYS;
					audiospec.channels = 2;
					audiospec.samples = MIN_TICK_LEN;
					audiospec.callback = audio_callback;
					audiospec.userdata = fxenv;
					if( SDL_OpenAudio( &audiospec, NULL ) >= 0 ) {
						SDL_PauseAudio( 0 );
					} else {
						ret = throw( exception, this->source, 0, SDL_GetError() );
					}
				}
			} else {
				ret = throw( exception, this->source, ticklen.integer_value, "Invalid tick length." );
			}
		} else {
			SDL_CloseAudio();
		}
		dispose_variable( &ticklen );
	}
	return ret;
}

static int execute_fxsample_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	/* fxsample index data$ loopstart looplen; */
	int ret, loop, llen, lend, idx = 0;
	struct string *data;
	struct variable params[ 4 ];
	struct expression *expr = this->source;
	struct fxenvironment *fxenv = ( struct fxenvironment * ) this->source->function->env;
	memset( params, 0, 4 * sizeof( struct variable ) );
	ret = expr->evaluate( expr, variables, &params[ idx++ ], exception );
	expr = expr->next;
	while( ret && expr ) {
		ret = expr->evaluate( expr, variables, &params[ idx++ ], exception );
		expr = expr->next;
	}
	if( ret ) {
		idx = params[ 0 ].integer_value - 1;
		if( idx >= 0 && idx < NUM_SAMPLES ) {
			data = params[ 1 ].string_value;
			if( data ) {
				if( data->length < MAX_SAMPLE_LEN ) {
					loop = params[ 2 ].integer_value;
					llen = params[ 3 ].integer_value;
					lend = loop + llen;
					if( loop >= 0 && lend >= loop && lend <= data->length ) {
						SDL_LockAudio();
						fxenv->samples[ idx ].loop_start = loop;
						fxenv->samples[ idx ].loop_length = llen;
						assign_variable( &params[ 1 ], &fxenv->samples[ idx ].sample_data );
						SDL_UnlockAudio();
					} else {
						ret = throw( exception, this->source, lend, "Loop out of bounds." );
					}
				} else {
					ret = throw( exception, this->source, data->length, "Sample data too long." );
				}
			} else {
				ret = throw( exception, this->source, 0, "Sample data not a string." );
			}
		} else {
			ret = throw( exception, this->source, idx, "Invalid sample index." );
		}
	}
	idx = 0;
	while( idx < 4 ) {
		dispose_variable( &params[ idx++ ] );
	}
	return ret;
}

static int execute_fxplay_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	/*
		Play sequence: fxplay channel sequence$;
		Queue sequence: fxqueue channel sequence$;
		2 and 4-byte sequencer commands packed into byte string:
			0x0xxx do nothing (used to pad 2-byte cmds to 4).
			0x1kkkiicc set key k, instrument i and sample offset 0 on channel c.
			0x2kkkiicc set key k and instrument i on channel c.
			0x3ssssscc set sample offset s on channel c.
			0xvvcc set volume (0x40-0x80) on channel c.
			0xppcc set panning (0x81-0xBF) on channel c.
			0xCxxx set sequence gain (default 0x40).
			0xDxxx set sequence transpose (default 0x800).
			0xEttt set tempo in samples per tick (at 24000hz).
			0xFwww wait w ticks.
		Instrument 0 / key 0 ignored.
		If instrument >= 0x40, set volume / panning instead.
		Keys specified in 96ths of an octave (480 = 16744hz).
	*/
	struct expression *expr = this->source;
	struct variable channel = { 0, NULL }, sequence = { 0, NULL };
	struct fxenvironment *fxenv = ( struct fxenvironment * ) this->source->function->env;
	int ret = expr->evaluate( expr, variables, &channel, exception );
	if( ret ) {
		expr = expr->next;
		ret = expr->evaluate( expr, variables, &sequence, exception );
		if( ret ) {
			if( channel.integer_value >= 0 && channel.integer_value < NUM_CHANNELS ) {
				if( sequence.string_value ) {
					SDL_LockAudio();
					if( this->local && fxenv->channels[ channel.integer_value ].sequence.string_value ) {
						assign_variable( &sequence, &fxenv->channels[ channel.integer_value ].next_sequence );
					} else {
						fxenv->channels[ channel.integer_value ].sequence_offset = 0;
						fxenv->channels[ channel.integer_value ].sequence_wait = 0;
						assign_variable( &sequence, &fxenv->channels[ channel.integer_value ].sequence );
						dispose_variable( &fxenv->channels[ channel.integer_value ].next_sequence );
						fxenv->channels[ channel.integer_value ].next_sequence.string_value = NULL;
						process_sequence( fxenv, channel.integer_value );
					}
					SDL_UnlockAudio();
				} else {
					ret = throw( exception, this->source, sequence.integer_value, "Not a string." );
				}
			} else {
				ret = throw( exception, this->source, channel.integer_value, "Invalid channel index." );
			}
			dispose_variable( &sequence );
		}
		dispose_variable( &channel );
	}
	return ret;
}

static struct element* parse_fxopen_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, env, func, prev, &execute_fxopen_statement, message );
}

static struct element* parse_fxshow_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	struct element *next = elem->next;
	struct statement *stmt = new_statement( message );
	if( stmt ) {
		stmt->execute = &execute_fxshow_statement;
		prev->next = stmt;
		next = next->next;
	}
	return next;
}

static struct element* parse_fxsurface_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, env, func, prev, &execute_fxsurface_statement, message );
}

static struct element* parse_fxblit_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, env, func, prev, &execute_fxblit_statement, message );
}

static struct element* parse_fxrect_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, env, func, prev, &execute_fxrect_statement, message );
}

static struct element* parse_fxsleep_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, env, func, prev, &execute_fxsleep_statement, message );
}

static struct element* parse_fxtimer_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, env, func, prev, &execute_fxtimer_statement, message );
}

static struct element* parse_fxaudio_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, env, func, prev, &execute_fxaudio_statement, message );
}

static struct element* parse_fxsample_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, env, func, prev, &execute_fxsample_statement, message );
}

static struct element* parse_fxplay_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, env, func, prev, &execute_fxplay_statement, message );
}

static struct element* parse_fxqueue_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	struct element *next = parse_expr_list_statement( elem, env, func, prev, &execute_fxplay_statement, message );
	if( prev->next ) {
		prev->next->local = 1;
	}
	return next;
}

static int handle_event_expression( struct expression *this, SDL_Event *event,
	struct variable *result, struct variable *exception ) {
	struct fxenvironment *fxenv = ( struct fxenvironment * ) this->function->env;
	int ret = 1;
	if( event->type == SDL_QUIT ) {
		ret = throw( exception, this, 0, NULL );
	} else {
		if( event->type == SDL_KEYDOWN || event->type == SDL_KEYUP ) {
			fxenv->key = event->key.keysym.sym;
		}
		dispose_variable( result );
		result->integer_value = event->type;
		result->string_value = NULL;
	}
	return ret;
}

static int evaluate_fxpoll_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	SDL_Event event;
	event.type = SDL_NOEVENT;
	SDL_PollEvent( &event );
	return handle_event_expression( this, &event, result, exception );
}

static int evaluate_fxwait_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	SDL_Event event;
	SDL_WaitEvent( &event );
	return handle_event_expression( this, &event, result, exception );
}

static int evaluate_millis_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	dispose_variable( result );
	result->integer_value = SDL_GetTicks();
	result->string_value = NULL;
	return 1;
}

static int evaluate_xmouse_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	dispose_variable( result );
	SDL_GetMouseState( &result->integer_value, NULL );
	result->string_value = NULL;
	return 1;
}

static int evaluate_ymouse_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	dispose_variable( result );
	SDL_GetMouseState( NULL, &result->integer_value );
	result->string_value = NULL;
	return 1;
}

static int evaluate_mousekey_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	dispose_variable( result );
	result->integer_value = SDL_GetMouseState( NULL, NULL );
	result->string_value = NULL;
	return 1;
}

static int evaluate_keyboard_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	dispose_variable( result );
	result->integer_value = ( ( struct fxenvironment * ) this->function->env )->key;
	result->string_value = NULL;
	return 1;
}

static int evaluate_keyshift_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	dispose_variable( result );
	result->integer_value = SDL_GetModState();
	result->string_value = NULL;
	return 1;
}

/* Returns a value incremented every sequencer tick while the audio device is running. */
static int evaluate_fxtick_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	dispose_variable( result );
	result->integer_value = ( ( struct fxenvironment * ) this->function->env )->tick;
	result->string_value = NULL;
	return 1;
}

/* Returns the number of sequences playing and/or queued on the specified channel. */
static int evaluate_fxseq_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct fxenvironment *env = ( struct fxenvironment * ) this->function->env;
	struct expression *parameter = this->parameters;
	struct variable chan = { 0, NULL };
	int ret = parameter->evaluate( parameter, variables, &chan, exception );
	if( ret ) {
		if( chan.integer_value >= 0 && chan.integer_value < NUM_CHANNELS ) {
			dispose_variable( result );
			if( env->channels[ chan.integer_value ].sequence.string_value == NULL ) {
				result->integer_value = 0;
			} else if( env->channels[ chan.integer_value ].next_sequence.string_value == NULL ) {
				result->integer_value = 1;
			} else {
				result->integer_value = 2;
			}
			result->string_value = NULL;
		} else {
			ret = throw( exception, this, chan.integer_value, "Invalid channel index." );
		}
		dispose_variable( &chan );
	}
	return ret;
}

static int evaluate_fxdir_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	DIR *dir;
	char message[ 64 ];
	struct dirent *dentry;
	struct variable path = { 0, NULL };
	struct element *elem, *tail, *head = NULL;
	struct expression *parameter = this->parameters;
	int len, line = 1;
	int ret = parameter->evaluate( parameter, variables, &path, exception );
	if( ret ) {
		if( path.string_value && path.string_value->string ) {
			dir = opendir( path.string_value->string );
			if( dir ) {
				dentry = readdir( dir );
				while( dentry && ret ) {
					len = write_byte_string( dentry->d_name, strlen( dentry->d_name ), NULL );
					if( len >= 0 ) {
						elem = new_element( len );
						if( elem ) {
							write_byte_string( dentry->d_name, strlen( dentry->d_name ), elem->str.string );
							elem->str.line = line++;
							if( head ) {
								tail->next = elem;
								tail = tail->next;
							} else {
								head = tail = elem;
							}
							dentry = readdir( dir );
						} else {
							ret = throw( exception, this, 0, OUT_OF_MEMORY );
						}
					} else {
						ret = throw( exception, this, 0, "String too large." );
					}
				}
				closedir( dir );
				if( ret ) {
					dispose_variable( result );
					result->integer_value = 0;
					result->string_value = &head->str;
				} else {
					unref_string( &head->str );
				}
			} else {
				strncpy( message, strerror( errno ), 63 );
				message[ 63 ] = 0;
				ret = throw( exception, this, 0, message );
			}
		} else {
			ret = throw( exception, this, 0, "Not a string." );
		}
		dispose_variable( &path );
	}
	return ret;
}

static struct constant fxconstants[] = {
	{ "FX_KEYDOWN", SDL_KEYDOWN, NULL },
	{ "FX_KEYUP", SDL_KEYUP, NULL },
	{ "FX_MOUSEMOTION", SDL_MOUSEMOTION, NULL },
	{ "FX_MOUSEKEYDOWN", SDL_MOUSEBUTTONDOWN, NULL },
	{ "FX_MOUSEKEYUP", SDL_MOUSEBUTTONUP, NULL },
	{ "FX_TIMER", SDL_USEREVENT, NULL },
	{ "FX_SEQUENCER", SDL_USEREVENT + 1, NULL },
	{ NULL }
};

static struct operator fxoperators[] = {
	{ "$millis",'$', 0, &evaluate_millis_expression, &fxoperators[ 1 ] },
	{ "$fxpoll",'$', 0, &evaluate_fxpoll_expression, &fxoperators[ 2 ] },
	{ "$fxwait",'$', 0, &evaluate_fxwait_expression, &fxoperators[ 3 ] },
	{ "$xmouse",'$', 0, &evaluate_xmouse_expression, &fxoperators[ 4 ] },
	{ "$ymouse",'$', 0, &evaluate_ymouse_expression, &fxoperators[ 5 ] },
	{ "$mousekey",'$', 0, &evaluate_mousekey_expression, &fxoperators[ 6 ] },
	{ "$keyboard",'$', 0, &evaluate_keyboard_expression, &fxoperators[ 7 ] },
	{ "$keyshift",'$', 0, &evaluate_keyshift_expression, &fxoperators[ 8 ] },
	{ "$fxtick",'$', 0, &evaluate_fxtick_expression, &fxoperators[ 9 ] },
	{ "$fxseq",'$', 1, &evaluate_fxseq_expression, &fxoperators[ 10 ] },
	{ "$fxdir",'$', 1, &evaluate_fxdir_expression, operators }
};

static struct keyword fxstatements[] = {
	{ "fxopen", "xxx;", &parse_fxopen_statement, &fxstatements[ 1 ] },
	{ "fxshow", ";", &parse_fxshow_statement, &fxstatements[ 2 ] },
	{ "fxsurface", "xxxx;", &parse_fxsurface_statement, &fxstatements[ 3 ] },
	{ "fxblit", "xxxxxxx;", &parse_fxblit_statement, &fxstatements[ 4 ] },
	{ "fxrect", "xxxxx;", &parse_fxrect_statement, &fxstatements[ 5 ] },
	{ "fxsleep", "x;", &parse_fxsleep_statement, &fxstatements[ 6 ] },
	{ "fxtimer", "x;", &parse_fxtimer_statement, &fxstatements[ 7 ] },
	{ "fxaudio", "x;", &parse_fxaudio_statement, &fxstatements[ 8 ] },
	{ "fxsample", "xxxx;", &parse_fxsample_statement, &fxstatements[ 9 ] },
	{ "fxqueue", "xx;", &parse_fxqueue_statement, &fxstatements[ 10 ] },
	{ "fxplay", "xx;", &parse_fxplay_statement, statements }
};

int main( int argc, char **argv ) {
	int exit_code = EXIT_FAILURE;
	char *file_name, message[ 256 ] = "";
	struct environment *env;
	struct fxenvironment *fxenv;
	struct variable result = { 0 }, except = { 0 };
	struct expression expr = { 0 };
	/* Handle command-line.*/
	if( argc < 2 ) {
		fprintf( stderr, "Usage: %s program.tt [args]\n", argv[ 0 ] );
		return EXIT_FAILURE;
	}
	file_name = argv[ 1 ];
	/* Parse program file. */
	fxenv = new_fxenvironment();
	if( fxenv ) {
		env = &fxenv->env;
		env->argc = argc - 1;
		env->argv = &argv[ 1 ];
		if( add_constants( fxconstants, env, message )
		&& add_constants( constants, env, message )  ) {
			env->statements = fxstatements;
			env->operators = fxoperators;
			if( parse_tt_file( file_name, env, message ) ) {
				if( env->entry_point ) {
					/* Initialize SDL. */
					if( SDL_Init( SDL_INIT_VIDEO | SDL_INIT_TIMER ) == 0 ) {
						/* Evaluate entry-point function. */
						expr.line = env->entry_point->line;
						expr.function = env->entry_point;
						expr.evaluate = &evaluate_function_expression;
						if( expr.evaluate( &expr, NULL, &result, &except ) ) {
							exit_code = EXIT_SUCCESS;
						} else if( except.string_value && except.string_value->string == NULL ) {
							exit_code = except.integer_value;
						} else {
							fprintf( stderr, "Unhandled exception %d.\n", except.integer_value );
							if( except.string_value && except.string_value->string ) {
								fprintf( stderr, "%s\n", except.string_value->string );
							}
						}
						dispose_variable( &result );
						dispose_variable( &except );
						SDL_Quit();
					} else {
						fprintf( stderr, "Unable to initialise SDL: %s\n", SDL_GetError() );
					}
				} else {
					fprintf( stderr, "No programs found.\n" );
				}
			} else {
				fprintf( stderr, "%s\n", message );
			}
		}
		dispose_fxenvironment( ( struct fxenvironment * )env );
	} else {
		fputs( "Out of memory.\n", stderr );
	}
	return exit_code;
}
