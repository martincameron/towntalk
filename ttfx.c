
#include "towntalk.c"

#include "SDL.h"
#include "dirent.h"

#define NUM_SURFACES 16
#define NUM_CHANNELS 16
#define NUM_SAMPLES 64
#define MIN_TICK_LEN 512
#define MAX_TICK_LEN 8192
#define MAX_SAMPLE_LEN 524200
#define SAMPLE_RATE 48000

static const int FREQ_TABLE[] = {
	16726, 16847, 16969, 17092, 17216, 17341, 17467, 17593,
	17721, 17849, 17978, 18109, 18240, 18372, 18505, 18639,
	18774, 18910, 19047, 19185, 19324, 19464, 19606, 19748,
	19891, 20035, 20180, 20326, 20474, 20622, 20771, 20922,
	21073, 21226, 21380, 21535, 21691, 21848, 22006, 22166,
	22327, 22488, 22651, 22815, 22981, 23147, 23315, 23484,
	23654, 23826, 23998, 24172, 24347, 24524, 24701, 24880,
	25061, 25242, 25425, 25609, 25795, 25982, 26170, 26360,
	26551, 26743, 26937, 27132, 27329, 27527, 27726, 27927,
	28130, 28334, 28539, 28746, 28954, 29164, 29375, 29588,
	29802, 30018, 30236, 30455, 30676, 30898, 31122, 31347,
	31574, 31803, 32034, 32266, 32500, 32735, 32972, 33211
};

struct fxsample {
	int loop_start, loop_length;
	struct variable sample_data;
};

struct fxchannel {
	struct fxsample *sample;
	int sample_pos, frequency, volume, panning;
	int sequence_offset, sequence_wait;
	struct variable sequence;
};

struct fxenvironment {
	struct environment env;
	struct SDL_Surface *surfaces[ NUM_SURFACES ];
	SDL_TimerID timer;
	SDLKey key;
	struct fxsample samples[ NUM_SAMPLES ];
	struct fxchannel channels[ NUM_CHANNELS ];
	int tick, tick_len, audio_idx, audio_end;
	int audio[ MAX_TICK_LEN * 2 ];
};

static Uint32 timer_callback( Uint32 interval, void *param ) {
	SDL_Event event;
	event.type = SDL_USEREVENT;
	event.user.code = 0;
	event.user.data1 = event.user.data2 = NULL;
	SDL_PushEvent( &event );
	return interval;
}

static void mix_channel( struct fxchannel *channel, int *output, int count ) {
	int idx, end, loop, llen, lend, spos, lamp, ramp, step, sam;
	char *data;
	if( channel->volume > 0 && channel->sample
	&& channel->sample->sample_data.string_value ) {
		loop = channel->sample->loop_start << 12;
		llen = channel->sample->loop_length << 12;
		lend = loop + llen;
		spos = channel->sample_pos;
		if( spos < lend || llen > 4096 ) {
			lamp = channel->volume * ( 128 - channel->panning );
			ramp = channel->volume * ( 128 + channel->panning );
			step = ( channel->frequency << 12 ) / SAMPLE_RATE;
			data = channel->sample->sample_data.string_value->string;
			idx = 0;
			end = count << 1;
			while( idx < end ) {
				if( spos >= 0 ) {
					if( spos >= lend ) {
						if( llen > 4096 ) {
							spos = loop + ( ( spos - loop ) % llen );
						} else {
							spos = lend;
							break;
						}
					}
					sam = data[ spos >> 12 ];
					output[ idx ] += ( sam * lamp ) >> 8;
					output[ idx + 1 ] += ( sam * ramp ) >> 8;
				}
				spos += step;
				idx += 2;
			}
			channel->sample_pos = spos;
		}
	}
}

static void process_sequence( struct fxenvironment *fxenv, int channel_idx ) {
	int off, len, cmd, tick, chan, key, ins, vol;
	struct fxchannel *channel, *cmdchan;
	char *seq;
	channel = &fxenv->channels[ channel_idx ];
	if( channel->sequence.string_value ) {
		off = channel->sequence_offset;
		len = channel->sequence.string_value->length;
		seq = channel->sequence.string_value->string;
		while( channel->sequence_wait < 1 && off < len - 1 ) {
			cmd = seq[ off ] & 0xFF;
			if( cmd & 0x80 ) {
				cmd = ( cmd & 0x7F ) << 8;
				cmd |= seq[ off + 1 ] & 0xFF;
				off += 2;
			} else if( off < len - 3 ) {
				cmd = cmd << 24;
				cmd |= ( seq[ off + 1 ] & 0xFF ) << 16;
				cmd |= ( seq[ off + 2 ] & 0xFF ) << 8;
				cmd |= seq[ off + 3 ] & 0xFF;
				off += 4;
			} else {
				off = len;
			}
			if( cmd & 040000 ) {
				/* 0tttttt4wwww ticklen + wait */
				tick = ( cmd >> 15 ) & 0177777;
				if( tick >= MIN_TICK_LEN && tick <= MAX_TICK_LEN ) {
					fxenv->tick_len = tick;
				}
				channel->sequence_wait = cmd & 037777;
			} else {
				chan = cmd & 077;
				if( chan + channel_idx < NUM_CHANNELS ) {
					cmdchan = &fxenv->channels[ chan + channel_idx ];
					if( cmd & 020000 ) {
						/* 0ssssss2vvcc sample offset + vol/pan + channel */
						cmdchan->sample_pos = ( ( cmd >> 3 ) & 01777770000 );
					} else {
						/* 0kkkkii0vvcc key + ins + vol/pan + channel */
						key = ( cmd >> 21 ) & 01777;
						if( key > 0 && key < 958 ) {
							cmdchan->frequency = ( FREQ_TABLE[ key % 96 ] << 4 ) >> ( 9 - key / 96 );
						}
						ins = ( cmd >> 15 ) & 077;
						if( ins > 0 && ins <= NUM_SAMPLES ) {
							cmdchan->sample = &fxenv->samples[ ins - 1 ];
							cmdchan->sample_pos = 0;
						}
					}
					vol = ( cmd >> 6 ) & 0177;
					if( vol < 65 ) {
						cmdchan->volume = vol;
					} else {
						cmdchan->panning = ( vol - 96 ) << 2;
					}
				}
			}
		}
		channel->sequence_offset = off;
	}
}

static void audio_callback( void *userdata, Uint8 *stream, int len ) {
	struct fxenvironment *fxenv = ( struct fxenvironment * ) userdata;
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
				if( channel->sequence_wait < 1 ) {
					channel->sequence_wait = 0;
					process_sequence( fxenv, chan_idx );
				}
				channel->sequence_wait--;
				chan_idx++;
			}
			fxenv->audio_idx = 0;
			fxenv->audio_end = fxenv->tick_len;
			memset( fxenv->audio, 0, sizeof( int ) * fxenv->tick_len * 2 );
			chan_idx = 0;
			while( chan_idx < NUM_CHANNELS ) {
				mix_channel( &fxenv->channels[ chan_idx++ ], fxenv->audio, fxenv->audio_end );
			}
			fxenv->tick++;
		}
	}
}

static void dispose_fxenvironment( struct fxenvironment *fxenv ) {
	int idx;
	if( fxenv ) {
		idx = 0;
		while( idx < NUM_SURFACES ) {
			if( fxenv->surfaces[ idx ] ) {
				SDL_FreeSurface( fxenv->surfaces[ idx ] );
			}
			idx++;
		}
		idx = 0;
		while( idx < NUM_SAMPLES ) {
			dispose_variable( &fxenv->samples[ idx ].sample_data );
			idx++;
		}
		idx = 0;
		while( idx < NUM_CHANNELS ) {
			dispose_variable( &fxenv->channels[ idx ].sequence );
			idx++;
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
		fxplay channel sequence$;
		32-bit octal sequencer commands packed into byte string:
			0kkkkii0vvcc key + ins + volume + channel
			0kkkkii100cc key + ins + vol(64) + channel
			0kkkkii1ppcc key + ins + pan(1-63) + channel
			0ssssss2vvcc sample offset + volume + channel
			0ssssss300cc sample offset + vol(64) + channel
			0ssssss3ppcc sample offset + pan(1-63) + channel
			0tttttt4wwww ticklen + wait
		Instrument 0 / key 0 ignored.
		Instrument >0 sets sample offset to 0.
	*/
	int ret, idx = 0;
	struct variable params[ 2 ];
	struct expression *expr = this->source;
	struct fxenvironment *fxenv = ( struct fxenvironment * ) this->source->function->env;
	memset( params, 0, 2 * sizeof( struct variable ) );
	ret = expr->evaluate( expr, variables, &params[ idx++ ], exception );
	expr = expr->next;
	while( ret && expr ) {
		ret = expr->evaluate( expr, variables, &params[ idx++ ], exception );
		expr = expr->next;
	}
	if( ret ) {
		idx = params[ 0 ].integer_value;
		if( idx >= 0 && idx < NUM_CHANNELS ) {
			if( params[ 1 ].string_value ) {
				SDL_LockAudio();
				fxenv->channels[ idx ].sequence_offset = 0;
				fxenv->channels[ idx ].sequence_wait = 0;
				assign_variable( &params[ 1 ], &fxenv->channels[ idx ].sequence );
				process_sequence( fxenv, idx );
				SDL_UnlockAudio();
			} else {
				ret = throw( exception, this->source, 0, "Not a string." );
			}
		} else {
			ret = throw( exception, this->source, idx, "Invalid channel index." );
		}
	}
	idx = 0;
	while( idx < 2 ) {
		dispose_variable( &params[ idx++ ] );
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

static int evaluate_fxtick_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	dispose_variable( result );
	result->integer_value = ( ( struct fxenvironment * ) this->function->env )->tick;
	result->string_value = NULL;
	return 1;
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
					elem = calloc( 1, sizeof( struct element ) );
					if( elem ) {
						elem->str.reference_count = 1;
						elem->str.line = line++;
						len = write_byte_string( dentry->d_name, strlen( dentry->d_name ), NULL );
						if( len >= 0 ) {
							elem->str.string = malloc( len + 1 );
							if( elem->str.string ) {
								write_byte_string( dentry->d_name, strlen( dentry->d_name ), elem->str.string );
								elem->str.string[ len ] = 0;
								elem->str.length = len;
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
					} else {
						ret = throw( exception, this, 0, OUT_OF_MEMORY );
					}
				}
				closedir( dir );
				if( ret ) {
					dispose_variable( result );
					result->integer_value = 0;
					result->string_value = &head->str;
				} else {
					dispose_string( &head->str );
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
	{ "FX_KEYDOWN", 2, NULL },
	{ "FX_KEYUP", 3, NULL },
	{ "FX_MOUSEMOTION", 4, NULL },
	{ "FX_MOUSEKEYDOWN", 5, NULL },
	{ "FX_MOUSEKEYUP", 6, NULL },
	{ "FX_TIMER", 24, NULL },
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
	{ "fxplay", "xx;", &parse_fxplay_statement, statements }
};

int main( int argc, char **argv ) {
	int exit_code = EXIT_FAILURE;
	char *file_name, message[ 256 ] = "";
	struct environment *env;
	struct variable result, except;
	struct expression expr;
	/* Handle command-line.*/
	if( argc < 2 ) {
		fprintf( stderr, "Usage: %s program.tt [args]\n", argv[ 0 ] );
		return EXIT_FAILURE;
	}
	file_name = argv[ 1 ];
	/* Parse program file. */
	env = calloc( 1, sizeof( struct fxenvironment ) );
	if( env ) {
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
						result.integer_value = except.integer_value = 0;
						result.string_value = except.string_value = NULL;
						expr.line = env->entry_point->line;
						expr.function = env->entry_point;
						expr.parameters = NULL;
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
