
#include "dirent.h"
#include "errno.h"
#include "signal.h"
#include "string.h"
#include "sys/stat.h"

#include "SDL.h"

#if defined( ALSA_MIDI )
#include "alsa/asoundlib.h"
#endif

#include "towntalk.h"

/*
	SDL graphics and sound extension for Towntalk (c)2020 Martin Cameron.
	
	Statements:
		display w, h, "caption";               Open a display window.
		rect x, y, w, h, 0xrrggbb;             Draw an opaque rectangle.
		show;                                  Update the display. May wait for the next vertical blank.
		surface num, width, height, pixels;    Set the specified surface to specified size and RGBA pixel array.
		blit num, x, y, cx, cy, cw, ch;        Draw the specified surface clipped to the specified region.
		sleep millis;                          Wait for the specified time period.
		timer millis;                          Generate timer events at the specified interval (0 to disable).
		audio period;                          Enable the audio system (period in samples per tick at 24000hz).
		sample num, "str", loop, looplength;   Set the specified sample to the specified signed 8-bit string and loop points.
		play channel, "sequence";              Play the specified sequence on the specified channel offset.
		queue channel, "sequence";             Queue the specified sequence on the specified channel offset.
		midi "device";                         Open the named MIDI input device for recieving events.
	
	Expressions:
		$millis                                Value incremented every millisecond.
		$pollevent                             Return an event or 0 if none avaliable.
		$waitevent                             Wait for an event and return it.
		$xmouse                                The horizontal mouse coordinate from the latest mouse event.
		$ymouse                                The vertical mouse coordinate from the latest mouse event.
		$mousekey                              The state of the mouse buttons from the latest mouse event.
		$mousewheel                            The state of the mouse wheel from the latest mouse event.
		$keyboard                              The key associated with the latest keyboard event.
		$keyshift                              The currently pressed modifier keys (least significant 2 bits are shift keys).
		$seqtick                               Return an integer incremented every sequencer period.
		$seqmsg                                The channel and parameter of the latest sequencer event (0xccpppppp).
		$dir("path")                           An element tree of the names and sizes of all files in the specified dir.
		$path("file")                          The full path of the specified file.
		$midimsg                               The message associated with the latest MIDI event.
		$winmsg                                The value of the latest window event.
		$keyheld                               Whether the latest keyboard event was from a held-down key.
		$datfile                               A string containing the datfile the current program was run from, or 0.
		$extract(datfile index)                Extract the specified string from the specified datfile.
		$stream(arr offset count)              Stream 16-bit stereo sample pairs to the audio system and return count written.
		
	Event Constants (for $pollevent and $waitevent):
		WINDOW_EVENT                           Window event.
		KEY_PRESSED_EVENT                      Key pressed event.
		KEY_RELEASED_EVENT                     Key released event.
		MOUSE_MOTION_EVENT                     Mouse moved event.
		MOUSE_PRESSED_EVENT                    Mouse button pressed event.
		MOUSE_RELEASED_EVENT                   Mouse button released event.
		MOUSE_WHEEL_EVENT                      Mouse wheel event.
		TIMER_EVENT                            Timer event.
		SEQUENCER_EVENT                        Sequencer event.
		MIDI_EVENT                             MIDI event.
		
	Window Event Constants (for $winmsg):
		WINDOW_EXPOSED_MSG                     Window exposed.
	
	Key Constants (for $keyboard):
		KEY_BACKSPACE                          Backspace.
		KEY_TAB                                Tab.
		KEY_RETURN                             Return.
		KEY_ESCAPE                             Escape.
		KEY_SPACE                              Space.
		KEY_0                                  Zero (other number keys can be calculated by adding to this value).
		KEY_A                                  A (other alphabetical keys can be calculated by adding to this value).
		KEY_PAD_0                              Key pad zero.
		KEY_PAD_1                              Key pad one (keys up to 9 can be calculated by adding to this value).
		KEY_PAD_PERIOD                         Key pad period.
		KEY_PAD_DIVIDE                         Key pad divide.
		KEY_PAD_MULTIPLY                       Key pad multiply.
		KEY_PAD_MINUS                          Key pad minus.
		KEY_PAD_PLUS                           Key pad plus.
		KEY_PAD_ENTER                          Key pad enter.
		KEY_PAD_EQUALS                         Key pad equals.
		KEY_UP                                 Cursor up.
		KEY_DOWN                               Cursor down.
		KEY_LEFT                               Cursor left.
		KEY_RIGHT                              Cursor right.
		KEY_INSERT                             Insert.
		KEY_DELETE                             Delete.
		KEY_HOME                               Home.
		KEY_END                                End.
		KEY_PAGE_UP                            Page Up.
		KEY_PAGE_DOWN                          Page Down.
		KEY_F1                                 F1 (other function keys can be calculated by adding to this value).
		
	Audio Sequencer Commands (2 and 4-bytes each packed into big-endian string for play and queue statements):
		0x00xx do nothing (used to pad 2-byte commands to 4).
		0x08xxxxxx fire an event containing the specified 24-bit parameter.
		0x1kkkiicc set key k, instrument i and sample offset 0 on channel c.
			Instrument 0 / key 0 ignored.
			If instrument >= 0x40, set volume / panning instead.
			Keys specified in 96ths of an octave (480 = 16744hz).
		0x2kkkiicc set key k and instrument i on channel c.
			The sample offset is not reset.
		0x3ssssscc set sample offset s on channel c.
		0xvvcc set volume (0x40-0x80) on channel c.
		0xppcc set panning (0x81-0xBF) on channel c.
		0xCxxx set sequence gain (default 0x40).
		0xDxxx set sequence transpose (default 0x800).
		0xEttt set tempo in samples per tick (at 24000hz).
		0xFwww wait w ticks.
		
	Datfiles:
		A datfile is a simple indexed file format which can be used to combine programs and data.
		The first 4 bytes is the ASCII string "TTFX", followed by 4-byte big-endian offsets into the file.
		The length of an item can be calculated by subtracting one offset from the next.
		The last item is typically identified by having zero length.
		An executable datfile has the program at the first offset.
*/

#if defined( __MINGW32__ )
#define realpath( path, resolved_path ) _fullpath( NULL, ( path ), 0 )
#endif

#define NUM_SAMPLES 64
#define NUM_CHANNELS 32
#define NUM_SURFACES 256
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
	struct variable datfile;
#if SDL_MAJOR_VERSION > 1
	SDL_Keycode key;
	struct SDL_Window *window;
	struct SDL_Renderer *renderer;
	struct SDL_Texture *target, *surfaces[ NUM_SURFACES ];
#else
	SDLKey key;
	struct SDL_Surface *surfaces[ NUM_SURFACES ];
#endif
	SDL_TimerID timer;
	int win_event, key_held, wheel;
	struct fxsample samples[ NUM_SAMPLES ];
	struct fxchannel channels[ NUM_CHANNELS ];
	int timer_event_type, seq_event_type, midi_event_type;
	int tick, tick_len, audio_idx, audio_end, stream_idx, seq_msg, midi_msg;
	int audio[ ( MAX_TICK_LEN + 33 ) * 4 ], stream[ MAX_TICK_LEN * 2 ], ramp_buf[ 64 ];
#if defined( ALSA_MIDI )
	int midi_buf, midi_idx;
	snd_rawmidi_t *midi_in;
#endif
};

static struct fxenvironment *fxenv;

static void ( *interrupt_handler )( int signum );

#if defined( MULTI_THREAD )
int worker_thread( void *data ) {
	struct expression expr = { 0 };
	struct worker *work = ( struct worker * ) data;
	initialize_call_expr( &expr, work->env->entry_points );
	expr.parameters = work->parameters;
	work->ret = expr.evaluate( &expr, NULL, &work->result, &work->exception );
	return 0;
}

/* Begin execution of the specified worker. Returns 0 on failure. */
int start_worker( struct worker *work ) {
	int success = 0;
	work->mutex = SDL_CreateMutex();
	if( work->mutex ) {
		work->thread = SDL_CreateThread( worker_thread, work->str.string, work );
		if( work->thread ) {
			success = 1;
		} else {
			SDL_DestroyMutex( ( SDL_mutex * ) work->mutex );
			work->mutex = NULL;
		}
	}
	return success;
}

/* Lock the specified worker mutex. Returns 0 on failure. */
int lock_worker( struct worker *work ) {
	return work->mutex == NULL || SDL_LockMutex( ( SDL_mutex * ) work->mutex ) == 0;
}

/* Unlock the specified worker mutex. Returns 0 on failure. */
int unlock_worker( struct worker *work ) {
	return work->mutex == NULL || SDL_UnlockMutex( ( SDL_mutex * ) work->mutex ) == 0;
}

/* Wait for the completion of the specified worker.
   If cancel is non-zero, the worker should be interrupted. */
void await_worker( struct worker *work, int cancel ) {
	if( work->thread ) {
		if( cancel ) {
			work->env->interrupted = 1;
		}
		SDL_WaitThread( ( SDL_Thread * ) work->thread, NULL );
		SDL_DestroyMutex( ( SDL_mutex * ) work->mutex );
		work->thread = work->mutex = NULL;
	}
}
#endif

static void signal_handler( int signum ) {
	signal( signum, signal_handler );
	fxenv->env.interrupted = 1;
	if( fxenv->env.worker ) {
		/* Terminate current worker. */
		fxenv->env.worker->env->interrupted = 1;
	}
	if( signum == SIGINT && interrupt_handler ) {
		interrupt_handler( SIGINT );
	}
}

static Uint32 timer_callback( Uint32 interval, void *param ) {
	SDL_Event event = { 0 };
	event.type = ( ( struct fxenvironment * ) param )->timer_event_type;
	SDL_PushEvent( &event );
	return interval;
}

static int datfile_extract( char *datfile, int datfile_length, int bank, char *buffer ) {
	int offset, end, length = -1;
	if( datfile_length < 4 || strncmp( datfile, "TTFX", 4 ) ) {
		/* Not a datfile. */
		return -2;
	}
	if( bank >= 0 && ( bank + 3 ) * 4 <= datfile_length ) {
		offset = unpack( datfile, bank + 1 );
		end = unpack( datfile, bank + 2 );
		if( offset > 0 && end >= offset && end <= datfile_length ) {
			length = end - offset;
			if( buffer ) {
				memcpy( buffer, &datfile[ offset ], length );
			}
		}
	}
	return length;
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
	signed char *data;
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
			data = ( signed char * ) channel->sample->sample_data.string_value->string;
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
	SDL_Event event = { 0 };
	char *seq;
	channel = &fxenv->channels[ channel_idx ];
	if( channel->sequence.string_value ) {
		off = channel->sequence_offset;
		len = channel->sequence.string_value->length;
		seq = channel->sequence.string_value->string;
		while( channel->sequence_wait < 1 && off < len ) {
			cmd = seq[ off ] & 0xFF;
			oper = cmd >> 4;
			if( ( cmd == 0 || oper >= 0x4 ) && ( off + 1 < len ) ) {
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
				tick = ( cmd & 0xFFF ) * SAMPLE_RATE / 24000;
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
					} else if( ( cmd & 0xF000000 ) == 0x8000000 ) {
						/* 0x08xxxxxx sequencer event. */
						event.type = fxenv->seq_event_type;
						event.user.code = ( channel_idx << 24 ) | ( cmd & 0xFFFFFF );
						SDL_PushEvent( &event );
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
	int *fxaudio = fxenv->audio, *fxstream = fxenv->stream;
	int out_idx, out_end, aud_idx, aud_end, ampl;
	int chan_idx, count, offset = 0;
	#if defined( ALSA_MIDI )
	SDL_Event event = { 0 };
	unsigned char chr;
	if( fxenv->midi_in ) {
		while( snd_rawmidi_read( fxenv->midi_in, &chr, 1 ) > 0 ) {
			/*printf( "%x\n", (int)chr );*/
			if( chr & 0x80 ) {
				fxenv->midi_buf = chr << 16;
				if( ( fxenv->midi_buf & 0xF00000 ) == 0xF00000 ) {
					/* 1-byte 0xFx message. */
					fxenv->midi_idx = 3;
				} else {
					fxenv->midi_idx = 1;
				}
			} else if( fxenv->midi_idx == 1 ) {
				fxenv->midi_buf |= chr << 8;
				if( ( fxenv->midi_buf & 0xE00000 ) == 0xC00000 ) {
					/* 2-byte message. */
					fxenv->midi_idx = 3;
				} else {
					fxenv->midi_idx = 2;
				}
			} else if( fxenv->midi_idx == 2 ) {
				/* 3-byte message. */
				fxenv->midi_buf |= chr;
				fxenv->midi_idx = 3;
			}
			if( fxenv->midi_idx == 3 ) {
				/* Push message.*/
				event.type = fxenv->midi_event_type;
				event.user.code = fxenv->midi_buf;
				SDL_PushEvent( &event );
				fxenv->midi_idx = 0;
			}
		}
	}
	#endif
	while( offset < samples ) {
		count = samples - offset;
		if( fxenv->audio_idx + count > fxenv->audio_end ) {
			count = fxenv->audio_end - fxenv->audio_idx;
		}
		out_idx = offset << 1;
		out_end = ( offset + count ) << 1;
		aud_idx = fxenv->audio_idx << 1;
		while( out_idx < out_end ) {
			ampl = fxaudio[ aud_idx++ ];
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
			aud_idx = 0;
			aud_end = fxenv->stream_idx << 1;
			while( aud_idx < aud_end ) {
				fxaudio[ aud_idx ] += fxstream[ aud_idx ];
				aud_idx++;
			}
			fxenv->stream_idx = 0;
			fxenv->tick++;
		}
	}
}

static enum result execute_display_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct variable width = { 0, NULL }, height = { 0, NULL }, caption = { 0, NULL };
	struct expression *expr = this->source;
#if SDL_MAJOR_VERSION > 1
	struct fxenvironment *fxenv = ( struct fxenvironment * ) expr->function->env;
#endif
	enum result ret = expr->evaluate( expr, variables, &width, exception );
	if( ret ) {
		expr = expr->next;
		ret = expr->evaluate( expr, variables, &height, exception );
		if( ret ) {
			expr = expr->next;
			ret = expr->evaluate( expr, variables, &caption, exception );
			if( ret ) {
#if SDL_MAJOR_VERSION > 1
				if( !fxenv->window ) {
					fxenv->window = SDL_CreateWindow(
						caption.string_value ? caption.string_value->string : "",
						SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED,
						width.integer_value, height.integer_value, 0 );
					if( fxenv->window ) {
						fxenv->renderer = SDL_CreateRenderer(
							fxenv->window, -1, SDL_RENDERER_TARGETTEXTURE | SDL_RENDERER_PRESENTVSYNC );
						if( fxenv->renderer ) {
							fxenv->target = SDL_CreateTexture( fxenv->renderer,
								SDL_PIXELFORMAT_RGBA8888, SDL_TEXTUREACCESS_TARGET,
								width.integer_value, height.integer_value );
							if( fxenv->target ) {
								SDL_SetRenderDrawColor( fxenv->renderer, 0, 0, 0, 255 );
								SDL_RenderClear( fxenv->renderer );
								SDL_SetRenderTarget( fxenv->renderer, fxenv->target );
								SDL_RenderClear( fxenv->renderer );
								SDL_RenderPresent( fxenv->renderer );
							} else {
								ret = throw( exception, this->source, 0, SDL_GetError() );
								SDL_DestroyRenderer( fxenv->renderer );
								fxenv->renderer = NULL;
								SDL_DestroyWindow( fxenv->window );
								fxenv->window = NULL;
							}
						} else {
							ret = throw( exception, this->source, 0, SDL_GetError() );
							SDL_DestroyWindow( fxenv->window );
							fxenv->window = NULL;
						}
					} else {
						ret = throw( exception, this->source, 0, SDL_GetError() );
					}
				} else {
					ret = throw( exception, this->source, 0, "Window already open." );
				}
#else
				if( caption.string_value ) {
					SDL_WM_SetCaption( caption.string_value->string, "" );
				}
				if( SDL_SetVideoMode( width.integer_value, height.integer_value, 32, SDL_HWSURFACE ) == NULL ) {
					ret = throw( exception, this->source, 0, SDL_GetError() );
				}
#endif
				dispose_variable( &caption );
			}
			dispose_variable( &height );
		}
		dispose_variable( &width );
	}
	return ret;
}

static enum result execute_surface_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	enum result ret;
	int surf, width, height, len, idx = 0;
	struct variable params[ 4 ], *values;
	struct array *arr;
	Uint32 *pixels;
#if SDL_MAJOR_VERSION > 1
	struct SDL_Texture *texture = NULL;
#else
	struct SDL_Surface *surface = NULL;
#endif
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
#if SDL_MAJOR_VERSION > 1
				texture = SDL_CreateTexture( fxenv->renderer,
					SDL_PIXELFORMAT_RGBA8888, SDL_TEXTUREACCESS_STATIC, width, height );
				if( texture ) {
					if( params[ 3 ].string_value ) {
						if( params[ 3 ].string_value->type == ARRAY ) {
							arr = ( struct array * ) params[ 3 ].string_value;
							values = arr->array;
							pixels = malloc( arr->length * sizeof( Uint32 ) );
							if( pixels ) {
								idx = 0;
								len = arr->length;
								if( len >= width * height ) {
									len = width * height;
									while( idx < len ) {
										pixels[ idx ] = values[ idx ].integer_value;
										idx++;
									}
									if( SDL_UpdateTexture( texture, NULL, pixels, width * sizeof( Uint32 ) ) ) {
										ret = throw( exception, this->source, 0, SDL_GetError() );
									}
									SDL_SetTextureBlendMode( texture, SDL_BLENDMODE_BLEND );
								} else {
									ret = throw( exception, this->source, len, "Array index out of bounds." );
								}
								free( pixels );
							} else {
								ret = throw( exception, this->source, 0, OUT_OF_MEMORY );
							}
						} else {
							ret = throw( exception, this->source, 0, "Not an array." );
						}
					}
					if( ret ) {
						if( fxenv->surfaces[ surf ] ) {
							SDL_DestroyTexture( fxenv->surfaces[ surf ] );
						}
						fxenv->surfaces[ surf ] = texture;
					} else {
						SDL_DestroyTexture( texture );
					}
				} else {
					ret = throw( exception, this->source, 0, SDL_GetError() );
				}
#else
				surface = SDL_CreateRGBSurface( SDL_HWSURFACE, width, height,
					32, 0xFF000000, 0x00FF0000, 0x0000FF00, 0x000000FF );
				if( surface ) {
					if( params[ 3 ].string_value ) {
						if( params[ 3 ].string_value->type == ARRAY ) {
							arr = ( struct array * ) params[ 3 ].string_value;
							values = arr->array;
							if( SDL_LockSurface( surface ) == 0 ) {
								idx = 0;
								len = arr->length;
								if( len >= width * height ) {
									len = width * height;
									pixels = ( Uint32 * ) surface->pixels;
									while( idx < len ) {
										pixels[ idx ] = values[ idx ].integer_value;
										idx++;
									}
								} else {
									ret = throw( exception, this->source, len, "Array index out of bounds." );
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
#endif
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

static enum result execute_blit_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int idx = 0;
	enum result ret;
#if SDL_MAJOR_VERSION > 1
	struct SDL_Rect clip, dest;
#else
	struct SDL_Rect src, dest;
#endif
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
#if SDL_MAJOR_VERSION > 1
			clip.x = params[ 5 ].integer_value;
			clip.y = params[ 6 ].integer_value;
			clip.w = params[ 3 ].integer_value;
			clip.h = params[ 4 ].integer_value;
			dest.x = clip.x - params[ 1 ].integer_value;
			dest.y = clip.y - params[ 2 ].integer_value;
			SDL_QueryTexture( fxenv->surfaces[ idx ], NULL, NULL, &dest.w, &dest.h );
			SDL_RenderSetClipRect( fxenv->renderer, &clip );
			if( SDL_RenderCopy( fxenv->renderer, fxenv->surfaces[ idx ], NULL, &dest ) ) {
				ret = throw( exception, this->source, 0, SDL_GetError() );
			}
#else
			src.x = params[ 1 ].integer_value;
			src.y = params[ 2 ].integer_value;
			src.w = params[ 3 ].integer_value;
			src.h = params[ 4 ].integer_value;
			dest.x = params[ 5 ].integer_value;
			dest.y = params[ 6 ].integer_value;
			if( SDL_BlitSurface( fxenv->surfaces[ idx ], &src, SDL_GetVideoSurface(), &dest ) ) {
				ret = throw( exception, this->source, 0, SDL_GetError() );
			}
#endif
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

static enum result execute_rect_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	enum result ret;
	int idx = 0;
	struct SDL_Rect rect;
	struct variable params[ 5 ];
	struct expression *expr = this->source;
#if SDL_MAJOR_VERSION > 1
	int colour;
	struct fxenvironment *fxenv = ( struct fxenvironment * ) expr->function->env;
#endif
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
#if SDL_MAJOR_VERSION > 1
		colour = params[ 4 ].integer_value;
		SDL_RenderSetClipRect( fxenv->renderer, NULL );
		SDL_SetRenderDrawColor( fxenv->renderer,
			( colour >> 16 ) & 0xFF, ( colour >> 8 ) & 0xFF, colour & 0xFF, 0xFF );
		if( SDL_RenderFillRect( fxenv->renderer, &rect ) ) {
			ret = throw( exception, this->source, 0, SDL_GetError() );
		}
#else
		if( SDL_FillRect( SDL_GetVideoSurface(), &rect, params[ 4 ].integer_value ) ) {
			ret = throw( exception, this->source, 0, SDL_GetError() );
		}
#endif
	}
	idx = 0;
	while( idx < 5 ) {
		dispose_variable( &params[ idx++ ] );
	}
	return ret;
}

static enum result execute_show_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
#if SDL_MAJOR_VERSION > 1
	struct fxenvironment *fxenv = ( struct fxenvironment * ) this->source->function->env;
	SDL_SetRenderTarget( fxenv->renderer, NULL );
	SDL_RenderCopy( fxenv->renderer, fxenv->target, NULL, NULL );
	SDL_RenderPresent( fxenv->renderer );
	SDL_SetRenderTarget( fxenv->renderer, fxenv->target );
#else
	SDL_UpdateRect( SDL_GetVideoSurface(), 0, 0, 0, 0 );
#endif
	return OKAY;
}

static enum result execute_sleep_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct variable millis = { 0, NULL };
	enum result ret = this->source->evaluate( this->source, variables, &millis, exception );
	if( ret ) {
		if( millis.integer_value > 0 ) {
			SDL_Delay( millis.integer_value );
		}
		dispose_variable( &millis );
	}
	return ret;
}

static enum result execute_timer_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct variable millis = { 0, NULL };
	struct fxenvironment *fxenv = ( struct fxenvironment * ) this->source->function->env;
	enum result ret = this->source->evaluate( this->source, variables, &millis, exception );
	if( ret ) {
		if( millis.integer_value > 0 ) {
			fxenv->timer = SDL_AddTimer( millis.integer_value, timer_callback, fxenv );
			if( !fxenv->timer ) {
				ret = throw( exception, this->source, millis.integer_value, "Unable to start timer." );
			}
		} else {
			if( SDL_RemoveTimer( fxenv->timer ) == 0 ) {
				ret = throw( exception, this->source, millis.integer_value, "Unable to stop timer." );
			}
		}
		dispose_variable( &millis );
	}
	return ret;
}

static enum result execute_audio_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct variable param = { 0, NULL };
	struct fxenvironment *fxenv = ( struct fxenvironment * ) this->source->function->env;
	SDL_AudioSpec audiospec = { 0 };
	int ticklen;
	enum result ret = this->source->evaluate( this->source, variables, &param, exception );
	if( ret ) {
		/* audio ticklen; (Samples per tick at 24khz, 480 = 50hz) */
		if( param.integer_value > 0 ) {
			ticklen = param.integer_value * SAMPLE_RATE / 24000;
			if( ticklen >= MIN_TICK_LEN && ticklen <= MAX_TICK_LEN ) {
				SDL_LockAudio();
				fxenv->tick_len = ticklen;
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
				ret = throw( exception, this->source, param.integer_value, "Invalid tick length." );
			}
		} else {
			SDL_CloseAudio();
		}
		dispose_variable( &param );
	}
	return ret;
}

static enum result execute_sample_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	/* sample index "data" loopstart looplen; */
	enum result ret;
	int loop, llen, lend, idx = 0;
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

static enum result execute_play_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct expression *expr = this->source;
	struct variable channel = { 0, NULL }, sequence = { 0, NULL };
	struct fxenvironment *fxenv = ( struct fxenvironment * ) this->source->function->env;
	enum result ret = expr->evaluate( expr, variables, &channel, exception );
	if( ret ) {
		expr = expr->next;
		ret = expr->evaluate( expr, variables, &sequence, exception );
		if( ret ) {
			if( channel.integer_value >= 0 && channel.integer_value < NUM_CHANNELS ) {
				if( sequence.string_value && sequence.string_value->type == STRING ) {
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

static enum result execute_midi_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	#if defined( ALSA_MIDI )
	int err = 0;
	struct fxenvironment *fxenv = ( struct fxenvironment * ) this->source->function->env;
	#endif
	struct variable device = { 0, NULL };
	enum result ret = this->source->evaluate( this->source, variables, &device, exception );
	if( ret ) {
		#if defined( ALSA_MIDI )
		SDL_LockAudio();
		if( fxenv->midi_in ) {
			err = snd_rawmidi_close( fxenv->midi_in );
			fxenv->midi_in = NULL;
		}
		if( err == 0 && device.string_value ) {
			err = snd_rawmidi_open( &fxenv->midi_in, NULL, device.string_value->string, SND_RAWMIDI_NONBLOCK );
		}
		SDL_UnlockAudio();
		if( err < 0 ) {
			ret = throw( exception, this->source, err, snd_strerror( err ) );
		}
		#else
		ret = throw( exception, this->source, 0, "MIDI not supported." );
		#endif
		dispose_variable( &device );
	}
	return ret;
}

static struct element* parse_display_statement( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, env, func, prev, execute_display_statement, message );
}

static struct element* parse_show_statement( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message ) {
	struct element *next = elem->next;
	struct statement *stmt = new_statement( message );
	if( stmt ) {
		stmt->source = calloc( 1, sizeof( struct expression ) );
		if( stmt->source ) {
			stmt->source->line = elem->line;
			stmt->source->function = func;
			stmt->execute = execute_show_statement;
			prev->next = stmt;
			next = next->next;
		} else {
			strcpy( message, OUT_OF_MEMORY );
		}
	}
	return next;
}

static struct element* parse_surface_statement( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, env, func, prev, execute_surface_statement, message );
}

static struct element* parse_blit_statement( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, env, func, prev, execute_blit_statement, message );
}

static struct element* parse_rect_statement( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, env, func, prev, execute_rect_statement, message );
}

static struct element* parse_sleep_statement( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, env, func, prev, execute_sleep_statement, message );
}

static struct element* parse_timer_statement( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, env, func, prev, execute_timer_statement, message );
}

static struct element* parse_audio_statement( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, env, func, prev, execute_audio_statement, message );
}

static struct element* parse_sample_statement( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, env, func, prev, execute_sample_statement, message );
}

static struct element* parse_play_statement( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, env, func, prev, execute_play_statement, message );
}

static struct element* parse_queue_statement( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message ) {
	struct element *next = parse_expr_list_statement( elem, env, func, prev, execute_play_statement, message );
	if( prev->next ) {
		prev->next->local = 1;
	}
	return next;
}

static struct element* parse_midi_statement( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, env, func, prev, execute_midi_statement, message );
}

static enum result handle_event_expression( struct expression *this, SDL_Event *event,
	struct variable *result, struct variable *exception ) {
	struct fxenvironment *fxenv = ( struct fxenvironment * ) this->function->env;
	enum result ret = OKAY;
	if( event->type == SDL_QUIT ) {
		ret = throw_exit( &fxenv->env, exception, 0 );
	} else {
		if( event->type == fxenv->seq_event_type ) {
			fxenv->seq_msg = event->user.code;
		} else if( event->type == fxenv->midi_event_type ) {
			fxenv->midi_msg = event->user.code;
#if SDL_MAJOR_VERSION > 1
		} else if( event->type == SDL_WINDOWEVENT ) {
			fxenv->win_event = event->window.event;
#else
		} else if( event->type == SDL_VIDEOEXPOSE ) {
			fxenv->win_event = SDL_VIDEOEXPOSE;
#endif
		} else if( event->type == SDL_KEYDOWN || event->type == SDL_KEYUP ) {
			fxenv->key = event->key.keysym.sym;
#if SDL_MAJOR_VERSION > 1
			fxenv->key_held = event->key.repeat;
		} else if( event->type == SDL_MOUSEWHEEL ) {
			fxenv->wheel = event->wheel.y;
#endif
		}
		dispose_variable( result );
		result->integer_value = event->type;
		result->string_value = NULL;
	}
	return ret;
}

static enum result evaluate_winmsg_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	dispose_variable( result );
#if SDL_MAJOR_VERSION > 1
	result->integer_value = ( ( struct fxenvironment * ) this->function->env )->win_event;
#else
	result->integer_value = SDL_VIDEOEXPOSE;
#endif
	result->string_value = NULL;
	return OKAY;
}

static enum result evaluate_midimsg_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	dispose_variable( result );
	result->integer_value = ( ( struct fxenvironment * ) this->function->env )->midi_msg;
	result->string_value = NULL;
	return OKAY;
}

static enum result evaluate_pollevent_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	SDL_Event event = { 0 };
#if SDL_MAJOR_VERSION > 1
	event.type = SDL_FIRSTEVENT;
#else
	event.type = SDL_NOEVENT;
#endif
	SDL_PollEvent( &event );
	return handle_event_expression( this, &event, result, exception );
}

static enum result evaluate_waitevent_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	SDL_Event event;
	SDL_WaitEvent( &event );
	return handle_event_expression( this, &event, result, exception );
}

static enum result evaluate_millis_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	dispose_variable( result );
	result->integer_value = SDL_GetTicks();
	result->string_value = NULL;
	return OKAY;
}

static enum result evaluate_xmouse_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	dispose_variable( result );
	SDL_GetMouseState( &result->integer_value, NULL );
	result->string_value = NULL;
	return OKAY;
}

static enum result evaluate_ymouse_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	dispose_variable( result );
	SDL_GetMouseState( NULL, &result->integer_value );
	result->string_value = NULL;
	return OKAY;
}

static enum result evaluate_mousekey_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	dispose_variable( result );
	result->integer_value = SDL_GetMouseState( NULL, NULL );
	result->string_value = NULL;
	return OKAY;
}

static enum result evaluate_mousewheel_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	dispose_variable( result );
	result->integer_value = ( ( struct fxenvironment * ) this->function->env )->wheel;
	result->string_value = NULL;
	return OKAY;
}

static enum result evaluate_keyboard_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	dispose_variable( result );
	result->integer_value = ( ( struct fxenvironment * ) this->function->env )->key;
	result->string_value = NULL;
	return OKAY;
}

static enum result evaluate_keyshift_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	dispose_variable( result );
	result->integer_value = SDL_GetModState();
	result->string_value = NULL;
	return OKAY;
}

static enum result evaluate_keyheld_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	dispose_variable( result );
	result->integer_value = ( ( struct fxenvironment * ) this->function->env )->key_held;
	result->string_value = NULL;
	return OKAY;
}

/* Returns a value incremented every sequencer tick while the audio device is running. */
static enum result evaluate_seqtick_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	dispose_variable( result );
	result->integer_value = ( ( struct fxenvironment * ) this->function->env )->tick;
	result->string_value = NULL;
	return OKAY;
}

/*
	Returns an integer of the form 0xccpppppp containing the
	channel and parameter of the most recently handled sequencer event.
*/
static enum result evaluate_seqmsg_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	dispose_variable( result );
	result->integer_value = ( ( struct fxenvironment * ) this->function->env )->seq_msg;
	result->string_value = NULL;
	return OKAY;
}

static struct element *new_directory_element( char *name, off_t size, struct element *prev ) {
	struct element *parent, *elem;
	int len = strlen( name );
	char num[ 24 ];
	parent = new_element( 2 );
	if( parent ) {
		parent->str.string = "{}";
		elem = new_element( write_byte_string( name, len, NULL ) );
		if( elem ) {
			parent->child = elem;
			write_byte_string( name, len, elem->str.string );
			sprintf( num, "%ld", size );
			elem = new_element( strlen( num ) ); 
			if( elem ) {
				parent->child->next = elem;
				strcpy( elem->str.string, num );
			}
		}
		if( elem == NULL ) {
			unref_string( &parent->str );
			parent = NULL;
		}
	}
	if( prev ) {
		prev->next = parent;
	}
	return parent;
}

static int get_file_length( char *path, char *name ) {
	struct stat status;
	int pathlen = strlen( path ), length = 0;
	char *file = malloc( pathlen + strlen( name ) + 2 );
	if( file ) {
		strcpy( file, path );
		if( path[ 0 ] == '/' ) {
			file[ pathlen ] = '/';
		} else {
			file[ pathlen ] = '\\';
		}
		strcpy( &file[ pathlen + 1 ], name );
		if( stat( file, &status ) ) {
			errno = status.st_mode = status.st_size = 0;
		}
		if( status.st_mode & S_IFDIR ) {
			length = -1;
		} else if( status.st_size > MAX_INTEGER ) {
			length = MAX_INTEGER;
		} else {
			length = status.st_size;
		}
		free( file );
	}
	return length;
}

static enum result evaluate_dir_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	DIR *dir;
	char *path;
	struct dirent *dentry;
	struct element *elem = NULL, *head = NULL;
	struct variable var = { 0, NULL };
	struct expression *parameter = this->parameters;
	enum result ret = parameter->evaluate( parameter, variables, &var, exception );
	if( ret ) {
		if( var.string_value && var.string_value->string ) {
			errno = 0;
			path = realpath( var.string_value->string, NULL );
			if( path ) {
				dir = opendir( path );
				if( dir ) {
					dentry = readdir( dir );
					while( dentry && ret ) {
						elem = new_directory_element( dentry->d_name, get_file_length( path, dentry->d_name ), elem );
						if( head == NULL ) {
							head = elem;
						}
						if( elem ) {
							dentry = readdir( dir );
						} else {
							ret = throw( exception, this, 0, OUT_OF_MEMORY );
						}
					}
					if( errno ) {
						ret = throw( exception, this, errno, strerror( errno ) );
					}
					if( ret ) {
						dispose_variable( result );
						result->integer_value = 0;
						if( head ) {
							result->string_value = &head->str;
						} else {
							result->string_value = NULL;
						}
					} else if( head ) {
						unref_string( &head->str );
					}
					closedir( dir );
				} else {
					ret = throw( exception, this, errno, strerror( errno ) );
				}
				free( path );
			} else {
				if( errno ) {
					ret = throw( exception, this, errno, strerror( errno ) );
				} else {
					ret = throw( exception, this, 0, "Invalid path." );
				}
			}
		} else {
			ret = throw( exception, this, 0, "Not a string." );
		}
		dispose_variable( &var );
	}
	return ret;
}

static enum result evaluate_path_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct expression *parameter = this->parameters;
	struct variable var = { 0, NULL };
	struct string *str;
	char *path;
	enum result ret = parameter->evaluate( parameter, variables, &var, exception );
	if( ret ) {
		if( var.string_value ) {
			errno = 0;
			path = realpath( var.string_value->string, NULL );
			if( path ) {
				str = new_string_value( strlen( path ) );
				if( str ) {
					memcpy( str->string, path, str->length );
					dispose_variable( result );
					result->integer_value = 0;
					result->string_value = str;
				} else {
					ret = throw( exception, this, 0, OUT_OF_MEMORY );
				}
				free( path );
			} else {
				if( errno ) {
					ret = throw( exception, this, errno, strerror( errno ) );
				} else {
					ret = throw( exception, this, 0, "Invalid path." );
				}
			}
		} else {
			ret = throw( exception, this, 0, "Not a string." );
		}
		dispose_variable( &var );
	}
	return ret;
}

/* If a program was loaded from bank 0 of a datfile, return the entire datfile as a string. */
static enum result evaluate_datfile_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct fxenvironment *fxenv = ( struct fxenvironment * ) this->function->env;
	assign_variable( &fxenv->datfile, result );
	return OKAY;
}

/* Extract the specified bank from the specified datfile. */
static enum result evaluate_extract_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int bank_length;
	struct string *str;
	struct variable datfile = { 0 }, bank = { 0 };
	struct expression *parameter = this->parameters;
	enum result ret = parameter->evaluate( parameter, variables, &datfile, exception );
	if( ret ) {
		if( datfile.string_value ) {
			parameter = parameter->next;
			ret = parameter->evaluate( parameter, variables, &bank, exception );
			if( ret ) {
				bank_length = datfile_extract( datfile.string_value->string,
					datfile.string_value->length, bank.integer_value, NULL );
				if( bank_length >= 0 ) {
					str = new_string_value( bank_length );
					if( str ) {
						datfile_extract( datfile.string_value->string,
							datfile.string_value->length, bank.integer_value, str->string );
						dispose_variable( result );
						result->integer_value = 0;
						result->string_value = str;
					} else {
						ret = throw( exception, this, 0, OUT_OF_MEMORY );
					}
				} else if( bank_length == -1 ) {
					ret = throw( exception, this, bank.integer_value, "Invalid bank." );
				} else {
					ret = throw( exception, this, bank_length, "Not a datfile." );
				}
				dispose_variable( &bank );
			}
		} else {
			ret = throw( exception, this, 0, "Not a string." );
		}
		dispose_variable( &datfile );
	}
	return ret;
}

/*
	Write up to the specified number of 16-bit pairs of stereo samples from the specified array
	into the audio output buffer. The number of samples written is returned.
*/
static enum result evaluate_stream_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	/* count = $stream( array offset count ) */
	int length, samples, idx, end, off;
	struct expression *parameter = this->parameters;
	struct variable arr = { 0, NULL }, offset = { 0, NULL }, count = { 0, NULL }, *values;
	struct fxenvironment *fxenv = ( struct fxenvironment * ) this->function->env;
	enum result ret = parameter->evaluate( parameter, variables, &arr, exception );
	if( ret ) {
		parameter = parameter->next;
		ret = parameter->evaluate( parameter, variables, &offset, exception );
		if( ret ) {
			parameter = parameter->next;
			ret = parameter->evaluate( parameter, variables, &count, exception );
			if( ret ) {
				if( arr.string_value && arr.string_value->string && arr.string_value->type == ARRAY ) {
					values = ( ( struct array * ) arr.string_value )->array;
					length = ( ( ( struct array * ) arr.string_value )->length ) >> 1;
					if( offset.integer_value >= 0 && count.integer_value >= 0
					&& MAX_INTEGER - count.integer_value >= offset.integer_value
					&& offset.integer_value + count.integer_value <= length ) {
						SDL_LockAudio();
						samples = fxenv->audio_end - fxenv->stream_idx;
						if( samples > count.integer_value ) {
							samples = count.integer_value;
						}
						idx = fxenv->stream_idx << 1;
						end = idx + ( samples << 1 );
						off = offset.integer_value << 1;
						while( idx < end ) {
							fxenv->stream[ idx++ ] = values[ off++ ].integer_value;
						}
						fxenv->stream_idx += samples;
						SDL_UnlockAudio();
						dispose_variable( result );
						result->integer_value = samples;
						result->string_value = NULL;
					} else {
						ret = throw( exception, this, offset.integer_value, "Range out of bounds." );
					}
				} else {
					ret = throw( exception, this, 0, "Not an array." );
				}	
				dispose_variable( &count );
			}
			dispose_variable( &offset );
		}
		dispose_variable( &arr );
	}
	return ret;
}

static struct constant fxconstants[] = {
#if SDL_MAJOR_VERSION > 1
	{ "WINDOW_EVENT", SDL_WINDOWEVENT, NULL },
	{ "WINDOW_EXPOSED_MSG", SDL_WINDOWEVENT_EXPOSED, NULL },
#else
	{ "WINDOW_EVENT", SDL_VIDEOEXPOSE, NULL },
	{ "WINDOW_EXPOSED_MSG", SDL_VIDEOEXPOSE, NULL },
#endif
	{ "KEY_PRESSED_EVENT", SDL_KEYDOWN, NULL },
	{ "KEY_RELEASED_EVENT", SDL_KEYUP, NULL },
	{ "MOUSE_MOTION_EVENT", SDL_MOUSEMOTION, NULL },
	{ "MOUSE_PRESSED_EVENT", SDL_MOUSEBUTTONDOWN, NULL },
	{ "MOUSE_RELEASED_EVENT", SDL_MOUSEBUTTONUP, NULL },
#if SDL_MAJOR_VERSION > 1
	{ "MOUSE_WHEEL_EVENT", SDL_MOUSEWHEEL, NULL },
#else
	{ "MOUSE_WHEEL_EVENT", SDL_NOEVENT, NULL },
#endif
	{ "KEY_BACKSPACE", SDLK_BACKSPACE, NULL },
	{ "KEY_TAB", SDLK_TAB, NULL },
	{ "KEY_RETURN", SDLK_RETURN, NULL },
	{ "KEY_ESCAPE", SDLK_ESCAPE, NULL },
	{ "KEY_SPACE", SDLK_SPACE, NULL },
	{ "KEY_0", SDLK_0, NULL },
	{ "KEY_A", SDLK_a, NULL },
#if SDL_MAJOR_VERSION > 1
	{ "KEY_PAD_0", SDLK_KP_0, NULL },
	{ "KEY_PAD_1", SDLK_KP_1, NULL },
#else
	{ "KEY_PAD_0", SDLK_KP0, NULL },
	{ "KEY_PAD_1", SDLK_KP1, NULL },
#endif
	{ "KEY_PAD_PERIOD", SDLK_KP_PERIOD, NULL },
	{ "KEY_PAD_DIVIDE", SDLK_KP_DIVIDE, NULL },
	{ "KEY_PAD_MULTIPLY", SDLK_KP_MULTIPLY, NULL },
	{ "KEY_PAD_MINUS", SDLK_KP_MINUS, NULL },
	{ "KEY_PAD_PLUS", SDLK_KP_PLUS, NULL },
	{ "KEY_PAD_ENTER", SDLK_KP_ENTER, NULL },
	{ "KEY_PAD_EQUALS", SDLK_KP_EQUALS, NULL },
	{ "KEY_UP", SDLK_UP, NULL },
	{ "KEY_DOWN", SDLK_DOWN, NULL },
	{ "KEY_LEFT", SDLK_LEFT, NULL },
	{ "KEY_RIGHT", SDLK_RIGHT, NULL },
	{ "KEY_INSERT", SDLK_INSERT, NULL },
	{ "KEY_DELETE", SDLK_DELETE, NULL },
	{ "KEY_HOME", SDLK_HOME, NULL },
	{ "KEY_END", SDLK_END, NULL },
	{ "KEY_PAGE_UP", SDLK_PAGEUP, NULL },
	{ "KEY_PAGE_DOWN", SDLK_PAGEDOWN, NULL },
	{ "KEY_F1", SDLK_F1, NULL },
	{ NULL }
};

static struct operator fxoperators[] = {
	{ "$millis", '$', 0, evaluate_millis_expression, &fxoperators[ 1 ] },
	{ "$pollevent", '$', 0, evaluate_pollevent_expression, &fxoperators[ 2 ] },
	{ "$waitevent", '$', 0, evaluate_waitevent_expression, &fxoperators[ 3 ] },
	{ "$xmouse", '$', 0, evaluate_xmouse_expression, &fxoperators[ 4 ] },
	{ "$ymouse", '$', 0, evaluate_ymouse_expression, &fxoperators[ 5 ] },
	{ "$mousekey", '$', 0, evaluate_mousekey_expression, &fxoperators[ 6 ] },
	{ "$mousewheel", '$', 0, evaluate_mousewheel_expression, &fxoperators[ 7 ] },
	{ "$keyboard", '$', 0, evaluate_keyboard_expression, &fxoperators[ 8 ] },
	{ "$keyshift", '$', 0, evaluate_keyshift_expression, &fxoperators[ 9 ] },
	{ "$seqtick", '$', 0, evaluate_seqtick_expression, &fxoperators[ 10 ] },
	{ "$seqmsg", '$', 0, evaluate_seqmsg_expression, &fxoperators[ 11 ] },
	{ "$dir", '$', 1, evaluate_dir_expression, &fxoperators[ 12 ] },
	{ "$path", '$', 1, evaluate_path_expression, &fxoperators[ 13 ] },
	{ "$midimsg", '$', 0, evaluate_midimsg_expression, &fxoperators[ 14 ] },
	{ "$winmsg", '$', 0, evaluate_winmsg_expression, &fxoperators[ 15 ] },
	{ "$keyheld", '$', 0, evaluate_keyheld_expression, &fxoperators[ 16 ] },
	{ "$datfile", '$', 0, evaluate_datfile_expression, &fxoperators[ 17 ] },
	{ "$extract", '$', 2, evaluate_extract_expression, &fxoperators[ 18 ] },
	{ "$stream", '$', 3, evaluate_stream_expression, NULL }
};

static struct keyword fxstatements[] = {
	{ "display", "xxx;", parse_display_statement, &fxstatements[ 1 ] },
	{ "show", ";", parse_show_statement, &fxstatements[ 2 ] },
	{ "surface", "xxxx;", parse_surface_statement, &fxstatements[ 3 ] },
	{ "blit", "xxxxxxx;", parse_blit_statement, &fxstatements[ 4 ] },
	{ "rect", "xxxxx;", parse_rect_statement, &fxstatements[ 5 ] },
	{ "sleep", "x;", parse_sleep_statement, &fxstatements[ 6 ] },
	{ "timer", "x;", parse_timer_statement, &fxstatements[ 7 ] },
	{ "audio", "x;", parse_audio_statement, &fxstatements[ 8 ] },
	{ "sample", "xxxx;", parse_sample_statement, &fxstatements[ 9 ] },
	{ "queue", "xx;", parse_queue_statement, &fxstatements[ 10 ] },
	{ "play", "xx;", parse_play_statement, &fxstatements[ 11 ] },
	{ "midi", "x;", parse_midi_statement, NULL }
};

static int add_event_constants( struct fxenvironment *env, char *message ) {
	struct constant event[ 4 ] = { NULL };
#if SDL_MAJOR_VERSION > 1
	int user_event = SDL_RegisterEvents( 3 );
#else
	int user_event = SDL_USEREVENT;
#endif
	env->timer_event_type = user_event++;
	event[ 0 ].name = "TIMER_EVENT";
	event[ 0 ].integer_value = env->timer_event_type;
	env->seq_event_type = user_event++;
	event[ 1 ].name = "SEQUENCER_EVENT";
	event[ 1 ].integer_value = env->seq_event_type;
	env->midi_event_type = user_event++;
	event[ 2 ].name = "MIDI_EVENT";
	event[ 2 ].integer_value = env->midi_event_type;
	return add_constants( &event[ 0 ], &env->env, message );
}

static void dispose_fxenvironment( struct fxenvironment *fxenv ) {
	int idx;
	if( fxenv ) {
		dispose_variable( &fxenv->datfile );
#if SDL_MAJOR_VERSION > 1
		if( fxenv->window ) {
			SDL_DestroyWindow( fxenv->window );
		}
		if( fxenv->renderer ) {
			SDL_DestroyRenderer( fxenv->renderer );
		}
#else
		for( idx = 0; idx < NUM_SURFACES; idx++ ) {
			if( fxenv->surfaces[ idx ] ) {
				SDL_FreeSurface( fxenv->surfaces[ idx ] );
			}
		}
#endif
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
		#if defined( ALSA_MIDI )
		if( fxenv->midi_in ) {
			SDL_CloseAudio();
			snd_rawmidi_close( fxenv->midi_in );
		}
		#endif
		dispose_environment( ( struct environment * ) fxenv );
	}
}

static struct fxenvironment* new_fxenvironment( char *message ) {
	int idx;
	struct fxenvironment *fxenv = calloc( 1, sizeof( struct fxenvironment ) );
	if( fxenv ) {
		for( idx = 0; idx < NUM_CHANNELS; idx++ ) {
			fxenv->channels[ idx ].gain = 4096;
		}
		if( !( initialize_environment( &fxenv->env, message )
		&& add_constants( fxconstants, &fxenv->env, message )
		&& add_event_constants( fxenv, message )
		&& add_statements( fxstatements, &fxenv->env, message )
		&& add_operators( fxoperators, &fxenv->env, message ) ) ) {
			dispose_fxenvironment( fxenv );
			fxenv = NULL;
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return fxenv;
}

static int parse_ttfx_file( char *file_name, struct fxenvironment *env, char *message ) {
	long file_length, bank_length, success = 0;
	struct string *program_buffer;
	/* Load program file into string.*/
	file_length = load_file( file_name, NULL, message );
	if( file_length >= MAX_INTEGER ) {
		strcpy( message, "File too large." );
	} else if( file_length >= 0 ) {
		program_buffer = new_string_value( file_length );
		if( program_buffer ) {
			file_length = load_file( file_name, program_buffer->string, message );
			bank_length = datfile_extract( program_buffer->string, file_length, 0, NULL );
			if( bank_length >= 0 ) {
				/* Extract program from bank 0 of datfile. */
				env->datfile.string_value = program_buffer;
				program_buffer = new_string_value( bank_length );
				if( program_buffer ) {
					file_length = datfile_extract( env->datfile.string_value->string, 
						env->datfile.string_value->length, 0, program_buffer->string );
				}
			} else if( bank_length == -1 ) {
				strcpy( message, "Invalid program file." );
			}
		}
		if( program_buffer ) {
			if( file_length >= 0 ) {
				/* Parse program structure.*/
				success = parse_tt_program( program_buffer->string, file_name, &env->env, message );
			}
			unref_string( program_buffer );
		} else {
			strcpy( message, OUT_OF_MEMORY );
		}
	}
	return success;
}

int main( int argc, char **argv ) {
	int exit_code = EXIT_FAILURE;
	char *file_name, message[ 256 ] = "";
	struct environment *env;
	struct variable result = { 0 }, except = { 0 };
	struct expression expr = { 0 };
	/* Handle command-line.*/
	if( argc < 2 ) {
		fprintf( stderr, "Usage: %s program.tt [args]\n", argv[ 0 ] );
		return EXIT_FAILURE;
	}
	file_name = argv[ 1 ];
	/* Parse program file. */
	fxenv = new_fxenvironment( message );
	if( fxenv ) {
		env = &fxenv->env;
		env->argc = argc - 1;
		env->argv = &argv[ 1 ];
		if( parse_ttfx_file( file_name, fxenv, message ) ) {
			if( env->entry_points ) {
				/* Initialize SDL. */
				if( SDL_Init( SDL_INIT_AUDIO | SDL_INIT_VIDEO | SDL_INIT_TIMER ) == 0 ) {
					/* Install signal handler. */
					interrupt_handler = signal( SIGINT, signal_handler );
					if( interrupt_handler != SIG_ERR ) {
						/* Evaluate the last entry-point function. */
						initialize_call_expr( &expr, env->entry_points );
						if( initialize_globals( env, &except ) && expr.evaluate( &expr, NULL, &result, &except ) ) {
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
					} else {
						fprintf( stderr, "Unable to install signal handler: %s\n", strerror( errno ) );
					}
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
		dispose_fxenvironment( ( struct fxenvironment * ) env );
	} else {
		fputs( message, stderr );
	}
	return exit_code;
}
