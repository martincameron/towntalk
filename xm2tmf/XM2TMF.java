
package xm2tmf;

public class XM2TMF {
	public static final String VERSION = "MOD/S3M/XM to TMF converter (c)2017 mumart@gmail.com";

	private static void writeInt16be( byte[] output, int offset, int value ) {
		output[ offset ] = ( byte ) ( value >> 8 );
		output[ offset + 1 ] = ( byte ) value;
	}

	private static void writeInt32be( byte[] output, int offset, int value ) {
		output[ offset ] = ( byte ) ( value >> 24 );
		output[ offset + 1 ] = ( byte ) ( value >> 16 );
		output[ offset + 2 ] = ( byte ) ( value >> 8 );
		output[ offset + 3 ] = ( byte ) value;
	}

	private static void writeLatin1( byte[] output, int offset, String value ) {
		int idx = 0;
		int end = value.length();
		while( idx < end ) {
			output[ offset + idx ] = ( byte ) value.charAt( idx );
			idx++;
		}
	}

	public static int convert( Module module, IBXM ibxm, byte[] output ) {
		int numInstruments = module.instruments.length;
		if( numInstruments > 63 ) {
			throw new IllegalArgumentException( "Module has too many instruments." );
		}
		if( module.numChannels > 16 ) {
			throw new IllegalArgumentException( "Module has too many channels." );
		}
		int length = 32 * 64;
		int seqlen = ibxm.writeSequence( null, 0 );
		if( output != null ) {
			System.out.println( "Number of Channels: " + module.numChannels );
			System.out.println( "Sequence length: " + seqlen + " bytes.");
			writeLatin1( output, 0, "TMF0" );
			writeInt32be( output, 4, seqlen );
			String songName = module.songName;
			if( songName.length() > 23 ) {
				songName = songName.substring( 0, 23 );
			}
			writeLatin1( output, 8, songName );
			ibxm.writeSequence( output, length );
		}
		length = length + seqlen;
		int idx = 1;
		while( idx < numInstruments ) {
			Instrument instrument = module.instruments[ idx ];
			Sample sample = instrument.samples[ 0 ];
			int loopStart = sample.getLoopStart();
			int loopLength = sample.getLoopLength();
			if( output != null ) {
				writeInt32be( output, idx * 32, loopStart );
				writeInt32be( output, idx * 32 + 4, loopLength );
				String instrumentName = instrument.name;
				if( instrumentName.length() > 23 ) {
					instrumentName = instrumentName.substring( 0, 23 );
				}
				writeLatin1( output, idx * 32 + 8, instrumentName );
				sample.getSampleData( output, length, loopStart + loopLength );
			} 
			length = length + loopStart + loopLength;
			idx++;
		}
		return length;
	}

	public static void main( String[] args ) throws Exception {
		if( args.length != 2 ) {
			System.err.println( VERSION );
			System.err.println( "Usage: java " + XM2TMF.class.getName() + " input.xm output.tmf\n" );
		} else {
			/* Read module file.*/
			Module module = new Module( new Data( new java.io.FileInputStream( args[ 0 ] ) ) );
			IBXM ibxm = new IBXM( module, 48000 );
			/* Perform conversion. */
			byte[] tmf = new byte[ convert( module, ibxm, null ) ];
			java.io.OutputStream outputStream = new java.io.FileOutputStream( args[ 1 ] );
			try {
				convert( module, ibxm, tmf );
				outputStream.write( tmf );
			} finally {
				outputStream.close();
			}
		}
	}
}
