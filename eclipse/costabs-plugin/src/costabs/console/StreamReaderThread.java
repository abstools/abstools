package costabs.console;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringWriter;

public class StreamReaderThread extends Thread {
		private StringWriter writer = null;
		private InputStream in = null;
		
		public StreamReaderThread(InputStream in) 
		{
			this.in = in;
		}
		
		public void run() 
		{
			try {
				if (in != null) {
					writer = new StringWriter();
					InputStreamReader isr = new InputStreamReader(in);
					BufferedReader br = new BufferedReader(isr);
					
					String line = null;
					
					//Read the lines of the buffered reader.
					while ( (line = br.readLine()) != null)
						writer.write(line + "\n");
					
					br.close();
				}
				
			} catch (IOException ioe) {
				ioe.printStackTrace();
			}
			
		}
		
		/*
		 * Gets the String representation of the BufferedReader.
		 */
		public String getContent() {
			if (this.writer != null)
				return writer.toString();
			else
				return "";
		}
	}