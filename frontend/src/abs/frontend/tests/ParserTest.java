//$Id$
package abs.frontend.tests;

import abs.frontend.ast.*;
import abs.frontend.parser.*;
import java.io.*;

public class ParserTest {



	public static void main(String[] args) throws Exception {

		Program p = null; 

		System.out.println("Testing parser");
		if (args.length == 0) {
			System.out.println("Usage java ParserTest filename");
		} else {
			System.out.println("Trying to parse: " + java.util.Arrays.toString(args));
			try{
				p = parse(args);
				System.out.println("Parsing suceeded. Result:");
			} catch (Error err) {
				System.err.println("Parsing failed with Error");
				System.err.println(err);
				err.printStackTrace(System.err);
			} catch (Exception e1) {
				System.err.println("Exception");
				System.err.println(e1);
				e1.printStackTrace(System.err);
			}
			//Dump tree for debug
			if (p!=null){
				System.out.println(p);
				p.dumpTree("  ", System.out);
			}

		}
	}


	protected static Program parse(String args[]) throws Exception {
		Reader reader = getReader(args);
		BufferedReader rd = null ;
		//Set to true to print source before parsing 
		boolean dumpinput=true;
		if (dumpinput){
			try {
				rd = new BufferedReader(reader);
				String line = null;
				int i = 1 ; 
				while ((line = rd.readLine()) != null) {
					System.out.println(i++ + "\t" + line);
				}
			} catch (IOException x) {
				System.err.println(x);
			} finally {
				if (rd != null) rd.close();
			}
			reader = getReader(args);

		}

		ABSParser parser = new ABSParser();
		ABSScanner scanner = new ABSScanner(reader);
		Program p = (Program)parser.parse(scanner);
		reader.close();
		return p; 
	}


	private static Reader getReader(String[] args) {
		Reader r = null;
		if (args.length != 1) {
			r = new InputStreamReader(System.in);
		} else {
			try {
				r = new FileReader(args[0]);
			} catch (FileNotFoundException e1) {
				System.err.println("Dumper: file " + args[0] + " not found");
			}
		}
		return r;
	}



}
