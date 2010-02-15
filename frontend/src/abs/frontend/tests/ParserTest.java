//$Id$
package abs.frontend.tests;

import abs.frontend.ast.*;
import abs.frontend.parser.*;
import java.io.*;
import java.util.ArrayList;

public class ParserTest {



	public static void main(String[] args) throws Exception {

		Program p = null;
		int errorcount = 0;
		ArrayList<String> errorfiles = new ArrayList<String>();

		System.out.println("Testing parser");
		if (args.length == 0) {
			args = new String[]{"block.abs", "boundedbuffer.abs"};
		} 
		for (String arg : args){
			System.out.println("Trying to parse: " + arg);
			System.out.println("==========");
			try{
				p = parse(arg);
				System.out.println("Parsing suceeded. Result:");
			} catch (Error err) {
				System.err.println("Parsing failed with Error");
				System.err.println(err);
				err.printStackTrace(System.err);
				errorfiles.add(arg);
				errorcount++;
			} catch (Exception e1) {
				System.err.println("Exception");
				System.err.println(e1);
				e1.printStackTrace(System.err);
				errorfiles.add(arg);
				errorcount++;
			}
			//Dump tree for debug
			if (p!=null){
				System.out.println(p);
				p.dumpTree("  ", System.out);
			}
		}
		if (errorcount == 0) {
			System.out.println("All tests succeeded.");
		} else {
			System.out.println(Integer.toString(errorcount) + " out of "
					+ Integer.toString(args.length) + " tests failed:");
			for (String file : errorfiles) {
				System.out.println("   " + file);
			}
		}
	}


	protected static Program parse(String file) throws Exception {
		Reader reader = new FileReader(file);
		BufferedReader rd = null;
		//Set to true to print source before parsing 
		boolean dumpinput = true;
		if (dumpinput){
			try {
				rd = new BufferedReader(new FileReader(file));
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
		}

		ABSParser parser = new ABSParser();
		ABSScanner scanner = new ABSScanner(reader);
		Program p = (Program)parser.parse(scanner);
		reader.close();
		return p; 
	}

}
