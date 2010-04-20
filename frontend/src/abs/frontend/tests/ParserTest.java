//$Id$
package abs.frontend.tests;

import abs.frontend.ast.*;
import abs.frontend.parser.*;
import java.io.*;
import java.util.ArrayList;

public class ParserTest {

	static boolean verbose = false ; 

	public static void main(String[] args) throws Exception {

		String[] shiftedArgs = null  ; 
		Model m = null;
		int errorcount = 0;
		ArrayList<String> errorfiles = new ArrayList<String>();
		
		//		System.out.println(args[0]);
		//shifting option -v 
		if (args[0].equals("-v")) {
			verbose = true;
			shiftedArgs = new String[args.length-1];
			System.arraycopy(args, 1, shiftedArgs, 0, args.length-1); 
			args = shiftedArgs ;
		}
		if (args.length == 0) {
			args = new String[]{
					"block.abs", "boundedbuffer.abs", "emptyblock.abs", "pingpong.abs", 
					"skeleton.abs", "skipblock.abs", "statements.abs", "trivial.abs",
					"PeerToPeer.abs",
					};
		} 
		for (String arg : args){
			System.out.println("Trying to parse: " + arg);
			System.out.println("==========");
			try{
				m = parse(arg);
				System.out.println("Parsing of " + arg + " suceeded. Result:");
			} catch (Error err) {
				System.out.flush();
				System.err.println("Parsing of " + arg + " failed with Error");
				System.err.println(err);
				err.printStackTrace(System.err);
				System.err.flush();
				errorfiles.add(arg);
				errorcount++;
			} catch (Exception e1) {
				System.out.flush();
				System.err.println("Parsing of " + arg +  " failed with Exception");
				System.err.println(e1);
				e1.printStackTrace(System.err);
				System.err.flush();
				errorfiles.add(arg);
				errorcount++;
			}
			//Dump tree for debug
			if (verbose){ 
				if (m!=null){
					System.out.println(m);
					m.dumpTree("  ", System.out);
				} else {
					System.out.println("(No result)");
				}
			}
		}
		if (errorcount == 0) {
			System.out.println("All " + Integer.toString(args.length) + " tests succeeded.");
		} else {
			System.out.println(Integer.toString(errorcount) + " out of "
					+ Integer.toString(args.length) + " tests failed:");
			for (String file : errorfiles) {
				System.out.println("   " + file);
			}
		}
	}


	protected static Model parse(String file) throws Exception {
		Reader reader = new FileReader(file);
		BufferedReader rd = null;
		//Set to true to print source before parsing 
		boolean dumpinput = verbose;
		if (dumpinput){
			try {
				rd = new BufferedReader(new FileReader(file));
				String line = null;
				int i = 1 ; 
				while ((line = rd.readLine()) != null) {
					System.out.println(i++ + "\t" + line);
				}
			} catch (IOException x) {
				System.out.flush();
				System.err.println(x);
				System.err.flush();
			} finally {
				if (rd != null) rd.close();
			}
		}

		ABSParser parser = new ABSParser();
		ABSScanner scanner = new ABSScanner(reader);
		Model m = (Model)parser.parse(scanner);
		reader.close();
		return m; 
	}

}
