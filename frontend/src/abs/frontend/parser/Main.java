//$Id$

package abs.frontend.tests;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;

import abs.frontend.ast.Model;
import abs.frontend.parser.ABSParser;
import abs.frontend.parser.ABSScanner;
import abs.frontend.parser.SyntaxError;

public class ParserTest {

	static boolean verbose = false ; 

	public static void main(String[] args) throws Exception {

		String[] shiftedArgs = null  ; 
		Model m = null;

		//shifting option -v 
		if (args.length > 0 && args[0].equals("-v")) {
			verbose = true;
			shiftedArgs = new String[args.length-1];
			System.arraycopy(args, 1, shiftedArgs, 0, args.length-1); 
			args = shiftedArgs ;
		}

		for (String arg : args){
			try{
				m = parse(arg);
			} catch (FileNotFoundException e1) {
				System.err.println("File not found: " + arg);
				System.err.flush();
			} catch (SyntaxError pex) {
				// Exc. thrown by the parser
				System.err.println(arg + ":" + pex.getMessage());
				System.err.flush();
			} catch (Exception e1) {
				// Catch-all
				System.err.println("Compilation of " + arg +  " failed with Exception");
				System.err.println(e1);
//				e1.printStackTrace(System.err);
				System.err.flush();
			}
			// Dump tree for debug
			if (verbose){ 
				System.out.println("Result:");
				if (m!=null){
					System.out.println(m);
					m.dumpTree("  ", System.out);
				} else {
					System.out.println("(No result)");
				}
			}
			if (m != null) {
				int numSemErrs = m.errors().size();
				
				if (numSemErrs > 0) {
					System.out.println("Semantic errors: " + numSemErrs);
					for (Object error : m.errors())
						System.err.println(arg + ":" + error);
					System.err.flush();
				}
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
