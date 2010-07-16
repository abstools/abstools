//$Id$

package abs.frontend.parser;


import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;

import abs.frontend.analyser.SemanticError;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.Model;
import abs.frontend.parser.ABSParser;
import abs.frontend.parser.ABSScanner;
import abs.frontend.parser.SyntaxError;

public class Main {

	static boolean verbose = false ;
	static boolean typecheck = false;

	public static void main(final String[] args) throws Exception {
		int numoptions = 0;
		
		for (String arg : args) {
		    if (arg.equals("-v"))
		        verbose = true;
		    else if (arg.equals("-t")) 
		        typecheck = true;
		    else if (arg.equals("-h")) {
		        printUsage();
		        System.exit(1);
		    } else
		        break;
		    
		    numoptions++;
		}

		for (int i = numoptions; i < args.length; i++){
		    String arg = args[i];
	        Model m = null;

			try{
				m = parse(arg);
				
				if (typecheck) {
				    SemanticErrorList typeerrors = m.typeCheck();
				    for (SemanticError se : typeerrors) {
				        System.err.println(arg+ ":" + se.getMsgString());
				    }
				}
			} catch (FileNotFoundException e1) {
				System.err.println("File not found: " + arg);
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
				int numSemErrs = m.getErrors().size();
				
				if (numSemErrs > 0) {
					System.out.println("Semantic errors: " + numSemErrs);
					for (SemanticError error : m.getErrors())
						System.err.println(arg + ":" + error.getMsgString());
					System.err.flush();
				}
			}
		}
	}


	
	
    private static void printUsage() {
        System.out.println("Usage: java "+Main.class.getName()+" [options] <absfiles>\n" +
        		"  <absfiles>   ABS files to parse\n" +
        		"Options:\n"+
        		"  -v   verbose output\n" +
        		"  -t   enable typechecking\n" +
        		"  -h   print this message\n");
        
    }




    public static Model parse(String file) throws Exception {
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

		return parse(reader); 
	}
	
	public static Model parse(InputStream stream) throws Exception {
	    return parse(new BufferedReader(new InputStreamReader(stream)));
	}
	    
    public static Model parse(Reader reader) throws Exception {
        ABSParser parser = new ABSParser();
        ABSScanner scanner = new ABSScanner(reader);
        Model m = (Model)parser.parse(scanner);
        reader.close();
	    return m;
	}
}
