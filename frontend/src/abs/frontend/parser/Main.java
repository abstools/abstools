//$Id$

package abs.frontend.parser;


import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;

import abs.frontend.analyser.SemanticError;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.CompilationUnit;
import abs.frontend.ast.List;
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

		List<CompilationUnit> units = new List<CompilationUnit>();
		for (int i = numoptions; i < args.length; i++){
		    String arg = args[i];

			try{
				units.add(parseUnit(arg));
				
			} catch (FileNotFoundException e1) {
				System.err.println("File not found: " + arg);
                System.exit(1);
			} catch (SyntaxError pex) {
				// Exc. thrown by the parser
				System.err.println(arg + ":" + pex.getMessage());
                System.exit(1);
			} catch (Exception e1) {
				// Catch-all
				System.err.println("Compilation of " + arg +  " failed with Exception");
				System.err.println(e1);
				System.exit(1);
			}
		}
		
		if (units.getNumChild() == 0) {
		    System.err.println("Please provide at least one input file.");
		    System.exit(1);
		}
		
        Model m = new Model(units);
		
        if (typecheck) {
            SemanticErrorList typeerrors = m.typeCheck();
            for (SemanticError se : typeerrors) {
                System.err.println(se.getMsgString());
            }
        }
        
        // Dump tree for debug
        if (verbose){ 
            System.out.println("Result:");
            System.out.println(m);
            m.dumpTree("  ", System.out);
        }

        int numSemErrs = m.getErrors().size();
            
        if (numSemErrs > 0) {
            System.out.println("Semantic errors: " + numSemErrs);
            for (SemanticError error : m.getErrors()) {
                System.err.println(error.getMsgString());
                System.err.flush();
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




    public static CompilationUnit parseUnit(String file) throws Exception {
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

		return parseUnit(file, reader); 
	}
    
    public static Model parse(String fileName) throws Exception {
        return new Model(new List<CompilationUnit>().add(parseUnit(fileName)));
    }
	
	public static Model parse(String fileName, InputStream stream) throws Exception {
	    return parse(fileName, new BufferedReader(new InputStreamReader(stream)));
	}
	    
	public static Model parse(String fileName, Reader reader) throws Exception {
	    return new Model(new List<CompilationUnit>().add(parseUnit(fileName, reader)));
	}
	
    public static CompilationUnit parseUnit(String fileName, Reader reader) throws Exception {
        try {
            ABSParser parser = new ABSParser();
            ABSScanner scanner = new ABSScanner(reader);
            
            return parser.parse(fileName,scanner);
        } finally {
            reader.close();
        }
	}


    public static Model parseString(String s) throws Exception {
        return parse("<unkown>", new StringReader(s));
    }

}
