package abs.backend.maude;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.PrintStream;
import java.io.Reader;
import java.util.ArrayList;

import abs.frontend.analyser.SemanticError;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.CompilationUnit;
import abs.frontend.ast.Model;
import abs.frontend.parser.ABSParser;
import abs.frontend.parser.ABSScanner;
import abs.frontend.parser.Main;
import abs.frontend.parser.ParserError;

public class MaudeCompiler {

	/**
	 * @param args
	 * @throws Exception 
	 */
	public static void main(String[] args) throws Exception {
		ArrayList<String> files = new ArrayList<String>();
		boolean hasErrors = false;
		boolean stdlib = true;
		boolean expectOutputFile = false;
		File outputfile = null;
        String module = "ABS-SIMULATOR-RL";
		for (String arg : args) {
			if (expectOutputFile) {
				expectOutputFile = false;
				outputfile = new File(arg);
			} else if (arg.equals("-nostdlib"))  {
                stdlib = false;
            } else if (arg.equals("-timed")) {
                module = "ABS-SIMULATOR-EQ-TIMED";
            } else if (arg.equals("-h")) {
		        printUsage();
		        System.exit(1);
		    } else if (arg.equals("-o")) {
		    	expectOutputFile = true;
		    } else {
		    	files.add(arg);
		    }
		}
		if (files.isEmpty()) {
			System.err.println("No input files specified.");
			System.exit(2);
		}
		Model m = abs.frontend.parser.Main.parse(files, stdlib);
		if (m.hasParserErrors()) {
			 hasErrors = true;
          for (ParserError e : m.getParserErrors()) {
          	System.err.println(e.getHelpMessage());
          }
		}
		
		if (!hasErrors) {
        SemanticErrorList l = m.typeCheck();
        if (!l.isEmpty()) {
        	hasErrors = true;
            for (SemanticError e : l) {
            	System.err.println(e.getHelpMessage());
            }
        }
		}
		
        if (!hasErrors) {
        	PrintStream stream = System.out;
        	if (outputfile != null) {
        		stream = new PrintStream(outputfile);
        	}
        	m.generateMaude(stream, module);
        	System.exit(0);
        } 
           else System.exit(1);
	}

    private static void printUsage() {
        System.out.println("Usage: java "+MaudeCompiler.class.getName()+" [options] <absfiles>\n" +
                           "  <absfiles>   ABS files to parse\n" +
                           "Options:\n"+
                           "  -o <file>  write output to <file> instead of standard output\n" +
                           "  -nostdlib  do not include the standard lib\n" +
                           "  -timed     generate code for timed interpreter\n" +
                           "  -h         print this message\n");
        
    }

}

// Local Variables:
// tab-width: 4
// End:
