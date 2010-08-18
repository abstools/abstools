package abs.backend.maude;

import java.io.FileReader;
import java.io.Reader;

import abs.frontend.analyser.SemanticError;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.CompilationUnit;
import abs.frontend.ast.Model;
import abs.frontend.parser.ABSParser;
import abs.frontend.parser.ABSScanner;

public class MaudeCompiler {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		Model m = new Model();
		boolean hasErrors = false;
        for (String arg : args) {
            try {
            	m.addCompilationUnit(parse(arg));
            } catch (Error err) {
				System.err.println("Parsing of " + arg + " failed with Error");
				System.err.println(err);
				err.printStackTrace(System.err);
                System.exit(1);
            } catch (Exception e) {
				System.err.println("Parsing of " + arg +  " failed with Exception");
				System.err.println(e);
				e.printStackTrace(System.err);
                System.exit(1);
            }
        }
        SemanticErrorList l = m.typeCheck();
        if (!l.isEmpty()) {
        	hasErrors = true;
            for (SemanticError e : l) {
            	System.err.println(e.getFileName() + ":" + e.getLine() + ": " + e.getMsg());
            }
        }
        if (!hasErrors) {
        	m.generateMaude(System.out);
        	System.exit(0);
        } else System.exit(1);
	}

	protected static CompilationUnit parse(String file) throws Exception {
		Reader reader = new FileReader(file);
		try {
			ABSParser parser = new ABSParser();
			ABSScanner scanner = new ABSScanner(reader);
			return parser.parse(file, scanner);
		} finally {
			reader.close();
		}
	}
}

// Local Variables:
// tab-width: 4
// End:
