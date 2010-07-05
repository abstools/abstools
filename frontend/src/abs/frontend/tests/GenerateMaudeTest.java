package abs.frontend.tests;

import java.io.FileReader;
import java.io.Reader;

import abs.frontend.ast.Model;
import abs.frontend.parser.ABSParser;
import abs.frontend.parser.ABSScanner;

public class GenerateMaudeTest {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		Model m = null;
        //if (args.length == 0) args = new String[]{"/Users/rudi/Source/hats/Tools/ABS/trunk/frontend/tests/abssamples/PeerToPeer.abs"};
        for (String arg : args) {
            try {
                m = parse(arg);
            } catch (Error err) {
				System.err.println("Parsing of " + arg + " failed with Error");
				System.err.println(err);
				err.printStackTrace(System.err);
                return;
            } catch (Exception e) {
				System.err.println("Parsing of " + arg +  " failed with Exception");
				System.err.println(e);
				e.printStackTrace(System.err);
                return;
            }
            if (m != null) m.generateMaude(System.out);
        }
	}

	protected static Model parse(String file) throws Exception {
		Reader reader = new FileReader(file);
		ABSParser parser = new ABSParser();
		ABSScanner scanner = new ABSScanner(reader);
		Model m = (Model)parser.parse(scanner);
		reader.close();
		return m;
	}
}

// Local Variables:
// tab-width: 4
// End:
