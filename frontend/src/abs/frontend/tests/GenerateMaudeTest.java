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
        if (args.length == 0) args = new String[]{"PeerToPeer.abs"};
        for (String arg : args) {
            try {
                m = parse(arg);
            } catch (Error err) {
				System.out.println(arg + ":" + err.getMessage());
                return;
            } catch (Exception e) {
				System.out.println("Parsing of " + arg +  " failed with Exception");
				System.out.println(e);
				e.printStackTrace(System.out);
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
