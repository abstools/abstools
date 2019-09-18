/**
 */

package abs.backend.prolog;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.abs_models.Absc;
import org.abs_models.frontend.parser.Main;

/**
 * Backward compatibility with collaboratory.
 *
 * This is a compatibility class to keep (older versions of) analysis tools
 * used in the collaboratory running.
 */
public class PrologBackend extends Main {
    public static void main(final String... args) {
        List<String> argslist = new ArrayList<String>(Arrays.asList(args));
        argslist.add(0, "--prolog");
        // dispatch via common code path
        Absc.main(argslist.toArray(new String[0]));
    }
}
