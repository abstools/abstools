package abs;

import static org.junit.Assert.fail;

import java.io.File;

import abs.frontend.analyser.SemanticError;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.Model;
import abs.frontend.parser.Main;
import static abs.ABSTest.Config.*;

public class ABSTest {
    public static enum Config {
        NONE,
        WITH_STD_LIB,
        WITHOUT_MODULE_NAME,
        EXPECT_PARSE_ERROR,
        EXPECT_TYPE_ERROR,
        ALLOW_INCOMPLETE_EXPR,
        TYPE_CHECK
    }
        
    protected boolean isSet(Config c, Config...configs) {
        if (configs == null)
            throw new IllegalArgumentException("Must give an array of configs");
        for (Config c2: configs) {
            if (c2 == c)
                return true;
        }
        return false;
    }
    
    protected Model assertParseOk(String s, Config... config) {
        return assertParse(s,config);
    }
    
    protected Model assertParse(String s, Config... config) {

        String preamble = "module UnitTest; export *; ";
        if (isSet(WITH_STD_LIB, config))
            preamble = preamble + " import * from ABS.StdLib;";
        if (!isSet(WITHOUT_MODULE_NAME, config))
            s = preamble + s;
        Model p = null;
        try {
            p = Main.parseString(s, isSet(WITH_STD_LIB, config), isSet(ALLOW_INCOMPLETE_EXPR, config));

            if (isSet(EXPECT_PARSE_ERROR,config)) {
                if (!p.hasParserErrors())
                    fail("Expected to find parse error");
            } else {
                if (p.hasParserErrors()) {
                    fail("Failed to parse: " + s + "\n" + p.getParserErrors().get(0).getMessage());
                }
            }
        } catch (Exception t) {
            throw new RuntimeException(t);
        }
        return p;
    }

    protected Model assertParseError(String absCode) {
        return assertParse(absCode, EXPECT_PARSE_ERROR);
    }

    protected Model assertParseFileOk(String fileName, Config... config) {
        Model m = null;
        try {
            m = Main.parse(new File(fileName), isSet(WITH_STD_LIB,config));
        } catch (Throwable e) {
            e.printStackTrace();
            fail("Failed to parse: " + fileName + "\n" + e.getMessage());
        }
        if (m != null) {
            int numSemErrs = m.getErrors().size();
            StringBuffer errs = new StringBuffer("Semantic errors: " + numSemErrs + "\n");
            if (numSemErrs > 0) {
                for (SemanticError error : m.getErrors())
                    errs = errs.append(error.getHelpMessage() + "\n");
                fail("Failed to parse: " + fileName + "\n" + errs.toString());
            } else if (isSet(TYPE_CHECK, config)) {
                SemanticErrorList l = m.typeCheck();
                if (!l.isEmpty()) {
                    for (SemanticError error : l)
                        errs = errs.append(error.getHelpMessage() + "\n");
                    fail("Failed to typecheck: " + fileName + "\n" + errs.toString());

                }
            }
        }
        return m;
    }

}
