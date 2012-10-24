/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs;

import static org.junit.Assert.fail;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;

import abs.frontend.analyser.SemanticError;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.Model;
import abs.frontend.parser.Main;
import static abs.ABSTest.Config.*;

public class ABSTest {
    public static enum Config {
        NONE,
        WITH_STD_LIB,
        WITH_DB_LIB,
        WITHOUT_MODULE_NAME,
        EXPECT_PARSE_ERROR,
        EXPECT_TYPE_ERROR,
        ALLOW_INCOMPLETE_EXPR,
        TYPE_CHECK
    }
        
    public static class ABSFileNameFilter implements FilenameFilter {
        @Override
        public boolean accept(File dir, String name) {
            return name.endsWith(".abs");
        }
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

    /**
     * Take a file name and returns that name if it points to an existing file,
     * otherwise if {@link File#getAbsoluteFile()} returns a file that points to
     * an existing file, this method returns {@link File#getAbsolutePath()}.
     * 
     * @param fileName
     * @return a valid file name from the input file name
     * @throws IllegalArgumentException
     *             if neither the input file name nor
     *             {@link File#getAbsoluteFile()} points to a valid file.
     */
    protected String resolveFileName(String fileName) {
        File f = new File(fileName);
        if (f.exists()) {
            return fileName;
        } 
        f = f.getAbsoluteFile();
        if (f.exists()) {
            return f.getAbsolutePath();
        }
        throw new IllegalArgumentException("File "+fileName+" cannot be read");
    }

    protected Model assertParseOk(String s, Config... config) {
        return assertParse(s,config);
    }
    
    protected Model assertParse(String s, Config... config) {

        String preamble = "module UnitTest; export *; ";
        if (isSet(WITH_STD_LIB, config))
            preamble = preamble + " import * from ABS.StdLib;";
        if (isSet(WITH_DB_LIB, config))
            for (String module : new String[] { "ABS.StdLib", "ABS.DB", "ABS.DB.Structure",
                    "ABS.DB.Operators.Structure", "ABS.DB.Transactions", "ABS.DB.Helpers" })
                preamble = preamble + " import * from " + module + ";";
        if (!isSet(WITHOUT_MODULE_NAME, config))
            s = preamble + s;
        try {
            Model p = Main.parseString(s, isSet(WITH_STD_LIB, config), isSet(WITH_DB_LIB, config),
                    isSet(ALLOW_INCOMPLETE_EXPR, config));

            if (isSet(EXPECT_PARSE_ERROR,config)) {
                if (!p.hasParserErrors())
                    fail("Expected to find parse error");
            } else {
                if (p.hasParserErrors()) {
                    fail("Failed to parse: " + s + "\n" + p.getParserErrors().get(0).getMessage());
                } else {
                    if (isSet(TYPE_CHECK, config)) {
                        SemanticErrorList l = p.typeCheck();
                        if (isSet(EXPECT_TYPE_ERROR,config)) {
                            if (l.isEmpty()) {
                                fail("Expected type errors, but none appeared");
                            }
                        } else {
                            if (!l.isEmpty()) {
                                fail("Failed to typecheck: " + s + "\n" + l.get(0).getMessage());
                            }
                        }
                    }                    
                }
            }
            return p;
        } catch (Exception t) {
            throw new RuntimeException(t); // TODO: remove
        }
    }

    protected Model assertParseError(String absCode) {
        return assertParse(absCode, EXPECT_PARSE_ERROR);
    }
    
    /**
     * Note: does not handle EXPECT_*.
     */
    protected Model assertParseFileOk(String fileName, Config... config) throws IOException {
        Main main = new Main();
        main.setWithStdLib(isSet(WITH_STD_LIB,config));
        Model m = main.parseFiles(resolveFileName(fileName));
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
