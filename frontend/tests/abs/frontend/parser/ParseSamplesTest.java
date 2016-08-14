/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.parser;

import java.util.Arrays;
import java.util.Collection;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import abs.frontend.FrontendTest;
import abs.frontend.ast.Model;

@RunWith(Parameterized.class)
public class ParseSamplesTest extends FrontendTest {

    /* Render filename and product in JUnit output via test name */
    @Parameters(name="{0}, {1}")
    public static Collection<?> data() {
        final Object[][] data = new Object[][] { { "examples/PeerToPeer.abs" , null }
                                               , { "examples/PingPong.abs" , null }
                                               , { "examples/BoundedBuffer.abs" , null}
                                               , { "examples/SmartHome/abs/" , null}
                                               , { "examples/SmartHome/abs/" , "OilHeating" }
                                               , { "examples/SmartHome/abs/" , "ElectricHeating" }
                                               , { "examples/SmartHome/abs/" , "FireAlarm" }
                                               , { "examples/SmartHome/abs/" , "Test" }
                                               , { "examples/chatPL/" , null }
                                               , { "examples/chatPL/" , "HighEnd" }
                                               , { "examples/chatPL/" , "LowEnd" }
                                               , { "examples/chatPL/" , "Regular" }
                                               , { "tests/abssamples/RandomBool.abs" , null}
                                               , { "tests/abssamples/LexicalTest.abs", null}
                                               , { "tests/abssamples/ReplicationSystem.abs" , null}
                                               };
        return Arrays.asList(data);
    }

    final private String input;
    final protected String product;
    protected Model m;

    public ParseSamplesTest(String input, String product) {
        this.input = input;
        this.product = product;
    }

    @Test
    public void test() throws Exception {
        m = parse(input);
    }

    protected Model parse(String input) throws Exception {
        return assertParseFileOk(input, true);
    }
}
