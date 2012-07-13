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

    /* TODO: JUnit 4.11 might allow setting reasonable names for a test */
    @Parameters
    public static Collection<?> data() {
        final Object[][] data = new Object[][] { { "examples/PeerToPeer.abs" }
                                               , { "examples/PingPong.abs" }
                                               , { "examples/BoundedBuffer.abs"}
                                               , { "tests/abssamples/RandomBool.abs"}
                                               , { "tests/abssamples/LexicalTest.abs"}
                                               , { "tests/abssamples/ReplicationSystem.abs"}
                                               };
        return Arrays.asList(data);
    }
    
    final private String input;
    protected Model m;

    public ParseSamplesTest(String input) {
        this.input = input;
    }

    @Test
    public void test() throws Exception {
        m = parse(input);
    }

    protected Model parse(String input) throws Exception {
        return assertParseFileOk(input, true);
    }
}
