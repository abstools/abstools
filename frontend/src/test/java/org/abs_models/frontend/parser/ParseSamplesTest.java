/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.parser;

import java.util.Arrays;
import java.util.Collection;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import org.abs_models.frontend.FrontendTest;
import org.abs_models.frontend.ast.Model;

/**
 * Render filename and product in JUnit output via test name.  Files are found
 * via {@link ABSTest#resolveFileName()}.
*/
@RunWith(Parameterized.class)
public class ParseSamplesTest extends FrontendTest {

    @Parameters(name="{0}, {1}")
    public static Collection<?> data() {
        final Object[][] data = new Object[][] { { "examples/PeerToPeer.abs" , null }
                                               , { "examples/PingPong.abs" , null }
                                               , { "examples/BoundedBuffer.abs" , null}
                                               , { "examples/SmartHome/abs/" , null}
                                               , { "examples/SmartHome/abs/" , "OilHeating" }
                                               , { "examples/SmartHome/abs/" , "ElectricHeating" }
                                               //, { "examples/SmartHome/abs/" , "FireAlarm" } // invalid example, `reset` method undefined
                                               , { "examples/SmartHome/abs/" , "Test" }
                                               , { "examples/chatPL/" , null }
                                               //, { "examples/chatPL/" , "HighEnd" } // invalid in HATS repository too (missing class definitions)
                                               , { "examples/chatPL/" , "LowEnd" }
                                               //, { "examples/chatPL/" , "Regular" } // invalid in HATS repository too (missing class definitions)
                                               , { "abssamples/RandomBool.abs" , null}
                                               , { "abssamples/ReplicationSystem.abs" , null}
                                               , { "abssamples/10wikipediadocstokenized.abs" , null}
                                               , { "examples/MapReduce.abs" , null }
                                               , { "examples/MapReduce.abs" , "WordcountModel" }
                                               , { "examples/MapReduce.abs" , "WordcountFull" }
                                               , { "examples/MapReduce.abs" , "WordcountDemo" }
                                               , { "examples/MapReduce.abs" , "WordsearchModel" }
                                               , { "examples/MapReduce.abs" , "WordsearchFull" }
                                               , { "examples/MapReduce.abs" , "WordsearchDemo" }
                                               , { "examples/MapReduce.abs" , "IndexingModel" }
                                               , { "examples/MapReduce.abs" , "IndexingFull" }
                                               , { "examples/MapReduce.abs" , "IndexingDemo" }                                               
        };
        return Arrays.asList(data);
    }

    final protected String input;
    final protected String product;
    protected Model m;

    public ParseSamplesTest(String input, String product) {
        this.input = input;
        this.product = product;
    }

    @Test
    public void test() throws Exception {
        m = assertParseFileOk(input);
    }
}
