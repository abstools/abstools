/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.dynamic;

import java.io.File;
import java.util.Collection;
import java.util.HashSet;

import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

@Ignore
@RunWith(Parameterized.class)
public class AbsSamples extends JavaBackendDynamicTest {

    private String fileName;

    public AbsSamples(String f) {
        super();
        this.fileName = f;
    }

    /*
     * Collect all ABS code samples in abssamples/meta/
     */
    @Parameters(name="{0}")
    public static Collection<String[]> fileNames() {
        final String s = File.separator;
        final String dir = "tests" + s + "abssamples" + s + "meta";
        ABSFileNameFilter filter = new ABSFileNameFilter();
        File dirHandle = new File(dir);
        final String[] absFiles = dirHandle.list(filter);
        Collection<String[]> data = new HashSet<>();
        for (int i=0; i < absFiles.length; i++) {
            String[] path = { dir + s + absFiles[i] };
            data.add(path);
        }
        return data;
    }


    @Test
    public void test() throws Exception {
        assertValidJavaExecution(fileName, true);
        // JavaCode code = getJavaCode(readAbsFile(dir + s + file), true);
        // assertEvalTrue(readAbsFile(dir + s + file));
    }

}
