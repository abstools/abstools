/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.delta;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;

import org.abs_models.backend.common.InternalBackendException;
import org.abs_models.common.WrongProgramArgumentException;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

import org.abs_models.frontend.ast.ClassDecl;
import org.abs_models.frontend.ast.DeltaDecl;
import org.abs_models.frontend.ast.Model;

public class SourceCodePositionTest extends DeltaTest {

    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    @Test
    public void test() throws IOException, InternalBackendException, DeltaModellingException, WrongProgramArgumentException {

        File fcore = folder.newFile("core.abs");
        File fd1 = folder.newFile("delta1.abs");
        File fd2 = folder.newFile("delta2.abs");
        {
            FileWriter writer = new FileWriter(fcore);
            writer.write(
                    "module M;"
                    + "class C0 { Unit m() {} }"
                    );
            writer.close();
        }
        {
            FileWriter writer = new FileWriter(fd1);
            writer.write(
                    "delta D1; uses M;"
                    + "adds class C1 { Unit m() {} }"
                    + "modifies class C0 { modifies Unit m() {} }"
                    );
            writer.close();
        }
        {
            FileWriter writer = new FileWriter(fd2);
            writer.write(
                    "delta D2; uses M;"
                    + "modifies class C1 { modifies Unit m() { original(); } }"
                    );
            writer.close();
        }

        HashSet<String> fileNames = new HashSet<>();
        fileNames.add(fcore.getCanonicalPath());
        fileNames.add(fd1.getCanonicalPath());
        fileNames.add(fd2.getCanonicalPath());

        Model model = assertParseFilesOk(fileNames);

        DeltaDecl d1 = findDelta(model, "D1");
        DeltaDecl d2 = findDelta(model, "D2");
        model.applyDeltas(new ArrayList<>(Arrays.asList(d1, d2)));

        {
            ClassDecl cls = (ClassDecl)findDecl(model, "M", "C0");
            assertEquals(cls.getMethod(0).getFileName(), d1.getFileName());
        }
        {
            ClassDecl cls = (ClassDecl)findDecl(model, "M", "C1");
            assertEquals(cls.getFileName(), d1.getFileName());
            assertEquals(cls.getMethod(0).getFileName(), d2.getFileName());
        }
    }

    //TODO test line and column positions
}
