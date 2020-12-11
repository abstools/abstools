package org.abs_models.backend.c;

import com.google.common.io.Files;
import org.abs_models.ABSTest;
import org.abs_models.backend.BackendTestDriver;
import org.abs_models.backend.c.codegen.CProject;
import org.abs_models.frontend.ast.Model;
import org.apache.commons.io.FileUtils;
import org.junit.Assume;

import java.io.File;
import java.io.IOException;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class CTestDriver extends ABSTest implements BackendTestDriver {
    @Override
    public String toString() { return "C"; }

    @Override
    public void assertEvalEquals(String absCode, boolean value) throws Exception {
        if (value)
            assertEvalTrue(absCode);
        else
            assertEvalFails(absCode);
    }

    @Override
    public void assertEvalFails(String absCode) throws Exception {
        Assume.assumeTrue("not implemented yet", false);
    }

    @Override
    public void assertEvalTrue(String absCode) throws Exception {
        Model model = assertParse(absCode, Config.TYPE_CHECK, Config.WITHOUT_MODULE_NAME);
        assertEvalTrue(model);
    }

    @Override
    public void assertEvalTrue(Model m) throws Exception {
        File f = null;
        try {
            f = Files.createTempDir();
            f.deleteOnExit();
            CBackend backend = new CBackend();
            CProject project = backend.compile(m, f);
            assertTrue("expected program to succeed", project.run());
        } finally {
            try {
                FileUtils.deleteDirectory(f);
            } catch (IOException e) { }
        }
    }

    @Override
    public BackendName getBackendName() { return BackendName.C; }

    @Override
    public boolean supportsTimedAbs() { return false; }

    @Override
    public boolean supportsCustomSchedulers() { return false; }

    @Override
    public boolean supportsExceptions() { return false; }

    @Override
    public boolean supportsDowncasting() { return false; }
}
