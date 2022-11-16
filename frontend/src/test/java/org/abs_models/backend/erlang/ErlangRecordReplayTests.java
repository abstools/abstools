package org.abs_models.backend.erlang;

import org.abs_models.ABSTest;
import org.abs_models.frontend.ast.Model;
import org.junit.Assert;
import org.junit.Test;

import java.io.File;
import java.util.List;

public class ErlangRecordReplayTests extends ABSTest {

    ErlangTestDriver driver = new ErlangTestDriver();

    private boolean recordMatchesReplay(final String name) {
        try {
            final File f = java.nio.file.Files.createTempDirectory(null).toFile();
            f.deleteOnExit();
            final Model m = assertParseFileOk(name, Config.WITHOUT_MODULE_NAME);
            final String mainModule = driver.genCode(m, f, false);
            final List<String> recordOutput = driver.runCompiledModel(f, mainModule, "-t", "trace.json");
            final List<String> replayOutput = driver.runCompiledModel(f, mainModule, "-r", "trace.json");
            return recordOutput.equals(replayOutput);
        } catch (Exception e) {
            return false;
        }
    }

    @Test
    public void naive_shared_buffer() {
        Assert.assertTrue(recordMatchesReplay("abssamples/backend/ReplayTests/naive_shared_buffer.abs"));
    }

    @Test
    public void time_and_resources() {
        Assert.assertTrue(recordMatchesReplay("abssamples/backend/ReplayTests/time_and_resources.abs"));
    }
}
