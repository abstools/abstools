/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.maude;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

import abs.backend.BackendTestDriver;
import abs.frontend.ast.Model;

public class MaudeTestDriver implements BackendTestDriver {

    final MaudeTests maude;

    public MaudeTestDriver(MaudeCompiler.SIMULATOR mode) {
        maude = new MaudeTests(mode);
    }

    @Override
    public String toString() {
        return "Maude "+maude.mode;
    }

    @Override
    public void assertEvalEquals(String absCode, boolean value) throws Exception {
        if (value)
            maude.assertTrueMaude(absCode);
        else
            maude.assertFalseMaude(absCode);
    }

    @Override
    public void assertEvalFails(String absCode) throws Exception {
        maude.assertFails(absCode);
    }

    @Override
    public void assertEvalTrue(String absCode) throws Exception {
        assertEvalEquals(absCode, true);
    }

    @Override
    public void assertEvalTrue(Model model) throws Exception {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        model.generateMaude(new PrintStream(out), maude.mode, 100, 0);
        String code = out.toString();
        maude.assertMaudeCodeResult(code, "ABS.StdLib.True");
    }

    @Override
    public BackendName getBackendName() {
        return BackendName.MAUDE;
    }

    @Override
    public boolean supportsCustomSchedulers() { return true; }

    @Override
    public boolean supportsTimedAbs() { return maude.mode == MaudeCompiler.SIMULATOR.EQ_TIMED; }
}
