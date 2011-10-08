/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.maude;

import abs.backend.BackendTestDriver;

public class MaudeTestDriver implements BackendTestDriver {

    final MaudeTests maude;

    public MaudeTestDriver(String mode) {
        maude = new MaudeTests(mode);
    }

    @Override
    public void assertEvalEquals(String absCode, boolean value) throws Exception {
        if (value)
            maude.assertTrueMaude(absCode);
        else
            maude.assertFalseMaude(absCode);
    }

    @Override
    public void assertEvalFails(String absCode) {
        maude.assertFails(absCode);
    }

    @Override
    public void assertEvalTrue(String absCode) throws Exception {
        assertEvalEquals(absCode, true);
    }
}
