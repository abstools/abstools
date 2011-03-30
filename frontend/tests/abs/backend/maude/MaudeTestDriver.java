/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.maude;

import abs.backend.BackendTestDriver;

public class MaudeTestDriver extends BackendTestDriver {

    MaudeTests maude = new MaudeTests();

    @Override
    public void assertEvalEquals(String absCode, boolean value) {
        if (value)
            maude.assertTrueMaude(absCode);
        else
            maude.assertFalseMaude(absCode);
    }

    @Override
    public void assertEvalFails(String absCode) {
        maude.assertFails(absCode);
    }

}
