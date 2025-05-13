/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.expr;

import java.util.ArrayList;

public class PatternBinding {
    ArrayList<Object> binding = new ArrayList<>();

    public void addBinding(Object dt) {
        binding.add(dt);
    }

    public Object getBinding(int i) {
        return binding.get(i);
    }
}
