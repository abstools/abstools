/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.expr;

public abstract class Branch<R> {
    Pattern pattern;

    public Branch(Pattern p) {
        this.pattern = p;
    }

    public Pattern getPattern() {
        return pattern;
    }

    public R apply(PatternBinding binding) {
        return null;
    }

}

// class Test {
//     /**
//      * data Foo = X | Bar(Foo);
//      * 
//      * Foo b = Bar(Bar(X)); case b { X => 1; Bar(X) => 2; Bar(x)False => 2; }
//      */

//     void m() {
//         Boolean b = Boolean.TRUE;
//     }
// }
