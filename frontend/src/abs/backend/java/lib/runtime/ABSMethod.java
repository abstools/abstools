/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

import abs.backend.java.observing.ClassView;
import abs.backend.java.observing.MethodView;

public class ABSMethod implements MethodView {
    private final ClassView classView;
    private final String name;
    
    public ABSMethod(ClassView v, String name) {
        this.classView = v;
        this.name = name;
    }
    
    @Override
    public ClassView getClassView() {
        return classView;
    }

    @Override
    public String getName() {
        return name;
    }
    
    @Override
    public String toString() {
        String res = name;
        if (classView != null) {
            res = classView.getName()+"." + res;
        }
        return res;
    }

}
