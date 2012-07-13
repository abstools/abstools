/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.observing.manipulation;

import abs.backend.java.lib.runtime.ABSClosure;
import abs.backend.java.observing.ClassView;

public interface ClassManipulator extends ClassView {
    void addField(String name, String type, ABSClosure init);
}
