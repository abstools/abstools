/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.observing;

import abs.backend.java.lib.types.ABSValue;

public interface FutView {
    TaskView getResolvingTask();

    boolean isResolved();

    ABSValue getValue();

    void registerFutObserver(FutObserver obs);

    int getID();
}
