/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.observing;

import java.util.List;

import abs.backend.java.lib.types.ABSValue;

public interface ObjectObserver {

    void methodCalled(ObjectView object, String method, List<ABSValue> args);

    void fieldRead(ObjectView object, String field, ABSValue value);

    void fieldUpdated(ObjectView object, String field, ABSValue oldValue, ABSValue newValue);

}
