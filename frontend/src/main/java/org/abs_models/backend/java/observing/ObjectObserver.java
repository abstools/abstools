/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.observing;

import java.util.List;

public interface ObjectObserver {

    void methodCalled(ObjectView object, String method, List<Object> args);

    void fieldRead(ObjectView object, String field, Object value);

    void fieldUpdated(ObjectView object, String field, Object oldValue, Object newValue);

}
