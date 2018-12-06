/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.observing;

import java.util.List;

public interface ClassView {
    String getName();

    List<String> getFieldNames();

    List<String> getMethodNames();
}
