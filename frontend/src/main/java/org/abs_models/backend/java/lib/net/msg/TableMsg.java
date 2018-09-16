/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.net.msg;

import org.abs_models.backend.java.lib.net.Router;

public class TableMsg implements Msg {
    private final Router router;

    public TableMsg(Router router) {
        this.router = router;
    }

    public Router getRouter() {
	return router;
    }
}
