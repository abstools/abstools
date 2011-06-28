/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.net;

import abs.backend.java.lib.net.msg.Msg;

public interface Router {
    Node getNextNode(Msg m);
}
