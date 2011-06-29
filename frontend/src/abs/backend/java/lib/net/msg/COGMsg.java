/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.net.msg;

import abs.backend.java.lib.net.NetCOG;

public class COGMsg implements Msg {
    public final NetCOG cog;
    
    public COGMsg(NetCOG cog) {
        this.cog = cog;
    }
}
