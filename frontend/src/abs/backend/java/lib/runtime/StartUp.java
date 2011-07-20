/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

import abs.backend.java.lib.net.ABSNetRuntime;
import abs.backend.java.lib.net.NetworkImpl;

public class StartUp {
    public static void startup(String[] args, Class<?> mainClass) throws InstantiationException, IllegalAccessException {
        RuntimeOptions options = new RuntimeOptions(args);
        ABSRuntime runtime;
        if (options.useNet.isTrue()) {
            NetworkImpl network = new NetworkImpl();
            runtime = new ABSNetRuntime(network);
        } else {
            runtime = new ABSRuntime();
        }
        new Config(runtime, options);
        runtime.start(mainClass);
    }
}
