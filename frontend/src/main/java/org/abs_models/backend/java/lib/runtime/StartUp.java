/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime;

import org.abs_models.backend.java.lib.net.ABSNetRuntime;
import org.abs_models.backend.java.lib.net.NetworkImpl;
import org.abs_models.backend.java.lib.net.NodeImpl;

public class StartUp {
    public static void startup(String[] args, Class<?> mainClass) throws InstantiationException, IllegalAccessException {
        RuntimeOptions options = new RuntimeOptions(args);
        Logging.setLogLevel(options.logLevel.stringValue());
        final ABSRuntime runtime;
        if (options.useNet.isTrue()) {
            NetworkImpl network = new NetworkImpl();
            network.addNode(new NodeImpl(0));
            runtime = ABSNetRuntime.getRuntime(network);
        } else if (options.dynamicUpdates.isTrue()) {
            runtime = ABSDynamicRuntime.getRuntime();
        } else {
            runtime = ABSRuntime.getRuntime();
        }
        ABSRuntime.setRunsInOwnProcess(true);
        new Config(runtime, options);
        runtime.start(mainClass);
    }
}
