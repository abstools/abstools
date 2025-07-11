/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime;

import java.io.IOException;
import java.util.List;
import java.util.concurrent.CountDownLatch;

import org.abs_models.backend.java.lib.net.ABSNetRuntime;
import org.abs_models.backend.java.lib.net.NetworkImpl;
import org.abs_models.backend.java.lib.net.NodeImpl;
import org.abs_models.backend.java.observing.DefaultSystemObserver;
import org.abs_models.backend.java.observing.GraphObserver;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.rdf.model.Model;

public class StartUp {
    public static void startup(String[] args, Class<?> mainClass) throws InstantiationException, IllegalAccessException, IOException, InterruptedException {
        final CountDownLatch latch = new CountDownLatch(1);
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
        final boolean useRDF = options.printRDF.isTrue()
                               || options.sparqlQuery.wasSet()
                               // for the SPARQL endpoint
                               || options.modelapiPort.wasSet();
        ABSRuntime.setRunsInOwnProcess(true);
        Config.initRuntimeFromOptions(runtime, options);
        runtime.addSystemObserver(new DefaultSystemObserver() {
            public void systemFinished() {
                // Avoid garbage collection of objects, cogs after
                // model is finished
                if (useRDF) GraphObserver.freezeObserver();
                latch.countDown();
            }
        });
        if (useRDF) {
            runtime.addSystemObserver(new GraphObserver());
        }
        runtime.start(mainClass);
        latch.await();

        if (useRDF) {
            Model model = GraphObserver.getModel();
            if (options.printRDF.isTrue()) GraphObserver.printGraph(model);
            if (options.sparqlQuery.wasSet()) {
                List<QuerySolution> results = GraphObserver.runQuery(model,
                    options.sparqlQuery.stringValue());
                // SELECT ?cog WHERE { ?obj abs:in ?cog }
                results.forEach((solution) -> {
                    System.out.println(solution);
                    // solution.varNames().forEachRemaining((String varName) -> {
                    //     System.out.println("Var " + varName + " is bound to " + solution.get(varName));
                    // });
                });
            }
        }
    }
}
