/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.net;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class RandomNetworkScheduler implements NetworkScheduler {
    private final Random random = new Random();
    private final List<NetNode> nodes = new ArrayList<>();

    @Override
    public void scheduleNext() {
        NetNode n = nodes.get(random.nextInt(nodes.size()));
        n.performStep();
    }

}
