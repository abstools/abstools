/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.net;


/**
 * Represents the whole network
 * 
 * @author Jan Schäfer
 *
 */
public interface Network {
    public Iterable<NetNode> getAllNodes();
    public Iterable<Arc> getAllArcs();
    public NetNode getStartNode();
    
}
