/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.net.msg;


/**
 * A message queue
 * 
 * @author Jan Schäfer
 *
 */
public interface MsgQueue {
    public void enqueue(Msg m);    
    public Msg dequeue();    
    public boolean isEmpty();
}
