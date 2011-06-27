/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.net;

/**
 * A message queue
 * 
 * @author Jan Schäfer
 *
 */
public interface MsgQueue {
    void enqueue(Msg m);
    Msg dequeue();
}
