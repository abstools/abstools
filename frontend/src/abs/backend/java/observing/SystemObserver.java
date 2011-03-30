/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.observing;

/**
 * An interface to observer global system behavior
 * 
 * @author Jan Schäfer
 *
 */
public interface SystemObserver {
    void systemStarted();

    void newCOGCreated(COGView cog, ObjectView initialObject);
    
    void systemFinished();
}
