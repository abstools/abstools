/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.observing;

import java.util.List;

/**
 * A view to task stack. 
 * A task stack represents the stack of a task at runtime.
 * The stack consists of multiple stack frames.
 * A stack frame represents a single method invocation.
 * The stack frames can be obtained by using the getFrames() method.
 * It is possible that a task has no stack frame at all if it has not been started yet.
 * 
 * @author Jan Schäfer
 *
 */
public interface TaskStackView {
    
    /**
     * Returns the task to which this stack belongs to
     * @return the task to which this stack belongs to
     */
    public TaskView getTask();
    
    /**
     * Returns the list of stack frames of this task stack.
     * The last element of the list represents the top of the stack
     * @return the list of stack frames of this task stack.
     */
    public List<? extends TaskStackFrameView> getFrames();
    
    /**
     * Returns the current active frame of the task stack.
     * I.e. the top of the stack.
     * Returns null if there is no stack frame
     * @return the current active frame of the task stack, or null if there is no frame 
     */
    public TaskStackFrameView getCurrentFrame();

    /**
     * Whether this task has any frames
     * @return whether this task has any frames
     */
    boolean hasFrames();
}
