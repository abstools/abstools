package abs.backend.java.observing;

import java.util.List;

/**
 * A view to task stack. 
 * A task stack represents the stack of a task at runtime.
 * The stack consists of multiple stack frames.
 * A stack frame represents a single method invocation.
 * The stack frames can be obtained by using the getFrames() method.
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
     * @return the current active frame of the task stack.
     */
    public TaskStackFrameView getCurrentFrame();
}
