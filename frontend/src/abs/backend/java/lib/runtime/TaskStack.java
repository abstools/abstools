/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import abs.backend.java.lib.runtime.TaskStack.Frame;
import abs.backend.java.lib.types.ABSValue;
import abs.backend.java.observing.MethodView;
import abs.backend.java.observing.TaskStackFrameView;
import abs.backend.java.observing.TaskStackView;
import abs.backend.java.observing.TaskView;

/**
 * TaskStack represents the stack of a task at runtime.
 * The stack consists of multiple TaskStack.Frames, which
 * represent a single stack frame of a method invocation.
 *
 * @author Jan Schäfer
 */
class TaskStack implements TaskStackView {
    private final List<Frame> frames = new ArrayList<>();
    private final Task<?> task;

    TaskStack(Task<?> task) {
        this.task = task;
    }

    synchronized Frame pushNewFrame(MethodView method) {
        Frame f = new Frame(method);
        frames.add(f);
        return f;
    }

    synchronized Frame popFrame() {
        return frames.remove(frames.size()-1);
    }

    synchronized int getDepth() {
        return frames.size();
    }

    public class Frame implements TaskStackFrameView {
        private final Map<String,ABSValue> values = new HashMap<>();
        private final MethodView method;

        Frame(MethodView v) {
            method = v;
        }

        @Override
        public synchronized Set<String> getVariableNames() {
            return values.keySet();
        }

        @Override
        public synchronized ABSValue getValue(String variableName) {
            return values.get(variableName);
        }

        synchronized void setValue(String variableName, ABSValue v) {
            values.put(variableName, v);
        }

        @Override
        public TaskStackView getStack() {
            return TaskStack.this;
        }

        @Override
        public MethodView getMethod() {
            return method;
        }
    }

    @Override
    public List<? extends TaskStackFrameView> getFrames() {
        return Collections.unmodifiableList(frames);
    }

    @Override
    public synchronized Frame getCurrentFrame() {
        return frames.get(frames.size()-1);
    }

    @Override
    public synchronized boolean hasFrames() {
        return ! frames.isEmpty();
    }

    @Override
    public TaskView getTask() {
        return task.getView();
    }
}
