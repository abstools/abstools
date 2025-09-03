/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.abs_models.backend.java.observing.MethodView;
import org.abs_models.backend.java.observing.TaskStackFrameView;
import org.abs_models.backend.java.observing.TaskStackView;
import org.abs_models.backend.java.observing.TaskView;

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
        private final Map<String, Object> values = new HashMap<>();
        private final MethodView method;

        Frame(MethodView v) {
            method = v;
        }

        @Override
        public synchronized Set<String> getVariableNames() {
            return values.keySet();
        }

        @Override
        public synchronized Object getValue(String variableName) {
            return values.get(variableName);
        }

        synchronized void setValue(String variableName, Object v) {
            values.put(variableName, v);
        }

        @Override
        public TaskStackView getStackView() {
            return TaskStack.this;
        }

        @Override
        public MethodView getMethodView() {
            return method;
        }
    }

    @Override
    public List<? extends TaskStackFrameView> getFrameViews() {
        return Collections.unmodifiableList(frames);
    }

    @Override
    public synchronized Frame getCurrentFrameView() {
        return frames.get(frames.size()-1);
    }

    @Override
    public synchronized boolean hasFrames() {
        return ! frames.isEmpty();
    }

    @Override
    public TaskView getTaskView() {
        return task.getView();
    }
}
