/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime;

import java.util.List;
import java.util.Map;

import org.abs_models.backend.java.lib.types.ABSRef;
import org.abs_models.backend.java.lib.types.ABSUnit;
import org.abs_models.backend.java.observing.COGView;
import org.abs_models.backend.java.observing.ClassView;
import org.abs_models.backend.java.observing.ObjectObserver;
import org.abs_models.backend.java.observing.ObjectView;

/**
 * The super class of all ABS classes
 *
 * @author Jan Schäfer
 *
 */
public abstract class ABSObject implements ABSRef {
    protected COG __cog;
    protected final long __id;

    /**
     * Flag to turn off cross-cog calls. Currently used by the runtime to call
     * methods on ABS.DC.DeploymentComponent instances during known-safe
     * states (during time advance, i.e., all threads are awaiting).
     */
    public boolean __checkSameCog = true;

    public ABSObject() {
        this(ABSThread.getCurrentCOG());
    }

    protected ABSObject(COG cog) {
        this.__cog = cog;
        this.__id = getFreshID();
        this.__cog.register(this);
    }


    private long getFreshID() {
        return ABSRuntime.getRuntime().getFreshObjectID(this.getClass());
    }

    public abstract String getClassName();

    public final COG getCOG() {
        return __cog;
    }

    /**
     * Represents the init block
     */
    protected void __ABS_init() {
    }

    /**
     * Represents an optional run method
     */
    public ABSUnit run() {
        return ABSUnit.UNIT;
    }

    public final boolean  __ABS_isSameCOG() {
        return __cog == ABSThread.getCurrentCOG();
    }

    public final void __ABS_checkSameCOG() {
        if (__checkSameCog && __cog != ABSThread.getCurrentCOG()) {
            throw new ABSIllegalSynchronousCallException();
        }
    }

    protected volatile ObjectView __view;

    public synchronized ObjectView getView() {
        if (__view == null) {
            __view = new View();
        }
        return __view;
    }

    protected Object getFieldValue(String fieldName) throws NoSuchFieldException {
        throw new NoSuchFieldException(fieldName);
    }

    private class View implements ObjectView, ClassView {

        @Override
        public COGView getCOG() {
            return __cog.getView();
        }

        @Override
        public ClassView getClassView() {
            return this;
        }

        @Override
        public String getClassName() {
            return ABSObject.this.getClassName();
        }

        @Override
        public Object getFieldValue(String fieldName) throws NoSuchFieldException {
            return ABSObject.this.getFieldValue(fieldName);
        }

        @Override
        public void registerObjectObserver(ObjectObserver l) {
            // FIXME: implement
        }

        @Override
        public String toString() {
            return ABSObject.this.toString();
        }

        public List<String> getFieldNames() {
            return ABSObject.this.getFieldNames();
        }

        @Override
        public long getID() {
            return ABSObject.this.__id;
        }

        @Override
        public String getName() {
            return getClassName();
        }

        @Override
        public List<String> getMethodNames() {
            return null;
        }

    }

    public abstract List<String> getFieldNames();

    // Return value is serializable by the Jackson JSON library
    public abstract List<Map<String, Object>> getHttpCallableMethodInfo();

    public ABSFut<? extends Object> invokeMethod(String name, List<Object> arguments) {
        return null;
    }
}
