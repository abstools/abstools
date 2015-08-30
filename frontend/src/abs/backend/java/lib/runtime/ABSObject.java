/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

import java.util.List;

import abs.backend.java.lib.types.ABSBool;
import abs.backend.java.lib.types.ABSRef;
import abs.backend.java.lib.types.ABSUnit;
import abs.backend.java.lib.types.ABSValue;
import abs.backend.java.observing.COGView;
import abs.backend.java.observing.ClassView;
import abs.backend.java.observing.ObjectObserver;
import abs.backend.java.observing.ObjectView;
import static abs.backend.java.lib.runtime.ABSRuntime.*;

/**
 * The super class of all ABS classes
 * 
 * @author Jan Schäfer
 *
 */
public abstract class ABSObject implements ABSRef {
    protected COG __cog;
    protected final long __id;

    public ABSObject() {
        this(getCurrentCOG());
    }

    protected ABSObject(COG cog) {
        this.__cog = cog;
        this.__id = getFreshID();
        this.__cog.register(this);
    }


    private long getFreshID() {
        return __ABS_getRuntime().getFreshObjectID(this.getClass());
    }

    public String toString() {
        return getClassName() + " " + __id;
    }

    public abstract String getClassName();

    public ABSRuntime __ABS_getRuntime() {
        return getCOG().getRuntime();
    }
    
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

    public final void __ABS_checkSameCOG() {
        if (__cog != getCurrentCOG()) {
            throw new ABSIllegalSynchronousCallException();
        }
    }

    @Override
    public final ABSBool eq(ABSValue o) {
        return ABSBool.fromBoolean(this == o);
    }

    @Override
    public final ABSBool notEq(ABSValue o) {
        return eq(o).negate();
    }

    @Override
    public ABSBool gt(ABSValue other) {
        if (other instanceof ABSObject) {
            ABSObject o = (ABSObject)other;
            int comp = getClassName().compareTo(o.getClassName()); 
            if (comp == 0) return ABSBool.fromBoolean(this.__id > ((ABSObject)other).getView().getID());
            else return ABSBool.fromBoolean(comp > 0);
        } else if (other == null) {
            return ABSBool.TRUE;
        } else {
            // type error, not reached
            return ABSBool.FALSE;
        }
    }

    @Override
    public ABSBool lt(ABSValue other) {
        if (other instanceof ABSObject) {
            ABSObject o = (ABSObject)other;
            int comp = getClassName().compareTo(o.getClassName()); 
            if (comp == 0) return ABSBool.fromBoolean(this.__id < ((ABSObject)other).getView().getID());
            else return ABSBool.fromBoolean(comp < 0);
        } else if (other == null) {
            return ABSBool.FALSE;
        } else {
            // type error, not reached
            return ABSBool.FALSE;
        }
    }

    @Override
    public ABSBool gtEq(ABSValue other) {
        if (other instanceof ABSObject) {
            return eq(other).or(gt(other));
        } else if (other == null) {
            return ABSBool.TRUE;
        } else {
            // type error, not reached
            return ABSBool.FALSE;
        }
    }

    @Override
    public ABSBool ltEq(ABSValue other) {
        if (other instanceof ABSObject) {
            return eq(other).or(lt(other));
        } else if (other == null) {
            return ABSBool.FALSE;
        } else {
            // type error, not reached
            return ABSBool.FALSE;
        }
    }

    @Override
    public final boolean isDataType() {
        return false;
    }

    @Override
    public final boolean isReference() {
        return true;
    }

    protected volatile ObjectView __view;

    public synchronized ObjectView getView() {
        if (__view == null) {
            __view = new View();
        }
        return __view;
    }

    protected ABSValue getFieldValue(String fieldName) throws NoSuchFieldException {
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
        public ABSValue getFieldValue(String fieldName) throws NoSuchFieldException {
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
    
    
   
}
