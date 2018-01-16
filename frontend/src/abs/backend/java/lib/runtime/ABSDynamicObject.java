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

import abs.backend.java.codegeneration.dynamic.DynamicException;
import abs.backend.java.lib.types.ABSUnit;
import abs.backend.java.lib.types.ABSValue;
import abs.backend.java.observing.COGView;
import abs.backend.java.observing.ClassView;
import abs.backend.java.observing.ObjectObserver;
import abs.backend.java.observing.ObjectView;

public class ABSDynamicObject extends ABSObject {
    private ABSDynamicClass clazz;
    private Map<String,ABSValue> fields;

    public ABSDynamicObject(ABSDynamicClass clazz, ABSValue... params) {
        this.clazz = clazz;
        initializeFields(params);
    }

    public ABSDynamicObject(COG cog, ABSDynamicClass clazz, ABSValue... params) {
        super(cog);
        this.clazz = clazz;
        initializeFields(params);
    }

    private void initializeFields(ABSValue[] params) {
        fields = new HashMap<>();

        // parameters
        if (params.length != clazz.getParams().size()) throw new DynamicException("Type mismatch");
        for (int i = 0; i < params.length; i++) {
            setFieldValue(clazz.getParams().get(i), params[i]);
        }
        // all other fields
        for (String f : clazz.getFieldNames()) {
            if (! fields.containsKey(f)) {
                setFieldValue(f, clazz.getField(f).init(this));
            }
        }
        this.getCOG().objectCreated(this);

//        for (String s : getFieldNames()) {
//            System.out.println("*** Class " + clazz.getName() + ": Field " + s + " = " + getFieldValue_Internal(s));
//        }
    }

    @Override
    public final ABSDynamicRuntime __ABS_getRuntime() {
        ABSRuntime runtime = super.__ABS_getRuntime();
        if (runtime instanceof ABSDynamicRuntime)
            return (ABSDynamicRuntime)super.__ABS_getRuntime();
        else
            throw new DynamicException("Dynamic ABS Java code detected. Please run with -dynamic switch.");
    }

    public String getClassName() {
        return getClazz().getName();
    }

    public void setCOG(COG cog) {
        this.__cog = cog;
    }

    public void setClazz(ABSDynamicClass clazz) {
        this.clazz = clazz;
    }

    public ABSDynamicClass getClazz() {
        return clazz;
    }

    public List<String> getFieldNames() {
        return new ArrayList<>(clazz.getFieldNames());
    }

    public ABSValue getFieldValue(String field) throws NoSuchFieldException {
        if (fields.containsKey(field))
            return fields.get(field);
        else
            throw new NoSuchFieldException(field);
    }

    public ABSValue getFieldValue_Internal(String field) {
        try {
            return getFieldValue(field);
        } catch (NoSuchFieldException e) {
            throw new DynamicException("Field not found: " + field);
        }
    }

    public void setFieldValue(String fieldName, ABSValue val) {
        fields.put(fieldName, val);
    }

    public void __ABS_init() {
        clazz.getConstructor().exec(this);
        this.getCOG().objectInitialized(this);
    }

    public ABSValue dispatch(String mName, ABSValue... params) {
        ABSClosure method = clazz.getMethod(mName);
        return method.exec(this, params);
    }

    public ABSUnit run() {
        ABSClosure cl = clazz.getMethod("run");
        if (cl != null) {
            cl.exec(this);
        }
        return ABSUnit.UNIT;
    }



    public synchronized ObjectView getView() {
        if (__view == null) {
            __view = new View();
        }
        return __view;
    }

    private class View implements ObjectView {

        @Override
        public COGView getCOG() {
            return __cog.getView();
        }

        @Override
        public ClassView getClassView() {
            return clazz.getView();
        }

        @Override
        public ABSValue getFieldValue(String fieldName) throws NoSuchFieldException {
            return ABSDynamicObject.this.getFieldValue(fieldName);
        }

        @Override
        public void registerObjectObserver(ObjectObserver l) {
            // FIXME: implement
        }

        @Override
        public String toString() {
            return ABSDynamicObject.this.toString();
        }

        @Override
        public long getID() {
            return ABSDynamicObject.this.__id;
        }

        @Override
        public String getClassName() {
            return clazz.getName();
        }

        @Override
        public List<String> getFieldNames() {
            if (fields == null) return Collections.emptyList();
            return new ArrayList<>(fields.keySet());
        }

    }

}
