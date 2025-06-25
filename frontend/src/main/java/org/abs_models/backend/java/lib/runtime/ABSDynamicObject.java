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

import org.abs_models.backend.java.codegeneration.dynamic.DynamicException;
import org.abs_models.backend.java.lib.types.ABSUnit;
import org.abs_models.backend.java.observing.COGView;
import org.abs_models.backend.java.observing.ClassView;
import org.abs_models.backend.java.observing.ObjectObserver;
import org.abs_models.backend.java.observing.ObjectView;

public class ABSDynamicObject extends ABSObject {
    private ABSDynamicClass clazz;
    private Map<String,Object> fields;

    public ABSDynamicObject(ABSDynamicClass clazz, Object... params) {
        this.clazz = clazz;
        initializeFields(params);
    }

    public ABSDynamicObject(COG cog, ABSDynamicClass clazz, Object... params) {
        super(cog);
        this.clazz = clazz;
        initializeFields(params);
    }

    private void initializeFields(Object[] params) {
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

    public Object getFieldValue(String field) throws NoSuchFieldException {
        if (fields.containsKey(field))
            return fields.get(field);
        else
            throw new NoSuchFieldException(field);
    }

    public Object getFieldValue_Internal(String field) {
        try {
            return getFieldValue(field);
        } catch (NoSuchFieldException e) {
            throw new DynamicException("Field not found: " + field);
        }
    }

    public void setFieldValue(String fieldName, Object val) {
        fields.put(fieldName, val);
    }

    public void __ABS_init() {
        clazz.getConstructor().exec(this);
        this.getCOG().objectInitialized(this);
    }

    public Object dispatch(String mName, Object... params) {
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


    private final  View __dynamicView = new View();

    public ObjectView getView() {
        if (view == null) {
            synchronized(this) {
                if (view == null) {
                    view = new View();
                }
            }
        }
        return view;
    }

    private class View implements ObjectView {

        @Override
        public COGView getCOGView() {
            return __cog.getView();
        }

        @Override
        public ClassView getClassView() {
            return clazz.getView();
        }

        @Override
        public Object getFieldValue(String fieldName) throws NoSuchFieldException {
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
        public ABSObject getObject() {
            return ABSDynamicObject.this;
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

    public List<Map<String, Object>> getHttpCallableMethodInfo() {
	throw new UnsupportedOperationException("Unimplemented method 'getHttpCallableMethodInfo'");
    }

    public ABSFut<? extends Object> invokeMethod(String name, List<Object> arguments) {
	throw new UnsupportedOperationException("Unimplemented method 'invokeMethod'");
    }

}
