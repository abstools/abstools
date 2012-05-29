/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import abs.backend.java.codegeneration.dynamic.DynamicException;
import abs.backend.java.lib.types.ABSValue;

public class ABSDynamicObject extends ABSObject {
    private ABSClass clazz;
    private Map<String,ABSValue> fields;
    
    public ABSDynamicObject(ABSClass clazz, ABSValue... params) {
        this.clazz = clazz;
        initializeFields(params);
    }
    
    private void initializeFields(ABSValue[] params) {
        if (params.length != clazz.getParams().size()) throw new DynamicException("Type mismatch");
        for (int i = 0; i < params.length; i++) {
            setFieldValue(clazz.getParams().get(i), params[i]);
        }
        this.getCOG().objectCreated(this);
    }

    public String getClassName() {
        return getClazz().getName();
    }

    public void setClazz(ABSClass clazz) {
        this.clazz = clazz;
    }

    public ABSClass getClazz() {
        return clazz;
    }
    
    public List<String> getFieldNames() {
        return new ArrayList<String>(clazz.getFieldNames());
    }
    
    public ABSValue getFieldValue(String field) {
        if (fields == null) {
            return null;
        }
        return fields.get(field);
    }
    
    public void setFieldValue(String field, ABSValue val) {
        if (fields == null) {
            fields = new HashMap<String,ABSValue>();
        }
        fields.put(field, val);
    }
    
    protected void __ABS_init() {
        clazz.getConstructor().exec(this);
        this.getCOG().objectInitialized(this);
    }
    
}
