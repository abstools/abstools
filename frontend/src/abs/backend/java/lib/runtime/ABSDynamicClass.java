/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import abs.backend.java.codegeneration.dynamic.DynamicException;
import abs.backend.java.lib.types.ABSBool;
import abs.backend.java.lib.types.ABSClass;
import abs.backend.java.lib.types.ABSValue;
import abs.backend.java.observing.manipulation.ClassManipulator;

public class ABSDynamicClass implements ABSClass {
    private String name;
    private Map<String, ABSField> fields;
    private Map<String, ABSClosure> methods;
    private ABSClosure constructor;
    private List<String> params;

    public void setName(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }
    
    public void addField(String fName, ABSField f) throws DynamicException {
        if (fields == null) {
            fields = new HashMap<String, ABSField>();
        }
        if (! fields.containsKey(fName))
            fields.put(fName,f);
        else
            throw new DynamicException("Field " + fName + " already defined for class " + name + ".");
    }
    
    public void removeField(String fName) throws DynamicException {
        if (fields.containsKey(fName))
            fields.remove(fName);
        else
            throw new DynamicException("Field " + fName + " not defined for class " + name + ".");
    }
    
    public ABSField getField(String fName) throws DynamicException {
        if (fields == null) {
            fields = new HashMap<String, ABSField>();
        }
        if (fields.containsKey(fName))
            return fields.get(fName);
        else
            throw new DynamicException("Field " + fName + " not defined for class " + name + ".");
    }
    
    public void addMethod(String mName, ABSClosure m) throws DynamicException {
        if (methods == null) {
            methods = new HashMap<String,ABSClosure>();
        }
        if (! methods.containsKey(mName))
            methods.put(mName, m);
        else
            throw new DynamicException("Method " + mName + " already defined for class " + name + ".");
    }
    
    public ABSClosure getMethod(String mName) throws DynamicException {
        if (methods == null) {
            methods = new HashMap<String,ABSClosure>();
        }
        if (methods.containsKey(mName))
            return methods.get(mName);
        else
            throw new DynamicException("Method " + mName + " not defined for class " + name + ".");
    }

    public void setConstructor(ABSClosure constructor) {
        this.constructor = constructor;
    }

    public ABSClosure getConstructor() {
        return constructor;
    }

    public Set<String> getFieldNames() {
        if (fields == null) {
            return Collections.emptySet();
        }
        return fields.keySet();
    }

    public void setParams(String... args) {
        setParams(Arrays.asList(args));
    }
    public void setParams(List<String> params) {
        this.params = params;
    }

    public List<String> getParams() {
        if (params == null) {
            return Collections.emptyList();
        }
        return params;
    }
    
    private View __view;
    public synchronized ClassManipulator getView() {
        if (__view == null) {
            __view = new View();
        }
        return __view;
    }
    
    private class View implements ClassManipulator {

        @Override
        public String getName() {
            return ABSDynamicClass.this.getName();
        }

        @Override
        public List<String> getFieldNames() {
            return new ArrayList<String>(ABSDynamicClass.this.getFieldNames());
        }

        @Override
        public List<String> getMethodNames() {
            if (methods == null) return Collections.emptyList();
            return new ArrayList<String>(methods.keySet());
        }

        @Override
        public void addField(String name, String type, ABSClosure init) {
            // FIXME
            if (fields == null) {
                fields = new HashMap<String,ABSField>();
            }
            fields.put(name, new ABSField());
        }
        
    }

    @Override
    public ABSBool eq(ABSValue o) {
        return ABSBool.fromBoolean(name.equals(((ABSDynamicClass)o).getName()));
    }

    @Override
    public ABSBool notEq(ABSValue o) {
        return eq(o).negate();
    }

    @Override
    public boolean isDataType() {
        return false;
    }

    @Override
    public boolean isReference() {
        return true;
    }
}
