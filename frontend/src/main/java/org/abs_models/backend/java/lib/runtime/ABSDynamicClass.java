/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.abs_models.backend.java.codegeneration.dynamic.DynamicException;
import org.abs_models.backend.java.lib.types.ABSClass;
import org.abs_models.backend.java.observing.manipulation.ClassManipulator;

public class ABSDynamicClass implements ABSClass {
    private String name;
    private Map<String, ABSField> fields = new HashMap<>();
    private Map<String, ABSClosure> methods = new HashMap<>();
    private ABSClosure constructor;
    private List<String> params;
    private ABSDynamicClass nextVersion;

    public void setName(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public void addField(String fName, ABSField f) throws DynamicException {
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
        if (fields.containsKey(fName))
            return fields.get(fName);
        else
            throw new DynamicException("Field " + fName + " not defined for class " + name + ".");
    }

    public boolean hasMethod(String mName) {
        return methods.containsKey(mName);
    }

    public void addMethod(String mName, ABSClosure m) throws DynamicException {
        if (! hasMethod(mName))
            methods.put(mName, m);
        else
            throw new DynamicException("Method " + mName + " already defined for class " + name + ".");
    }

    public void removeMethod(String mName) throws DynamicException {
        if (hasMethod(mName))
            methods.remove(mName);
        else
            throw new DynamicException("Method " + mName + " not defined for class " + name + ".");
    }

    public ABSClosure getMethod(String mName) throws DynamicException {
        if (hasMethod(mName))
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
        return fields.keySet();
    }

    public Set<String> getMethodNames() {
        return methods.keySet();
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

    // Create the "next version" of this class, which can be updated independently
    public ABSDynamicClass createNextVersion() {
        ABSDynamicClass copy = new ABSDynamicClass();
        copy.name = name;
        for (String f : fields.keySet())
            copy.fields.put(f, fields.get(f));
        for (String m : methods.keySet())
            copy.methods.put(m, methods.get(m));
        copy.params = params;
        copy.constructor = constructor;
        copy.nextVersion = null;
        nextVersion = copy;
        return copy;
    }

    public ABSDynamicClass getNextVersion() {
        return nextVersion;
    }

    private View view;
    public ClassManipulator getView() {
        if (view == null) {
            synchronized(this) {
                if (view == null) {
                    view = new View();
                }
            }
        }
        return view;
    }

    private class View implements ClassManipulator {

        @Override
        public String getName() {
            return ABSDynamicClass.this.getName();
        }

        @Override
        public List<String> getFieldNames() {
            return new ArrayList<>(ABSDynamicClass.this.getFieldNames());
        }

        @Override
        public List<String> getMethodNames() {
            if (methods == null) return Collections.emptyList();
            return new ArrayList<>(methods.keySet());
        }

        @Override
        public void addField(String name, String type, ABSClosure init) {
            // FIXME
            if (fields == null) {
                fields = new HashMap<>();
            }
            fields.put(name, new ABSField());
        }

    }

}
