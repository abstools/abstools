/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class ABSClass {
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
    
    public void addField(String name, ABSField f) {
        if (fields == null) {
            fields = new HashMap<String,ABSField>();
        }
        fields.put(name,f);
    }
    public void getField(String name) {
        if (fields == null) {
            fields = new HashMap<String,ABSField>();
        }
        fields.get(name);
    }
    
    public void addMethod(String name, ABSClosure m) {
        if (methods == null) {
            methods = new HashMap<String,ABSClosure>();
        }
        methods.put(name, m);
    }
    
    public ABSClosure getMethod(String name) {
        if (methods == null) {
            methods = new HashMap<String,ABSClosure>();
        }
        return methods.get(name);
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
    

}
