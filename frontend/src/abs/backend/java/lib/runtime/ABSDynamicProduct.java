/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Set;

import abs.backend.java.lib.types.ABSBool;
import abs.backend.java.lib.types.ABSClass;
import abs.backend.java.lib.types.ABSValue;

public class ABSDynamicProduct implements ABSClass {

    private String name;
    private Set<String> features;
    private HashMap<String, ArrayList<String>> adaptations = new HashMap<String, ArrayList<String>>();
    
    public String getName() {
        return name;
    }
    public void setName(String name) {
        this.name = name;
    }
   
    public Set<String> getFeatures() {
        return features;
    }
    public void setFeatures(Set<String> features) {
        this.features = features;
    }
    public void addFeature(String feature) {
        this.features.add(feature);
    }

    public HashMap<String, ArrayList<String>> getAdaptations() {
        return adaptations;
    }
    public void addAdaptation(String productName, ArrayList<String> deltaNames) {
        this.adaptations.put(productName, deltaNames);
    }


    @Override
    public ABSBool eq(ABSValue o) {
        if (o instanceof ABSDynamicProduct)
            return ABSBool.fromBoolean(name.equals(((ABSDynamicProduct)o).getName()));
        else 
            return ABSBool.FALSE;
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
