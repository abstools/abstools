/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import abs.backend.java.codegeneration.dynamic.DynamicException;
import abs.backend.java.lib.runtime.metaABS.Product;

public class ABSDynamicProduct extends ABSDynamicObject {

    public ABSDynamicProduct() {
        super(Product.singleton());
    }

    protected String name;
    private Set<ABSDynamicFeature> features = new HashSet<>();
    private Map<ABSDynamicProduct,ABSDynamicReconfiguration> reconfigurations = new HashMap<>();

    public String getName() {
        return name;
    }
    public void setName(String name) {
        this.name = name;
    }

    public Set<ABSDynamicFeature> getFeatures() {
        return features;
    }
    public void setFeatures(Set<ABSDynamicFeature> features) {
        this.features = features;
    }
    public void addFeature(ABSDynamicFeature feature) {
        features.add(feature);
    }
    public void addFeature(String name) {
        ABSDynamicFeature f = new ABSDynamicFeature();
        f.setName(name);
        features.add(f);
    }

    public Set<ABSDynamicProduct> getConfigurableProducts() {
        return reconfigurations.keySet();
    }

    public void addReconfiguration(ABSDynamicProduct targetP, ABSDynamicReconfiguration recf) {
        reconfigurations.put(targetP, recf);
    }

    public ABSDynamicReconfiguration getReconfiguration(ABSDynamicProduct targetP) {
        if (! reconfigurations.containsKey(targetP))
            return reconfigurations.get(targetP);
        else
            throw new DynamicException("The product " + name + " cannot be reconfigured into product " + targetP.getName() + ".");

    }
}
