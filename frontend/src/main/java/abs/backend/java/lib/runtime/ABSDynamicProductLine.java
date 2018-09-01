/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

import java.util.HashSet;
import java.util.Set;

import abs.backend.java.codegeneration.dynamic.DynamicException;
import abs.backend.java.lib.runtime.metaABS.ProductLine;

public class ABSDynamicProductLine extends ABSDynamicObject {

    public ABSDynamicProductLine() {
        super(ProductLine.singleton());
    }

    private Set<ABSDynamicProduct> products = new HashSet<>();
    private Set<ABSDynamicReconfiguration> reconfigurations = new HashSet<>();
    private ABSDynamicProduct currentProduct = null;


    public ABSDynamicProduct getCurrentProduct() {
        if (currentProduct == null)
            throw new DynamicException("The current system does not represent a product of the SPL. Please specify the initial product when compiling.");
        else
            return currentProduct;
    }

    public void setCurrentProduct(ABSDynamicProduct prod) {
        currentProduct = prod;
    }

    public ABSDynamicProduct getProduct(String name) {
        for (ABSDynamicProduct p : products) {
            if (name.equals(p.getName()))
                return p;
        }
        throw new DynamicException("The DSPL contains no product named " + name + ".");
    }

    public void reconfigure(ABSDynamicProduct prod) {
        ABSDynamicReconfiguration recf = prod.getReconfiguration(prod);
        for (ABSDynamicDelta delta : recf.getDeltas())
            delta.apply();
        recf.getUpdate().apply();
        setCurrentProduct(prod);
    }

    public void addProduct(ABSDynamicProduct prod) {
        products.add(prod);
    }

    public void removeProduct(ABSDynamicProduct prod) {
        if (products.contains(prod))
            products.remove(prod);
        else
           throw new DynamicException("The product " + prod.getName() + " is not part of the DSPL and cannot be removed.");
    }

    public void addReconfiguration(ABSDynamicReconfiguration recf) {
        reconfigurations.add(recf);
    }

    public void removeReconfiguration(ABSDynamicReconfiguration recf) {
        if (reconfigurations.contains(recf))
            reconfigurations.remove(recf);
        else
           throw new DynamicException("The reconfiguration " + recf.getName() + " is not part of the DSPL and cannot be removed.");
    }
}
