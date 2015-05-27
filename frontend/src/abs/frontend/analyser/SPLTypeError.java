/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.analyser;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.google.common.base.Joiner;

import abs.frontend.ast.ASTNode;
import abs.frontend.ast.DeltaDecl;
import abs.frontend.ast.Feature;
import abs.frontend.ast.ImplicitProduct;

public class SPLTypeError extends TypeError {

    List<DeltaDecl> deltas; // Sequence of applied deltas that have led to this error
    ImplicitProduct product; // Product that we tried to build

    public SPLTypeError(ASTNode<?> node, ErrorMessage msg, List<DeltaDecl> deltas, ImplicitProduct product, String... args) {
        super(node, msg, args);
        this.deltas = deltas;
        this.product = product;
    }

    @Override
    public String getMessage() {
        String currentDelta = deltas.get(deltas.size()-1).getName();

        StringBuilder s = new StringBuilder();
        for (int i = deltas.size()-2; i >= 0; i--) {
            s.append(deltas.get(i).getName());
            s.append(" < ");
        }
        s.append("core");
        String appliedDeltas = s.toString();

        String targetProduct = "";
        if (product != null) {
            Set<String> features = new HashSet<String>();
            for (Feature feature : product.getFeatures())
                features.add(feature.getName());
            Joiner joiner = Joiner.on(",").skipNulls();
            String featureSet = joiner.join(features);
            targetProduct = ", while building product {" + featureSet + "}";
        }

        return getMsg()
                + " When applying delta " + currentDelta
                + " on top of " + appliedDeltas
                + targetProduct
                + ".";
    }

}
