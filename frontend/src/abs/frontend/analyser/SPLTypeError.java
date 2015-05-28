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

    private final List<DeltaDecl> deltas; // Sequence of applied deltas that have led to this error
    private final ImplicitProduct product; // Product that we tried to build

    /**
     * A TypeError that tracks the circumstances in which an SPL type error occurred:
     * Which product were we trying to build? What delta application sequence led to error?
     *
     * @param node      ASTNode where this error occurred
     * @param msg       Error message to display
     * @param deltas    List of applied DeltaDecls that have led to this error (size of list must be > 0)
     * @param product   ImplicitProduct that we tried to build (non-null)
     * @param args      Strings to include in ErrorMessage
     */
    public SPLTypeError(ASTNode<?> node, ErrorMessage msg, List<DeltaDecl> deltas, ImplicitProduct product, String... args) {
        super(node, msg, args);
        this.deltas = deltas;
        this.product = product;
        assert deltas.size() > 0;
        assert product != null;
    }

    @Override
    public String getMessage() {
        String lastDelta = deltas.get(deltas.size()-1).getName();

        StringBuilder s = new StringBuilder();
        for (int i = deltas.size()-2; i >= 0; i--) {
            s.append(deltas.get(i).getName());
            s.append(" >> ");
        }
        s.append("core");
        String appliedDeltas = s.toString();

        Set<String> features = new HashSet<String>();
        for (Feature feature : product.getFeatures())
            features.add(feature.getName());
        Joiner joiner = Joiner.on(",").skipNulls();
        String featureSet = joiner.join(features);

        return getMsg()
                + " When applying delta " + lastDelta
                + " on top of " + appliedDeltas
                + ", while building product {" + featureSet + "}.";
    }

}
