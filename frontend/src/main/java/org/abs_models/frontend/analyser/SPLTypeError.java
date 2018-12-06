/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.analyser;

import java.util.List;
import org.abs_models.frontend.ast.ASTNode;
import org.abs_models.frontend.ast.DeltaDecl;
import org.abs_models.frontend.ast.Product;

public class SPLTypeError extends TypeError {

    private final List<DeltaDecl> deltas; // Sequence of applied deltas that have led to this error
    private final Product product; // Product that we tried to build

    /**
     * A TypeError that tracks the circumstances in which an SPL type error occurred:
     * Which product were we trying to build? What delta application sequence led to error?
     *
     * @param node      ASTNode where this error occurred
     * @param msg       Error message to display
     * @param deltas    List of applied DeltaDecls that have led to this error (size of list must be {@code > 0})
     * @param product   Product that we tried to build (non-null)
     * @param args      Strings to include in ErrorMessage
     */
    public SPLTypeError(ASTNode<?> node, ErrorMessage msg, List<DeltaDecl> deltas, Product product, String... args) {
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

        return getMsg()
                + " When applying delta " + lastDelta
                + " on top of " + appliedDeltas
                + ", while building product " + product.getFeatureSetAsString() + ".";
    }

}
