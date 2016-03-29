/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

import abs.frontend.FrontendTest;
import abs.frontend.analyser.SemanticConditionList;
import abs.frontend.ast.*;
import abs.frontend.typechecker.TypeCheckerHelper;

public class DeltaTest extends FrontendTest {

    // helper method: find a Decl node in given module
    static protected Decl findDecl(Model model, String moduleName, String name) {
        for (ModuleDecl m : model.getModuleDecls()) {
            if (m.getName().equals(moduleName))
                for (Decl d : m.getDecls()) {
                    if (d.getName().equals(name))
                        return d;
                }
        }
        return null;
    }

    protected DeltaDecl findDelta(Model model, String name) {
        return model.findDelta(name);
    }

    protected void typeCheck(Model m, ProductDecl p, SemanticConditionList e) {
        Map<String,Feature> featureNames = null;
        if (m.hasProductLine()) {
            featureNames = new HashMap<String,Feature>();
            for (Feature f : m.getProductLine().getFeatures()) {
                featureNames.put(f.getName(),f);
            }
        }
        HashSet<String> productNames = new HashSet<String>();
        for (ProductDecl prod : m.getProductDecls()) {
            productNames.add(prod.getName());
        }
        HashSet<String> updateNames = new HashSet<String>();
        for (UpdateDecl upd : m.getUpdateDecls()) {
            updateNames.add(upd.getName());
        }
        TypeCheckerHelper.typeCheckProductDecl(p, featureNames, productNames, m.getDeltaDeclsMap(), updateNames, e);
    }
}
