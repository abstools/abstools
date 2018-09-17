/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.typechecker;

import java.util.HashMap;
import java.util.Map;

import org.abs_models.frontend.analyser.ErrorMessage;
import org.abs_models.frontend.analyser.TypeError;
import org.abs_models.frontend.ast.ModuleDecl;
import org.abs_models.frontend.ast.Name;

@SuppressWarnings("serial")
public class ResolvedMap extends HashMap<KindedName, ResolvedName> {

    public ResolvedMap() {
        super();
    }

    public ResolvedMap(Map<KindedName, ResolvedName> init) {
        super(init);
    }

    public ResolvedMap getAllNames(String name) {
        ResolvedMap map = new ResolvedMap();
        for (KindedName.Kind k : KindedName.Kind.values()) {
            KindedName kn = new KindedName(k, name);
            ResolvedName rn = get(kn);
            if (rn != null) {
                map.put(kn, rn);
            }
        }
        return map;
    }

    public void putKindedNames(String name, ResolvedMap sourceMap) {
        putAll(sourceMap.getAllNames(name));
    }

    public void putKindedNamesNoHiding(String name, ResolvedMap sourceMap) {
        addAllNamesNoHiding(sourceMap.getAllNames(name));
    }

    /**
     * add all entries from one map to an this one. 
     * when there is a duplicate key, the ResolvedName is not overridden, but replaced 
     * by an instance of ResolvedAmbigiousName
     * 
     * @param source
     */
    void addAllNamesNoHiding(ResolvedMap source) {
        for (Entry<KindedName, ResolvedName> e : source.entrySet()) {
            KindedName kindedName = e.getKey();
            ResolvedName resolvedName = e.getValue();
            ResolvedName prev = put(kindedName, resolvedName);
            if (prev != null && !(prev.equals(resolvedName))) {
                // name is ambiguous
                put(kindedName, new ResolvedAmbigiousName(prev, resolvedName));
            }
        }
    }

    public void putNamesOfModule(ModuleDecl mod, ResolvedMap with, String moduleName, String simpleNamePattern) {
        for (Map.Entry<KindedName, ResolvedName> entry : with.entrySet()) {
            KindedName kn = entry.getKey();
            if (kn.isQualified()) {
                if (kn.getModuleName().equals(moduleName)) {
                    String simpleName = kn.getSimpleName();
                    if (simpleNamePattern == null || simpleNamePattern.equals(simpleName)) {
                        put(new KindedName(kn.getKind(), mod.getName() + "." + simpleName), entry.getValue());
                        put(new KindedName(kn.getKind(), simpleName), entry.getValue());
                    }
                }
            }
        }
    }

    public void addAllNames(ResolvedMap source, Name n) {
        ResolvedMap allNames = source.getAllNames(n.getSimpleName());
        if (!allNames.isEmpty()) {
            for (Entry<KindedName, ResolvedName> e : allNames.entrySet()) {
                put(new KindedName(e.getKey().getKind(), n.getModuleName() + "." + e.getKey().getName()),
                        e.getValue());
            }
        } else {
            throw new TypeCheckerException(new TypeError(n, ErrorMessage.NAME_NOT_EXPORTED_BY_MODULE,
                    n.getSimpleName(), n.getModuleName()));
        }
    }
}
