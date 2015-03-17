/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.analyser.TypeError;
import abs.frontend.ast.*;

public class ProgramTypeAbstraction {
    private final Map<String, Map<String, Set<String>>> classes;
    private final SemanticErrorList errors;

    // Constructor
    public ProgramTypeAbstraction(SemanticErrorList errors) {
        this.errors = errors;
        classes = new HashMap<String, Map<String, Set<String>>>();
    }

    // Copy constructor
    public ProgramTypeAbstraction(ProgramTypeAbstraction sourceTA) {
        this.errors = sourceTA.errors;
        classes = new HashMap<String, Map<String, Set<String>>>();
        for (String className : sourceTA.classes.keySet()) {
            addClass(className);
            for (String name : sourceTA.classes.get(className).get("fields"))
                classes.get(className).get("fields").add(name);
            for (String name : sourceTA.classes.get(className).get("methods"))
                classes.get(className).get("methods").add(name);
        }
    }

    // TODO record type errors:
    // - element is added but already exists
    // - element is removed/modified but does not exist

    public void addClass(String className, AddClassModifier node) {
        if (classes.containsKey(className))
            errors.add(new TypeError(node, ErrorMessage.DUPLICATE_CLASS_NAME, node.getName()));
        else
            addClass(className);
    }
    private void addClass(String className) {
        classes.put(className, new HashMap<String, Set<String>>());
        classes.get(className).put("fields", new HashSet<String>());
        classes.get(className).put("methods", new HashSet<String>());

        System.out.print("[add " + className + "]");
    }

    public void removeClass(String className, RemoveClassModifier node) {
        if (classes.containsKey(className))
            classes.remove(className);
        else
            errors.add(new TypeError(node, ErrorMessage.NO_CLASS_DECL, node.getName()));

        System.out.print("[remove " + className + "]");
    }

    public void addField(String className, String name) {
        if (classes.get(className) == null)
            addClass(className);
        classes.get(className).get("fields").add(name);
    }

    public void addMethod(String className, String name) {
        if (classes.get(className) == null)
            addClass(className);
        classes.get(className).get("methods").add(name);
    }


    public void applyDelta(DeltaDecl delta) {
        for (ModuleModifier mod : delta.getModuleModifiers()) {
            //System.out.println("*** applying ModuleModifier " + mod + " to TypeAbstraction");
            mod.applyToTypeAbstraction(this);
        }
    }

    @Override
    public String toString() {
        StringBuilder s = new StringBuilder();

        for (String cls : classes.keySet()) {
            s.append(cls + ";");
        }
        return "<<" + s.toString() + ">>";
    }
}
