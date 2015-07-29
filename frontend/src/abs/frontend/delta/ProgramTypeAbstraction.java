/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.analyser.SPLTypeError;
import abs.frontend.ast.*;

public class ProgramTypeAbstraction {
    private final Map<String, Map<String, Set<String>>> classes;
    private final SemanticErrorList errors;

    // Keep track of sequence of deltas already applied to this TA, for better error reporting
    private final java.util.List<DeltaDecl> deltas;

    // Keep track of the product we are currently trying to build, for better error reporting
    private ImplicitProduct product;

    // Constructor
    public ProgramTypeAbstraction(SemanticErrorList errors) {
        this.errors = errors;
        deltas = new ArrayList<DeltaDecl>();
        classes = new HashMap<String, Map<String, Set<String>>>();
    }

    // Copy constructor
    public ProgramTypeAbstraction(ProgramTypeAbstraction sourceTA) {
        this.errors = sourceTA.errors;
        this.deltas = new ArrayList<DeltaDecl>(sourceTA.deltas);
        classes = new HashMap<String, Map<String, Set<String>>>();
        for (String className : sourceTA.classes.keySet()) {
            addClass(className);
            for (String name : sourceTA.classes.get(className).get("fields"))
                classes.get(className).get("fields").add(name);
            for (String name : sourceTA.classes.get(className).get("methods"))
                classes.get(className).get("methods").add(name);
        }
    }

    public void applyDelta(DeltaDecl delta, ImplicitProduct product) {
        deltas.add(delta);
        this.product = product;
        for (ModuleModifier mod : delta.getModuleModifiers()) {
            mod.applyToTypeAbstraction(this);
        }
    }

    public void addClass(AddClassModifier node) {
        String className = node.qualifiedName();
        if (classes.containsKey(className))
            errors.add(new SPLTypeError(node, ErrorMessage.DUPLICATE_CLASS_NAME, deltas, product, className));
        else
            addClass(className);
    }
    public void addClass(String className) {
        classes.put(className, new HashMap<String, Set<String>>());
        classes.get(className).put("fields", new HashSet<String>());
        classes.get(className).put("methods", new HashSet<String>());
    }

    public void removeClass(RemoveClassModifier node) {
        String className = node.qualifiedName();
        if (classes.containsKey(className))
            classes.remove(className);
        else
            errors.add(new SPLTypeError(node, ErrorMessage.NO_CLASS_DECL, deltas, product, className));
    }

    public void addField(String className, AddFieldModifier node) {
        String name = node.getFieldDecl().getName();
        if (! classes.get(className).get("fields").contains(name))
            addField(className, name);
        else
            errors.add(new SPLTypeError(node, ErrorMessage.DUPLICATE_FIELD_NAME, deltas, product, name));
    }
    public void addField(String className, String fieldName) {
        classes.get(className).get("fields").add(fieldName);
    }

    public void removeField(String className, RemoveFieldModifier node) {
        String name = node.getFieldDecl().getName();
        if (classes.get(className).get("fields").contains(name))
            classes.get(className).get("fields").remove(name);
        else
            errors.add(new SPLTypeError(node, ErrorMessage.NO_FIELD_DECL, deltas, product, name));
    }

    public void addMethod(String className, AddMethodModifier node) {
        String name = node.getMethodImpl().getMethodSig().getName();
        if (! classes.get(className).get("methods").contains(name))
            addMethod(className, name);
        else
            errors.add(new SPLTypeError(node, ErrorMessage.DUPLICATE_METHOD_NAME, deltas, product, name));
    }
    public void addMethod(String className, String methodName) {
        classes.get(className).get("methods").add(methodName);
    }

    public void modifyMethod(String className, AddMethodModifier node) {
        String name = node.getMethodImpl().getMethodSig().getName();
        if (! classes.get(className).get("methods").contains(name))
            errors.add(new SPLTypeError(node, ErrorMessage.NO_METHOD_IMPL, deltas, product, name));  // FIXME Error message probably not suitable
    }

    public void removeMethod(String className, RemoveMethodModifier node) {
        String name = node.getMethodSig().getName();
        if (classes.get(className).get("methods").contains(name))
            classes.get(className).get("methods").remove(name);
        else
            errors.add(new SPLTypeError(node, ErrorMessage.NO_METHOD_IMPL, deltas, product, name));  // FIXME Error message probably not suitable
    }


    // helper method
    public boolean existsClass(ModifyClassModifier node) {
        String className = node.qualifiedName();
        if (classes.containsKey(className)) {
            return true;
        } else {
            errors.add(new SPLTypeError(node, ErrorMessage.NO_CLASS_DECL, deltas, product, className));
            return false;
        }
    }

    @Override
    public String toString() {
        StringBuilder s = new StringBuilder();
        final Map<String,Object> sortedClasses = new TreeMap<String, Object>(classes);

        for (String cls : sortedClasses.keySet()) {
            s.append("  Class: " + cls + "\n" + "    Fields:\n");
            for (String field : classes.get(cls).get("fields"))
                s.append("      " + field + "\n");
            s.append("    Methods:\n");
            for (String method : classes.get(cls).get("methods"))
                s.append("      " + method + "\n");
        }
        return s.toString();
    }
}
