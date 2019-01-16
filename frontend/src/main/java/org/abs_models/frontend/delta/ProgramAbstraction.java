/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.delta;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;

import com.google.common.collect.ImmutableList;

import org.abs_models.common.StringUtils;
import org.abs_models.frontend.analyser.ErrorMessage;
import org.abs_models.frontend.analyser.SemanticConditionList;
import org.abs_models.frontend.analyser.SPLTypeError;
import org.abs_models.frontend.ast.*;

public class ProgramAbstraction {
    /* Remember essential program information in a Program Signature Table (PST)
     * This includes:
     * - classes
     *   - implemented interfaces (name)
     *   - class fields (name => type)
     *   - class methods (name => return type, argument types, TODO? argument names)
     * - interfaces
     *   - extended interfaces (name) TODO
     *   - interface methods (name => return type, argument types, TODO? argument names)
     *
     * We use a data structure based on maps (in the hope that maps can be created/accessed faster than objects)
     * Goal: using ProgramAbstractions instead of accessing the AST node objects should be much faster
     */
    private final Map<String, Map<String, Map<String,java.util.List<String>>>> classes;
    public Map<String, Map<String, Map<String, java.util.List<String>>>> getClasses() {
        return classes;
    }

    private final Map<String, Map<String, Map<String,java.util.List<String>>>> interfaces;
    public Map<String, Map<String, Map<String, java.util.List<String>>>> getInterfaces() {
        return interfaces;
    }

    private final SemanticConditionList errors;

    // Keep track of sequence of deltas already applied to this TA, for better error reporting
    private final java.util.List<DeltaDecl> deltas;

    // Keep track of the product we are currently trying to build, for better error reporting
    private Product product;

    // Constructor
    public ProgramAbstraction(SemanticConditionList errors) {
        this.errors = errors;
        deltas = new ArrayList<>();
        classes = new HashMap<>();
        interfaces = new HashMap<>();
    }

    // Copy constructor
    public ProgramAbstraction(ProgramAbstraction sourceTA) {
        this.errors = sourceTA.errors;
        this.deltas = new ArrayList<>(sourceTA.deltas);
        classes = new HashMap<>();
        for (String className : sourceTA.classes.keySet()) {
            classAdd(className);
            assert classes.get(className).containsKey("fields");
            assert classes.get(className).containsKey("methods");
            assert classes.get(className).containsKey("interfaces");
            for (String field : sourceTA.classes.get(className).get("fields").keySet())
                classes.get(className).get("fields").put(field, sourceTA.classes.get(className).get("fields").get(field));
            for (String method : sourceTA.classes.get(className).get("methods").keySet())
                classes.get(className).get("methods").put(method, sourceTA.classes.get(className).get("methods").get(method));
            for (String iface : sourceTA.classes.get(className).get("interfaces").keySet())
                classes.get(className).get("interfaces").put(iface, sourceTA.classes.get(className).get("methods").get(iface));
        }
        interfaces = new HashMap<>();
        for (String ifName : sourceTA.interfaces.keySet()) {
            interfaceAdd(ifName);
            assert interfaces.get(ifName).containsKey("methods");
            for (String method : sourceTA.interfaces.get(ifName).get("methods").keySet())
                interfaces.get(ifName).get("methods").put(method, sourceTA.interfaces.get(ifName).get("methods").get(method));
        }
    }

    public void applyDelta(DeltaDecl delta, Product product) {
        deltas.add(delta);
        this.product = product;
        for (ModuleModifier mod : delta.getModuleModifiers()) {
            mod.applyToProgramAbstraction(this);
        }
    }

    /*
     * Classes
     */
    public void classAdd(AddClassModifier node) {
        String className = node.getQualifiedName();
        if (classes.containsKey(className))
            errors.add(new SPLTypeError(node, ErrorMessage.DUPLICATE_CLASS_NAME, deltas, product, className,
                    // TODO add " at file:line" with location of original definition
                    ""));
        else {
            classAdd(className);
            for (ParamDecl field : node.getClassDecl().getParams())
                classFieldAdd(className, field.getName(), getVarType(field));
            for (FieldDecl field : node.getClassDecl().getFields())
                classFieldAdd(className, field.getName(), getVarType(field));
            for (MethodImpl method : node.getClassDecl().getMethods()) {
                java.util.List<String> types = getMethodParameterTypes(method.getMethodSig());
                classMethodAdd(className, method.getMethodSig().getName(), types);
            }
        }
    }
    public void classAdd(String className) {
        classes.put(className, new HashMap<>());
        classes.get(className).put("fields", new HashMap<>());
        classes.get(className).put("methods", new HashMap<>());
        classes.get(className).put("interfaces", new HashMap<>());
    }
    public void classRemove(RemoveClassModifier node) {
        String className = node.getQualifiedName();
        if (classes.containsKey(className))
            classes.remove(className);
        else
            errors.add(new SPLTypeError(node, ErrorMessage.NO_CLASS_DECL, deltas, product, className));
    }

    public void classFieldAdd(String className, AddFieldModifier node) {
        String name = node.getFieldDecl().getName();
        String type = getVarType(node.getFieldDecl());
        if (! classes.get(className).get("fields").containsKey(name))
            classFieldAdd(className, name, type);
        else
            errors.add(new SPLTypeError(node, ErrorMessage.DUPLICATE_FIELD_NAME, deltas, product, name));
    }

    public void classFieldAdd(String className, String name, String type) {
        classes.get(className).get("fields").put(name, ImmutableList.of(type));
    }

    public void classFieldRemove(String className, RemoveFieldModifier node) {
        String name = node.getFieldDecl().getName();
        String type = getVarType(node.getFieldDecl());
        if (classes.get(className).get("fields").containsKey(name) && type.equals(classes.get(className).get("fields").get(name).get(0)))
            classes.get(className).get("fields").remove(name);
        else
            errors.add(new SPLTypeError(node, ErrorMessage.NO_FIELD_DECL, deltas, product, type + " " + name));
    }

    public void classMethodAdd(String className, AddMethodModifier node) {
        String name = node.getMethodImpl().getMethodSig().getName();
        java.util.List<String> types = getMethodParameterTypes(node.getMethodImpl().getMethodSig());
        if (! classes.get(className).get("methods").containsKey(name))
            classMethodAdd(className, name, types);
        else
            errors.add(new SPLTypeError(node, ErrorMessage.DUPLICATE_METHOD_NAME, deltas, product,
                    types.get(0) + " "
                            + name + "("
                            + StringUtils.join(",", types.subList(1, types.size())) + ")"));
    }

    public void classMethodAdd(String className, String methodName, java.util.List<String> types) {
        classes.get(className).get("methods").put(methodName, types);
    }

    public void classMethodRemove(String className, RemoveMethodModifier node) {
        String name = node.getMethodSig(0).getName();
        java.util.List<String> types = getMethodParameterTypes(node.getMethodSig(0)); // A MethodModifier should only reference one method
        if (classes.get(className).get("methods").containsKey(name) && types.equals(classes.get(className).get("methods").get(name)))
            classes.get(className).get("methods").remove(name);
        else
            errors.add(new SPLTypeError(node, ErrorMessage.NO_METHOD_DECL, deltas, product,
                    types.get(0) + " "
                            + name + "("
                            + StringUtils.join(",", types.subList(1, types.size())) + ")"));
    }

    public void classMethodModify(String className, ModifyMethodModifier node) {
        String name = node.getMethodImpl().getMethodSig().getName();
        java.util.List<String> types = getMethodParameterTypes(node.getMethodImpl().getMethodSig());
        if (!(classes.get(className).get("methods").containsKey(name) && types.equals(classes.get(className).get("methods").get(name))))
            errors.add(new SPLTypeError(node, ErrorMessage.NO_METHOD_DECL, deltas, product,
                    types.get(0) + " "
                            + name + "("
                            + StringUtils.join(",", types.subList(1, types.size())) + ")"));
    }

    public void classInterfaceAdd(String className, InterfaceTypeUse node) {
        String name = node.getName();
        if (classes.get(className).get("interfaces").containsKey(name))
            errors.add(new SPLTypeError(node, ErrorMessage.DUPLICATE_INTERFACE_IMPLEMENTATION, deltas, product, name,
                    // TODO add " at file:line" with location of original definition
                    ""));
        else
            classes.get(className).get("interfaces").put(name, ImmutableList.<String>of());
    }

    public void classInterfaceRemove(String className, InterfaceTypeUse node) {
        String name = node.getName();
        if (! classes.get(className).get("interfaces").containsKey(name))
            errors.add(new SPLTypeError(node, ErrorMessage.MISSING_INTERFACE_IMPLEMENTATION, deltas, product, name,
                    // TODO add " at file:line" with location of original definition
                    ""));
        else
            classes.get(className).get("interfaces").remove(name);
    }

    /*
     * Interfaces
     * - add
     * - remove
     * - modify
     */
    public void interfaceAdd(AddInterfaceModifier node) {
        String name = node.getQualifiedName();
        if (interfaces.containsKey(name))
            errors.add(new SPLTypeError(node, ErrorMessage.DUPLICATE_INTERFACE_NAME, deltas, product, name,
                    // TODO add " at file:line" with location of original definition
                    ""));
        else
            interfaceAdd(name);
    }
    public void interfaceAdd(String name) {
        interfaces.put(name, new HashMap<>());
        interfaces.get(name).put("methods", new HashMap<>());
    }

    // types: first entry is return type; rest is argument types
    public void interfaceMethodAdd(String name, String methodName, java.util.List<String> types) {
        interfaces.get(name).get("methods").put(methodName, types);
    }

    public void interfaceMethodRemove(String ifName, RemoveMethodSigModifier node) {
        String name = node.getMethodSig().getName();
        java.util.List<String> types = getMethodParameterTypes(node.getMethodSig());
        if (interfaces.get(ifName).get("methods").containsKey(name) && types.equals(interfaces.get(ifName).get("methods").get(name)))
            interfaces.get(ifName).get("methods").remove(name);
        else
            errors.add(new SPLTypeError(node, ErrorMessage.NO_METHOD_DECL, deltas, product,
                    types.get(0) + " "
                            + name + "("
                            + StringUtils.join(",", types.subList(1, types.size())) + ")"));
    }


    /*
     * Helper method
     */
    public boolean existsClass(ModifyClassModifier node) {
        String className = node.getQualifiedName();
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
        final Map<String,Object> sortedClasses = new TreeMap<>(classes);
        final Map<String,Object> sortedInterfaces = new TreeMap<>(interfaces);

        for (String cls : sortedClasses.keySet()) {
            s.append("  Class: " + cls + "\n" + "    Fields:\n");
            for (String field : classes.get(cls).get("fields").keySet())
                s.append("      " + classes.get(cls).get("fields").get(field).get(0) + " " + field + "\n");
            s.append("    Methods:\n");
            for (String method : classes.get(cls).get("methods").keySet()) {
                java.util.List<String> l = classes.get(cls).get("methods").get(method);
                String argtypes = StringUtils.join(",", l.subList(1, l.size()));
                s.append("      " + classes.get(cls).get("methods").get(method).get(0) + " " + method + "(" + argtypes + ")\n");
            }
        }
        for (String iface : sortedInterfaces.keySet()) {
            s.append("  Interface: " + iface + "\n" + "    Methods:\n");
            for (String method : interfaces.get(iface).get("methods").keySet()) {
                java.util.List<String> l = interfaces.get(iface).get("methods").get(method);
                String argtypes = StringUtils.join(",", l.subList(1, l.size()));
                s.append("      " + interfaces.get(iface).get("methods").get(method).get(0) + " " + method + "(" + argtypes + ")\n");
            }
        }
        return s.toString();
    }

    /*
     * Get the return type & parameter types of a method as list of Strings
     * Format: (return type, parameter types...)
     *
     * We use the types as declared in the source, not as determined by type resolution, which might fail.
     * I.e. we simply use the Access's String representation (and not Access.getType()).
     */
    public static java.util.List<String> getMethodParameterTypes(MethodSig sig) {
        java.util.List<String> types = new ArrayList<>();
        types.add(sig.getReturnType().toString());
        for (ParamDecl par : sig.getParams()) {
            types.add(par.getAccess().toString());
        }
        return ImmutableList.copyOf(types);
    }

    public static String getVarType(TypedVarOrFieldDecl var) {
        return var.getAccess().toString();
    }

}
