/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.prolog;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;

import abs.frontend.ast.ASTNode;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.FunctionDecl;
import abs.frontend.ast.InitBlock;
import abs.frontend.ast.InterfaceDecl;
import abs.frontend.ast.InterfaceTypeUse;
import abs.frontend.ast.MainBlock;
import abs.frontend.ast.MethodImpl;
import abs.frontend.ast.MethodSig;
import abs.frontend.ast.NewExp;
import abs.frontend.ast.ParametricFunctionDecl;
import abs.frontend.typechecker.InterfaceType;

public class ReachabilityInformation {
    private HashSet<String> reachableMethods;
    private HashSet<String> reachableInterfacesOrClasses;
    private Hashtable<ASTNode<?>, Boolean> reachableFuncts;
    private Hashtable<MainBlock, Boolean> reachableMainBlocks;

    private HashSet<String> processedMethods;

    private boolean changed;

    public ReachabilityInformation(ArrayList<ASTNode<?>> entries) {
        // Sets creation
        reachableMethods = new HashSet<String>();
        reachableInterfacesOrClasses = new HashSet<String>();
        reachableFuncts = new Hashtable<ASTNode<?>, Boolean>();
        reachableMainBlocks = new Hashtable<MainBlock, Boolean>();

        processedMethods = new HashSet<String>();

        ClassDecl ownerClass;
        abs.frontend.ast.List<InterfaceTypeUse> interfaces;

        // Initialization of tables
        for (ASTNode<?> entry : entries) {
            if (entry instanceof MethodImpl) {
                ownerClass = ObtainOwnerClass((MethodImpl) entry);
                if (ownerClass != null) {
                    reachableInterfacesOrClasses.add(getClassId(ownerClass));
                    reachableMethods.add(getMethodId(ownerClass, ((MethodImpl) entry).getMethodSig()));

                    interfaces = ownerClass.getImplementedInterfaceUseList();
                    for (InterfaceTypeUse inter : interfaces) {
                        reachableInterfacesOrClasses.add(getInterfaceId(inter));
                        reachableMethods.add(getMethodId(inter, ((MethodImpl) entry).getMethodSig()));
                    }
                }
            } else if (entry instanceof MainBlock) {
                reachableMainBlocks.put((MainBlock) entry, Boolean.FALSE);
            } else
                reachableFuncts.put(entry, Boolean.FALSE);
        }

        changed = true;
    }

    private String getInterfaceId(InterfaceTypeUse inter) {
        return inter.getType().getQualifiedName();
    }

    private String getMethodId(InterfaceTypeUse inter, MethodSig method) {
        return getInterfaceId(inter) + method.getName();
    }

    private String getInterfaceId(InterfaceType inter) {
        return inter.getQualifiedName();
    }

    private String getMethodId(InterfaceType inter, MethodSig method) {
        return getInterfaceId(inter) + method.getName();
    }

    private String getClassId(ClassDecl clazz) {
        return clazz.qualifiedName();
    }

    private String getMethodId(ClassDecl clazz, MethodSig method) {
        return getClassId(clazz) + method.getName();
    }

    private ClassDecl ObtainOwnerClass(ASTNode<?> node) {
        ASTNode<?> ancestor;
        boolean foundClass = false;

        ancestor = node;

        while (!foundClass) {
            ancestor = ancestor.getParent();
            if (ancestor != null) {
                foundClass = ancestor instanceof ClassDecl;
            } else
                foundClass = true;
        }
        return (ClassDecl) ancestor;
    }

    public boolean changed() {
        if (changed) {
            changed = false;
            return true;
        } else
            return false;
    }

    public boolean isReachable(FunctionDecl funct) {
        return reachableFuncts.containsKey(funct);
    }

    public boolean isReachable(ParametricFunctionDecl funct) {
        return reachableFuncts.containsKey(funct);
    }

    public boolean isReachable(MainBlock mBlock) {
        return reachableMainBlocks.containsKey(mBlock);
    }

    public boolean isReachable(ClassDecl clazz) {
        boolean reachable = false;
        abs.frontend.ast.List<InterfaceTypeUse> interfaces = clazz.getImplementedInterfaceUseList();
        Iterator<InterfaceTypeUse> it = interfaces.iterator();

        reachable = reachableInterfacesOrClasses.contains(getClassId(clazz));
        while (!reachable && it.hasNext()) {
            reachable = isReachable(it.next());
        }
        return reachable;

    }

    private boolean isReachable(InterfaceTypeUse inter) {
        boolean reachable = false;
        reachable = reachableInterfacesOrClasses.contains(getInterfaceId(inter));
        if (!reachable) {

            abs.frontend.ast.List<InterfaceTypeUse> interfaces = ((InterfaceDecl) inter.getDecl())
                    .getExtendedInterfaceUseList();
            Iterator<InterfaceTypeUse> it = interfaces.iterator();
            while (!reachable && it.hasNext()) {
                reachable = isReachable(it.next());
            }
        }
        return reachable;
    }

    public boolean isReachable(MethodImpl method) {
        ClassDecl clazz = ObtainOwnerClass(method);
        boolean reachable = false;

        if (clazz != null) {
            abs.frontend.ast.List<InterfaceTypeUse> interfaces = clazz.getImplementedInterfaceUseList();
            Iterator<InterfaceTypeUse> it = interfaces.iterator();
            reachable = reachableMethods.contains(getMethodId(clazz, method.getMethodSig()));
            while (!reachable && it.hasNext())
                reachable = isReachable(it.next(), method.getMethodSig());
            return reachable;
        } else
            return false;
    }

    private boolean isReachable(InterfaceTypeUse inter, MethodSig method) {
        boolean reachable = false;
        reachable = reachableMethods.contains(getMethodId(inter, method));
        if (!reachable) {
            abs.frontend.ast.List<InterfaceTypeUse> interfaces = ((InterfaceDecl) inter.getDecl())
                    .getExtendedInterfaceUseList();
            Iterator<InterfaceTypeUse> it = interfaces.iterator();
            while (!reachable && it.hasNext()) {
                reachable = isReachable(it.next(), method);
            }
        }
        return reachable;
    }

    public boolean isReachable(InitBlock block) {
        ClassDecl clazz = ObtainOwnerClass(block);
        if (clazz != null)
            return reachableMethods.contains(getClassId(clazz) + "init");
        else
            return false;
    }

    // True is returned if it was not processed before
    // that is, if the boolean value was false
    public boolean setProcessed(FunctionDecl funct) {
        return !reachableFuncts.put(funct, Boolean.TRUE);
    }

    public boolean setProcessed(ParametricFunctionDecl funct) {
        return !reachableFuncts.put(funct, Boolean.TRUE);
    }

    public boolean setProcessed(MainBlock mBlock) {
        return !reachableMainBlocks.put(mBlock, Boolean.TRUE);
    }

    public boolean setProcessed(MethodImpl method) {
        ClassDecl clazz = ObtainOwnerClass(method);
        if (clazz != null)
            return processedMethods.add(clazz.qualifiedName() + method.getMethodSig().toString());
        else
            return false;
    }

    public boolean setProcessed(InitBlock block) {
        ClassDecl clazz = ObtainOwnerClass(block);
        if (clazz != null)
            return processedMethods.add(clazz.qualifiedName() + "init");
        else
            return false;
    }

    public boolean addReachability(FunctionDecl funct) {
        if (!reachableFuncts.containsKey(funct)) {
            reachableFuncts.put(funct, Boolean.FALSE);
            changed = true;
            return true;
        }
        return false;
    }

    public boolean addReachability(ParametricFunctionDecl funct) {
        if (!reachableFuncts.containsKey(funct)) {
            reachableFuncts.put(funct, Boolean.FALSE);
            changed = true;
            return true;
        }
        return false;
    }

    public boolean addReachability(InterfaceType inter) {
        if (reachableInterfacesOrClasses.add(getInterfaceId(inter))) {
            changed = true;
            return true;
        }
        return false;
    }

    public boolean addReachability(ClassDecl clazz) {
        if (reachableInterfacesOrClasses.add(getClassId(clazz))) {
            changed = true;
            return true;
        }
        return false;
    }

    public boolean addReachability(InterfaceType inter, MethodSig method) {
        if (reachableMethods.add(getMethodId(inter, method))) {
            changed = true;
            return true;
        }
        return false;
    }

    public boolean addReachability(ClassDecl clazz, MethodSig method) {
        if (reachableMethods.add(getMethodId(clazz, method))) {
            changed = true;
            return true;
        }
        return false;

    }

    public boolean addReachability(ClassDecl clazz, NewExp newExp) {
        if (reachableMethods.add(getClassId(clazz) + "init")) {
            changed = true;
            return true;
        }
        return false;

    }

    public String toString() {
        StringBuilder strBld = new StringBuilder();
        strBld.append("Reachable Methods:" + reachableMethods.size() + "\n");
        strBld.append(reachableMethods.toString() + "\n");
        strBld.append("Reachable Function:" + reachableFuncts.size() + "\n");
        strBld.append(reachableFuncts.toString());
        return strBld.toString();
    }

}
