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

/**
 * Auxiliary class for obtaining the reachable methods and functions of a given
 * model from a given set of initial entries.
 * It can record the processed elements and the reachable elements. It can
 * also be queried about the reachability of an element.
 *
 * @author aeflores
 *
 */
public class ReachabilityInformation {
    /**
     * Records the method names that are reachable. The names can be:
     * -Interface name+ method name
     * -Class name+ method name
     */
    private HashSet<String> reachableMethods;
    /**
     * Records the class or interface names that are reachable
     */
    private HashSet<String> reachableInterfacesOrClasses;
    /**
     * Records the complete function nodes that are reachable and whether
     * they have already been processed or not.
     */
    private Hashtable<ASTNode<?>, Boolean> reachableFuncts;
    /**
     * Records the MainBlocks that are reachable.
     * (these will only be added by the constructor)
     */
    private Hashtable<MainBlock, Boolean> reachableMainBlocks;
    /**
     * Records the method names that have been processed. The names can be:
     * -Interface name+ method name
     * -Class name+ method name
     */
    private HashSet<String> processedMethods;
    /**
     * Flag that indicates if there has been any changes in the reachable sets.
     */
    private boolean changed;

    /**
     * Initializes the databases with an initial set of elements.
     * @param entries Initial set of reachable entries.
     */
    public ReachabilityInformation(ArrayList<ASTNode<?>> entries) {
        // Sets creation
        reachableMethods = new HashSet<>();
        reachableInterfacesOrClasses = new HashSet<>();
        reachableFuncts = new Hashtable<>();
        reachableMainBlocks = new Hashtable<>();

        processedMethods = new HashSet<>();

        ClassDecl ownerClass;
        abs.frontend.ast.List<InterfaceTypeUse> interfaces;

        // Initialization of tables
        for (ASTNode<?> entry : entries) {
            if (entry instanceof MethodImpl) {
                ownerClass = obtainOwnerClass((MethodImpl) entry);
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
/*
 * Methods that create standard name from the different combinations of elements
 */
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
        return clazz.getQualifiedName();
    }

    private String getMethodId(ClassDecl clazz, MethodSig method) {
        return getClassId(clazz) + method.getName();
    }
/**
 * finds the class where the given class belongs to
 * @param node
 * @return the class that owns the parameter node or null if node does not belong to any class
 */
    private ClassDecl obtainOwnerClass(ASTNode<?> node) {
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
        // FIXME: these two should be equivalent, so better use
        assert ancestor == node.calcContextNode(ClassDecl.class);
        return (ClassDecl) ancestor;
    }
/**
 * returns true if there has been any change since the last time it was executed
 * @return
 */
    public boolean changed() {
        if (changed) {
            changed = false;
            return true;
        } else
            return false;
    }
/**
 * checks if the parameter is reachable
 * @param funct
 * @return true if funct is reachable, false otherwise
 */
    public boolean isReachable(FunctionDecl funct) {
        return reachableFuncts.containsKey(funct);
    }
    /**
     * checks if the parameter is reachable
     * @param funct
     * @return true if funct is reachable, false otherwise
     */
    public boolean isReachable(ParametricFunctionDecl funct) {
        return reachableFuncts.containsKey(funct);
    }
    /**
     * checks if the parameter is reachable
     * @param mBlock
     * @return true if mBlock is reachable, false otherwise
     */
    public boolean isReachable(MainBlock mBlock) {
        return reachableMainBlocks.containsKey(mBlock);
    }
/**
 * checks if the class is reachable. It looks at the class name
 * and all the interface names that are directly or indirectly
 * implemented by clazz
 * @param clazz
 * @return true if clazz is reachable, false otherwise
 */
    public boolean isReachable(ClassDecl clazz) {
        boolean reachable = false;
        abs.frontend.ast.List<InterfaceTypeUse> interfaces = clazz.getImplementedInterfaceUseList();
        Iterator<InterfaceTypeUse> it = interfaces.iterator();
        //checks if any interface implemented by clazz is reachable
        reachable = reachableInterfacesOrClasses.contains(getClassId(clazz));
        while (!reachable && it.hasNext()) {
            reachable = isReachable(it.next());
        }
        return reachable;

    }

    private boolean isReachable(InterfaceTypeUse inter) {
        boolean reachable = false;
        //checks if inter is reachable
        reachable = reachableInterfacesOrClasses.contains(getInterfaceId(inter));
        if (!reachable) {
            //checks if any interface implemented by inter is reachable
            abs.frontend.ast.List<InterfaceTypeUse> interfaces = ((InterfaceDecl) inter.getDecl())
                    .getExtendedInterfaceUseList();
            Iterator<InterfaceTypeUse> it = interfaces.iterator();
            while (!reachable && it.hasNext()) {
                reachable = isReachable(it.next());
            }
        }
        return reachable;
    }
    /**
     * checks if the method is reachable. It looks at the class name
     * and all the interface names that are directly or indirectly
     * implemented by the method's class
     * @param method
     * @return true if method is reachable, false otherwise
     */
    public boolean isReachable(MethodImpl method) {
        ClassDecl clazz = obtainOwnerClass(method);
        boolean reachable = false;

        if (clazz != null) {
            abs.frontend.ast.List<InterfaceTypeUse> interfaces = clazz.getImplementedInterfaceUseList();
            Iterator<InterfaceTypeUse> it = interfaces.iterator();
            //checks if the method is reachable with its class name
            reachable = reachableMethods.contains(getMethodId(clazz, method.getMethodSig()));
          //checks if the method is reachable with any interface name that is
          //implemented by its class
            while (!reachable && it.hasNext())
                reachable = isReachable(it.next(), method.getMethodSig());
            return reachable;
        } else
            return false;
    }

    private boolean isReachable(InterfaceTypeUse inter, MethodSig method) {
        boolean reachable = false;
        //checks if the method is reachable with inter
        reachable = reachableMethods.contains(getMethodId(inter, method));
        if (!reachable) {
            //checks if the method is reachable with any interface that is
            //implemented by inter
            abs.frontend.ast.List<InterfaceTypeUse> interfaces = ((InterfaceDecl) inter.getDecl())
                    .getExtendedInterfaceUseList();
            Iterator<InterfaceTypeUse> it = interfaces.iterator();
            while (!reachable && it.hasNext()) {
                reachable = isReachable(it.next(), method);
            }
        }
        return reachable;
    }
    /**
     * checks if the constructor is reachable, it is treated as a special
     * method called init.
     * @param block
     * @return true if block is reachable, false otherwise
     */
    public boolean isReachable(InitBlock block) {
        ClassDecl clazz = obtainOwnerClass(block);
        if (clazz != null)
            return reachableMethods.contains(getClassId(clazz) + "init");
        else
            return false;
    }


    /**
     * Sets the parameter to processed
     * @param funct
     * @return true if the parameter had not been processed before.
     */
    public boolean setProcessed(FunctionDecl funct) {
        return !reachableFuncts.put(funct, Boolean.TRUE);
    }
    /**
     * Sets the parameter to processed
     * @param funct
     * @return true if the parameter had not been processed before.
     */
    public boolean setProcessed(ParametricFunctionDecl funct) {
        return !reachableFuncts.put(funct, Boolean.TRUE);
    }
    /**
     * Sets the parameter to processed
     * @param mBlock
     * @return true if the parameter had not been processed before.
     */
    public boolean setProcessed(MainBlock mBlock) {
        return !reachableMainBlocks.put(mBlock, Boolean.TRUE);
    }
    /**
     * Sets the parameter to processed
     * @param method
     * @return true if the parameter had not been processed before.
     */
    public boolean setProcessed(MethodImpl method) {
        ClassDecl clazz = obtainOwnerClass(method);
        if (clazz != null)
            return processedMethods.add(clazz.getQualifiedName() + method.getMethodSig().toString());
        else
            return false;
    }
    /**
     * Sets the parameter to processed
     * @param block
     * @return true if the parameter had not been processed before.
     */
    public boolean setProcessed(InitBlock block) {
        ClassDecl clazz = obtainOwnerClass(block);
        if (clazz != null)
            return processedMethods.add(clazz.getQualifiedName() + "init");
        else
            return false;
    }
/**
 *Sets the parameter to reachable and not processed if it was not reachable before
 * @param funct
 * @return true if the parameter was not reachable before
 */
    public boolean addReachability(FunctionDecl funct) {
        if (!reachableFuncts.containsKey(funct)) {
            reachableFuncts.put(funct, Boolean.FALSE);
            changed = true;
            return true;
        }
        return false;
    }
    /**
     *Sets the parameter to reachable and not processed if it was not reachable before
     * @param funct
     * @return true if the parameter was not reachable before
     */
    public boolean addReachability(ParametricFunctionDecl funct) {
        if (!reachableFuncts.containsKey(funct)) {
            reachableFuncts.put(funct, Boolean.FALSE);
            changed = true;
            return true;
        }
        return false;
    }
    /**
     *Sets the parameter to reachable and not processed if it was not reachable before
     * @param inter
     * @return true if the parameter was not reachable before
     */
    public boolean addReachability(InterfaceType inter) {
        if (reachableInterfacesOrClasses.add(getInterfaceId(inter))) {
            changed = true;
            return true;
        }
        return false;
    }
    /**
     *Sets the parameter to reachable and not processed if it was not reachable before
     * @param clazz
     * @return true if the parameter was not reachable before
     */
    public boolean addReachability(ClassDecl clazz) {
        if (reachableInterfacesOrClasses.add(getClassId(clazz))) {
            changed = true;
            return true;
        }
        return false;
    }
    /**
     *Sets the method to reachable using the interface inter.
     * @param inter
     * @param method
     * @return true if the parameter was not reachable before
     */
    public boolean addReachability(InterfaceType inter, MethodSig method) {
        if (reachableMethods.add(getMethodId(inter, method))) {
            changed = true;
            return true;
        }
        return false;
    }
    /**
     *Sets the method to reachable using the class clazz.
     * @param clazz
     * @param method
     * @return true if the parameter was not reachable before
     */
    public boolean addReachability(ClassDecl clazz, MethodSig method) {
        if (reachableMethods.add(getMethodId(clazz, method))) {
            changed = true;
            return true;
        }
        return false;

    }
    /**
     *Sets the corresponding constructor to reachable.
     * @param clazz
     * @param newExp only uses to distinguish the method form others
     * @return true if the parameter was not reachable before
     */
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
