/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.prolog;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;

import abs.frontend.ast.ASTNode;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.FunctionDecl;
import abs.frontend.ast.InterfaceTypeUse;
import abs.frontend.ast.MethodImpl;
import abs.frontend.ast.MethodSig;
import abs.frontend.ast.ParametricFunctionDecl;
import abs.frontend.typechecker.InterfaceType;

public class ReachabilityInformation {
    private HashSet<String> reachableMethods;
    private HashSet<String> reachableInterfaces;
    private HashSet<ASTNode<?>> reachableFuncts;
    
    private HashSet<String> processedMethods;
    private HashSet<ASTNode<?>> processedFuncts;
    
    private boolean changed;
   
 public ReachabilityInformation(ArrayList<ASTNode<?>> entries){
     //Sets creation
     reachableMethods=new HashSet<String>();
     reachableInterfaces=new HashSet<String>();
     reachableFuncts=new HashSet<ASTNode<?>>();
     
     processedMethods=new HashSet<String>();
     processedFuncts=new HashSet<ASTNode<?>>();
     
     ClassDecl ownerClass;
     abs.frontend.ast.List<InterfaceTypeUse> interfaces;
     
     //Initialization of tables
     for(ASTNode<?> entry: entries){
         if(entry instanceof MethodImpl){
             ownerClass=ObtainOwnerClass((MethodImpl)entry);
             if(ownerClass!=null){
                 interfaces=ownerClass.getImplementedInterfaceUseList();
                 for(InterfaceTypeUse inter: interfaces){
                     reachableInterfaces.add(getInterfaceId(inter));
                     reachableMethods.add(getMethodId(inter,((MethodImpl)entry).getMethodSig()));
                 }
             }
         } else
             reachableFuncts.add(entry);
     }
     
     changed=true;
 }
private String getInterfaceId(InterfaceTypeUse inter){
    return inter.getType().getQualifiedName();
}
private String getMethodId(InterfaceTypeUse inter,MethodSig method){
    return getInterfaceId(inter)+method.toString();
}
private String getInterfaceId(InterfaceType inter){
    return inter.getQualifiedName();
}
private String getMethodId(InterfaceType inter,MethodSig method){
    return getInterfaceId(inter)+method.toString();
}

private ClassDecl ObtainOwnerClass(MethodImpl method){
    ASTNode<?> ancestor;
    boolean foundClass=false;
    
    ancestor=method;
    
    while(!foundClass){
        ancestor=ancestor.getParent();
        if(ancestor!=null){
            foundClass=ancestor instanceof ClassDecl;
        }else
            foundClass=true;
    }
    return (ClassDecl)ancestor;
}

public boolean changed(){
    if(changed){
        changed=false;
        return true;
    }else
        return false;
}

public boolean isReachable(FunctionDecl funct){
    return reachableFuncts.contains(funct);
}
public boolean isReachable(ParametricFunctionDecl funct){
    return reachableFuncts.contains(funct);
}
public boolean isReachable(ClassDecl clazz){
    boolean reachable=false;
    abs.frontend.ast.List<InterfaceTypeUse> interfaces=clazz.getImplementedInterfaceUseList();
    Iterator<InterfaceTypeUse> it=interfaces.iterator();
    
    while(!reachable && it.hasNext())
        reachable=reachableInterfaces.contains(getInterfaceId(it.next()));
    
    return reachable;

}
public boolean isReachable(MethodImpl method){
    ClassDecl clazz=ObtainOwnerClass(method);
    boolean reachable=false;
    
    if(clazz!=null){
        abs.frontend.ast.List<InterfaceTypeUse> interfaces=clazz.getImplementedInterfaceUseList();
        Iterator<InterfaceTypeUse> it=interfaces.iterator();
        while(!reachable && it.hasNext())
                reachable=reachableMethods.contains(getMethodId(it.next(),method.getMethodSig()));
        return reachable;        
    }else
        return false;
  }

public boolean setProcessed(FunctionDecl funct){
    return processedFuncts.add(funct);
}
public boolean setProcessed(ParametricFunctionDecl funct){
    return processedFuncts.add(funct);
}
public boolean setProcessed(MethodImpl method){
    ClassDecl clazz=ObtainOwnerClass(method);
    if(clazz!=null)
        return processedMethods.add(clazz.getName()+method.getMethodSig().toString());
    else
        return false;
}


public boolean addReachability(FunctionDecl funct){
    if(reachableFuncts.add(funct)){
        changed=true;
        return true;
    }
    return false;
}
public boolean addReachability(InterfaceType inter){
    if(reachableInterfaces.add(getInterfaceId(inter))){
        changed=true;
        return true;
    }
    return false;
}
public boolean addReachability(InterfaceType inter,MethodSig method){
    if(reachableMethods.add(getMethodId(inter,method))){
        changed=true;
        return true;
    }
    return false;
}

public String toString(){
    StringBuilder strBld=new StringBuilder();
    strBld.append("Reachable Methods:"+ reachableMethods.size()+"\n");
    strBld.append(reachableMethods.toString());
    strBld.append("Reachable Function:"+ reachableFuncts.size()+"\n");
    strBld.append(reachableFuncts.toString());
    return strBld.toString();
   }
}
