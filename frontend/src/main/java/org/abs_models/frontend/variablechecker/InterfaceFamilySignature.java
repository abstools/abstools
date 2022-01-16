package org.abs_models.frontend.variablechecker;

import org.abs_models.frontend.analyser.ErrorMessage;
import org.abs_models.frontend.analyser.SemanticConditionList;
import org.abs_models.frontend.analyser.SemanticError;
import org.abs_models.frontend.ast.InterfaceDecl;
import org.abs_models.frontend.ast.InterfaceTypeUse;
import org.abs_models.frontend.ast.MethodSig;
import org.abs_models.frontend.typechecker.Type;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Objects;

public class InterfaceFamilySignature extends BottomFamilySignature{
    final public InterfaceDecl decl;
    final private ModuleFamilySignature moduleSig;

    public InterfaceFamilySignature(InterfaceDecl decl, ModelFamilySignature modelSig, ModuleFamilySignature moduleSig){
        super(modelSig);
        this.moduleSig = moduleSig;
        this.decl = decl;
        if(decl != null) {
            for (MethodSig sig : decl.getAllMethodSigs()){
                methods.put(sig.getName(), sig);
            }
        }
    }

    public HashSet<BottomFamilySignature> resolveInheritance(SemanticConditionList e) {
        HashSet<BottomFamilySignature> set = new HashSet<>();
        for(InterfaceTypeUse use : decl.getExtendedInterfaceUses()){
            if(Objects.equals(use.getName(), "ABS.StdLib.Object")) continue;
            if(Objects.equals(use.getName(), "DeploymentComponent")) continue;
            if(Objects.equals(use.getName(), "CloudProvider")) continue;
            BottomFamilySignature i = moduleSig.resolve(use.getName());
            if(i == null){
                i = modelSig.resolve(use.getName()); //TODO: this ignores export/import for now
                if(i == null){
                    e.add(new SemanticError(decl,ErrorMessage.NAME_NOT_RESOLVABLE,use.getName()));
                } else {
                    set.add(i);
                }
            } else {
                set.add(i);
            }
        }
        return set;
    }

    @Override
    public boolean extendsInterface(BottomFamilySignature above) {
        if(this == above) return true;
        for( InterfaceTypeUse iUse : decl.getExtendedInterfaceUses()){
            if(iUse.getName().equals("ABS.StdLib.Object")) continue;
            BottomFamilySignature next = modelSig.resolve(iUse.getName());
            if(next == null)
                return false;
            if(next.extendsInterface(above)) return true;
        }
        return false;
    }

    @Override
    public String toString() {
        return "InterfaceFamilySignature "+decl.getName();
    }

    public Type toType(){
        return decl.getType();
    }
}
