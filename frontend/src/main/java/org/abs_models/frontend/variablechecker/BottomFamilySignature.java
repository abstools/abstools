package org.abs_models.frontend.variablechecker;

import org.abs_models.frontend.analyser.ErrorMessage;
import org.abs_models.frontend.analyser.SemanticConditionList;
import org.abs_models.frontend.analyser.SemanticError;
import org.abs_models.frontend.ast.InterfaceTypeUse;
import org.abs_models.frontend.ast.MethodSig;

import java.util.Collection;
import java.util.HashMap;

public abstract class BottomFamilySignature {
    final protected HashMap<String, MethodSig> methods = new HashMap<>();
    protected ModelFamilySignature modelSig;

    protected BottomFamilySignature(ModelFamilySignature modelSig) {
        this.modelSig =modelSig;
    }


    protected void addMethod(MethodSig sig, SemanticConditionList e){
        if(methods.containsKey(sig.getName())){
            MethodSig oldSig = methods.get(sig.getName());
            if(!oldSig.getReturnTypeNoTransform().equals(sig.getReturnTypeNoTransform()))
                e.add(new SemanticError(sig, ErrorMessage.NOT_TYPE_UNIFORM, ""));
            if(oldSig.getNumParamNoTransform() != sig.getNumParamNoTransform())
            e.add(new SemanticError(sig, ErrorMessage.NOT_TYPE_UNIFORM, ""));
            for(int i = 0; i < oldSig.getNumParamNoTransform(); i++){
                if(!oldSig.getParamNoTransform(i).getAccessNoTransform().equals(sig.getParamNoTransform(i).getAccessNoTransform()))
                    e.add(new SemanticError(sig, ErrorMessage.NOT_TYPE_UNIFORM, ""));
                if(!oldSig.getParamNoTransform(i).getName().equals(sig.getParamNoTransform(i).getName()))
                    e.add(new SemanticError(sig, ErrorMessage.NOT_TYPE_UNIFORM, ""));
            }
        }
        methods.put(sig.getName(), sig);
    }

    public Collection<MethodSig> methods() {
        return methods.values();
    }

    abstract public boolean extendsInterface(BottomFamilySignature above);
}
