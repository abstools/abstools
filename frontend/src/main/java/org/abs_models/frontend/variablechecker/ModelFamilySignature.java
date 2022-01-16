package org.abs_models.frontend.variablechecker;

import org.abs_models.frontend.analyser.ErrorMessage;
import org.abs_models.frontend.analyser.SemanticConditionList;
import org.abs_models.frontend.analyser.SemanticError;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.ast.ModuleDecl;

import java.util.HashMap;
import java.util.HashSet;

public class ModelFamilySignature {
    private HashMap<String, ModuleFamilySignature> elements = new HashMap<>();

    public ModelFamilySignature(Model model, SemanticConditionList e){
        for(ModuleDecl decl : model.getModuleDecls())
            elements.put(decl.getName(), new ModuleFamilySignature(decl, e, this));
    }

    public void checkInheritance(SemanticConditionList e) {
        HashMap<InterfaceFamilySignature, HashSet<BottomFamilySignature>> inher = new HashMap<>();
        for(ModuleFamilySignature module : elements.values()){
            inher.putAll(module.getInheritance(e));
        }

        for(InterfaceFamilySignature start : inher.keySet() ){
            if(hasCycleLocal(start, new HashSet<>(), inher)) {
                e.add(new SemanticError(start.decl, ErrorMessage.CYCLIC_INHERITANCE, start.decl.getName()));
            }
        }
    }

    private boolean hasCycleLocal(BottomFamilySignature node, HashSet<BottomFamilySignature> seen, HashMap<InterfaceFamilySignature, HashSet<BottomFamilySignature>> inher) {
        if(seen.contains(node)) return true;
        seen.add(node);
        for(BottomFamilySignature nn : inher.get(node)){
            if(hasCycleLocal(nn, seen, inher)) return true;
        }
        return false;
    }

    public BottomFamilySignature resolve(String name) {
        for(ModuleFamilySignature moduleFamilySignature : elements.values()){
            BottomFamilySignature i = moduleFamilySignature.resolve(name);
            if(i != null) return i;
        }
        return null;
    }

    public ModuleFamilySignature getModuleSignature(String mod){
        return elements.get(mod);
    }
}
