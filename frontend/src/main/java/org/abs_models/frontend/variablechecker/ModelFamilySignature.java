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
        if(name.contains(".")) { //dots are allowed in module names, so we try again by splitting at the last dot
            int pos = name.lastIndexOf(".");
            String modName = name.substring(0, pos);
            if(elements.containsKey(modName)) {
                return elements.get(modName).resolve(name.substring(pos+1, name.length() ));
            } else return null;
        }
        return null;
    }

    public ModuleFamilySignature getModuleSignature(String mod){
        return elements.get(mod);
    }
}
