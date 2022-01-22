package org.abs_models.frontend.variablechecker;

import org.abs_models.frontend.analyser.ErrorMessage;
import org.abs_models.frontend.analyser.SemanticConditionList;
import org.abs_models.frontend.analyser.SemanticError;
import org.abs_models.frontend.ast.*;

import java.util.HashMap;
import java.util.HashSet;

public class ClassFamilySignature extends BottomFamilySignature{

    final private HashMap<String, Access> fields;
    final private ClassDecl decl;
    final private HashSet<String> directImplements;

    public void addField(FieldDecl field, SemanticConditionList e){
        if(fields.containsKey(field.getName()) && !fields.get(field.getName()).equals(field.getAccessNoTransform())){
            e.add(new SemanticError(field, ErrorMessage.NOT_TYPE_UNIFORM, ""));
        }
        fields.put(field.getName(), field.getAccessNoTransform());
    }

    public ClassFamilySignature(ClassDecl decl, ModelFamilySignature modelSig){
        super(modelSig);
        this.decl = decl;
        this.fields = new HashMap<>();
        this.directImplements = new HashSet<>();

        if(decl != null) {
            for (MethodSig sig : decl.getAllMethodSigs()){
                methods.put(sig.getName(), sig);
            }
            for(InterfaceTypeUse use : decl.getImplementedInterfaceUseList()){
                directImplements.add(use.getName());
            }
        }
    }

    @Override
    public boolean extendsInterface(BottomFamilySignature above) {
        for( InterfaceTypeUse iUse : decl.getImplementedInterfaceUseList()){
            BottomFamilySignature next = modelSig.resolve(iUse.getName());
            if(next.extendsInterface(above)) return true;
        }
        return false;
    }

    @Override
    public String toString() {
        return "ClassFamilySignature "+decl.getName();
    }
    public HashMap<String, Access> getFields() {
        return fields;
    }
}
