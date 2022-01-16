package org.abs_models.frontend.variablechecker;

import org.abs_models.frontend.analyser.SemanticConditionList;
import org.abs_models.frontend.ast.*;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

public class ModuleFamilySignature {
    private HashMap<String, ClassFamilySignature> classes = new HashMap<>();
    private HashMap<String, InterfaceFamilySignature> interfaces = new HashMap<>();
    private ModelFamilySignature modelSig;

    //TODO: this should be handled in its own jrag in a proper OO way
    public ModuleFamilySignature(ModuleDecl moduleDecl, SemanticConditionList e, ModelFamilySignature modelSig){
        this.modelSig = modelSig;
        for(Decl decl : moduleDecl.getDeclsNoTransform()){
            if(decl instanceof InterfaceDecl){
                interfaces.put(decl.getName(), new InterfaceFamilySignature((InterfaceDecl) decl, modelSig, this));
            }
            if(decl instanceof ClassDecl){
                classes.put(decl.getName(), new ClassFamilySignature((ClassDecl) decl, modelSig));
            }
        }
        if(moduleDecl.hasProductLine()){
            LocalProductLine pl = moduleDecl.getProductLine();
            for(DeltaDecl delta : pl.getDeltaDecls()){
                for(ModuleModifier mod : delta.getModuleModifiersNoTransform()) {
                    if(mod instanceof AddInterfaceModifier)
                        handleAddInterface(e, (AddInterfaceModifier) mod);
                    if(mod instanceof  ModifyInterfaceModifier)
                        handleModifyInterface(e, (ModifyInterfaceModifier) mod);
                    if(mod instanceof AddClassModifier)
                        handleClassInterface(e, (AddClassModifier) mod);
                    if(mod instanceof  ModifyClassModifier)
                        handleModifyClass(e, (ModifyClassModifier) mod);
                }
            }
        }
    }

    private void handleModifyClass(SemanticConditionList e, ModifyClassModifier mod) {
        for(Modifier sigMod : mod.getModifiersNoTransform()){
            if(sigMod instanceof AddFieldModifier){
                ClassFamilySignature sig = classes.get(mod.getName());
                if(sig == null){
                    ClassFamilySignature novel = new ClassFamilySignature(null, modelSig);
                    novel.addField(((AddFieldModifier) sigMod).getFieldDecl(), e);
                    classes.put(mod.getName(), novel);
                } else {
                    sig.addField(((AddFieldModifier) sigMod).getFieldDecl(), e);
                }
            }
            if(sigMod instanceof AddMethodModifier){
                ClassFamilySignature sig =  classes.get(mod.getName());
                if(sig == null){
                    ClassFamilySignature novel = new ClassFamilySignature(null, modelSig);
                    novel.addMethod(((AddMethodModifier) sigMod).getMethodImpl().getMethodSig(), e);
                    classes.put(mod.getName(), novel);
                } else {
                    sig.addMethod(((AddMethodModifier) sigMod).getMethodImpl().getMethodSig(), e);
                }
            }
            if(sigMod instanceof ModifyMethodModifier){
                ClassFamilySignature sig = classes.get(mod.getName());
                if(sig == null){
                    ClassFamilySignature novel = new ClassFamilySignature(null, modelSig);
                    novel.addMethod(((ModifyMethodModifier) sigMod).getMethodImpl().getMethodSig(), e);
                    classes.put(mod.getName(), novel);
                } else {
                    sig.addMethod(((ModifyMethodModifier) sigMod).getMethodImpl().getMethodSig(), e);
                }
            }
        }
    }

    private void handleClassInterface(SemanticConditionList e, AddClassModifier mod) {
        ClassDecl cDecl = mod.getClassDecl();
        ClassFamilySignature sig = classes.get(cDecl.getName());
        if(sig == null){
            classes.put(cDecl.getName(), new ClassFamilySignature(cDecl, modelSig));
        } else {
            for(MethodSig mSig : cDecl.getAllMethodSigs())
                sig.addMethod(mSig, e);
            for(FieldDecl fDecl : cDecl.getFieldsNoTransform())
                sig.addField(fDecl, e);
        }
    }

    private void handleModifyInterface(SemanticConditionList e, ModifyInterfaceModifier iMod) {
        for(MethodSigModifier sigMod : iMod.getMethodSigModifierListNoTransform()){
            if(sigMod instanceof AddMethodSigModifier){
                InterfaceFamilySignature sig = interfaces.get(iMod.getName());
                if(sig == null){
                    InterfaceFamilySignature novel = new InterfaceFamilySignature(null, modelSig, this);
                    novel.addMethod(((AddMethodSigModifier) sigMod).getMethodSig(), e);
                    interfaces.put(iMod.getName(), novel);
                } else {
                    sig.addMethod(((AddMethodSigModifier) sigMod).getMethodSig(), e);
                }
            }
        }
    }

    private void handleAddInterface(SemanticConditionList e, AddInterfaceModifier iMod) {
        InterfaceDecl iDecl = iMod.getInterfaceDecl();
        InterfaceFamilySignature sig = interfaces.get(iDecl.getName());
        if(sig == null){
            interfaces.put(iDecl.getName(), new InterfaceFamilySignature(iDecl, modelSig, this));
        } else {
            for(MethodSig mSig : iDecl.getAllMethodSigs())
                sig.addMethod(mSig, e);
        }
    }

    public HashMap<InterfaceFamilySignature, HashSet<BottomFamilySignature>> getInheritance(SemanticConditionList e) {
        HashMap<InterfaceFamilySignature, HashSet<BottomFamilySignature>> map = new HashMap<>();
        for(InterfaceFamilySignature target : interfaces.values()){
                HashSet<BottomFamilySignature> direct = target.resolveInheritance(e);
                map.put(target, direct);
        }
        return map;
    }

    public BottomFamilySignature resolve(String name) {
        if(interfaces.containsKey(name) ) return interfaces.get(name);
        if(classes.containsKey(name) ) return classes.get(name);
        return null;
    }

    public BottomFamilySignature getBottomSignature(String mod){ //TODO: wth
        if(interfaces.containsKey(mod)) return interfaces.get(mod); else return classes.get(mod);
    }

    public ClassFamilySignature getClassSignature(String mod) {
        return classes.get(mod);
    }
}
