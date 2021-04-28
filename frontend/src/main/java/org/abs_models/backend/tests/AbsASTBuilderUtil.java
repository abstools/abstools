/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.tests;

import java.util.HashSet;
import java.util.Set;

import org.abs_models.frontend.ast.AssignStmt;
import org.abs_models.frontend.ast.AsyncCall;
import org.abs_models.frontend.ast.Block;
import org.abs_models.frontend.ast.Call;
import org.abs_models.frontend.ast.ClassDecl;
import org.abs_models.frontend.ast.DataTypeUse;
import org.abs_models.frontend.ast.Decl;
import org.abs_models.frontend.ast.DeltaDecl;
import org.abs_models.frontend.ast.Exp;
import org.abs_models.frontend.ast.ExpressionStmt;
import org.abs_models.frontend.ast.FieldDecl;
import org.abs_models.frontend.ast.FnApp;
import org.abs_models.frontend.ast.FromImport;
import org.abs_models.frontend.ast.Import;
import org.abs_models.frontend.ast.InterfaceDecl;
import org.abs_models.frontend.ast.InterfaceTypeUse;
import org.abs_models.frontend.ast.List;
import org.abs_models.frontend.ast.Local;
import org.abs_models.frontend.ast.MethodImpl;
import org.abs_models.frontend.ast.MethodSig;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.ast.ModifyClassModifier;
import org.abs_models.frontend.ast.ModuleDecl;
import org.abs_models.frontend.ast.ModuleModifier;
import org.abs_models.frontend.ast.Name;
import org.abs_models.frontend.ast.NewExp;
import org.abs_models.frontend.ast.Opt;
import org.abs_models.frontend.ast.ParamDecl;
import org.abs_models.frontend.ast.ParametricDataTypeUse;
import org.abs_models.frontend.ast.PureExp;
import org.abs_models.frontend.ast.SyncCall;
import org.abs_models.frontend.ast.TypeUse;
import org.abs_models.frontend.ast.VarDecl;
import org.abs_models.frontend.ast.VarDeclStmt;
import org.abs_models.frontend.ast.VarOrFieldUse;
import org.abs_models.frontend.ast.VarUse;

/**
 *
 * @author woner
 *
 */
public final class AbsASTBuilderUtil {

    public interface Predicate<T> {
        boolean predicate(T t);
    }

    protected abstract static class Named {

        protected final String name;

        protected Named(String name) {
            this.name = name;
        }
    }

    public static final class DeclNamePredicate<T extends Decl> extends Named implements Predicate<T> {

        public DeclNamePredicate(String name) {
            super(name);
        }

        @Override
        public boolean predicate(T t) {
            return name.equals(t.getName());
        }

    }

    public static final class ModifyClassModifierNamePredicate extends Named implements Predicate<ModifyClassModifier> {

        public ModifyClassModifierNamePredicate(String name) {
            super(name);
        }

        @Override
        public boolean predicate(ModifyClassModifier t) {
            return name.equals(t.getName());
        }

    }
    public static final class MethodNamePredicate extends Named implements Predicate<MethodImpl> {

        public MethodNamePredicate(String name) {
            super(name);
        }

        @Override
        public boolean predicate(MethodImpl t) {
            return name.equals(t.getMethodSig().getName());
        }

    }

    public static final class MethodSigNamePredicate extends Named implements Predicate<MethodSig> {

        public MethodSigNamePredicate(String name) {
            super(name);
        }

        @Override
        public boolean predicate(MethodSig t) {
            return name.equals(t.getName());
        }

    }

    public static final class FieldNamePredicate extends Named implements Predicate<FieldDecl> {

        public FieldNamePredicate(String name) {
            super(name);
        }

        @Override
        public boolean predicate(FieldDecl t) {
            return name.equals(t.getName());
        }

    }

    public static final <T extends Decl> Predicate<T> namePred(String name) {
        return new DeclNamePredicate<>(name);
    }

    public static final FieldDecl getFieldDecl(ClassDecl clazz, Predicate<FieldDecl> p) {
        for (FieldDecl f : clazz.getFields()) {
            if (p.predicate(f)) {
                return f;
            }
        }
        return null;
    }

    public static final <T extends Decl> T getDecl(Model model, Class<T> klass, Predicate<T> p) {
        return getDecl(model, null, klass, p);
    }

    public static final <T extends Decl> T getDecl(Model model, String moduleName, Class<T> klass, Predicate<T> p) {
        if (moduleName == null) {
            for (ModuleDecl module : model.getModuleDecls()) {
                T r = getDecl(module, klass, p);
                if (r != null)
                    return r;
            }
        } else {
            for (ModuleDecl module : model.getModuleDecls()) {
                if (moduleName.equals(module.getName())) {
                    T r = getDecl(module, klass, p);
                    if (r != null)
                        return r;
                }
            }
        }
        return null;
    }

    public static final <T extends Decl> T getDecl(ModuleDecl module, Class<T> klass, Predicate<T> p) {
        for (Decl d : module.getDeclList()) {
            if (klass.isInstance(d)) {
                T t = klass.cast(d);
                if (p.predicate(t)) {
                    return t;
                }
            }
        }
        return null;
    }

    public static final MethodImpl findMethodImpl(ClassDecl clazz, Predicate<MethodImpl> pred) {
        for (MethodImpl m : clazz.getMethodList()) {
                if (pred.predicate(m)) {
                        return m;
                }
        }
        return null;
    }

    public static final MethodSig findMethodSig(InterfaceDecl inf, Predicate<MethodSig> pred) {
        for (MethodSig m : inf.getAllMethodSigs()) {
                if (pred.predicate(m)) {
                        return m;
                }
        }
        return null;
    }

    public static final FnApp getFnApp(String fn, PureExp... exps) {
        List<PureExp> ps = new List<>();
        for (PureExp p : exps) {
            ps.add(p);
        }
        return new FnApp(fn, ps);
    }

    public static final AssignStmt getVAssign(VarOrFieldUse v, Exp exp) {
        AssignStmt s = new AssignStmt();
        s.setVar(v);
        s.setValue(exp);
        return s;
    }

    public static final AssignStmt getVAssign(String v, Exp exp) {
        return getVAssign(new VarUse(v), exp);
    }

    public static final ExpressionStmt getExpStmt(Exp exp) {
        ExpressionStmt ex = new ExpressionStmt();
        ex.setExp(exp);
        return ex;
    }

    public static final Call getCall(PureExp who, String method, boolean sync, PureExp... exps) {
        Call call;
        if (sync) {
            call = new SyncCall();
        } else {
            call = new AsyncCall();
        }
        call.setCallee(who);
        call.setMethod(method);
        for (int i=0; i<exps.length; i++) {
            call.setParam(exps[i], i);
        }
        return call;
    }

    public static final InterfaceDecl createInterface(String interfaceName) {
        return new InterfaceDecl(interfaceName,
            new List<>(),
            new List<>(),
            new List<>());
    }

    public static final MethodImpl createMethodImpl(MethodSig method) {
        MethodImpl methodImpl = new MethodImpl(method.copy(), new Block());
        return methodImpl;
    }


    public static final MethodSig createMethodSig(String methodName,
            TypeUse returnType,
            ParamDecl... decls) {

        List<ParamDecl> dl =
            new List<>();

        for (ParamDecl d : decls) {
                dl.add(d);
        }

        MethodSig method = new MethodSig(methodName, returnType, dl);

        return method;
    }

    public static final <T extends ModuleModifier> T findClassOrIfaceModifier(
            DeltaDecl delta, Class<T> klazz, Predicate<T> predicate) {

        List<ModuleModifier> modifiers =
                        delta.getModuleModifiers();

        for (int i=0; i<modifiers.getNumChild(); i++) {
                ModuleModifier modifier = modifiers.getChild(i);
                if (klazz.isInstance(modifier)) {
                        T obj = klazz.cast(modifier);
                        if (predicate.predicate(obj)) {
                                return obj;
                        }
                }
        }
        return null;
    }

    public static final PureExp getThis() {
        return new VarUse("this");
    }

    public static final DataTypeUse getFutUnitType() {
        return getType("Fut", getType("Unit"));
    }

    public static final DataTypeUse getType(String n, TypeUse... types) {
        if (types.length > 0) {
            ParametricDataTypeUse set = new ParametricDataTypeUse();
            set.setName(n);
            for (TypeUse d : types) {
                set.addParam(d);
            }
            return set;
        } else {
            DataTypeUse set = new DataTypeUse();
            set.setName(n);
            return set;
        }

    }

    public static final VarDeclStmt getVarDecl(String name, TypeUse a, Exp exp) {
        Opt<Exp> opt = new Opt<>();
        if (exp != null) {
            opt.setChild(exp, 0);
        }
        return new VarDeclStmt(new List<>(), new VarDecl(name, a, opt));
    }

    public static final FieldDecl makeFieldDecl(TypeUse access, String name) {
        FieldDecl fd = new FieldDecl();
        fd.setName(name);
        fd.setTypeUse(access);
        return fd;
    }

    public static final NewExp newObj(ClassDecl clazz, boolean local, PureExp... ps) {
        NewExp ne = new NewExp();
        ne.setClassName(clazz.getName());
        for (int i=0; i < ps.length; i++) {
            ne.setParam(ps[i], i);
        }
        if (local) {
            ne.setLocal(new Local());
        }
        return ne;
    }

    public static final VarDeclStmt newObj(InterfaceDecl inf, ClassDecl clazz, String name, boolean local) {
        return getVarDecl(name, new InterfaceTypeUse(inf.getName(), new List<>()), newObj(clazz, local));
    }

    /**
     * Generates import statement {@code import DeclName from DeclModule;}
     *
     * @param decl
     * @return the import statement
     */
    public static final Import generateImportAST(Decl decl) {
        return generateImportAST(decl.getName(), decl.getModuleDecl().getName());
    }

    public static final Set<Import> generateImportsAST(Set<TypeUse> typeUses) {
        Set<Import> imports = new HashSet<>();
        for (TypeUse type : typeUses) {
            if (type instanceof DataTypeUse) {
                imports.addAll(generateImportAST(type));
            } else {
                imports.add(generateImportAST(type.getName(), type.getModuleDecl().getName()));
            }
        }
        return imports;
    }

    public static final Set<Import> generateImportAST(TypeUse t) {
        Set<Import> imports = new HashSet<>();
        imports.add(generateImportAST(t.getName(), t.getModuleDecl().getName()));
        if (t instanceof ParametricDataTypeUse) {
            for (TypeUse st : ((ParametricDataTypeUse) t).getParams()) {
                imports.addAll(generateImportAST(st));
            }
        }
        return imports;
    }

    /**
     * Generates import statement {@code import name from module;}
     *
     * @param name
     * @param module
     * @return the import statement
     */
    public static final Import generateImportAST(String name, String module) {
        return new FromImport(new List<Name>().add(new Name(name)), module);
    }

    public static final DataTypeUse getUnit() {
        return new DataTypeUse("Unit", new List<>());
    }

}
