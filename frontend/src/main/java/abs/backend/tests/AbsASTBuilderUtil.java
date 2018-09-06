/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.tests;

import java.util.HashSet;
import java.util.Set;

import abs.frontend.ast.Access;
import abs.frontend.ast.Annotation;
import abs.frontend.ast.AssignStmt;
import abs.frontend.ast.AsyncCall;
import abs.frontend.ast.Block;
import abs.frontend.ast.Call;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.DataTypeUse;
import abs.frontend.ast.Decl;
import abs.frontend.ast.DeltaDecl;
import abs.frontend.ast.Exp;
import abs.frontend.ast.ExpressionStmt;
import abs.frontend.ast.FieldDecl;
import abs.frontend.ast.FnApp;
import abs.frontend.ast.FromImport;
import abs.frontend.ast.Import;
import abs.frontend.ast.InterfaceDecl;
import abs.frontend.ast.InterfaceTypeUse;
import abs.frontend.ast.List;
import abs.frontend.ast.Local;
import abs.frontend.ast.MethodImpl;
import abs.frontend.ast.MethodSig;
import abs.frontend.ast.Model;
import abs.frontend.ast.ModifyClassModifier;
import abs.frontend.ast.ModuleDecl;
import abs.frontend.ast.ModuleModifier;
import abs.frontend.ast.Name;
import abs.frontend.ast.NewExp;
import abs.frontend.ast.Opt;
import abs.frontend.ast.ParamDecl;
import abs.frontend.ast.ParametricDataTypeUse;
import abs.frontend.ast.PureExp;
import abs.frontend.ast.SyncCall;
import abs.frontend.ast.TypeUse;
import abs.frontend.ast.VarDecl;
import abs.frontend.ast.VarDeclStmt;
import abs.frontend.ast.VarOrFieldUse;
import abs.frontend.ast.VarUse;

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
            new abs.frontend.ast.List<>(),
            new abs.frontend.ast.List<>(),
            new abs.frontend.ast.List<>());
    }

    public static final MethodImpl createMethodImpl(MethodSig method) {
        MethodImpl methodImpl = new MethodImpl(method.copy(), new Block(), false);
        return methodImpl;
    }


    public static final MethodSig createMethodSig(String methodName,
            Access returnType,
            ParamDecl... decls) {

        abs.frontend.ast.List<ParamDecl> dl =
            new abs.frontend.ast.List<>();

        for (ParamDecl d : decls) {
                dl.add(d);
        }

        MethodSig method = new MethodSig(methodName,
            new abs.frontend.ast.List<>(),
                        returnType, dl);

        return method;
    }

    public static final <T extends ModuleModifier> T findClassOrIfaceModifier(
            DeltaDecl delta, Class<T> klazz, Predicate<T> predicate) {

        abs.frontend.ast.List<ModuleModifier> modifiers =
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

    public static final VarDeclStmt getVarDecl(String name, Access a, Exp exp) {
        Opt<Exp> opt = new Opt<>();
        if (exp != null) {
            opt.setChild(exp, 0);
        }
        return new VarDeclStmt(new List<>(), new VarDecl(name, a, opt));
    }

    public static final FieldDecl makeFieldDecl(Access access, String name) {
        FieldDecl fd = new FieldDecl();
        fd.setName(name);
        fd.setAccess(access);
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
     * @return
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
     * @return
     */
    public static final Import generateImportAST(String name, String module) {
        return new FromImport(new List<Name>().add(new Name(name)), module);
    }

    public static final DataTypeUse getUnit() {
        return new DataTypeUse("Unit", new abs.frontend.ast.List<>());
    }

}
