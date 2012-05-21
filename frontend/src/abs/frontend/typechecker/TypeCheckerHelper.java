/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.typechecker;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import beaver.Symbol;

import abs.common.Constants;
import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.SemanticError;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.analyser.TypeError;
import abs.frontend.ast.*;
import abs.frontend.typechecker.KindedName.Kind;

public class TypeCheckerHelper {
    public static void typeCheck(ConstructorPattern p, SemanticErrorList e, Type t) {
        if (!t.isDataType())
            return;

        Decl decl = p.lookup(new KindedName(Kind.DATA_CONSTRUCTOR, p.getConstructor()));
        if (decl == null || !(decl instanceof DataConstructor)) {
            e.add(new SemanticError(p, ErrorMessage.CONSTRUCTOR_NOT_RESOLVABLE, p.getConstructor()));
            return;
        }
        
        DataConstructor c = (DataConstructor) decl;
        if (c.getNumConstructorArg() != p.getNumParam()) {
            e.add(new TypeError(p, ErrorMessage.WRONG_NUMBER_OF_ARGS, c.getNumConstructorArg(), p.getNumParam()));
            return;
        }
        
        DataTypeType dtt = (DataTypeType) t;
        if (!dtt.getDecl().equals(c.getDataTypeDecl())) {
            e.add(new TypeError(p, ErrorMessage.WRONG_CONSTRUCTOR, t.toString(), p.getConstructor()));
        }
            

        Type myType = p.getType();

        if (myType == null || !(myType instanceof DataTypeType))
            return;

        if (!(t instanceof DataTypeType)) {
            e.add(new TypeError(p, ErrorMessage.TYPE_MISMATCH, myType, t));
            return;
        }

        DataTypeType myDType = (DataTypeType) myType;
        DataTypeType otherType = (DataTypeType) t;
        if (!myDType.getDecl().equals(otherType.getDecl())) {
            e.add(new TypeError(p, ErrorMessage.TYPE_MISMATCH, myDType, t));
            return;
        }

        TypeCheckerHelper.typeCheckMatchingParamsPattern(e, p, c, p.getParams());
    }    
    
    public static void assertHasType(SemanticErrorList l, Exp e, Type t) {
        if (!e.getType().isAssignable(t)) {
            l.add(new TypeError(e, ErrorMessage.EXPECTED_TYPE, t, e.getType()));
        }
    }

    public static void checkAssignment(SemanticErrorList l, ASTNode<?> n, Type lht, Exp rhte) {
        Type te = rhte.getType();
        if (!te.isAssignable(lht)) {
            l.add(new TypeError(n, ErrorMessage.CANNOT_ASSIGN, te, lht));
        }

    }

    public static void typeCheckParamList(SemanticErrorList l, List<ParamDecl> params) {
        HashSet<String> names = new HashSet<String>();
        for (ParamDecl d : params) {
            if (names.contains(d.getName())) {
                l.add(new TypeError(d, ErrorMessage.DUPLICATE_PARAM_NAME, d.getName()));
            } else {
                names.add(d.getName());
            }
            d.typeCheck(l);
        }
    }

    public static void typeCheckEqualParams(SemanticErrorList l, ASTNode<?> n, List<ParamDecl> params, List<PureExp> args) {
        typeCheckEqual(l, n, getTypes(params), args);
    }

    public static void typeCheckMatchingParams(SemanticErrorList l, ASTNode<?> n, List<? extends ConstructorArg> params, List<PureExp> args) {        
        final java.util.List<Type> paramTypes = getTypesFromConstructorArgs(params);
        final Map<TypeParameter, Type> binding = getTypeParamBinding(n, paramTypes, args);
        typeCheckEqual(l, n, applyBinding(binding, paramTypes), args);
    }

    public static void typeCheckMatchingParamsPattern(SemanticErrorList l, ASTNode<?> n, DataConstructor decl,
            List<Pattern> args) {
        java.util.List<Type> patternTypes = getTypesFromPattern(args);
        Map<TypeParameter, Type> binding = getTypeParamBinding(n, getTypesFromConstructorArgs(decl.getConstructorArgs()),
                patternTypes);
        java.util.List<Type> types = applyBinding(binding, getTypesFromConstructorArgs(decl.getConstructorArgs()));
        typeCheckEqualPattern(l, n, types, args);
    }

    public static java.util.List<Type> getTypesFromConstructorArgs(List<? extends ConstructorArg> constructorArgs) {
        java.util.List<Type> res = new ArrayList<Type>();
        for (ConstructorArg arg : constructorArgs) {
            res.add(arg.getType());
        }
        return res;
    }

    public static void typeCheckMatchingParams(SemanticErrorList l, ASTNode<?> n, ParametricFunctionDecl decl,
            List<PureExp> args) {
        Map<TypeParameter, Type> binding = getTypeParamBindingFromParamDecl(n, decl.getParams(), args);
        java.util.List<Type> types = applyBinding(binding, getTypes(decl.getParams()));
        typeCheckEqual(l, n, types, args);
    }

    public static java.util.List<Type> applyBinding(Map<TypeParameter, Type> binding, java.util.List<Type> types) {
        ArrayList<Type> res = new ArrayList<Type>(types.size());
        for (Type t : types) {
            res.add(applyBinding(binding, t));
        }
        return res;
    }

    public static Type applyBinding(Map<TypeParameter, Type> binding, Type t) {
        if (t.isTypeParameter()) {
            Type res = binding.get((TypeParameter) t);
            if (res == null)
                return new BoundedType();
            else
                return res;
        } else if (t.isDataType()) {
            DataTypeType dt = (DataTypeType) t;
            if (dt.hasTypeArgs()) {
                java.util.List<Type> argTypes = applyBinding(binding, dt.getTypeArgs());
                return new DataTypeType(dt.getDecl(), argTypes);
            }
        }

        return t;
    }

    public static void typeCheckEqualDataTypeUses(SemanticErrorList l, ASTNode<?> n, List<? extends DataTypeUse> params,
            List<PureExp> args) {
        java.util.List<Type> types = getTypesFromDataTypeUse(params);
        typeCheckEqual(l, n, types, args);
    }

    public static void typeCheckEqual(SemanticErrorList l, ASTNode<?> n, java.util.List<Type> params, List<PureExp> args) {
        if (params.size() != args.getNumChild()) {
            l.add(new TypeError(n, ErrorMessage.WRONG_NUMBER_OF_ARGS, params.size(), args.getNumChild()));
        } else {
            for (int i = 0; i < params.size(); i++) {
                Type argType = params.get(i);
                PureExp exp = args.getChild(i);
                exp.typeCheck(l);
                Type expType = exp.getType();
                if (!expType.isAssignable(argType)) {
                    l.add(new TypeError(n, ErrorMessage.TYPE_MISMATCH, exp.getType(), argType));
                }
            }
        }
    }

    public static void typeCheckEqualPattern(SemanticErrorList l, ASTNode<?> n, java.util.List<Type> params,
            List<Pattern> args) {
        if (params.size() != args.getNumChild()) {
            l.add(new TypeError(n, ErrorMessage.WRONG_NUMBER_OF_ARGS, params.size(), args.getNumChild()));
        } else {
            for (int i = 0; i < params.size(); i++) {
                Type argType = params.get(i);
                Pattern exp = args.getChild(i);
                exp.typeCheck(l, argType);
            }
        }
    }

    public static void typeCheckDeltaClause(DeltaClause dc, SemanticErrorList e) {
        { /* Does the Delta exist ? */
            final String name = dc.getDeltaspec().getName();
            KindedName symbol = new KindedName(KindedName.Kind.TYPE_DECL, name); // TODO: idiom
            if (!dc.getModuleDecl().getVisibleDeltas().containsKey(symbol)) {
                e.add(new TypeError(dc.getDeltaspec(),ErrorMessage.NAME_NOT_RESOLVABLE, name));
            }
        }
        assert dc.getModuleDecl().hasProductLine(); // otherwise we wouldn't be here...
        ProductLine p = dc.getModuleDecl().getProductLine();
        /* Do the referenced features exist? */
        for (Feature f : dc.getFeatures()) {
            boolean found = false;
            for (Feature cf : p.getOptionalFeatures()) {
                if (cf.getName().equals(f.getName())) {
                    found = true;
                    break;
                }
            }
            if (!found)
                e.add(new TypeError(f,ErrorMessage.NAME_NOT_RESOLVABLE, f.getName()));
        }
        /* What about 'after ...' clauses? */
        for (DeltaID d : dc.getDeltaIDs()) {
            KindedName kn = new KindedName(KindedName.Kind.TYPE_DECL, d.getName());
            if (!dc.getModuleDecl().getVisibleDeltas().containsKey(kn)) {
                e.add(new TypeError(d,ErrorMessage.NAME_NOT_RESOLVABLE, d.getName()));
            }                 
        }
     }
    
    public static java.util.List<Type> getTypesFromDataTypeUse(List<? extends DataTypeUse> params) {
        ArrayList<Type> res = new ArrayList<Type>();
        for (DataTypeUse u : params) {
            Type t = u.getType();
            res.add(t);
        }
        return res;
    }
    
    public static java.util.List<Type> getTypesFromTypeParamDecls(List<TypeParameterDecl> params) {
        ArrayList<Type> res = new ArrayList<Type>();
        for (TypeParameterDecl u : params) {
            res.add(u.getType());
        }
        return res;
    }

    /**
     * @deprecated
     * Unused.
     */
    public static java.util.List<Type> getUnboundTypes(List<TypeParameterDecl> params) {
        ArrayList<Type> res = new ArrayList<Type>();
        for (TypeParameterDecl u : params) {
            res.add(new BoundedType());
        }
        return res;
    }

    public static java.util.List<Type> getTypes(List<ParamDecl> params) {
        ArrayList<Type> res = new ArrayList<Type>();
        for (ParamDecl d : params) {
            res.add(d.getType());
        }
        return res;
    }

    public static java.util.List<Type> getTypesFromExp(List<PureExp> args) {
        ArrayList<Type> res = new ArrayList<Type>();
        for (PureExp e : args) {
            res.add(e.getType());
        }
        return res;
    }

    private static java.util.List<Type> getTypesFromPattern(List<Pattern> args) {
        ArrayList<Type> res = new ArrayList<Type>();
        for (Pattern p : args) {
            res.add(p.getType());
        }
        return res;
    }

    public static Map<TypeParameter, Type> getTypeParamBindingFromDataTypeUse(ASTNode<?> node, List<? extends DataTypeUse> params,
            List<PureExp> args) {
        return getTypeParamBinding(node, getTypesFromDataTypeUse(params), args);
    }

    public static Map<TypeParameter, Type> getTypeParamBindingFromParamDecl(ASTNode<?> node, List<ParamDecl> params, List<PureExp> args) {
        return getTypeParamBinding(node, getTypes(params), args);
    }

    public static Map<TypeParameter, Type> getTypeParamBinding(ASTNode<?> node, java.util.List<Type> params, List<PureExp> args) {
        return getTypeParamBinding(node, params, getTypesFromExp(args));
    }

    public static Map<TypeParameter, Type> getTypeParamBinding(ASTNode<?> node, java.util.List<Type> params, java.util.List<Type> args) {
        Map<TypeParameter, Type> binding = new HashMap<TypeParameter, Type>();
        addTypeParamBinding(node, binding, params, args);
        return binding;
    }

    public static void addTypeParamBinding(ASTNode<?> node, Map<TypeParameter, Type> binding, java.util.List<Type> params,
            java.util.List<Type> args) {
        if (params.size() != args.size())
            throw new TypeCheckerException(new TypeError(node, ErrorMessage.WRONG_NUMBER_OF_ARGS, params.size(),args.size()));
        for (int i = 0; i < params.size(); i++) {
            Type paramType = params.get(i);
            Type argType = args.get(i);
            if (argType == null)
                return;
            if (argType.isBoundedType()) {
                BoundedType bt = (BoundedType) argType;
                if (bt.hasBoundType())
                    argType = bt.getBoundType();
            }

            if (paramType.isTypeParameter()) {
                binding.put((TypeParameter) paramType, argType);
            } else if (paramType.isDataType() && argType.isDataType()) {
                DataTypeType paramdt = (DataTypeType) paramType;
                DataTypeType argdt = (DataTypeType) argType;
                if (paramdt.numTypeArgs() == argdt.numTypeArgs()) {
                    addTypeParamBinding(node, binding, paramdt.getTypeArgs(), argdt.getTypeArgs());
                }
            }
        }
    }

    public static <A extends ASTNode<?>> Collection<A> prepend(A a, List<A> list) {
        ArrayList<A> res = new ArrayList<A>();
        res.add(a);
        for (A x : list) {
            res.add(x);
        }
        return res;
    }

    static final StarImport STDLIB_IMPORT = new StarImport(Constants.STDLIB_NAME);

    public static void checkForDuplicateDecls(ModuleDecl mod, SemanticErrorList errors) {
        ArrayList<KindedName> duplicateNames = new ArrayList<KindedName>();
        Map<KindedName, ResolvedName> names = getDefinedNames(mod, duplicateNames);
        for (KindedName n : duplicateNames) {
            ResolvedName rn = names.get(n);
            ErrorMessage msg = null;
            switch (n.getKind()) {
            case CLASS:
                msg = ErrorMessage.DUPLICATE_CLASS_NAME;
                break;
            case FUN:
                msg = ErrorMessage.DUPLICATE_FUN_NAME;
                break;
            case DATA_CONSTRUCTOR:
                msg = ErrorMessage.DUPLICATE_CONSTRUCTOR;
                break;
            case TYPE_DECL:
                msg = ErrorMessage.DUPLICATE_TYPE_DECL;
                break;
            }
            errors.add(new TypeError(rn.getDecl(), msg, n.getName()));
        }
    }

    public static Map<KindedName, ResolvedName> getDefinedNames(ModuleDecl mod) {
        return getDefinedNames(mod, new ArrayList<KindedName>());
    }

    public static Map<KindedName, ResolvedName> getDefinedNames(ModuleDecl mod,
            java.util.List<KindedName> foundDuplicates) {
        HashMap<KindedName, ResolvedName> res = new HashMap<KindedName, ResolvedName>();
        ResolvedModuleName moduleName = new ResolvedModuleName(mod);

        for (Decl d : mod.getDeclList()) {
            ResolvedDeclName rn = new ResolvedDeclName(moduleName, d);
            if (res.containsKey(rn.getSimpleName()))
                foundDuplicates.add(rn.getSimpleName());
            res.put(rn.getSimpleName(), rn);
            res.put(rn.getQualifiedName(), rn);

            if (d instanceof DataTypeDecl) {
                DataTypeDecl dataDecl = (DataTypeDecl) d;
                for (DataConstructor c : dataDecl.getDataConstructors()) {
                    rn = new ResolvedDeclName(moduleName, c);
                    if (res.containsKey(rn.getSimpleName()))
                        foundDuplicates.add(rn.getSimpleName());
                    res.put(rn.getSimpleName(), rn);
                    res.put(rn.getQualifiedName(), rn);
                }
            }

        }

        return res;
    }

    public static Map<KindedName, ResolvedName> getImportedNames(ModuleDecl mod) {
        HashMap<KindedName, ResolvedName> res = new HashMap<KindedName, ResolvedName>();

        for (Import i : mod.getImports()) {
            if (i instanceof StarImport) {
                StarImport si = (StarImport) i;
                ModuleDecl md = mod.lookupModule(si.getModuleName());
                if (md == null) {
                    if (!Constants.STDLIB_NAME.equals(si.getModuleName()))
                        throw new TypeCheckerException(new TypeError(si, ErrorMessage.MODULE_NOT_RESOLVABLE,
                                si.getModuleName()));
                } else {
                    addAllNamesNoHiding(md.getExportedNames(), res);
                }
            } else if (i instanceof NamedImport) {
                NamedImport ni = (NamedImport) i;
                for (Name n : ni.getNames()) {
                    ModuleDecl md = mod.lookupModule(n.getModuleName());
                    if (md == null) {
                        if (!Constants.STDLIB_NAME.equals(n.getModuleName()))
                            throw new TypeCheckerException(new TypeError(n, ErrorMessage.MODULE_NOT_RESOLVABLE,
                                    n.getModuleName()));
                    }
                    Map<KindedName, ResolvedName> allNames = getAllNames(n.getSimpleName(), md.getExportedNames());
                    if (!allNames.isEmpty()) {
                        for (Entry<KindedName, ResolvedName> e : allNames.entrySet()) {
                            res.put(new KindedName(e.getKey().getKind(), n.getModuleName() + "." + e.getKey().getName()),
                                    e.getValue());
                        }
                    } else {
                        throw new TypeCheckerException(new TypeError(n, ErrorMessage.NAME_NOT_EXPORTED_BY_MODULE,
                                n.getSimpleName(), n.getModuleName()));
                    }
                }
            } else if (i instanceof FromImport) {
                FromImport fi = (FromImport) i;
                ModuleDecl md = mod.lookupModule(fi.getModuleName());
                if (md != null) {
                    Map<KindedName, ResolvedName> en = md.getExportedNames();
                    for (Name n : fi.getNames()) {
                        putKindedNamesNoHiding(n.getString(), en, res);
                        putKindedNamesNoHiding(fi.getModuleName() + "." + n.getString(), en, res);
                    }
                }
            }
        }
        return res;
    }

    public static Map<KindedName, ResolvedName> getVisibleNames(ModuleDecl mod) {
        HashMap<KindedName, ResolvedName> res = new HashMap<KindedName, ResolvedName>();
        // add imported names:
        res.putAll(mod.getImportedNames());
        // defined names hide imported names:
        res.putAll(mod.getDefinedNames());      
        return res;
    }

    /**
     * add all entries from one map to an other. 
     * when there is a duplicate key, the ResolvedName is not overridden, but replaced 
     * by an instance of ResolvedAmbigiousName
     * 
     * @param source
     * @param target
     */
    private static void addAllNamesNoHiding(Map<KindedName, ResolvedName> source, Map<KindedName, ResolvedName> target) {
        for (Entry<KindedName, ResolvedName> e : source.entrySet()) {
            KindedName kindedName = e.getKey();
            ResolvedName resolvedName = e.getValue();
            ResolvedName prev = target.put(kindedName, resolvedName);
            if (prev != null && !(prev.equals(resolvedName))) {
                // name is ambigious
                target.put(kindedName, new ResolvedAmbigiousName(prev, resolvedName));
            }
        }
    }

    
    
    public static <RN extends ResolvedName> Map<KindedName, RN> getAllNames(String name, Map<KindedName, RN> sourceMap) {
        HashMap<KindedName, RN> map = new HashMap<KindedName, RN>();
        for (Kind k : Kind.values()) {
            KindedName kn = new KindedName(k, name);
            RN rn = sourceMap.get(kn);
            if (rn != null) {
                map.put(kn, rn);
            }
        }
        return map;
    }

    public static <RN extends ResolvedName> void putKindedNames(String name, Map<KindedName, RN> sourceMap,
            Map<KindedName, RN> targetMap) {
        targetMap.putAll(getAllNames(name, sourceMap));
    }
    
    public static void putKindedNamesNoHiding(String name, Map<KindedName, ResolvedName> sourceMap,
            Map<KindedName, ResolvedName> targetMap) {
        addAllNamesNoHiding(getAllNames(name, sourceMap), targetMap);
    }

    public static boolean isQualified(String name) {
        return name.contains(".");
    }

    public static String getModuleName(String qualifiedName) {
        return qualifiedName.substring(0, qualifiedName.lastIndexOf('.'));

    }

    public static String getSimpleName(String name) {
        if (isQualified(name)) {
            return name.substring(name.lastIndexOf('.') + 1, name.length());
        } else {
            return name;
        }

    }

    public static Map<KindedName, ResolvedName> getExportedNames(ModuleDecl mod) {
        HashMap<KindedName, ResolvedName> res = new HashMap<KindedName, ResolvedName>();
        for (Export e : mod.getExports()) {
            if (e instanceof StarExport) {
                StarExport se = (StarExport) e;
                if (!se.hasModuleName()) {
                    res.putAll(mod.getDefinedNames());
                } else {
                    String moduleName = se.getModuleName().getName();

                    putNamesOfModule(mod, res, moduleName, null);
                }
            } else if (e instanceof FromExport) {
                FromExport fe = (FromExport) e;
                String moduleName = fe.getModuleName();
                for (Name n : fe.getNames()) {
                    String simpleName = n.getSimpleName();
                    putNamesOfModule(mod, res, moduleName, simpleName);
                }
            } else if (e instanceof NamedExport) {
                NamedExport ne = (NamedExport) e;
                for (Name n : ne.getNames()) {
                    String simpleName = getSimpleName(n.getString());
                    putKindedNames(simpleName, mod.getVisibleNames(), res);
                    putKindedNames(mod.getName() + "." + simpleName, mod.getVisibleNames(), res);
                }
            }

        }
        return res;
    }

    public static void typeCheckBinary(SemanticErrorList e, Binary b, Type t) {
        assertHasType(e, b.getLeft(),t);
        assertHasType(e, b.getRight(),t);
        b.getLeft().typeCheck(e);
        b.getRight().typeCheck(e);
        
    }
    
    /**
     * checks whether the local variable v was already defined in the current function
     */
    public static void checkForDuplicatesOfVarDecl(SemanticErrorList e, VarDeclStmt v) {
        String varName = v.getVarDecl().getName();
        VarOrFieldDecl otherVar = v.lookupVarOrFieldName(varName , false);
        if (otherVar != null && inSameMethodOrBlock(v, otherVar)) {
            e.add(new TypeError(v,ErrorMessage.VARIABLE_ALREADY_DECLARED, varName));
        }
    }

    /**
     * checks whether the two elements are defined in the same method or block
     */
    private static boolean inSameMethodOrBlock(ASTNode<?> a, ASTNode<?> b) {
        return a.getContextMethod() != null && b.getContextMethod() == a.getContextMethod()
            || a.getContextBlock()  != null && b.getContextBlock() == a.getContextBlock();
    }
    
    
    private static void putNamesOfModule(ModuleDecl mod, HashMap<KindedName, ResolvedName> res, String moduleName,
            String simpleNamePattern) {
        for (Entry<KindedName, ResolvedName> entry : mod.getVisibleNames().entrySet()) {
            KindedName kn = entry.getKey();
            if (isQualified(kn.getName())) {
                if (getModuleName(kn.getName()).equals(moduleName)) {
                    String simpleName = getSimpleName(kn.getName());
                    if (simpleNamePattern == null || simpleNamePattern.equals(simpleName)) {
                        res.put(new KindedName(kn.getKind(), mod.getName() + "." + simpleName), entry.getValue());
                        res.put(new KindedName(kn.getKind(), simpleName), entry.getValue());
                    }
                }
            }
        }
    }
    
    /**
     * check a list of compilation units for duplicate module names
     */
    public static void checkForDuplicateModules(SemanticErrorList errors, Iterable<CompilationUnit> compilationUnits) {
        Set<String> seenModules = new HashSet<String>();
        for (CompilationUnit u : compilationUnits) {
            for (ModuleDecl module : u.getModuleDecls()) {
                if (!seenModules.add(module.getName())) {
                    errors.add(new TypeError(module, ErrorMessage.DUPLICATE_MODULE_NAME,module.getName()));
                }
            }
        }
    }

    /**
     * checks if a declaration is unknown and adds an appropriate error message to the semantic error list
     * @param use the use of the declaration (this is where the error will be shown)
     * @param decl the declaration which is referenced by the use
     * @param e the semantic error list
     * @param errorMessage the error message for unknown declarations 
     * @param name the name of the declaration (used in the error message)
     * @return
     */
    public static boolean checkDecl(ASTNode<?> use, Decl decl, SemanticErrorList e, ErrorMessage errorMessage, String name) {
        if (decl.isUnknown()) {
            if (decl instanceof AmbiguousDecl) {
                AmbiguousDecl ambigiousDecl = (AmbiguousDecl) decl;
                e.add(new TypeError(use, ErrorMessage.AMBIGIOUS_USE, name, getAlternativesAsString(ambigiousDecl)));
            } else {
                e.add(new TypeError(use, errorMessage, name));
            }
            return false;
        }
        return true;
    }

    /**
     * get all the alternative declarations of an ambiguous declaration formated as a list
     * which can be used in error messages
     * @param a
     * @return
     */
    public static String getAlternativesAsString(AmbiguousDecl a) {
        String result = "";
        for (Decl alternative : a.getAlternative()) {
            result += "\n * " + alternative.qualifiedName() +  " (defined in " + 
                    alternative.getFileName() + ", line " + Symbol.getLine(alternative.getStart()) + ")";
        }
        return result;
    }

    public static void checkDataTypeUse(SemanticErrorList e, DataTypeUse use) {
        if (! (use.getType() instanceof DataTypeType))
            return;
        
        DataTypeType type = (DataTypeType) use.getType();
        if (type.getDecl() instanceof ParametricDataTypeDecl) {
            int expected = ((ParametricDataTypeDecl)type.getDecl()).getNumTypeParameter();
            if (expected != type.numTypeArgs()) {
                e.add(new TypeError(use, ErrorMessage.WRONG_NUMBER_OF_TYPE_ARGS,type.toString(),""+expected,""+type.numTypeArgs()));
            } else if (expected > 0) {
                if (use instanceof ParametricDataTypeUse) {
                        for (DataTypeUse du : ((ParametricDataTypeUse)use).getParams()) {
                                du.typeCheck(e);
                        }
                } else if (use.getDecl() instanceof TypeSynDecl) {
                    // nothing to check as this is already checked at the TypeSynDecl
                } else {
                    e.add(new TypeError(use, ErrorMessage.WRONG_NUMBER_OF_TYPE_ARGS,type.toString(),""+expected,"0"));
                }
            }
        }
    }

    public static void checkDefBeforeUse(SemanticErrorList e, VarOrFieldUse use) {
        if (use.getType().isUnknownType()) {
            e.add(new TypeError(use,ErrorMessage.NAME_NOT_RESOLVABLE, use.getName()));
        } else {
            // check that fields are not used before they are defined:
            boolean isUsedInFieldDecl = use.getContextFieldDecl() != null;
            if (isUsedInFieldDecl && use.getDecl().getEndPos() > use.getStartPos()) {
                e.add(new TypeError(use,
                        use instanceof VarUse ? ErrorMessage.VAR_USE_BEFORE_DEFINITION
                                              : ErrorMessage.FIELD_USE_BEFORE_DEFINITION , use.getName()));
            }
        }
    }
}
