/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.typechecker;

import java.util.*;

import abs.common.Constants;
import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.SemanticError;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.analyser.TypeError;
import abs.frontend.ast.List;
import abs.frontend.ast.*;
import abs.frontend.typechecker.KindedName.Kind;

public class TypeCheckerHelper {
    public static void typeCheck(ConstructorPattern p, SemanticErrorList e, Type t) {
        if (!t.isDataType())
            return;

        Decl decl = p.lookup(new KindedName(Kind.DATA_CONSTRUCTOR, p.getConstructor()));
        if (!(decl instanceof DataConstructor)) {
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

        if (!(myType instanceof DataTypeType))
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

        typeCheckMatchingParamsPattern(e, p, c);
    }

    public static void checkAssignment(SemanticErrorList l, ASTNode<?> n, Type lht, Exp rhte) {
        Type te = rhte.getType();
        if (!te.isAssignable(lht)) {
            l.add(new TypeError(n, ErrorMessage.CANNOT_ASSIGN, te, lht));
        }
    }

    public static void typeCheckParamList(SemanticErrorList l, HasParams params) {
        HashSet<String> names = new HashSet<String>();
        for (ParamDecl d : params.getParams()) {
            if (!names.add(d.getName())) {
                l.add(new TypeError(d, ErrorMessage.DUPLICATE_PARAM_NAME, d.getName()));
            }
            d.typeCheck(l);
        }
    }

    public static void typeCheckMatchingParams(SemanticErrorList l, DataConstructorExp n) {
        final Map<TypeParameter, Type> binding = n.getTypeParamBinding(n, n.getDataConstructor());
        typeCheckEqual(l, n, n.getDataConstructor().applyBindings(binding));
    }

    private static void typeCheckMatchingParamsPattern(SemanticErrorList l, ConstructorPattern n, DataConstructor decl) {
        Map<TypeParameter, Type> binding = decl.getTypeParamBinding(n, n.getTypes());
        java.util.List<Type> types = decl.applyBindings(binding);
        typeCheckEqualPattern(l, n, types);
    }

    public static java.util.List<Type> applyBindings(Map<TypeParameter, Type> binding, java.util.List<Type> types) {
        ArrayList<Type> res = new ArrayList<Type>(types.size());
        for (Type t : types) {
            res.add(t.applyBinding(binding));
        }
        return res;
    }

    public static void typeCheckMatchingParams(SemanticErrorList l, FnApp n, ParametricFunctionDecl decl) {
        Map<TypeParameter, Type> binding = n.getTypeParamBindingFromParamDecl(decl);
        java.util.List<Type> types = decl.applyBindings(binding);
        typeCheckEqual(l, n, types);
    }

    public static void typeCheckEqual(SemanticErrorList l, ASTNode<?> n, java.util.List<Type> params) {
        List<PureExp> args = ((HasActualParams)n).getParams();
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

    private static void typeCheckEqualPattern(SemanticErrorList l, ConstructorPattern n, java.util.List<Type> params) {
        List<Pattern> args = n.getParams();
        if (params.size() != args .getNumChild()) {
            l.add(new TypeError(n, ErrorMessage.WRONG_NUMBER_OF_ARGS, params.size(), args.getNumChild()));
        } else {
            for (int i = 0; i < params.size(); i++) {
                Type argType = params.get(i);
                Pattern exp = args.getChild(i);
                exp.typeCheck(l, argType);
            }
        }
    }

    public static void typeCheckDeltaClause(DeltaClause clause, Set<String> deltaNames, Set<String> definedFeatures, SemanticErrorList e) {

        /* Does the delta exist? */
        if (! deltaNames.contains(clause.getDeltaspec().getName()))
            e.add(new TypeError(clause.getDeltaspec(), ErrorMessage.NAME_NOT_RESOLVABLE, clause.getDeltaspec().getName()));

        /* Do the referenced features exist? */
        if (clause.hasAppCond()) {
            clause.getAppCond().typeCheck(definedFeatures, e);
        }
        if (clause.hasFromAppCond()) {
            clause.getFromAppCond().typeCheck(definedFeatures, e);
        }

        /* What about deltas mentioned in the 'after' clause? */
        for (DeltaID did : clause.getDeltaIDs()) {
            if (! deltaNames.contains(did.getName())) {
                e.add(new TypeError(did, ErrorMessage.NAME_NOT_RESOLVABLE, did.getName()));
            }
        }
    }

    public static void typeCheckProduct(Product prod, Set<String> prodNames, SemanticErrorList e) {
        /* Does the product named in the adaptation clause exist */
        for (ProductAdaptation ad : prod.getProductAdaptations()) {
            if (! prodNames.contains(ad.getProductID()))
                e.add(new TypeError(ad, ErrorMessage.NAME_NOT_RESOLVABLE, ad.getProductID()));
        }
        // TODO check that features of product exist, also feature attributes
    }

    public static java.util.List<Type> getTypes(List<ASTNode<?>> params) {
        ArrayList<Type> res = new ArrayList<Type>();
        for (ASTNode<?> u : params) {
            res.add(((HasType)u).getType());
        }
        return res;
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

    public static ResolvedMap getDefinedNames(ModuleDecl mod,
            java.util.List<KindedName> foundDuplicates) {
        ResolvedMap res = new ResolvedMap();
        ResolvedModuleName moduleName = new ResolvedModuleName(mod);

        for (Decl d : mod.getDeclList()) {
            ResolvedDeclName rn = new ResolvedDeclName(moduleName, d);
            if (res.put(rn.getSimpleName(), rn) != null)
                foundDuplicates.add(rn.getSimpleName());
            res.put(rn.getQualifiedName(), rn);

            if (d instanceof DataTypeDecl) {
                DataTypeDecl dataDecl = (DataTypeDecl) d;
                for (DataConstructor c : dataDecl.getDataConstructors()) {
                    rn = new ResolvedDeclName(moduleName, c);
                    if (res.put(rn.getSimpleName(), rn) != null)
                        foundDuplicates.add(rn.getSimpleName());
                    res.put(rn.getQualifiedName(), rn);
                }
            }
        }
        return res;
    }

    public static ResolvedMap getImportedNames(ModuleDecl mod) {
        ResolvedMap res = new ResolvedMap();

        for (Import i : mod.getImports()) {
            if (i instanceof StarImport) {
                StarImport si = (StarImport) i;
                ModuleDecl md = mod.lookupModule(si.getModuleName());
                if (md != null) {
                    res.addAllNamesNoHiding(md.getExportedNames());
                }
            } else if (i instanceof NamedImport) {
                NamedImport ni = (NamedImport) i;
                for (Name n : ni.getNames()) {
                    ModuleDecl md = mod.lookupModule(n.getModuleName());
                    if (md != null)
                        try {
                            res.addAllNames(md.getExportedNames(), n);
                        } catch (TypeCheckerException e) {} // NADA
                }
            } else if (i instanceof FromImport) {
                FromImport fi = (FromImport) i;
                ModuleDecl md = mod.lookupModule(fi.getModuleName());
                if (md != null) {
                    ResolvedMap en = md.getExportedNames();
                    for (Name n : fi.getNames()) {
                        res.putKindedNamesNoHiding(n.getString(), en);
                        res.putKindedNamesNoHiding(fi.getModuleName() + "." + n.getString(), en);
                    }
                }
            }
        }
        return res;
    }

    public static ResolvedMap getVisibleNames(ModuleDecl mod) {
        ResolvedMap res = new ResolvedMap();
        // add imported names:
        res.putAll(mod.getImportedNames());
        // defined names hide imported names:
        res.putAll(mod.getDefinedNames());
        return res;
    }

    public static boolean isQualified(String name) {
        return name.contains(".");
    }

    public static ResolvedMap getExportedNames(ModuleDecl mod) {
        ResolvedMap res = new ResolvedMap();
        for (Export e : mod.getExports()) {
            if (e instanceof StarExport) {
                StarExport se = (StarExport) e;
                if (!se.hasModuleName()) {
                    res.putAll(mod.getDefinedNames());
                } else {
                    String moduleName = se.getModuleName().getName();
                    res.putNamesOfModule(mod, mod.getVisibleNames(), moduleName, null);
                }
            } else if (e instanceof FromExport) {
                FromExport fe = (FromExport) e;
                String moduleName = fe.getModuleName();
                for (Name n : fe.getNames()) {
                    String simpleName = n.getSimpleName();
                    res.putNamesOfModule(mod, mod.getVisibleNames(), moduleName, simpleName);
                }
            } else if (e instanceof NamedExport) {
                NamedExport ne = (NamedExport) e;
                for (Name n : ne.getNames()) {
                    String simpleName = n.getSimpleName();
                    res.putKindedNames(simpleName, mod.getVisibleNames());
                    res.putKindedNames(mod.getName() + "." + simpleName, mod.getVisibleNames());
                }
            }

        }
        return res;
    }

    public static void typeCheckBinary(SemanticErrorList e, Binary b, Type t) {
        b.getLeft().assertHasType(e, t);
        b.getRight().assertHasType(e, t);
        b.getLeft().typeCheck(e);
        b.getRight().typeCheck(e);
    }

    /**
     * checks whether the local variable v was already defined in the current function
     */
    public static void checkForDuplicatesOfVarDecl(SemanticErrorList e, VarDeclStmt v) {
        String varName = v.getVarDecl().getName();
        VarOrFieldDecl otherVar = v.lookupVarOrFieldName(varName , false);
        if (otherVar != null && v.inSameMethodOrBlock(otherVar)) {
            e.add(new TypeError(v,ErrorMessage.VARIABLE_ALREADY_DECLARED, varName));
        }
    }

    /**
     * check a list of compilation units for duplicate module names, product names, delta names
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
    public static void checkForDuplicateProducts(SemanticErrorList errors, Iterable<CompilationUnit> compilationUnits) {
        Set<String> seen = new HashSet<String>();
        for (CompilationUnit u : compilationUnits) {
            for (Product p : u.getProducts()) {
                if (!seen.add(p.getName()))
                    errors.add(new TypeError(p, ErrorMessage.DUPLICATE_PRODUCT, p.getName()));
            }
        }
    }
    public static void checkForDuplicateDeltas(SemanticErrorList errors, Iterable<CompilationUnit> compilationUnits) {
        Set<String> seen = new HashSet<String>();
        for (CompilationUnit u : compilationUnits) {
            for (DeltaDecl d : u.getDeltaDecls()) {
                if (!seen.add(d.getName()))
                    errors.add(new TypeError(d, ErrorMessage.DUPLICATE_DELTA, d.getName()));
            }
        }
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
                    alternative.getFileName() + ", line " + alternative.getStartLine() + ")";
        }
        return result;
    }

    public static void checkDataTypeUse(SemanticErrorList e, DataTypeUse use) {
        Type type = use.getType();
        if (type.getDecl() instanceof ParametricDataTypeDecl) {
            DataTypeType t = (DataTypeType) type;
            int expected = ((ParametricDataTypeDecl)type.getDecl()).getNumTypeParameter();
            if (expected != t.numTypeArgs()) {
                e.add(new TypeError(use, ErrorMessage.WRONG_NUMBER_OF_TYPE_ARGS,type.toString(),""+expected,""+t.numTypeArgs()));
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
            boolean isUsedInFieldDecl = use.getDecl() instanceof FieldDecl;
            if (isUsedInFieldDecl && use.getDecl().getEndPos() > use.getStartPos()) {
                e.add(new TypeError(use,
                        use instanceof VarUse ? ErrorMessage.VAR_USE_BEFORE_DEFINITION
                                              : ErrorMessage.FIELD_USE_BEFORE_DEFINITION , use.getName()));
            }
        }
    }
}
