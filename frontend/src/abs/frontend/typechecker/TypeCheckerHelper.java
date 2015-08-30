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

public class TypeCheckerHelper {
    public static void typeCheck(ConstructorPattern p, SemanticErrorList e, Type t) {
        DataConstructor c = p.getDataConstructor();
        if (c == null) {
            e.add(new SemanticError(p, ErrorMessage.CONSTRUCTOR_NOT_RESOLVABLE, p.getConstructor()));
            return;
        }

        if (c.getNumConstructorArg() != p.getNumParam()) {
            e.add(new TypeError(p, ErrorMessage.WRONG_NUMBER_OF_ARGS, c.getNumConstructorArg(), p.getNumParam()));
            return;
        }

        assert t.isDataType() || t.isExceptionType() : t; // TODO: more instances of this? Maybe exception should imply datatype?

        if (!t.getDecl().equals(c.getDataTypeDecl())) {
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
        Set<String> names = new HashSet<String>();
        for (ParamDecl d : params.getParams()) {
            if (!names.add(d.getName())) {
                l.add(new TypeError(d, ErrorMessage.DUPLICATE_PARAM_NAME, d.getName()));
            }
            d.typeCheck(l);
        }
    }

    public static void typeCheckRunMethodSig(SemanticErrorList l, MethodSig m) {
        if (m.getNumParam() > 0) {
            l.add(new TypeError(m, ErrorMessage.RUN_METHOD_WRONG_NUM_PARAMS, m.getNumParam()));
        }
        if (!m.getReturnType().getType().isUnitType()) {
            l.add(new TypeError(m, ErrorMessage.RUN_METHOD_WRONG_RETURN_TYPE,
                                m.getReturnType().getType().toString()));
        }
    }

    public static void typeCheckMatchingParams(SemanticErrorList l, DataConstructorExp n, DataConstructor c) {
        assert n.getDecl() == c;
        final Map<TypeParameter, Type> binding = n.getTypeParamBinding(n, c);
        typeCheckEqual(l, n, c.applyBindings(binding));
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

    public static void typeCheckDeltaClause(DeltaClause clause, Map<String,DeltaDecl> deltaNames, Set<String> definedFeatures, SemanticErrorList e) {

        /* Does the delta exist? */
        final Deltaspec spec = clause.getDeltaspec();
        if (! deltaNames.containsKey(spec.getDeltaID()))
            e.add(new TypeError(spec, ErrorMessage.NAME_NOT_RESOLVABLE, spec.getDeltaID()));
        else {
            DeltaDecl dd = deltaNames.get(spec.getDeltaID());
            if (dd.getNumParam() != spec.getNumDeltaparam()) {
                e.add(new TypeError(spec, ErrorMessage.WRONG_NUMBER_OF_ARGS,dd.getNumParam(),spec.getNumDeltaparam()));
            } else {
                for (int i=0; i<dd.getNumParam(); i++) {
                    DeltaParamDecl formal = dd.getParam(i);
                    Deltaparam actual = spec.getDeltaparam(i);
                    // TODO: W00t?!
                    if (actual instanceof Const) {
                        Value a = ((Const) actual).getValue();
                        if (! formal.accepts(a)) {
                            e.add(new TypeError(a, ErrorMessage.CANNOT_ASSIGN, a.getName(), formal.getType().getSimpleName()));
                        }
                    }
                }
            }
        }

        /* Do the referenced features exist? */
        if (clause.hasAppCond()) {
            clause.getAppCond().typeCheck(definedFeatures, e);
        }
        if (clause.hasFromAppCond()) {
            clause.getFromAppCond().typeCheck(definedFeatures, e);
        }

        /* What about deltas mentioned in the 'after' clause? */
        for (DeltaID did : clause.getAfterDeltaIDs()) {
            if (! deltaNames.containsKey(did.getName())) {
                e.add(new TypeError(did, ErrorMessage.NAME_NOT_RESOLVABLE, did.getName()));
            }
        }
    }

    public static void typeCheckProduct(Product prod,
            Map<String,Feature> featureNames,
            Set<String> prodNames,
            Map<String,DeltaDecl> deltaNames,
            Set<String> updateNames,
            SemanticErrorList e) {
        if (featureNames != null) {
            // Do the features exist in the PL declaration (and also check feature attributes)?
            Model m = prod.getModel();
            for (Feature f : prod.getFeatures()) {
                if (!featureNames.containsKey(f.getName()))
                    e.add(new TypeError(prod, ErrorMessage.NAME_NOT_RESOLVABLE, f.getName()));
                else {
                    Collection<DeltaClause> dcs = findDeltasForFeature(m,f);
                    for (int i = 0; i<f.getNumAttrAssignment(); i++) {
                        AttrAssignment aa = f.getAttrAssignment(i);
                        for (DeltaClause dc : dcs) {
                            DeltaDecl dd = m.findDelta(dc.getDeltaspec().getDeltaID());
                            DeltaParamDecl dp = dd.getParam(i);
                            // FIXME: we assumed here that delta
                            // parameters and feature parameters have
                            // same order, arity.  This is clearly
                            // wrong, and parameters for the delta are
                            // named with the feature.  we should find a
                            // dp with the same name as aa, and ignore
                            // any superfluous aa (the value is simply
                            // not used by this delta).
                            if (dp != null && !dp.accepts(aa.getValue())) {
                                e.add(new TypeError(aa, ErrorMessage.CANNOT_ASSIGN, aa.getValue().getName(), dp.getType().getSimpleName()));
                            }
                        }
                    }
                }
            }
        }
        Set<String> seen = new HashSet<String>();
        for (Reconfiguration recf : prod.getReconfigurations()) {
            if (!seen.add(recf.getTargetProductID()))
                e.add(new TypeError(recf, ErrorMessage.DUPLICATE_RECONFIGURATION, recf.getTargetProductID()));

            // Does the reconfiguration target product exist?
            if (! prodNames.contains(recf.getTargetProductID()))
                e.add(new TypeError(recf, ErrorMessage.NAME_NOT_RESOLVABLE, recf.getTargetProductID()));
            // Do the deltas used for reconfiguration exist?
            for (DeltaID d : recf.getDeltaIDs()) {
                if (! deltaNames.containsKey(d.getName()))
                    e.add(new TypeError(recf, ErrorMessage.NAME_NOT_RESOLVABLE, d.getName()));
            }
            // Does the update used for reconfiguration exist?
            if (! updateNames.contains(recf.getUpdateID()))
                e.add(new TypeError(recf, ErrorMessage.NAME_NOT_RESOLVABLE, recf.getUpdateID()));
        }
    }

    /**
     * Look for all deltas that have a particular feature in the application condition --
     * up to the boolean madness that lies within AppConds (delta D(F.x) when ~F will
     * be checked when actually trying to flatten the product, I hope.
     */
    private static Collection<DeltaClause> findDeltasForFeature(Model m, Feature f) {
        Collection<DeltaClause> dcs = new ArrayList<DeltaClause>();
        for (int i = 0; i < m.getProductLine().getNumDeltaClause(); i++) {
            DeltaClause dc = m.getProductLine().getDeltaClause(i);
            if (dc.refersTo(f)) {
                dcs.add(dc);
            }
        }
        return dcs;
    }

    public static <T extends ASTNode<?>> java.util.List<Type> getTypes(List<T> params) {
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
            case MODULE:
                assert false; // doesn't happen, no modules within modules
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
            } else if (d instanceof ExceptionDecl) {
                ExceptionDecl ed = (ExceptionDecl) d;
                DataConstructor ec = ed.dataConstructor;
                assert ec != null : ed.getName();
                if (ec.getName().equals(d.getName())) {
                    // should always be true, see Main.java where the data
                    // constructor gets constructed
                    rn = new ResolvedDeclName(moduleName, ec);
                    // If it's already in there, is it from the same location -- from stdlib? 
                    ResolvedName tryIt = res.get(rn);
                    if (tryIt != null && tryIt.getDecl() != ed)
                        foundDuplicates.add(rn.getSimpleName());
                    else {
                        res.put(rn.getQualifiedName(), rn);
                    }
                }
            }
            else if (d instanceof ExceptionDecl) {
                ExceptionDecl ed = (ExceptionDecl)d;
                DataConstructor ec = ed.dataConstructor;
                if (ec.getName().equals(d.getName())) {
                    // should always be true, see Main.java where the data
                    // constructor gets constructed
                    rn = new ResolvedDeclName(moduleName, ec);
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
    public static void checkForDuplicateUpdates(SemanticErrorList errors, Iterable<CompilationUnit> compilationUnits) {
        Set<String> seen = new HashSet<String>();
        for (CompilationUnit u : compilationUnits) {
            for (UpdateDecl d : u.getUpdateDecls()) {
                if (!seen.add(d.getName()))
                    errors.add(new TypeError(d, ErrorMessage.DUPLICATE_UPDATE, d.getName()));
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
                        for (TypeUse du : ((ParametricDataTypeUse)use).getParams()) {
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
            /* Check that fields are not used before they are defined,
             * when we are NOT inside a method, e.g. when initialising a field upon declaration.
             */
            boolean isUsedInFieldDecl = use instanceof FieldUse;
            if (isUsedInFieldDecl && use.getContextMethod() == null && use.getDecl().getEndPos() > use.getStartPos()) {
                e.add(new TypeError(use,
                        !isUsedInFieldDecl ? ErrorMessage.VAR_USE_BEFORE_DEFINITION
                                           : ErrorMessage.FIELD_USE_BEFORE_DEFINITION , use.getName()));
            }
        }
    }
}
