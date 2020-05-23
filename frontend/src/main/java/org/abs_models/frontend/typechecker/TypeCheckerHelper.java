/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.typechecker;

import java.util.*;

import org.abs_models.common.Constants;
import org.abs_models.frontend.analyser.ErrorMessage;
import org.abs_models.frontend.analyser.SemanticError;
import org.abs_models.frontend.analyser.SemanticWarning;
import org.abs_models.frontend.analyser.SemanticConditionList;
import org.abs_models.frontend.analyser.TypeError;
import org.abs_models.frontend.ast.*;
import org.abs_models.frontend.parser.Main;
import org.abs_models.frontend.parser.SourcePosition;
import com.google.common.collect.ImmutableSet;

public class TypeCheckerHelper {

    // Warn about using constructors that should not have been exported from
    // their module (used in DataConstructorExp.typeCheck() defined in
    // TypeChecker.jadd)
    public static Set<String> deprecatedConstructors
        = ImmutableSet.of("ABS.StdLib.Set", "ABS.StdLib.EmptySet", "ABS.StdLib.Insert",
                          "ABS.StdLib.Map", "ABS.StdLib.EmptyMap", "ABS.StdLib.InsertAssoc");



    public static void typeCheck(ConstructorPattern p, SemanticConditionList e, Type t) {
        DataConstructor c = p.getDataConstructor();
        if (c == null) {
            e.add(new SemanticError(p, ErrorMessage.CONSTRUCTOR_NOT_RESOLVABLE, p.getConstructor()));
            return;
        }
        String cname = c.getQualifiedName();
        if (deprecatedConstructors.contains(cname)
            && !(cname.startsWith(p.getModuleDecl().getName()))) {
            e.add(new SemanticWarning(p, ErrorMessage.DEPRECATED_CONSTRUCTOR, cname));
        }

        if (c.getNumConstructorArg() != p.getNumParam()) {
            e.add(new TypeError(p, ErrorMessage.WRONG_NUMBER_OF_ARGS, c.getNumConstructorArg(), p.getNumParam()));
            return;
        }

        assert t.isDataType() || t.isExceptionType() : t; // isExceptionType only for clarity, since exceptions are datatypes

        if (!t.isExceptionType()) {
            if (!t.getDecl().equals(c.getDataTypeDecl())) {
                e.add(new TypeError(p, ErrorMessage.WRONG_CONSTRUCTOR, t.toString(), p.getConstructor()));
            }
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

    public static void checkAssignment(SemanticConditionList l, ASTNode<?> n, Type lht, Exp rhte) {
        Type te = rhte.getType();
        if (!te.isAssignableTo(lht)) {
            l.add(new TypeError(n, ErrorMessage.CANNOT_ASSIGN, te, lht));
        }
    }

    public static void typeCheckParamList(SemanticConditionList l, HasParams params) {
        Set<String> names = new HashSet<>();
        for (ParamDecl d : params.getParams()) {
            if (!names.add(d.getName())) {
                l.add(new TypeError(d, ErrorMessage.DUPLICATE_PARAM_NAME, d.getName()));
            }
            d.typeCheck(l);
        }
    }

    public static void typeCheckRunMethodSig(SemanticConditionList l, MethodSig m) {
        if (m.getNumParam() > 0) {
            l.add(new TypeError(m, ErrorMessage.RUN_METHOD_WRONG_NUM_PARAMS, m.getNumParam()));
        }
        if (!m.getReturnType().getType().isUnitType()) {
            l.add(new TypeError(m, ErrorMessage.RUN_METHOD_WRONG_RETURN_TYPE,
                                m.getReturnType().getType().toString()));
        }
    }

    public static void typeCheckMatchingParams(SemanticConditionList l, DataConstructorExp n, DataConstructor c) {
        assert n.getDecl() == c;
        final Map<TypeParameter, Type> binding = n.getTypeParamBinding(n, c);
        typeCheckEqual(l, n, c.applyBindings(binding));
    }

    private static void typeCheckMatchingParamsPattern(SemanticConditionList l, ConstructorPattern n, DataConstructor decl) {
        Map<TypeParameter, Type> binding = decl.getTypeParamBinding(n, n.getTypes());
        java.util.List<Type> types = decl.applyBindings(binding);
        typeCheckEqualPattern(l, n, types);
    }

    public static java.util.List<Type> applyBindings(Map<TypeParameter, Type> binding, java.util.List<Type> types) {
        ArrayList<Type> res = new ArrayList<>(types.size());
        for (Type t : types) {
            res.add(t.applyBinding(binding));
        }
        return res;
    }

    public static void typeCheckMatchingParams(SemanticConditionList l, FnApp n, ParametricFunctionDecl decl) {
        Map<TypeParameter, Type> binding = n.getTypeParamBindingFromParamDecl(decl);
        java.util.List<Type> types = decl.applyBindings(binding);
        typeCheckEqual(l, n, types);
    }

    public static void typeCheckEqual(SemanticConditionList l, ASTNode<?> n, java.util.List<Type> params) {
        org.abs_models.frontend.ast.List<PureExp> args = ((HasActualParams)n).getParams();
        if (params.size() != args.getNumChild()) {
            l.add(new TypeError(n, ErrorMessage.WRONG_NUMBER_OF_ARGS, params.size(), args.getNumChild()));
        } else {
            for (int i = 0; i < params.size(); i++) {
                Type argType = params.get(i);
                PureExp exp = args.getChild(i);
                int nerrors = l.getErrorCount();
                exp.typeCheck(l);
                if (nerrors == l.getErrorCount()) {
                    Type expType = exp.getType();
                    if (!expType.isAssignableTo(argType)) {
                        l.add(new TypeError(exp, ErrorMessage.TYPE_MISMATCH, exp.getType(), argType));
                    }
                }
            }
        }
    }

    private static void typeCheckEqualPattern(SemanticConditionList l, ConstructorPattern n, java.util.List<Type> params) {
        org.abs_models.frontend.ast.List<Pattern> args = n.getParams();
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

    public static void typeCheckDeltaClause(DeltaClause clause, Map<String,DeltaDecl> deltaNames, Set<String> definedFeatures, SemanticConditionList e) {

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

    public static void typeCheckProductDecl(ProductDecl prod,
            Map<String,Feature> featureNames,
            Set<String> prodNames,
            Map<String,DeltaDecl> deltaNames,
            SemanticConditionList e) {
        if (featureNames != null) {
            // Do the features exist in the PL declaration (and also check feature attributes)?
            Model m = prod.getModel();
            for (Feature f : prod.getProduct().getFeatures()) {
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

        // Check the right side of product expression that contains in prodNames
        Set<String> productNames = new HashSet<>();
        prod.getProductExpr().setRightSideProductNames(productNames);
        for (String productName : productNames) {
            if (!prodNames.contains(productName)) {
                e.add(new TypeError(prod, ErrorMessage.UNDECLARED_PRODUCT, productName));
            }
        }

        // Check solution from getProduct()
        if (prod.getProduct() != null) {
            java.util.List<String> errors = prod.getModel().instantiateCSModel().checkSolutionWithErrors(
                    prod.getProduct().getSolution(),
                    prod.getModel());

            if (!errors.isEmpty()) {
                String failedConstraints = "";
                for (String s: errors)
                    failedConstraints += "\n- " + s;

                e.add(new TypeError(prod, ErrorMessage.INVALID_PRODUCT, prod.getName(), failedConstraints));
            }
        }
    }

    /**
     * Look for all deltas that have a particular feature in the application condition --
     * up to the boolean madness that lies within AppConds (delta D(F.x) when ~F will
     * be checked when actually trying to flatten the product, I hope.
     */
    private static Collection<DeltaClause> findDeltasForFeature(Model m, Feature f) {
        Collection<DeltaClause> dcs = new ArrayList<>();
        for (int i = 0; i < m.getProductLine().getNumDeltaClause(); i++) {
            DeltaClause dc = m.getProductLine().getDeltaClause(i);
            if (dc.refersTo(f)) {
                dcs.add(dc);
            }
        }
        return dcs;
    }

    public static <T extends ASTNode<?>> java.util.List<Type> getTypes(org.abs_models.frontend.ast.List<T> params) {
        ArrayList<Type> res = new ArrayList<>();
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
                if (binding.containsKey(paramType)) {
                    Type prevArgType = binding.get(paramType);
                    if (prevArgType.isAssignableTo(argType)
                        && !argType.isAssignableTo(prevArgType))
                    {
                        // Replace, e.g., "Int" with "Rat".  If the two types
                        // do not match at all, we'll raise a type error
                        // later.
                        binding.put((TypeParameter)paramType, argType);
                    }
                } else {
                    binding.put((TypeParameter) paramType, argType);
                }
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

    public static void checkForDuplicateDecls(ModuleDecl mod, SemanticConditionList errors) {
        Map<KindedName, ResolvedName> duplicateNames = new HashMap<>();
        Map<KindedName, ResolvedName> names = getVisibleNames(mod, duplicateNames);
        for (KindedName n : duplicateNames.keySet()) {
            ResolvedName rn = names.get(n);
            ResolvedName origrn = duplicateNames.get(n);
            ErrorMessage msg = null;
            String location = "";
            Decl decl = null;
            if (origrn instanceof ResolvedDeclName) {
                decl = ((ResolvedDeclName)origrn).getDecl();
            } else if (origrn instanceof ResolvedAmbigiousName) {
                decl = ((AmbiguousDecl)((ResolvedAmbigiousName)origrn).getDecl()).getAlternative().get(0);
            }
            if (decl != null && !decl.getFileName().equals(Main.UNKNOWN_FILENAME)) {
                location = " at " + decl.getFileName() + ":" + decl.getStartLine() + ":" + decl.getStartColumn();
            }
            switch (n.getKind()) {
            case CLASS:
                msg = ErrorMessage.DUPLICATE_CLASS_NAME;
                break;
            case FUN:
                msg = ErrorMessage.DUPLICATE_FUN_NAME;
                break;
            case PARTIAL_FUN:
                msg = ErrorMessage.DUPLICATE_PARTIAL_FUN_NAME;
                break;
            case DATA_CONSTRUCTOR:
                msg = ErrorMessage.DUPLICATE_CONSTRUCTOR;
                break;
            case TYPE_DECL:
                msg = ErrorMessage.DUPLICATE_TYPE_DECL;
                break;
            case TRAIT_DECL:
                msg = ErrorMessage.DUPLICATE_TRAIT_NAME;
                break;
            case MODULE:
                assert false; // doesn't happen, no modules within modules
                break;
            default:
                assert false;   // detect if we added a new KindedName.Kind
                break;
            }
            errors.add(new TypeError(rn.getDecl(), msg, n.getName(), location));
        }
    }

    public static ResolvedMap getDefinedNames(ModuleDecl mod,
            Map<KindedName, ResolvedName> foundDuplicates) {
        ResolvedMap res = new ResolvedMap();
        ResolvedModuleName moduleName = new ResolvedModuleName(mod);

        for (Decl d : mod.getDeclList()) {
            ResolvedDeclName rn = new ResolvedDeclName(moduleName, d);
            if (res.containsKey(rn.getSimpleName()))
                foundDuplicates.put(rn.getSimpleName(), res.get(rn.getSimpleName()));
            res.put(rn.getSimpleName(), rn);
            res.put(rn.getQualifiedName(), rn);

            if (d instanceof DataTypeDecl) {
                DataTypeDecl dataDecl = (DataTypeDecl) d;
                for (DataConstructor c : dataDecl.getDataConstructors()) {
                    rn = new ResolvedDeclName(moduleName, c);
                    if (res.containsKey(rn.getSimpleName()))
                        foundDuplicates.put(rn.getSimpleName(), res.get(rn.getSimpleName()));
                    res.put(rn.getSimpleName(), rn);
                    res.put(rn.getQualifiedName(), rn);
                }
            } else if (d.isException()) {
                // FIXME unreachable
                ExceptionDecl ed = (ExceptionDecl) d;
                DataConstructor ec = ed.getDataConstructor(0);
                assert ec != null : ed.getName();
                if (ec.getName().equals(d.getName())) {
                    // should always be true, see CreateJastAddASTListener
                    rn = new ResolvedDeclName(moduleName, ec);
                    // If it's already in there, is it from the same location -- from stdlib?
                    ResolvedName tryIt = res.get(rn);
                    if (tryIt != null && tryIt.getDecl() != ed)
                        foundDuplicates.put(rn.getSimpleName(), tryIt);
                    else {
                        res.put(rn.getQualifiedName(), rn);
                    }
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
                    // statements like "import X;" lead to earlier type error;
                    // avoid calling lookupModule(null) here
                    if (!n.isSimple()) {
                        ModuleDecl md = mod.lookupModule(n.getModuleName());
                        if (md != null)
                            try {
                                res.addAllNames(md.getExportedNames(), n);
                            } catch (TypeCheckerException e) {} // NADA
                    }
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

    public static ResolvedMap getVisibleNames(ModuleDecl mod, Map<KindedName, ResolvedName> foundDuplicates) {
        ResolvedMap res = new ResolvedMap();
        ResolvedMap ownNames = getDefinedNames(mod, foundDuplicates);

        // add imported names:
        res.putAll(mod.getImportedNames());

        // Find shadowing entries
        for (KindedName entry : ownNames.keySet()) {
            if (res.containsKey(entry) && !ownNames.get(entry).equals(res.get(entry))) {
                foundDuplicates.put(entry, res.get(entry));
            }
        }

        // defined names hide imported names for the purpose of this method
        res.putAll(ownNames);
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

    public static void typeCheckBinary(SemanticConditionList e, Binary b, Type t) {
        b.getLeft().assertHasType(e, t);
        b.getRight().assertHasType(e, t);
        b.getLeft().typeCheck(e);
        b.getRight().typeCheck(e);
    }

    /**
     * checks whether the local variable v was already defined in the current function
     */
    public static void checkForDuplicatesOfVarDecl(SemanticConditionList e, VarDeclStmt v) {
        String varName = v.getVarDecl().getName();
        VarOrFieldDecl otherVar = v.lookupVarOrFieldName(varName , false);
        if (otherVar != null && v.inSameMethodOrBlock(otherVar)) {
            String location = "";
            if (!otherVar.getFileName().equals(Main.UNKNOWN_FILENAME)) {
                location = " at " + otherVar.getFileName()
                    + ":" + otherVar.getStartLine() + ":" + otherVar.getStartColumn();
            }
            e.add(new TypeError(v,ErrorMessage.VARIABLE_ALREADY_DECLARED, varName, location));
        }
    }

    /**
     * check a list of compilation units for duplicate module names, product names, delta names
     */
    public static void checkForDuplicateModulesAndDeltas(SemanticConditionList errors, Iterable<CompilationUnit> compilationUnits) {
        Map<String, ModuleDecl> seenModules = new HashMap<>();
        Map<String, DeltaDecl> seenDeltas = new HashMap<>();
        for (CompilationUnit u : compilationUnits) {
            for (ModuleDecl module : u.getModuleDecls()) {
                if (seenModules.containsKey(module.getName())) {
                    ModuleDecl prev = seenModules.get(module.getName());
                    String location = "";
                    if (!prev.getFileName().equals(Main.UNKNOWN_FILENAME)) {
                        location = " at " + prev.getFileName()
                            + ":" + prev.getStartLine() + ":" + prev.getStartColumn();
                    }
                    errors.add(new TypeError(module, ErrorMessage.DUPLICATE_MODULE_NAME,module.getName(), location));
                } else {
                    seenModules.put(module.getName(), module);
                }
            }
            for (DeltaDecl d : u.getDeltaDecls()) {
                if (seenModules.containsKey(d.getName())) {
                    ModuleDecl prev = seenModules.get(d.getName());
                    String location = "";
                    if (!prev.getFileName().equals(Main.UNKNOWN_FILENAME)) {
                        location = " at " + prev.getFileName()
                            + ":" + prev.getStartLine() + ":" + prev.getStartColumn();
                    }
                    errors.add(new TypeError(d, ErrorMessage.DELTA_USES_NAME_OF_MODULE, d.getName(), location));
                }
                if (seenDeltas.containsKey(d.getName())) {
                    DeltaDecl prev = seenDeltas.get(d.getName());
                    String location = "";
                    if (!prev.getFileName().equals(Main.UNKNOWN_FILENAME)) {
                        location = " at " + prev.getFileName()
                            + ":" + prev.getStartLine() + ":" + prev.getStartColumn();
                    }
                    errors.add(new TypeError(d, ErrorMessage.DUPLICATE_DELTA, d.getName(), location));
                } else {
                    seenDeltas.put(d.getName(), d);
                }
            }
        }
    }

    public static void checkForDuplicateProducts(SemanticConditionList errors, Iterable<CompilationUnit> compilationUnits) {
        Set<String> seen = new HashSet<>();
        for (CompilationUnit u : compilationUnits) {
            for (ProductDecl p : u.getProductDecls()) {
                if (!seen.add(p.getName()))
                    errors.add(new TypeError(p, ErrorMessage.DUPLICATE_PRODUCT, p.getName()));
            }
        }
    }

    /**
     * get all the alternative declarations of an ambiguous declaration formated as a list
     * which can be used in error messages
     * @param a
     * @return alternatives as string
     */
    public static String getAlternativesAsString(AmbiguousDecl a) {
        String result = "";
        for (Decl alternative : a.getAlternative()) {
            result += "\n * " + alternative.getQualifiedName() +  " (defined in " +
                    alternative.getFileName() + ", line " + alternative.getStartLine() + ")";
        }
        return result;
    }

    public static void checkDataTypeUse(SemanticConditionList e, DataTypeUse use) {
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

    public static void checkDefBeforeUse(SemanticConditionList e, VarOrFieldUse use) {
        if (use.getType().isUnknownType()) {
            e.add(new TypeError(use,ErrorMessage.NAME_NOT_RESOLVABLE, use.getName()));
        } else if (use instanceof FieldUse) {
            /* Check that fields are not used before they are defined, This
             * can happen when we are NOT inside a method, i.e., when
             * initialising a field with the value of another field.  We check
             * that the FieldDecl that contains our FieldUse is defined later
             * than the FieldDecl that defines our FieldUse.
             */
            // NOTE: the equivalent check for VarUse is done before, in the
            // "first" of two type-checking passes, and results in an "unknown
            // identifier" error message, therefore we only check fields here.
            if (use.getContextMethod() != null
                || use.uppermostParentOfType(InitBlock.class) != null) {
                // the field is used inside a method or the init block;
                // we’re safe
                return;
            }
            VarOrFieldDecl field_decl_where_defined = use.getDecl();
            if (field_decl_where_defined instanceof ParamDecl) {
                // it’s a class parameter not a field declaration: impossible
                // to use this before its definition
                return;
            }
            FieldDecl field_decl_where_used = use.uppermostParentOfType(FieldDecl.class);
            ClassDecl container = use.uppermostParentOfType(ClassDecl.class);
            if (container == null) {
                // we’re probably in a delta -- if there’s a problem the base
                // class will catch it after delta application
                return;
            }
            for (FieldDecl d : container.getFields()) {
                if (d.equals(field_decl_where_used)) {
                    // check this first, to catch the case "Bool x = x;"
                    e.add(new TypeError(use,
                                        ErrorMessage.FIELD_USE_BEFORE_DEFINITION,
                                        use.getName()));
                    return;
                } else if (d.equals(field_decl_where_defined)) {
                    // definition before use; ok
                    return;
                }
            }
        }
    }
}
