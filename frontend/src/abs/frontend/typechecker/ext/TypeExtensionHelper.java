/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.typechecker.ext;

import java.util.ArrayList;
import java.util.HashMap;

import abs.frontend.analyser.SemanticConditionList;
import abs.frontend.ast.*;
import abs.frontend.typechecker.BoundedType;
import abs.frontend.typechecker.DataTypeType;
import abs.frontend.typechecker.KindedName;
import abs.frontend.typechecker.Type;
import abs.frontend.typechecker.TypeParameter;
import abs.frontend.typechecker.KindedName.Kind;

public class TypeExtensionHelper implements TypeSystemExtension {
    private java.util.List<TypeSystemExtension> obs = new ArrayList<TypeSystemExtension>();

    private void registerDefaultExtensions(Model m) {
        register(new ClassKindTypeExtension(m));
        register(new FinalAnnotationTypeExtension(m));
        register(new AtomicityChecker(m));
        register(new NewExpressionChecker(m));
        register(new DeadlineChecker(m));
        register(new SizeAnnotationChecker(m));
        register(new CostAnnotationChecker(m));
        register(new SchedulerChecker(m));
        register(new MainBlockChecker(m));
        register(new HttpExportChecker(m));
    }

    public TypeSystemExtension getFirstRegisteredTypeExtension(Class<?> clazz) {
        for (TypeSystemExtension tse : obs) {
            if (tse.getClass().equals(clazz)) {
                return tse;
            }
        }
        return null;
    }

    public void setSemanticConditionList(SemanticConditionList s) {
        for (TypeSystemExtension tse : obs) {
            tse.setSemanticConditionList(s);
        }
    }

    public void typeCheckStarted(Model m, SemanticConditionList e) {
        registerDefaultExtensions(m);
        setSemanticConditionList(e);
    }

    public void register(TypeSystemExtension tse) {
        obs = new ArrayList<TypeSystemExtension>(obs);
        obs.add(tse);
    }

    public void unregister(TypeSystemExtension tse) {
        obs = new ArrayList<TypeSystemExtension>(obs);
        obs.remove(tse);
    }

    public void clearTypeSystemExtensions() {
        obs = new ArrayList<TypeSystemExtension>();
    }

    public java.util.List<TypeSystemExtension> getTypeSystemExtensionList() {
        return new ArrayList<TypeSystemExtension>(obs);
    }

    public void checkMethodCall(Call call) {
        for (TypeSystemExtension tse : obs) {
            tse.checkMethodCall(call);
        }
    }

    @Override
    public void checkOverride(MethodSig impl, MethodSig overriden) {
        for (TypeSystemExtension tse : obs) {
            tse.checkOverride(impl,overriden);
        }

        assert overriden.getParent().getParent() instanceof InterfaceDecl;
        Type expectedReturnType = overriden.getType();
        Type actualReturnType = impl.getType();
        checkAssignable(actualReturnType, expectedReturnType, impl);

        for (int i = 0; i < overriden.getNumParam(); i++) {
            Type texpected = overriden.getParam(i).getType();
            Type tactual = impl.getParam(i).getType();
            checkAssignable(texpected, tactual, impl);
        }
    }

    public void checkNewExp(NewExp e) {
        for (TypeSystemExtension tse : obs) {
            tse.checkNewExp(e);
        }

        ClassDecl d = (ClassDecl) e.lookup(new KindedName(Kind.CLASS,e.getClassName()));
        checkAssignable(e.getType(),d,e);
    }

    @Override
    public void checkGetExp(GetExp e) {
        for (TypeSystemExtension tse : obs) {
            tse.checkGetExp(e);
        }
    }

    public void checkAssignStmt(AssignStmt s) {
        for (TypeSystemExtension tse : obs) {
            tse.checkAssignStmt(s);
        }

        checkAssignable(s.getValue().getType(),s.getVar().getType(), s);
    }

    public void checkReturnStmt(ReturnStmt s) {
        for (TypeSystemExtension tse : obs) {
            tse.checkReturnStmt(s);
        }

        MethodImpl m = s.getContextMethod();

        if (m == null) {
            return;
        }

        checkAssignable(s.getRetExp().getType(), m.getMethodSig().getType(), s);
    }

    public void checkAssignable(Type callee, HasParams params, ASTNode<?> n) {
        java.util.List<Type> paramsTypes = params.getTypes();
        for (int i = 0; i < paramsTypes.size(); i++) {
            Type argType = paramsTypes.get(i);
            PureExp exp = ((HasActualParams)n).getParams().getChild(i);
            checkAssignable(callee, AdaptDirection.TO, exp.getType(), argType, n);
        }
    }

    public void checkAssignable(Type rht, Type lht, ASTNode<?> n) {
        checkAssignable(null, null, rht, lht, n);
    }

    public void checkAssignable(Type adaptTo, AdaptDirection dir, Type rht, Type lht, ASTNode<?> n) {
        rht = resolveBoundedType(rht);

        if (lht.isDataType() && rht.isDataType()) {
            DataTypeType dtl = (DataTypeType) lht;
            DataTypeType dtr = (DataTypeType) rht;
            if (dtl.hasTypeArgs() && dtr.hasTypeArgs() && dtl.getTypeArgs().size() == dtr.getTypeArgs().size()) {
                for (int i = 0; i < dtl.getTypeArgs().size(); i++) {
                    checkAssignable(adaptTo, dir, dtr.getTypeArg(i), dtl.getTypeArg(i), n);
                }
            }
        }
        if (lht.isReferenceType() && rht.isReferenceType()) {
            for (TypeSystemExtension tse : obs) {
                tse.checkAssignable(adaptTo, dir, rht, lht, n);
            }
        }
    }

    public void annotateType(Type t, ASTNode<?> originatingNode) {
        annotateType(t, originatingNode, null);
    }

    public void annotateType(Type t, ASTNode<?> originatingNode, ASTNode<?> typeNode) {
        if (t.isDataType()) {
            DataTypeType dt = (DataTypeType) t;
            if (dt.hasTypeArgs()) {

                ParametricDataTypeUse pu = null;
                // typeNode maybe a type synonym
                if (typeNode instanceof ParametricDataTypeUse)
                    pu = (ParametricDataTypeUse) typeNode;
                int i = 0;
                for (Type ta : dt.getTypeArgs()) {
                    ASTNode<?> childTypeNode = null;
                    if (pu != null)
                        childTypeNode = pu.getParam(i);
                    annotateType(ta, originatingNode, childTypeNode);
                    i++;
                }
            }
        }
        if (t.isReferenceType() || t.isNullType()) {
            for (TypeSystemExtension tse : obs) {
                tse.annotateType(t, originatingNode, typeNode);
            }
        }
    }

    public void checkCaseExp(CaseExp e) {
        Type t = e.getType();
        for (CaseBranch b : e.getBranchs()) {
            checkAssignable(b.getType(),t, b.getRight());
        }
    }

    public void checkIfExp(IfExp e) {
    }


    public void checkDataConstructorExp(DataConstructorExp e) {
        DataConstructor decl = (DataConstructor) e.getDecl();
        if (decl.getDataTypeDecl() instanceof ParametricDataTypeDecl) {
            HashMap<TypeParameter, Type> map = new HashMap<TypeParameter, Type>();
            for (int i = 0; i < decl.getNumConstructorArg(); i++) {
                Type rht = e.getParam(i).getType();
                Type arg = decl.getConstructorArg(i).getType();
                checkTypeParameter(map, rht, arg, e.getParam(i));
            }
        }
    }


    public void checkFnApp(FnApp f) {
        FunctionDecl decl = (FunctionDecl) f.getDecl();
        if (decl instanceof ParametricFunctionDecl) {
            HashMap<TypeParameter, Type> map = new HashMap<TypeParameter, Type>();
            for (int i = 0; i < decl.getNumParam(); i++) {
                Type t = f.getParam(i).getType();
                Type arg = decl.getParam(i).getType();
                checkTypeParameter(map, t, arg, f.getParam(i));
            }
        } else {
            checkAssignable(null, decl, f);
        }
    }


    private void checkTypeParameter(HashMap<TypeParameter, Type> map, Type rht, Type lht, ASTNode<?> origin) {
        rht = resolveBoundedType(rht);
        if (rht.isBoundedType())
            return;
        if (lht.isTypeParameter() && rht.isReferenceType()) {
            TypeParameter typeParam = (TypeParameter) lht;
            Type lt = map.get(typeParam);
            if (lt != null) {
                checkEq(lt,rht,origin);
            } else {
                map.put(typeParam, rht);
            }
        } else if (lht.isDataType()) {
            DataTypeType argdt = (DataTypeType) lht;
            if (argdt.hasTypeArgs()) {
                DataTypeType dt = (DataTypeType)rht;
                for (int i = 0; i < dt.numTypeArgs(); i++) {
                     checkTypeParameter(map,dt.getTypeArg(i),argdt.getTypeArg(i),origin);
                }
            }
        } else if (lht.isReferenceType()) {
            checkEq(lht,rht,origin);
        }
    }

    private Type resolveBoundedType(Type t) {
        if (t.isBoundedType()) {
            BoundedType bt = (BoundedType)t;
            if (bt.hasBoundType())
                t = bt.getBoundType();
        }
        return t;
    }

    public void checkEq(Type lht, Type rht, ASTNode<?> origin) {

        if (lht.isDataType() && rht.isDataType()) {
            DataTypeType dtl = (DataTypeType) lht;
            DataTypeType dtr = (DataTypeType) rht;
            if (dtl.hasTypeArgs() && dtr.hasTypeArgs() && dtl.getTypeArgs().size() == dtr.getTypeArgs().size()) {
                for (int i = 0; i < dtl.getTypeArgs().size(); i++) {
                    checkEq(dtr.getTypeArg(i), dtl.getTypeArg(i), origin);
                }
            }
        }
        if (lht.isReferenceType() && rht.isReferenceType()) {
            for (TypeSystemExtension tse : obs) {
                tse.checkEq(rht, lht, origin);
            }
        }
    }

    public void finished() {
        for (TypeSystemExtension tse : obs) {
            tse.finished();
        }
    }

    @Override
    public void checkClassDecl(ClassDecl decl) {
        for (TypeSystemExtension tse : obs) {
            tse.checkClassDecl(decl);
        }
    }

    @Override
    public void checkInterfaceDecl(InterfaceDecl decl) {
        for (TypeSystemExtension tse : obs) {
            tse.checkInterfaceDecl(decl);
        }
    }

    @Override
    public void checkMethodImpl(MethodImpl method) {
        for (TypeSystemExtension tse : obs) {
            tse.checkMethodImpl(method);
        }
    }

    @Override
    public void checkStmt(Stmt s) {
        for (TypeSystemExtension tse : obs) {
            tse.checkStmt(s);
        }
    }

    @Override
    public void checkAssertStmt(AssertStmt assertStmt) {
        for (TypeSystemExtension tse : obs) {
            tse.checkAssertStmt(assertStmt);
        }
    }

    @Override
    public void checkAwaitStmt(AwaitStmt awaitStmt) {
        for (TypeSystemExtension tse : obs) {
            tse.checkAwaitStmt(awaitStmt);
        }
    }

    @Override
    public void checkBlock(Block block) {
        for (TypeSystemExtension tse : obs) {
            tse.checkBlock(block);
        }
    }

    @Override
    public void checkExpressionStmt(ExpressionStmt expressionStmt) {
        for (TypeSystemExtension tse : obs) {
            tse.checkExpressionStmt(expressionStmt);
        }
    }

    @Override
    public void checkIfStmt(IfStmt ifStmt) {
        for (TypeSystemExtension tse : obs) {
            tse.checkIfStmt(ifStmt);
        }
    }

    @Override
    public void checkSuspendStmt(SuspendStmt suspendStmt) {
        for (TypeSystemExtension tse : obs) {
            tse.checkSuspendStmt(suspendStmt);
        }
    }

    @Override
    public void checkVarDeclStmt(VarDeclStmt varDeclStmt) {
        for (TypeSystemExtension tse : obs) {
            tse.checkVarDeclStmt(varDeclStmt);
        }
    }

    @Override
    public void checkDurationStmt(DurationStmt durationStmt) {
        for (TypeSystemExtension tse : obs) {
            tse.checkDurationStmt(durationStmt);
        }
    }

    @Override
    public void checkWhileStmt(WhileStmt whileStmt) {
        for (TypeSystemExtension tse : obs) {
            tse.checkWhileStmt(whileStmt);
        }
    }

    @Override
    public void checkModel(Model model) {
        for (TypeSystemExtension tse : obs) {
            tse.checkModel(model);
        }
    }        

    public void registerAll(java.util.List<TypeSystemExtension> curr) {
        obs = new ArrayList<TypeSystemExtension>(obs);
        obs.addAll(curr);
    }
}
