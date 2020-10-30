package org.abs_models.frontend.typechecker.nullable;

import org.abs_models.frontend.analyser.BitVec;
import org.abs_models.frontend.analyser.ErrorMessage;
import org.abs_models.frontend.analyser.SemanticWarning;
import org.abs_models.frontend.analyser.TypeError;
import org.abs_models.frontend.ast.*;
import org.abs_models.frontend.typechecker.Type;
import org.abs_models.frontend.typechecker.TypeAnnotation;
import org.abs_models.frontend.typechecker.ext.AdaptDirection;
import org.abs_models.frontend.typechecker.ext.DefaultTypeSystemExtension;

import java.util.ArrayList;
import java.util.List;

public class NullCheckerExtension extends DefaultTypeSystemExtension {
    public static String NULLABLE_KEY = "NULLABLE_KEY";
    private NullableType defaultType = NullableType.Nullable;
    private boolean warnAboutMissingAnnotation = false;
    private boolean checkNullCall = false;

    public NullCheckerExtension(Model m) {
        super(m);
    }

    public NullCheckerExtension(Model m, NullableType defaultType) {
        super(m);
        this.defaultType = defaultType;
    }

    public void setDefaultType(NullableType nt) {
        defaultType = nt;
    }

    public void setWarnAboutMissingAnnotation(boolean v) {
        warnAboutMissingAnnotation = v;
    }

    public void checkNullCall() {
        this.checkNullCall = true;
    }

    @Override
    public void checkClassDecl(ClassDecl decl) {
        for (ParamDecl p : decl.getParams()) {
            setAnnotatedType(p.getType(), p, p.getAccess());
        }

        List<FieldDecl> nonNullFields = new ArrayList<>();
        for (FieldDecl f : decl.getFields()) {
            setAnnotatedType(f.getType(), f, f.getAccess());
            if (f.nonNull() && !f.hasInitExp()) {
                nonNullFields.add(f);
            }
        }

        if (nonNullFields.isEmpty()) return;

        if (!decl.hasInitBlock()) {
            errors.add(new TypeError(
                nonNullFields.get(0),
                ErrorMessage.NULLABLE_TYPE_MISMATCH,
                NullableType.NonNull.toString(),
                NullableType.Nullable.toString()));
            return;
        }
        // Get all fields that are nonNull at the end of the init block
        BitVec<VarOrFieldDecl> out = decl.getInitBlock().exit().nonNull_in();
        for (FieldDecl f : nonNullFields) {
            if (!out.contains(f)) {
                errors.add(new TypeError(
                    f,
                    ErrorMessage.NULLABLE_TYPE_MISMATCH,
                    NullableType.NonNull.toString(),
                    NullableType.Nullable.toString()));
                // Only report one error
                return;
            }
        }
    }

    @Override
    public void checkInterfaceDecl(InterfaceDecl decl) {
        for (MethodSig ms : decl.getBodyList()) {
            checkMethodSig(ms);
        }
    }

    public void checkMethodSig(MethodSig ms) {
        setAnnotatedType(ms.getType(), ms, ms.getReturnType());
        for (ParamDecl p : ms.getParams()) {
            setAnnotatedType(p.getType(), p, p.getAccess());
        }
    }

    @Override
    public void checkEq(Type lt, Type t, ASTNode<?> origin) {
        System.out.println(origin);
        // Ideally, we would use the behavioral nullable types for the parameter
        // but we can't because the type we are checking may be a type parameter,
        // where we don't have a corresponding expression
        NullableType lnt = getNullableTypeDefault(lt);
        NullableType rnt = getNullableTypeDefault(t);
        checkAssignable(lnt, rnt, origin);
        checkAssignable(rnt, lnt, origin);
    }

    @Override
    public void checkAssignable(Type l, Exp r, ASTNode<?> n) {
        checkAssignable(getNullableTypeDefault(l), r.getNullableType(), n);
    }

    public void checkAssignable(NullableType l, NullableType r, ASTNode<?> n) {
        if (!r.assignableTo(l)) {
            errors.add(new TypeError(n, ErrorMessage.NULLABLE_TYPE_MISMATCH, r.toString(), l.toString()));
        }
    }

    @Override
    public void checkMethodCall(Call call) {
        PureExp callee = call.getCallee();
        if (checkNullCall && callee.isNull()) {
            errors.add(new TypeError(call, ErrorMessage.NULLABLETYPE_NULLCALL, callee.toString()));
        }
    }

    @Override
    public void checkAssertStmt(AssertStmt assertStmt) {
        PureExp c = assertStmt.getCondition();
        checkCondition(c);
    }

    @Override
    public void checkWhileStmt(WhileStmt whileStmt) {
        PureExp c = whileStmt.getCondition();
        checkCondition(c);
    }

    @Override
    public void checkIfStmt(IfStmt ifStmt) {
        PureExp c = ifStmt.getCondition();
        checkCondition(c);
    }

    public void checkCondition(PureExp cond) {
        VarOrFieldDecl d = cond.testsNotNull();
        if (d != null) {
            if (cond.nonNull_in().contains(d)) {
                errors.add(new SemanticWarning(cond, ErrorMessage.NULLABLE_TYPE_COND_ALWAYS_SAME, "true"));
            }
            if (cond.null_in().contains(d)) {
                errors.add(new SemanticWarning(cond, ErrorMessage.NULLABLE_TYPE_COND_ALWAYS_SAME, "false"));
            }
            return;
        }
        d = cond.testsNull();
        if (d != null) {
            if (cond.nonNull_in().contains(d)) {
                errors.add(new SemanticWarning(cond, ErrorMessage.NULLABLE_TYPE_COND_ALWAYS_SAME, "false"));
            }
            if (cond.null_in().contains(d)) {
                errors.add(new SemanticWarning(cond, ErrorMessage.NULLABLE_TYPE_COND_ALWAYS_SAME, "true"));
            }
        }
    }

    @Override
    public void checkMethodImpl(MethodImpl method) {
        MethodSig ms = method.getMethodSig();
        checkMethodSig(ms);
    }

    @Override
    public void checkVarDeclStmt(VarDeclStmt varDeclStmt) {
        VarDecl d = varDeclStmt.getVarDecl();
        Type t = d.getType();
        setAnnotatedType(t, varDeclStmt, d.getAccess());
        if (!shouldHaveNullableType(t)) return;
        NullableType nt = getNullableTypeDefault(t);
        if (nt == NullableType.NonNull && !d.hasInitExp()) {
            errors.add(new TypeError(varDeclStmt, ErrorMessage.NULLABLE_TYPE_MISMATCH, NullableType.Null.toString(), nt.toString()));
        }
    }

    @Override
    public void checkOverride(MethodSig impl, MethodSig overriden) {
        Type expectedReturnType = overriden.getType();
        Type actualReturnType = impl.getType();
        NullableType expected = getNullableTypeDefault(expectedReturnType);
        NullableType actual = getNullableTypeDefault(actualReturnType);
        checkAssignable(expected, actual, impl);

        for (int i = 0; i < overriden.getNumParam(); i++) {
            Type tExpected = overriden.getParam(i).getType();
            Type tActual = impl.getParam(i).getType();
            NullableType nte = getNullableTypeDefault(tExpected);
            NullableType nta = getNullableTypeDefault(tActual);
            checkAssignable(nta, nte, impl);
        }
    }

    @Override
    public void annotateType(Type t, ASTNode<?> originatingNode, ASTNode<?> typeNode) {
        setAnnotatedType(t, originatingNode, typeNode);
    }

    private void setAnnotatedType(Type t, ASTNode<?> originatingNode, ASTNode<?> typeNode) {
        try {
            NullableType nt = getNullableTypeFromAnnotation(t);
            if (shouldWarn(t, nt, originatingNode, typeNode)) {
                errors.add(new SemanticWarning(originatingNode, ErrorMessage.NULLABLETYPE_MISSING_ANNOTATION, new String[0]));
            }
            if (shouldHaveNullableType(t))
                setNullableType(t, nt);
        } catch (NullCheckerException e) {
            errors.add(e.getTypeError());
        }
    }

    private boolean shouldWarn(Type t, NullableType nt, ASTNode<?> originatingNode, ASTNode<?> typeNode) {
        if (!warnAboutMissingAnnotation || nt != null || !shouldHaveNullableType(t)) return false;

        return !(originatingNode instanceof NullExp)
            && !(originatingNode instanceof ThisExp)
            && !(originatingNode instanceof NewExp)
            && !(originatingNode instanceof TypeUse)
            && !(originatingNode instanceof Call)
            && !(originatingNode instanceof CaseExp)
            && !(originatingNode instanceof IfExp)
            && !(originatingNode instanceof VarDeclStmt);
    }

    public static NullableType getNullableTypeFromAnnotation(Type t) {
        NullableType res = null;
        for (TypeAnnotation an : t.getTypeAnnotations()) {
            if (an.getType().getQualifiedName().equals("ABS.StdLib.NullableType")) {
                DataConstructorExp de = (DataConstructorExp) an.getValue();
                String name = de.getDecl().getName();
                if (res != null) {
                    throw new NullCheckerException(new TypeError(an.getValue(), ErrorMessage.NULLABLE_TYPE_MULTIPLE, new String[0]));
                } else {
                    if (!shouldHaveNullableType(t)) {
                        throw new NullCheckerException(new TypeError(an.getValue(), ErrorMessage.NULLABLE_TYPE_ONLY_REF_OR_FUT, t.toString()));
                    }
                    res = NullableType.fromName(name);
                }
            }
        }
        return res;
    }

    public NullableType defaultIfNull(NullableType nt) {
        if (nt == null) {
            return defaultType;
        }
        return nt;
    }

    private void setNullableType(Type t, NullableType nt) {
        t.addMetaData(NULLABLE_KEY, defaultIfNull(nt));
    }

    public NullableType getNullableTypeDefault(Type t) {
        return defaultIfNull(getNullableType(t));
    }

    public static NullableType getNullableType(Type t) {
        return (NullableType) t.getMetaData(NULLABLE_KEY);
    }

    public static boolean shouldHaveNullableType(Type t) {
        return t.isFutureType() || t.isReferenceType();
    }

    public static org.abs_models.frontend.ast.List<Annotation> getAnnotations(Type t) {
        org.abs_models.frontend.ast.List<Annotation> as = new org.abs_models.frontend.ast.List<>();
        NullableType nt = NullCheckerExtension.getNullableTypeFromAnnotation(t);
        if (nt == null && NullCheckerExtension.shouldHaveNullableType(t)) {
            nt = NullableType.Nullable;
        }
        if (nt != null) {
            // We may have to add an annotation for nullable types
            as.add(nt.toAnnotation());
        }
        return as;
    }
}
