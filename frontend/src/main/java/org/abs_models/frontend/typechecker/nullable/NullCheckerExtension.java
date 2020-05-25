package org.abs_models.frontend.typechecker.nullable;

import org.abs_models.frontend.analyser.ErrorMessage;
import org.abs_models.frontend.analyser.SemanticWarning;
import org.abs_models.frontend.analyser.TypeError;
import org.abs_models.frontend.ast.*;
import org.abs_models.frontend.typechecker.Type;
import org.abs_models.frontend.typechecker.TypeAnnotation;
import org.abs_models.frontend.typechecker.ext.AdaptDirection;
import org.abs_models.frontend.typechecker.ext.DefaultTypeSystemExtension;

public class NullCheckerExtension extends DefaultTypeSystemExtension {
    public static String NULLABLE_KEY = "NULLABLE_KEY";
    private NullableType defaultType = NullableType.Nullable;

    public NullCheckerExtension(Model m) {
        super(m);
    }

    public NullCheckerExtension(Model m, NullableType defaultType) {
        super(m);
        this.defaultType = defaultType;
    }

    @Override
    public void checkInterfaceDecl(InterfaceDecl decl) {
        for (MethodSig ms : decl.getBodyList()) {
            checkMethodSig(ms);
        }
    }

    public void checkMethodSig(MethodSig ms) {
        setAnnotatedType(ms.getType());
        for (ParamDecl p : ms.getParams()) {
            setAnnotatedType(p.getType());
        }
    }

    @Override
    public void checkAssignStmt(AssignStmt s) {
        annotateExp(s.getValue());
    }

    @Override
    public void checkAssignable(Type adaptTo, AdaptDirection dir, Type rht, Type lht, ASTNode<?> n) {
        NullableType rnt = getNullableTypeDefault(rht);
        NullableType lnt = getNullableTypeDefault(lht);
        if (!rnt.assignableTo(lnt)) {
            errors.add(new TypeError(n, ErrorMessage.NULLABLE_TYPE_MISMATCH, rnt.toString(), lnt.toString()));
        }
    }

    @Override
    public void checkMethodCall(Call call) {
        // TODO
    }

    @Override
    public void checkExpressionStmt(ExpressionStmt expressionStmt) {
        annotateExp(expressionStmt.getExp());
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
        NullableType nt = getNullableTypeDefault(ms.getType());
        Type mt = ms.getType();

        if (!method.hasReturnStmt()) return;
        Stmt last = method.getBlock().getStmt(method.getBlock().getNumStmt() - 1);
        ReturnStmt rs = (ReturnStmt) last;
        Exp e = rs.getRetExp();
        annotateExp(e);
    }

    @Override
    public void checkVarDeclStmt(VarDeclStmt varDeclStmt) {
        VarDecl d = varDeclStmt.getVarDecl();
        Type t = d.getType();
        setAnnotatedType(t);
        if (!shouldHaveNullableType(t)) return;
        NullableType nt = getNullableTypeDefault(t);
        if (nt == NullableType.NonNull && !d.hasInitExp()) {
            errors.add(new TypeError(varDeclStmt, ErrorMessage.NULLABLE_TYPE_MISMATCH, NullableType.Null.toString(), nt.toString()));
        }
        if (d.hasInitExp()) {
            Exp e = d.getInitExp();
            annotateExp(e);
        }
    }

    @Override
    public void checkOverride(MethodSig impl, MethodSig overriden) {
        // TODO
    }

    @Override
    public void annotateType(Type t, ASTNode<?> originatingNode, ASTNode<?> typeNode) {
        setAnnotatedType(t);
    }

    public NullableType annotateExp(Exp e) {
        Type t = e.getType();
        assert !e.nonNull() || !e.isNull();
        if (!shouldHaveNullableType(t)) return null;
        NullableType nt = null;
        if (e.nonNull()) {
            nt = NullableType.NonNull;
        }
        if (e.isNull()) {
            nt = NullableType.Null;
        }
        setNullableType(t, nt);
        return nt;
    }

    private void setAnnotatedType(Type t) {
        try {
            NullableType nt = getNullableTypeFromAnnotation(t);
            setNullableType(t, nt);
        } catch (NullCheckerException e) {
            errors.add(e.getTypeError());
        }
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

    private static boolean shouldHaveNullableType(Type t) {
        return t.isFutureType() || t.isReferenceType();
    }
}
