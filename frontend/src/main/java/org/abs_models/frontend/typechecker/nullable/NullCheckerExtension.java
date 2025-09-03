package org.abs_models.frontend.typechecker.nullable;

import org.abs_models.frontend.analyser.BitVec;
import org.abs_models.frontend.analyser.ErrorMessage;
import org.abs_models.frontend.analyser.SemanticWarning;
import org.abs_models.frontend.analyser.TypeError;
import org.abs_models.frontend.ast.*;
import org.abs_models.frontend.typechecker.*;
import org.abs_models.frontend.typechecker.ext.DefaultTypeSystemExtension;
import com.google.common.collect.ImmutableMap;
import org.jspecify.annotations.NonNull;

import javax.annotation.Nonnull;
import java.util.ArrayList;
import java.util.List;

/**
 * Checks whether all assignments, calls, etc. adhere to the declared nullable types
 */
public class NullCheckerExtension extends DefaultTypeSystemExtension {
    public static String NULLABLE_KEY = "NULLABLE_KEY";
    private PrimitiveNullableType defaultType = PrimitiveNullableType.Nullable;
    private boolean warnAboutMissingAnnotation = false;
    private boolean checkNullCall = false;

    public NullCheckerExtension(Model m) {
        super(m);
    }

    public NullCheckerExtension(Model m, PrimitiveNullableType defaultType) {
        super(m);
        this.defaultType = defaultType;
    }

    public void setDefaultType(PrimitiveNullableType nt) {
        defaultType = nt;
    }

    public void setWarnAboutMissingAnnotation(boolean v) {
        warnAboutMissingAnnotation = v;
    }

    @Override
    public void checkClassDecl(ClassDecl decl) {
        for (ParamDecl p : decl.getParams()) {
            setAnnotatedType(p.getType(), p);
        }

        List<FieldDecl> nonNullFields = new ArrayList<>();
        for (FieldDecl f : decl.getFields()) {
            setAnnotatedType(f.getType(), f);
            if (f.getNullableType() == PrimitiveNullableType.Nonnull && !f.hasInitExp()) {
                nonNullFields.add(f);
            }
        }

        if (nonNullFields.isEmpty()) return;

        if (!decl.hasInitBlock()) {
            errors.add(new TypeError(
                nonNullFields.get(0),
                ErrorMessage.NULLABLE_TYPE_MISMATCH,
                PrimitiveNullableType.Nonnull.toString(),
                PrimitiveNullableType.Nullable.toString()));
            return;
        }
        // Get all fields that are nonNull at the end of the init block
        ImmutableMap<@NonNull VarOrFieldDecl, @NonNull NullableType> out =
            decl.getInitBlock().exit().nullableTypes_out();
        for (FieldDecl f : nonNullFields) {
            if (out.get(f) != PrimitiveNullableType.Nonnull) {
                errors.add(new TypeError(
                    f,
                    ErrorMessage.NULLABLE_TYPE_MISMATCH,
                    PrimitiveNullableType.Nonnull.toString(),
                    PrimitiveNullableType.Nullable.toString()));
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
        setAnnotatedType(ms.getType(), ms);
        for (ParamDecl p : ms.getParams()) {
            setAnnotatedType(p.getType(), p);
        }
    }

    @Override
    public void checkEq(Type lt, Type t, ASTNode<?> origin) {
        // Ideally, we would use the behavioral nullable types for the parameter
        // but we can't because the type we are checking may be a type parameter,
        // where we don't have a corresponding expression
        NullableType lnt = getNullableTypeDefault(lt);
        NullableType rnt = getNullableTypeDefault(t);
        checkAssignable(lnt, rnt, origin);
        checkAssignable(rnt, lnt, origin);
    }

    @Override
    public void checkAssignableBehaviorType(Type l, Exp r, ASTNode<?> n) {
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
        if (checkNullCall && callee.getNullableType() == PrimitiveNullableType.Null) {
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
            if (cond.nullableTypes_in().get(d) == PrimitiveNullableType.Nonnull) {
                errors.add(new SemanticWarning(cond, ErrorMessage.NULLABLE_TYPE_COND_ALWAYS_SAME, "true"));
            }
            if (cond.nullableTypes_in().get(d) == PrimitiveNullableType.Null) {
                errors.add(new SemanticWarning(cond, ErrorMessage.NULLABLE_TYPE_COND_ALWAYS_SAME, "false"));
            }
            return;
        }
        d = cond.testsNull();
        if (d != null) {
            if (cond.nullableTypes_in().get(d) == PrimitiveNullableType.Nonnull) {
                errors.add(new SemanticWarning(cond, ErrorMessage.NULLABLE_TYPE_COND_ALWAYS_SAME, "false"));
            }
            if (cond.nullableTypes_in().get(d) == PrimitiveNullableType.Null) {
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
        setAnnotatedType(t, varDeclStmt);
        if (!shouldHaveNullableType(t)) return;
        NullableType nt = getNullableTypeDefault(t);
        if (nt == PrimitiveNullableType.Nonnull && !d.hasInitExp()) {
            errors.add(new TypeError(varDeclStmt, ErrorMessage.NULLABLE_TYPE_MISMATCH, PrimitiveNullableType.Null.toString(), nt.toString()));
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
        setAnnotatedType(t, originatingNode);
    }

    private void setAnnotatedType(Type t, ASTNode<?> originatingNode) {
        try {
            PrimitiveNullableType nt = getNullableTypeFromAnnotation(t);
            if (shouldWarn(t, nt, originatingNode)) {
                errors.add(new SemanticWarning(originatingNode, ErrorMessage.NULLABLETYPE_MISSING_ANNOTATION, new String[0]));
            }
            if (shouldHaveNullableType(t))
                setNullableType(t, nt);
        } catch (NullCheckerException e) {
            errors.add(e.getTypeError());
        }
    }

    private boolean shouldWarn(Type t, NullableType nt, ASTNode<?> originatingNode) {
        if (!warnAboutMissingAnnotation || nt != null || !shouldHaveNullableType(t)) return false;

        return !(originatingNode instanceof NullExp)
            && !(originatingNode instanceof ThisExp)
            && !(originatingNode instanceof DestinyExp)
            && !(originatingNode instanceof NewExp)
            && !(originatingNode instanceof TypeUse)
            && !(originatingNode instanceof Call)
            && !(originatingNode instanceof CaseExp)
            && !(originatingNode instanceof IfExp)
            && !(originatingNode instanceof VarDeclStmt);
    }

    public static PrimitiveNullableType getNullableTypeFromAnnotation(Type t) {
        PrimitiveNullableType res = null;
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
                    res = PrimitiveNullableType.fromName(name);
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

    private void setNullableType(Type t, PrimitiveNullableType nt) {
        t.addMetaData(NULLABLE_KEY, defaultIfNull(nt));
    }

    public NullableType getNullableTypeDefault(Type t) {
        return defaultIfNull(getNullableType(t));
    }

    public static ImmutableMap<@NonNull TypeParameterDecl, @NonNull NullableType> nullableTypeMapping(DataTypeType dtt, DataTypeNullableType dnt) {
        ImmutableMap.Builder<@NonNull TypeParameterDecl, @NonNull NullableType> builder = new ImmutableMap.Builder<>();
        for (int i = 0; i < dtt.numTypeArgs(); i++) {
            builder.put(((TypeParameter) dtt.getTypeArg(i)).getDecl(), dnt.getParam(i));
        }
        return builder.build();
    }

    public static NullableType getNullableType(Type t) {
        var annotated = (PrimitiveNullableType) t.getMetaData(NULLABLE_KEY);
        if (annotated != null)
            return annotated;
        if (t.isReferenceType())
            return PrimitiveNullableType.Nullable;
        if (t.isDataType()) {
            var dtt = (DataTypeType) t;
            List<NullableType> list = new ArrayList<>(dtt.numTypeArgs());
            for (var arg : dtt.getTypeArgs()) {
                list.add(getNullableType(arg));
            }
            if (dtt.isDestinyType()) {
                list = List.of(new DataTypeNullableType(list));
            }
            return new DataTypeNullableType(list);
        }
        if (t.isBoundedType()) {
            var bt = (BoundedType) t;
            if (bt.hasBoundType()) return getNullableType(bt.getBoundType());
            return PrimitiveNullableType.Unknown;
        } else return PrimitiveNullableType.NonApplicable;
    }

    public static boolean shouldHaveNullableType(Type t) {
        return t.isReferenceType();
    }

    public static org.abs_models.frontend.ast.List<Annotation> getAnnotations(Type t) {
        org.abs_models.frontend.ast.List<Annotation> as = new org.abs_models.frontend.ast.List<>();
        PrimitiveNullableType nt = NullCheckerExtension.getNullableTypeFromAnnotation(t);
        if (nt == null && NullCheckerExtension.shouldHaveNullableType(t)) {
            nt = PrimitiveNullableType.Nullable;
        }
        if (nt != null) {
            // We may have to add an annotation for nullable types
            as.add(nt.toAnnotation());
        }
        return as;
    }

    public static NullableType emptyInitialNullableType(Type t) {
        if (t instanceof ReferenceType) return PrimitiveNullableType.Null;
        if (t instanceof DataTypeType dtt) {
            var args = new ArrayList<NullableType>(dtt.numTypeArgs());
            for (int i = 0; i < dtt.numTypeArgs(); i++) {
                args.add(emptyInitialNullableType(dtt.getTypeArg(i)));
            }
            return new DataTypeNullableType(args);
        }
        return PrimitiveNullableType.Unknown;
    }
}
