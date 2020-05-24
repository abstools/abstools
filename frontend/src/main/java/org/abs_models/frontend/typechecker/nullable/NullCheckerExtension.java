package org.abs_models.frontend.typechecker.nullable;

import org.abs_models.frontend.analyser.ErrorMessage;
import org.abs_models.frontend.analyser.TypeError;
import org.abs_models.frontend.ast.*;
import org.abs_models.frontend.typechecker.Type;
import org.abs_models.frontend.typechecker.TypeAnnotation;
import org.abs_models.frontend.typechecker.ext.DefaultTypeSystemExtension;
import org.abs_models.frontend.typechecker.locationtypes.LocationType;
import org.abs_models.frontend.typechecker.locationtypes.LocationTypeCheckerException;

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
    public void checkMethodImpl(MethodImpl method) {
        MethodSig ms = method.getMethodSig();

    }

    @Override
    public void annotateType(Type t, ASTNode<?> originatingNode, ASTNode<?> typeNode) {
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

    public NullableType getNullableType(Type t) {
        return defaultIfNull((NullableType) t.getMetaData(NULLABLE_KEY));
    }
}
