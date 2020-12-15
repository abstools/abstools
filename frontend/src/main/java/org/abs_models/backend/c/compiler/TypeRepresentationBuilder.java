package org.abs_models.backend.c.compiler;

import org.abs_models.backend.c.codegen.CFile;
import org.abs_models.frontend.ast.ConstructorArg;
import org.abs_models.frontend.ast.DataConstructor;
import org.abs_models.frontend.typechecker.DataTypeType;
import org.abs_models.frontend.typechecker.Type;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class TypeRepresentationBuilder {
    static public VoidTypeRepresentation VOID_TYPE = new VoidTypeRepresentation();
    Map<Type, TypeRepresentation> data = new HashMap<>();
    int idGenerator = 0;

    public TypeRepresentation get(Type type) {
        if (type.isUnitType()) {
            return VOID_TYPE;
        }

        TypeRepresentation repr = data.get(type);
        if (repr != null) return repr;

        if (type.isDataType()) {
            repr = buildDataType((DataTypeType) type);
        }

        if (repr == null) throw new RuntimeException("Unknown type: " + type);

        data.put(type, repr);
        return repr;
    }

    public void declare(CFile cFile) throws IOException {
        for (TypeRepresentation repr : data.values()) {
            repr.declare(cFile);
        }
    }

    private DataTypeRepresentation buildDataType(DataTypeType type) {
        ArrayList<DataTypeRepresentation.Variant> variants = new ArrayList<>();
        int tag = 0;
        for (DataConstructor dataConstructor : type.getDecl().getDataConstructors()) {
            ArrayList<DataTypeRepresentation.Field> fields = new ArrayList<>();
            int fieldId = 0;
            for (ConstructorArg arg : dataConstructor.getConstructorArgs()) {
                String fieldName = "f" + fieldId;
                Type argType = type.substituteTypeParams(arg.getType());
                fields.add(new DataTypeRepresentation.Field(get(argType), fieldName));
                fieldId++;
            }
            variants.add(new DataTypeRepresentation.Variant(dataConstructor, "v" + tag, tag, fields));
            tag++;
        }
        return new DataTypeRepresentation(type, generateName(), variants);
    }

    private String generateName() {
        return "abst_" + idGenerator++;
    }
}
