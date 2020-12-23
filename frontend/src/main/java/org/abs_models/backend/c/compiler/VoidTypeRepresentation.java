package org.abs_models.backend.c.compiler;

import org.abs_models.backend.c.codegen.CFile;
import org.abs_models.backend.rvsdg.abs.ComparisonNode;

import java.io.IOException;

public class VoidTypeRepresentation implements TypeRepresentation {
    @Override
    public boolean isRepresentable() {
        return false;
    }

    @Override
    public String getCType() {
        throw new Error("not representable");
    }

    @Override
    public void setValue(CFile cFile, String ident, String value) throws IOException {
        throw new Error("not representable");
    }

    @Override
    public void writeToString(CFile cFile, String builder, String value) throws IOException {
    }

    @Override
    public void writeCompare(CFile cFile, String result, ComparisonNode.Operator operator, String left, String right) {
        throw new Error("not representable");
    }
}
