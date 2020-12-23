package org.abs_models.backend.c.compiler;

import org.abs_models.backend.c.codegen.CFile;
import org.abs_models.backend.rvsdg.abs.ComparisonNode;
import org.abs_models.frontend.typechecker.Type;

import java.io.IOException;

public class NativeTypeRepresentation implements TypeRepresentation {
    public final Type type;
    public final String cname;

    public NativeTypeRepresentation(Type type, String cname) {
        this.type = type;
        this.cname = cname;
    }

    @Override
    public String getCType() {
        return cname;
    }

    @Override
    public void writeToString(CFile cFile, String builder, String value) throws IOException {
        cFile.writeLine(cname + "_tostring(" + builder + "," + value + ");");
    }

    @Override
    public void writeCompare(CFile cFile, String result, ComparisonNode.Operator operator, String left, String right) throws IOException {
        String cmp = cname + "_compare(" + left + "," + right + ") " + cFile.encodeComparisonOperator(operator);
        cFile.writeLine("*(" + result + ") = (" + cmp + ") ? 0 : 1;");
    }
}
