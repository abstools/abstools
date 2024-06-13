package org.abs_models.backend.c.codegen;

import org.abs_models.backend.rvsdg.abs.BinaryArithmeticNode;
import org.abs_models.backend.rvsdg.abs.ComparisonNode;

import java.io.IOException;
import java.io.Writer;

public class CFile {
    public final CProject cProject;
    public final Writer writer;

    public CFile(CProject cProject, Writer writer) {
        this.cProject = cProject;
        this.writer = writer;
    }

    public void writeLine(String line) throws IOException {
        this.writer.write(line + "\n");
    }

    public void close() throws IOException {
        this.writer.close();
    }

    public void startFunction(CFunctionDecl decl) throws IOException {
        writeLine(decl.cString());
        writeLine("{");
    }

    public void stopFunction() throws IOException {
        writeLine("}");
    }

    public String encodeCString(byte[] bytes) {
        StringBuilder sb = new StringBuilder();
        sb.append('"');
        for (byte b : bytes) {
            if (b == '\"' || b == '\\' || b == '\n') {
                sb.append('\\');
                sb.append(b);
            } else if (b > 0) {
                sb.append((char) b);
            } else {
                throw new RuntimeException("todo");
            }
        }
        sb.append('"');
        return sb.toString();
    }

    /**
     * Encodes an a comparison operator (`ComparisonNode.Operator`) into an expression
     * which can be used on the result from a comparison function as used by e.g. qsort.
     *
     * Example: `Eq` is turned into ` == 0`
     */
    public String encodeComparisonOperator(ComparisonNode.Operator op) {
        switch (op) {
            case Eq:
                return " == 0";
            case NotEq:
                return " != 0";
            case Lt:
                return " < 0";
            case Lte:
                return " <= 0";
            case Gt:
                return " > 0";
            case Gte:
                return " >= 0";
            default:
                throw new RuntimeException("Unknown operator: " + op);
        }
    }

    public String encodeBinaryArithmeticOperator(BinaryArithmeticNode.Operator op) {
        switch (op) {
            case Add:
                return "add";
            case Sub:
                return "sub";
            case Mul:
                return "mul";
            case Div:
                return "div";
            case Mod:
                return "mod";
            default:
                throw new RuntimeException("Unknown operator: " + op);
        }
    }
}
