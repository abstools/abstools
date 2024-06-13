package org.abs_models.backend.c.compiler;

import org.abs_models.backend.c.codegen.CFile;
import org.abs_models.backend.rvsdg.abs.ComparisonNode;

import java.io.IOException;

/**
 * A C representation of a Type.
 */
public interface TypeRepresentation {
    default boolean isRepresentable() {
        return true;
    }

    /**
     * Returns a string representing the C type.
     */
    String getCType();

    /**
     * Declares the type representation in the file.
     * After declare has been executed it should be possible to use the string returned by getCType().
     */
    default void declare(CFile cFile) throws IOException {
    }

    /**
     * Adds additional code required for the representation to be usable at runtime.
     */
    default void implement(CFile cFile) throws IOException {
    }

    /**
     * Initializes lvalue to a zero value.
     *
     * @throws IOException
     */
    default void initZero(CFile cFile, String lvalue) throws IOException {
        cFile.writeLine(getCType() + "_initzero(&(" + lvalue + "));");
    }

    /**
     * Initializes lvalue by coping from rvalue.
     *
     * @throws IOException
     */
    default void initCopy(CFile cFile, String lvalue, String rvalue) throws IOException {
        cFile.writeLine(getCType() + "_initcopy(&(" + lvalue + "), " + rvalue + ");");
    }

    /**
     * Deinitializes lvalue.
     *
     * @throws IOException
     */
    default void deinit(CFile cFile, String lvalue) throws IOException {
        cFile.writeLine(getCType() + "_deinit(&(" + lvalue + "));");
    }

    /**
     * Converts the `value` into a string stored inside `builder`.
     */
    default void convertToString(CFile cFile, String builder, String value) throws IOException {
        cFile.writeLine(getCType() + "_tostring(" + builder + "," + value + ");");
    }

    /**
     * Compares `left` and `right` and stores the result in `result`.
     */
    default void writeCompare(CFile cFile, String result, ComparisonNode.Operator operator, String left, String right) throws IOException {
        String cmp = getCType() + "_compare(" + left + "," + right + ") " + cFile.encodeComparisonOperator(operator);
        cFile.writeLine(result + " = (" + cmp + ") ? 0 : 1;");
    }
}
