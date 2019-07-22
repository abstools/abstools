package org.abs_models.frontend.pardef;

import org.abs_models.frontend.ast.Model;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * @author Björn Petersen
 * @since 26.06.2018
 */
public class ParametricPartialFunctionTest extends AbstractPartialFunctionTest {

    @Rule
    public final ExpectedException exception = ExpectedException.none();

    @Test
    public void multipleFunctionsExpectingDifferentTypeParam() {
        Model model = parse(
            "addResults(expectsString, expectsInt)(1);",
            "def Int addResults<T>(f, g)(T t) = f(t) + g(t);",
            "def Int expectsString(String s) = 1;",
            "def Int expectsInt(Int i) = 2;"
        );

        exception.expect(PardefModellingException.class);
        testExpand(model);
    }

    @Test
    public void simpleReturnType() {
        Model model = parse(
            "returnResult(one)();",
            "def T returnResult<T>(f)() = f();",
            "def Int one() = 1;"
        );

        testExpand(model);
    }

    @Test
    public void boundParametricDataTypeReturnType() {
        Model model = parse(
            "returnResult(oneList)();",
            "def T returnResult<T>(f)() = f();",
            "def List<Int> oneList() = list[1];"
        );

        testExpand(model);
    }

    @Test
    public void unboundParametricDataTypeReturnType() {
        Model model = parse(
            "returnResult(listOf)(1);",
            "def T returnResult<E, T>(f)(E e) = f(e);",
            "def List<E> listOf<E>(E e) = list[e];"
        );

        testExpand(model);
    }

    @Test
    public void nestedParametricReturnType() {
        Model model = parse(
            "List<Int> l = flatten(list[list[1, 2, 3], list[4, 5, 6]]);",
            "def List<T> flatten<T>(List<List<T>> l) = foldl(concatenate)(l, Nil);"
        );

        testExpand(model);
    }
}
