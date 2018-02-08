package abs.frontend.utils;

import static abs.common.TreeUtilsHelper.cast;
import static abs.common.TreeUtilsHelper.recurse;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import abs.frontend.FrontendTest;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.AddExp;
import abs.frontend.ast.Decl;
import abs.frontend.ast.ExpFunctionDef;
import abs.frontend.ast.FnApp;
import abs.frontend.ast.FunctionDecl;
import abs.frontend.ast.FunctionDef;
import abs.frontend.ast.IntLiteral;
import abs.frontend.ast.Model;
import abs.frontend.ast.PureExp;
import abs.frontend.ast.Stmt;
import com.google.common.collect.ImmutableList;
import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.junit.Test;

public class TreeUtilsTest extends FrontendTest {

    @Test
    public void closestParent() {
        Model model = assertParseOkStdLib("def Int test() = 1;");
        FunctionDecl functionDecl = getLastFunctionDecl(model);
        assertEquals("test", functionDecl.getName());
        PureExp exp = ((ExpFunctionDef) functionDecl.getFunctionDef()).getRhs();

        assertSame(functionDecl, exp.closestParent(Decl.class));
        assertSame(functionDecl, exp.closestParent(FunctionDecl.class));
    }

    @Test
    public void closestParentIgnoresSelf() {
        Model model = assertParseOkStdLib("def Int test() = 1;");
        FunctionDecl functionDecl = getLastFunctionDecl(model);
        assertEquals("test", functionDecl.getName());
        FunctionDef functionDef = functionDecl.getFunctionDef();
        assertSame(functionDecl, functionDef.closestParent(ASTNode.class));
    }

    @Test
    public void closestParentNotFound() {
        Model model = assertParseOkStdLib("def Int test() = 1;");
        FunctionDecl functionDecl = getLastFunctionDecl(model);
        assertNull(functionDecl.closestParent(FunctionDecl.class));
    }

    @Test
    public void findChildrenList() {
        Model model = assertParseOkStdLib("def Int test(Int i) = test(1);");
        FunctionDecl functionDecl = getLastFunctionDecl(model);
        assertEquals("test", functionDecl.getName());

        List<PureExp> children = functionDecl.getFunctionDef().findChildren(PureExp.class);
        assertEquals(2, children.size());
    }

    @Test
    public void findChildrenListLazy() {
        Model model = assertParseOkStdLib("def Int test(Int i) = test(1);");
        FunctionDecl functionDecl = getLastFunctionDecl(model);
        assertEquals("test", functionDecl.getName());

        List<PureExp> children = functionDecl.getFunctionDef().findChildren(PureExp.class, true);
        assertEquals(1, children.size());
        assertTrue(children.get(0) instanceof FnApp);
    }

    @Test
    public void findChildrenListNotNull() {
        Model model = assertParseOkStdLib("def Int test(Int i) = test(1);");
        FunctionDecl functionDecl = getLastFunctionDecl(model);
        assertEquals("test", functionDecl.getName());

        List<Stmt> children = functionDecl.getFunctionDef().findChildren(Stmt.class);
        assertNotNull(children);
        assertTrue(children.isEmpty());
    }

    @Test
    public void castCorrect() {
        String s = "";
        Integer i = 1;

        Function<Object, String> stringCast = cast(String.class);
        assertNull(stringCast.apply(i));
        assertSame(s, stringCast.apply(s));
    }

    @Test
    public void multiCastCorrect() {
        String s = "";
        Integer i = 1;
        Double d = 0.0;

        Function<Object, Number> numberCast = cast(ImmutableList.of(Integer.class, Double.class));
        assertNull(numberCast.apply(s));
        assertSame(i, numberCast.apply(i));
        assertSame(d, numberCast.apply(d));
    }

    @Test
    public void recurseCorrect() {
        Integer i = 1;
        Double d = 0.0;

        Predicate<Number> numberPredicate = recurse(ImmutableList.of(Double.class));
        assertTrue(numberPredicate.test(d));
        assertFalse(numberPredicate.test(i));
    }

    @Test
    public void findChildrenMultipleTypes() {
        Model model = assertParseOkStdLib("def Int test(Int i) = test(1);");
        FunctionDecl functionDecl = getLastFunctionDecl(model);
        assertEquals("test", functionDecl.getName());
        FunctionDef def = functionDecl.getFunctionDef();

        Stream<PureExp> children = def.findChildren(cast(ImmutableList.of(FnApp.class, IntLiteral.class)), n -> true);
        assertNotNull(children);
        List<PureExp> result = children.distinct().collect(Collectors.toList());
        assertEquals(2, result.size());
        for (PureExp exp : result) {
            assertTrue(exp instanceof FnApp || exp instanceof IntLiteral);
        }
    }

    @Test
    public void recurseForSome() {
        Model model = assertParseOkStdLib("def Int test(Int i) = 1 + test(2);");
        FunctionDecl functionDecl = getLastFunctionDecl(model);
        assertEquals("test", functionDecl.getName());
        FunctionDef def = functionDecl.getFunctionDef();

        Stream<PureExp> children = def.findChildren(cast(PureExp.class), AddExp.class::isInstance);
        assertNotNull(children);
        List<PureExp> result = children.distinct().collect(Collectors.toList());

        // expecting AddExp, IntLiteral(1), FnApp, NOT IntLiteral(2)
        assertEquals(3, result.size());
        for (PureExp exp : result) {
            assertFalse(exp instanceof IntLiteral && ((IntLiteral) exp).getContent().equals("2"));
        }
    }

    @Test
    public void findChildrenIncludeSelf() {
        Model model = assertParseOkStdLib("def Int test(Int i) = test(1);");
        FunctionDecl functionDecl = getLastFunctionDecl(model);
        assertEquals("test", functionDecl.getName());
        ExpFunctionDef def = (ExpFunctionDef) functionDecl.getFunctionDef();
        PureExp exp = def.getRhs();

        Stream<PureExp> children = def.findChildren(cast(PureExp.class), n -> true);
        assertNotNull(children);
        List<PureExp> result = children.distinct().collect(Collectors.toList());
        assertEquals(2, result.size());
        assertTrue(result.contains(exp));
    }

    @Test
    public void findChildrenIncludeOnlySelf() {
        Model model = assertParseOkStdLib("def Int test(Int i) = test(1);");
        FunctionDecl functionDecl = getLastFunctionDecl(model);
        assertEquals("test", functionDecl.getName());
        ExpFunctionDef def = (ExpFunctionDef) functionDecl.getFunctionDef();
        PureExp exp = def.getRhs();

        Stream<PureExp> children = def.findChildren(cast(PureExp.class), n -> false);
        assertNotNull(children);
        List<PureExp> result = children.distinct().collect(Collectors.toList());
        assertEquals(1, result.size());
        assertTrue(result.contains(exp));
    }
}
