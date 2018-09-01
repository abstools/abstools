/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.tests;

import static abs.backend.tests.ReflectionUtils.getField;
import static abs.backend.tests.ReflectionUtils.setField;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.hamcrest.CoreMatchers.both;
import static org.hamcrest.CoreMatchers.everyItem;
import static org.hamcrest.CoreMatchers.hasItem;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.hamcrest.BaseMatcher;
import org.hamcrest.Description;
import org.hamcrest.Matcher;
import org.junit.Test;

import abs.frontend.analyser.SemanticCondition;
import abs.frontend.analyser.SemanticConditionList;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.InterfaceDecl;
import abs.frontend.ast.Model;
import abs.frontend.ast.ModuleDecl;
import abs.frontend.parser.Main;
import abs.frontend.parser.ParserError;

/**
 * Unit tests for {@link ASTBasedABSTestRunnerGenerator}
 * @author woner
 *
 */
public class ASTBasedABSTestRunnerGeneratorTest {

    private final static String ABS_UNIT =
    		"module AbsUnit; export *;" +
    		"[TypeAnnotation] data DataPoint = DataPoint; " +
    		"[TypeAnnotation] data Ignored = Ignored;" +
    		"[TypeAnnotation] data Test = Test; " +
    		"[TypeAnnotation] data Suite = Suite; " +
    		"[TypeAnnotation] data Fixture = Fixture; ";

    private final static String TEST_CODE =
                "module Test; export *; import * from AbsUnit;" +
                "[Fixture] interface T { [Test] Unit t(); }" +
                "[Suite] class TI implements T { Unit t() { } }";
    
    private final static Iterable<Entry<InterfaceDecl, Set<ClassDecl>>> EMPTY_MAP = 
                Collections.<InterfaceDecl, Set<ClassDecl>>emptyMap().entrySet();
    
    private static class SizeMatcher 
    extends BaseMatcher<Iterable<Entry<InterfaceDecl, Set<ClassDecl>>>> {
        
        private final int size;
        
        public SizeMatcher(int size) {
            this.size = size;
        }
        
        public boolean matches(Object arg0) {
            if (arg0 instanceof Iterable) {
                Iterable<?> it = (Iterable<?>) arg0;
                Iterator<?> tr = it.iterator();
                
                int count = 0;
                while (count < size) {
                    if (! tr.hasNext()) {
                        return false;
                    }
                    tr.next();
                    count++;
                }
                return ! tr.hasNext();
            }
            return false;
        }
    
        @SuppressWarnings("unused")
        public void describeTo(Description arg0) {
            // TODO Auto-generated method stub
            
        }
        
    }

    /**
     * @see ASTBasedABSTestRunnerGeneratorTest.ModuleMatcher below for note about generics!
     */
    private static class TestClassMatcher<I,C>
    extends BaseMatcher<Entry<I, Set<C>>> {
    
        public boolean matches(Object arg0) {
            if (!(arg0 instanceof Entry)) {
                return false;
            }
            
            final Entry<?, ?> entry = (Entry<?, ?>) arg0;
            if (!(entry.getKey() instanceof InterfaceDecl)) {
                return false;
            }
            
            if (!(entry.getValue() instanceof Set)) {
                return false;
            }
            
            final Set<?> set = (Set<?>) entry.getValue();
            if (set.size() != 1) {
                return false;
            }
            
            final Object ele = set.iterator().next();
            if (!(ele instanceof ClassDecl)) {
                return false;
            }
            
            final InterfaceDecl intf = (InterfaceDecl) entry.getKey();
            final ClassDecl clazz = (ClassDecl) ele;
            
            return intf.getName().equals("T") &&
                   clazz.getName().equals("TI");
        }
    
        @SuppressWarnings("unused")
        public void describeTo(Description arg0) {
        }
        
    }
    
    /**
     * NB: type patched to be generic instead of the more specific ModuleDecl because
     * javac is too picky about hamcrests' generics!
     */
    private static class ModuleMatcher<T> 
    extends BaseMatcher<T> {

        public boolean matches(Object arg0) {
            if (arg0 instanceof ModuleDecl) {
                ModuleDecl module = (ModuleDecl) arg0;
                if (module.getName().equals(ASTBasedABSTestRunnerGenerator.RUNNER_MAIN)) {
                    return module.hasBlock();
                }
            }
            return false;
        }

        @SuppressWarnings("unused")
        public void describeTo(Description arg0) {
            // TODO Auto-generated method stub
            
        }
    }

    @SuppressWarnings("unchecked")
    private static void assertMatches(
            Model model,
            Matcher<Object> testType, 
            Matcher<Object> dataPointType,
            Matcher<Object> fixtureType,
            Matcher<Object> suiteType,
            Matcher<Iterable<Entry<InterfaceDecl, Set<ClassDecl>>>> tests,
            Boolean isEmpty,
            ABSTestRunnerGenerator aut) {
        
        assertSame(model,getField(aut, AbstractABSTestRunnerGenerator.class, "model"));
        assertThat(getField(aut, AbstractABSTestRunnerGenerator.class, "testType"),testType);
        assertThat(getField(aut, AbstractABSTestRunnerGenerator.class, "dataPointType"),dataPointType);
        assertThat(getField(aut, AbstractABSTestRunnerGenerator.class, "fixtureType"),fixtureType);
        assertThat(getField(aut, AbstractABSTestRunnerGenerator.class, "suiteType"),suiteType);
        
        Map<InterfaceDecl, Set<ClassDecl>> actual = 
            (Map<InterfaceDecl, Set<ClassDecl>>) getField(aut, AbstractABSTestRunnerGenerator.class, "tests");
        assertThat(actual.entrySet(),tests);
        assertEquals(isEmpty,getField(aut, AbstractABSTestRunnerGenerator.class, "isEmpty"));
    }

    @SuppressWarnings("unused")
    @Test(expected=IllegalArgumentException.class)
    public final void testABSTestRunnerGeneratorNull() {
        new ASTBasedABSTestRunnerGenerator(null);
    }
    
    @Test
    public final void testABSTestRunnerGenerator() {
        Model model = new Model();
        ABSTestRunnerGenerator generator = 
            new ASTBasedABSTestRunnerGenerator(model);
        
        assertMatches(model, 
                nullValue(), nullValue(), nullValue(), nullValue(), 
                equalTo(EMPTY_MAP), Boolean.TRUE,
                generator);
        
        try {
            model = Main.parseString(ABS_UNIT, true);
            generator = new ASTBasedABSTestRunnerGenerator(model);
            
            assertMatches(model, 
                    notNullValue(), notNullValue(), notNullValue(), notNullValue(), 
                    equalTo(EMPTY_MAP), Boolean.TRUE,
                    generator);
            
            model = Main.parseString(ABS_UNIT + TEST_CODE, true);
            generator = new ASTBasedABSTestRunnerGenerator(model);
            
            assertMatches(model, 
                    notNullValue(), notNullValue(), notNullValue(), notNullValue(), 
                    both(everyItem(new TestClassMatcher())).
                    and(new SizeMatcher(1)), Boolean.FALSE,
                    generator);
            
        } catch (Exception e) {
            throw new IllegalStateException("Cannot parse test code",e);
        }
    }
    
    @Test
    public final void testHasUnitTest() {
        Model model = new Model();
        ABSTestRunnerGenerator generator = 
            new ASTBasedABSTestRunnerGenerator(model);
        
        generator = setField(generator, AbstractABSTestRunnerGenerator.class, "isEmpty", Boolean.TRUE);
        assertFalse(generator.hasUnitTest());
        generator = setField(generator, AbstractABSTestRunnerGenerator.class, "isEmpty", Boolean.FALSE);
        assertTrue(generator.hasUnitTest());
    }
    
    @Test
    public final void testGenerateTestRunner() {
        final Model model;
        try {
            model = Main.parseString(ABS_UNIT + TEST_CODE, true);
        } catch (Exception e) {
            throw new IllegalStateException("Cannot parse test code",e);
        }
        
        ABSTestRunnerGenerator generator = new ASTBasedABSTestRunnerGenerator(model);
        ByteArrayOutputStream stream = new ByteArrayOutputStream();
        PrintStream print = new PrintStream(stream); 
        generator.generateTestRunner(print);
        String runner = stream.toString();
        
        try {
            Model result = Main.parseString(ABS_UNIT + TEST_CODE + runner, true);
            
            StringBuilder parseErrors = new StringBuilder();
            if (result.hasParserErrors()) {
                parseErrors.append("Syntactic errors: ");
                List<ParserError> es = result.getParserErrors();
                parseErrors.append(es.size());
                parseErrors.append("\n");
                for (ParserError e : es) {
                    parseErrors.append(e.getHelpMessage());
                    parseErrors.append("\n");
                }
            }
            
            assertFalse("Generated code must not have parse error: "+parseErrors,result.hasParserErrors());
            
            StringBuilder errors = new StringBuilder();
            if (result.hasErrors()) {
                SemanticConditionList el = result.getErrors();
                errors.append("Semantic errors: ");
                errors.append(el.getErrorCount());
                errors.append("\n");
                for (SemanticCondition error : el) {
                    errors.append(error.getHelpMessage());
                    errors.append("\n");
                }
            }
            
            assertFalse("Generated code must not have semantic error: "+errors,result.hasErrors());

            result.typeCheck();
            assertFalse("Generated code must not have type error",result.hasTypeErrors());
           
            assertThat("Has one module that has the name 'AbsUnit.TestRunner' and a main block",
                        result.getModuleDecls(),hasItem(new ModuleMatcher()));
            
        } catch (Exception e) {
            fail("Cannot throw an exception ");
        }
        
    }
    
}
