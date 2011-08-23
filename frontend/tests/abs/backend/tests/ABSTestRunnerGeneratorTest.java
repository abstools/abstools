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
import static org.junit.matchers.JUnitMatchers.both;
import static org.junit.matchers.JUnitMatchers.everyItem;
import static org.junit.matchers.JUnitMatchers.hasItem;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.Collections;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.hamcrest.BaseMatcher;
import org.hamcrest.Description;
import org.hamcrest.Matcher;
import org.junit.Test;

import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.InterfaceDecl;
import abs.frontend.ast.Model;
import abs.frontend.ast.ModuleDecl;
import abs.frontend.parser.Main;

/**
 * Unit tests for {@link ABSTestRunnerGenerator}
 * @author woner
 *
 */
public class ABSTestRunnerGeneratorTest {

    private final static String ABS_UNIT =
    		"module AbsUnit; export *;" +
    		"[TypeAnnotation] data DataPoint = DataPoint; " +
    		"[TypeAnnotation] data Test = Test; " +
    		"[TypeAnnotation] data TestClass = TestClass; " +
    		"[TypeAnnotation] data TestClassImpl = TestClassImpl; ";

    private final static String TEST_CODE =
                "module Test; import * from AbsUnit;" +
                "[TestClass] interface T { [Test] Unit t(); }" +
                "[TestClassImpl] class TI implements T { Unit t() { } }";
    
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

    private static class TestClassMatcher 
    extends BaseMatcher<Entry<InterfaceDecl, Set<ClassDecl>>> {
    
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
    
    private static class ModuleMatcher 
    extends BaseMatcher<ModuleDecl> {

        public boolean matches(Object arg0) {
            if (arg0 instanceof ModuleDecl) {
                ModuleDecl module = (ModuleDecl) arg0;
                if (module.getName().equals(ABSTestRunnerGenerator.RUNNER_MAIN)) {
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
            Matcher<Object> testClassType,
            Matcher<Object> testClassImplType,
            Matcher<Iterable<Entry<InterfaceDecl, Set<ClassDecl>>>> tests,
            Boolean isEmpty,
            ABSTestRunnerGenerator aut) {
        
        assertSame(model,getField(aut, "model"));
        assertThat(getField(aut, "testType"),testType);
        assertThat(getField(aut, "dataPointType"),dataPointType);
        assertThat(getField(aut, "testClassType"),testClassType);
        assertThat(getField(aut, "testClassImplType"),testClassImplType);
        
        Map<InterfaceDecl, Set<ClassDecl>> actual = 
            (Map<InterfaceDecl, Set<ClassDecl>>) getField(aut, "tests");
        assertThat(actual.entrySet(),tests);
        assertEquals(isEmpty,getField(aut, "isEmpty"));
    }

    @SuppressWarnings("unused")
    @Test(expected=IllegalArgumentException.class)
    public final void testABSTestRunnerGeneratorNull() {
        new ABSTestRunnerGenerator(null);
    }
    
    @Test
    public final void testABSTestRunnerGenerator() {
        Model model = new Model();
        ABSTestRunnerGenerator generator = 
            new ABSTestRunnerGenerator(model);
        
        assertMatches(model, 
                nullValue(), nullValue(), nullValue(), nullValue(), 
                equalTo(EMPTY_MAP), Boolean.TRUE,
                generator);
        
        try {
            model = Main.parseString(ABS_UNIT, true);
            generator = new ABSTestRunnerGenerator(model);
            
            assertMatches(model, 
                    notNullValue(), notNullValue(), notNullValue(), notNullValue(), 
                    equalTo(EMPTY_MAP), Boolean.TRUE,
                    generator);
            
            model = Main.parseString(ABS_UNIT + TEST_CODE, true);
            generator = new ABSTestRunnerGenerator(model);
            
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
            new ABSTestRunnerGenerator(model);
        
        generator = setField(generator, "isEmpty", Boolean.TRUE);
        assertFalse(generator.hasUnitTest());
        generator = setField(generator, "isEmpty", Boolean.FALSE);
        assertTrue(generator.hasUnitTest());
    }

    @Test
    public final void testGenerateTestRunner() {
        Model model = new Model();
        ABSTestRunnerGenerator generator = 
            new ABSTestRunnerGenerator(model);
        
        try {
            model = Main.parseString(ABS_UNIT + TEST_CODE, true);
        } catch (Exception e) {
            throw new IllegalStateException("Cannot parse test code",e);
        }
        
        ByteArrayOutputStream stream = new ByteArrayOutputStream();
        PrintStream print = new PrintStream(stream); 
        generator.generateTestRunner(print);
        String runner = stream.toString();
        
        try {
            model = Main.parseString(ABS_UNIT + TEST_CODE + runner, true);
            assertFalse("Generated code must not have parse error",model.hasParserErrors());
            assertFalse("Generated code must not have type error",model.hasTypeErrors());
           
            assertThat("Has one module that has the name 'AbsUnit.TestRunner' and a main block",
                        model.getModuleDecls(),hasItem(new ModuleMatcher()));
            
        } catch (Exception e) {
            fail("Cannot throw an exception ");
        }
        
    }
    
}
