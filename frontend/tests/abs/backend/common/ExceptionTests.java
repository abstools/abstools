/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.common;

import java.io.File;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import abs.backend.BackendTestDriver;

@RunWith(Parameterized.class)
public class ExceptionTests extends SemanticTests {
   public ExceptionTests(BackendTestDriver d) {
      super(d);
   }
        
   @Test
   public void ticket175() {
       assertEvalTrue(new File("tests/abssamples/backend/StmtTests/exception_ticket175.abs"));
   }
 
   @Test
   public void testException() {
       assertEvalTrue("exception MyE(Bool); { Bool testresult = False; try throw MyE(True); catch MyE(value) => testresult = value; }");
   }
   
   @Test
   public void divByZero() throws Exception {
       assertEvalFails("{Bool testresult = 1/0 != 0;}");
   }

   @Test
   public void divByZeroCaught() throws Exception {
       assertEvalTrue("{Bool testresult = False; try 1/0; catch DivisionByZeroException => testresult = True; }");
   }

   @Test
   public void testExceptionPropagation() {
       assertEvalTrue(new File("tests/abssamples/backend/StmtTests/exception_propagation.abs"));
   }
}
