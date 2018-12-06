/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.common;

import java.io.File;

import org.junit.Assume;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import org.abs_models.backend.BackendTestDriver;
import org.abs_models.backend.BackendTestDriver.BackendName;

@RunWith(Parameterized.class)
public class ExceptionTests extends SemanticTests {
   public ExceptionTests(BackendTestDriver d) {
      super(d);
   }
        
   @Test
   public void ticket175() {
       Assume.assumeFalse("Not implemented on Java backend yet", driver.getBackendName() == BackendName.JAVA);
       assertEvalTrue(new File("abssamples/backend/StmtTests/exception_ticket175.abs"));
   }
 
   @Test
   public void testException() {
       Assume.assumeFalse("Not implemented on Java backend yet", driver.getBackendName() == BackendName.JAVA);
       assertEvalTrue("exception MyE(Bool); { Bool testresult = False; try throw MyE(True); catch MyE(value) => testresult = value; }");
   }
   
   @Test
   public void divByZero() throws Exception {
       Assume.assumeFalse("Not implemented on Maude backend yet", driver.getBackendName() == BackendName.MAUDE);
       assertEvalFails("{Bool testresult = 1/0 != 0;}");
   }

   @Test
   public void divByZeroCaught() throws Exception {
       Assume.assumeFalse("Not implemented on Java / Maude backend yet", driver.getBackendName() == BackendName.JAVA || driver.getBackendName() == BackendName.MAUDE);
       assertEvalTrue("{Bool testresult = False; try 1/0; catch DivisionByZeroException => testresult = True; }");
   }

   @Test
   public void testExceptionPropagation() {
       Assume.assumeFalse("Not implemented on Java backend yet", driver.getBackendName() == BackendName.JAVA);
       assertEvalTrue(new File("abssamples/backend/StmtTests/exception_propagation.abs"));
   }
   
   @Test
   public void testExceptionNoPropagation() {
       // https://github.com/abstools/abstools/issues/115
       Assume.assumeFalse("Not implemented on Java backend yet", driver.getBackendName() == BackendName.JAVA);
       assertEvalTrue(new File("abssamples/backend/StmtTests/exception_no_propagation.abs"));
   }
   
   @Test
   public void testExceptionNullFuture() {
       Assume.assumeFalse("Not implemented on Java / Maude backend yet", driver.getBackendName() == BackendName.JAVA || driver.getBackendName() == BackendName.MAUDE);
       assertEvalTrue(new File("abssamples/backend/StmtTests/exception_nullfuture.abs"));
   }

   @Test
   public void testExceptionGuard() {
       Assume.assumeFalse("Not implemented on Java / Maude backend yet", driver.getBackendName() == BackendName.JAVA || driver.getBackendName() == BackendName.MAUDE);
       assertEvalTrue(new File("abssamples/backend/ObjectTests/exception_guard.abs"));
   }
}
