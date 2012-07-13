/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.common;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import abs.backend.BackendTestDriver;

@RunWith(Parameterized.class)
public class TimeTests extends SemanticTests {
   public TimeTests(BackendTestDriver d) {
      super(d);
   }
        
   @Test
   public void ticket258() {
      assertEvalTrue("{ Bool testresult = True; await duration(1,1); }");
   }
}
