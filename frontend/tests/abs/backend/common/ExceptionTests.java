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
public class ExceptionTests extends SemanticTests {
   public ExceptionTests(BackendTestDriver d) {
      super(d);
   }
        
   @Test
   public void ticket175() {
       String program =
  "exception MyException(Int, Bool);"
+ "{"
+ "  Int x=0;"
+ "  Bool testresult = False;"
+ "  try {"
+ "    x = x + 1;"
+ "    throw MyException(x, True);"
+ "    x = x + 100;"
+ "  } catch {"
+ "    MyException(0,False) => skip;"
+ "    MyException(1,False) => skip;"
+ "    MyException(1,True) => { x=x+1; x = x + 2; }"
+ "  }"
+ "  finally {"
+ "    x=x+1;"
+ "  }"
+ "  testresult = (x == 5);"
+ "}" ;
      assertEvalTrue(program);
   }
   
}
