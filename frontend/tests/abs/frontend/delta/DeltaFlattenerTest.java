/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import org.junit.Test;
import static org.junit.Assert.*;

import abs.frontend.FrontendTest;
import abs.frontend.delta.exceptions.*;
import abs.frontend.ast.*;

public class DeltaFlattenerTest extends FrontendTest {
    
    @Test
    public void removeClass() throws ASTNodeNotFoundException {
        ModuleDecl m = new ModuleDecl();
        m.addDecl(new ClassDecl("MyClass1", 
                new List<Annotation>(), 
                new List<ParamDecl>(),
                new List<InterfaceTypeUse>(),
                new Opt<InitBlock>(),
                new List<FieldDecl>(),
                new List<MethodImpl>()
                ));
        m.addDecl(new ClassDecl("MyClass2", 
                new List<Annotation>(), 
                new List<ParamDecl>(),
                new List<InterfaceTypeUse>(),
                new Opt<InitBlock>(),
                new List<FieldDecl>(),
                new List<MethodImpl>()
                ));

        assertTrue(m.getDeclList().getNumChild() == 2);

        RemoveClassModifier cm = new RemoveClassModifier("MyClass1");
        cm.applyTo(m);
        assertTrue(m.getDeclList().getNumChild() == 1);

        cm = new RemoveClassModifier("MyClass2");
        cm.applyTo(m);
        assertTrue(m.getDeclList().getNumChild() == 0);
    }

    @Test
    public void addClass() throws ASTNodeNotFoundException {
        ModuleDecl m = new ModuleDecl();
        assertTrue(m.getDeclList().getNumChild() == 0);
        
        AddClassModifier cm = new AddClassModifier(new ClassDecl("MyNewClass", 
                new List<Annotation>(), 
                new List<ParamDecl>(),
                new List<InterfaceTypeUse>(),
                new Opt<InitBlock>(),
                new List<FieldDecl>(),
                new List<MethodImpl>()
        ));
        
        cm.applyTo(m);
        assertTrue(m.getDeclList().getNumChild() == 1);
        
        
    }
    
    // TODO test modifyClass
}
