package org.abs_models.frontend.typesystem;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import org.abs_models.ABSTest;
import org.abs_models.frontend.FrontendTest;
import org.abs_models.frontend.ast.Model;
import org.junit.Test;

public class NullableTests extends FrontendTest {
    @Test
    public void test1() {
        Model m = assertParse("{skip; skip;}");
        
        
        System.out.println(m.getMainBlock().getStmt(0).nullable_out());
        assertTrue(true);
    }
}
