/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.mtvl;

import static org.junit.Assert.*;

import java.util.HashSet;
import java.util.Set;

import org.junit.Test;

import abs.frontend.FrontendTest;
import abs.frontend.ast.Model;

public class FeatureModelTest extends FrontendTest {

    @Test
    public void minimalFM() {
        Model model = assertParseOk(
                "root FM"
                );
    }
    @Test
    public void minimalFMwithProduct() {
        Model model = assertParseOk(
                "product P (FM);"
                        + "root FM"
                );
    }

    @Test
    public void allValidConfigurations() {
        Model model = assertParseOk(
                "root FM {"
                        + " group [1..*] {"
                        + "A, B,"
                        + "C { Int attrib in [0..9]; }"
                        + "}"
                        + "}"
                        + "extension A { require: B; }"
                );

        Set<Set<String>> solutions = model.getFeatureModelConfigurations(); // disregards feature attributes
        assertTrue(solutions.size() == 5);

        Set<Set<String>> expected = new HashSet<Set<String>>();

        HashSet<String> exp1 = new HashSet<String>();
        exp1.add("FM"); exp1.add("A"); exp1.add("B"); exp1.add("C");
        HashSet<String> exp2 = new HashSet<String>();
        exp2.add("FM"); exp2.add("B"); exp2.add("C");
        HashSet<String> exp3 = new HashSet<String>();
        exp3.add("FM"); exp3.add("A"); exp3.add("B");
        HashSet<String> exp4 = new HashSet<String>();
        exp4.add("FM"); exp4.add("B");
        HashSet<String> exp5 = new HashSet<String>();
        exp5.add("FM"); exp5.add("C");

        expected.add(exp1);
        expected.add(exp2);
        expected.add(exp3);
        expected.add(exp4);
        expected.add(exp5);

        int pass = 0;
        for (Set<String> sol : solutions) {
            for (Set<String> exp : expected) {
                if (sol.containsAll(exp) && exp.containsAll(sol))
                    pass++;
            }
        }
        assertTrue(pass == 5);
    }


}
