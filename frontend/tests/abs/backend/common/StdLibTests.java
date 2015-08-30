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
public class StdLibTests extends SemanticTests {
    public StdLibTests(BackendTestDriver d) {
        super(d);
    }

    @Test
    public void listNth() {
        assertEvalTrue("{ List<Int> list = list[1, 2, 3]; Bool testresult = nth(list,2) == 3; }");
    }

    @Test
    public void setContains1() {
        assertEvalTrue("{ Set<Int> s = set[1, 2, 3]; Bool testresult = contains(s, 3); }");
    }

    @Test
    public void setContains2() {
        assertEvalTrue("{ Set<Int> s = set[1, 2, 3]; Bool testresult = !contains(s, 4); }");
    }

    @Test
    public void setUnion() {
        assertEvalTrue(" { Set<Int> s1 = set[1,2,3]; Set<Int> s2 = set[1,2,4]; Set<Int> res = union(s1,s2);"
                + "Bool testresult = (size(res) == 4) && contains(res,4) && contains(res,3); }");
    }

    @Test
    public void setRemove() {
        assertEvalTrue("{ Set<Int> set = set[1, 2, 3]; Bool testresult = !contains(remove(set, 3), 3); }");
    }

    @Test
    public void mapLookup() {
        assertEvalTrue("{ Map<Int, Int> map = map[Pair(1, 100), Pair(2, 200), Pair(3, 300)]; Bool testresult = lookup(map, 3) == Just(300); }");
    }

    @Test
    public void mapLookupDefault1() {
        assertEvalTrue("{ Map<Int, Int> map = map[Pair(1, 100), Pair(2, 200), Pair(3, 300)]; Bool testresult = lookupDefault(map, 3, -1) == 300; }");
    }

    @Test
    public void mapLookupDefault2() {
        assertEvalTrue("{ Map<Int, Int> map = map[Pair(1, 100), Pair(2, 200), Pair(3, 300)]; Bool testresult = lookupDefault(map, 5, -1) == -1; }");
    }

    @Test
    public void mapPut1() {
        assertEvalTrue("{ Map<Int, Int> map = map[Pair(1, 100), Pair(2, 200), Pair(3, 300)]; Bool testresult = put(map, 2, -1) == map[Pair(1, 100), Pair(2, -1), Pair(3, 300)]; }");
    }

    @Test
    public void mapPut2() {
        assertEvalTrue("{ Map<Int, Int> map = map[Pair(1, 100), Pair(2, 200), Pair(3, 300)]; Bool testresult = put(map, 4, 400) == map[Pair(1, 100), Pair(2, 200), Pair(3, 300), Pair(4, 400)]; }");
    }

    // BUILT-IN FUNCTIONS

    @Test
    public void stringSubstr() {
        assertEvalTrue("{ Bool testresult = substr(\"foobar\", 1, 3) == \"oob\"; }");
    }

    @Test
    public void stringLength() {
        assertEvalTrue("{ Bool testresult = strlen(\"foobar\") == 6; }");
    }

    @Test
    public void stringLength1WS() {
        assertEvalTrue("{ Bool testresult = strlen(\" \") == 1; }");
    }

    @Test
    public void truncate() {
        assertEvalTrue("{ Bool testresult = truncate(11/2) == 5; }");
    }

    @Test
    public void truncate2() {
        assertEvalTrue("{ Bool testresult = truncate(5) == 5; }");
    }
}
