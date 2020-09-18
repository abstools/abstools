/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.common;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import org.abs_models.backend.BackendTestDriver;

@RunWith(Parameterized.class)
public class StdLibTests extends SemanticTests {
    public StdLibTests(BackendTestDriver d) {
        super(d);
    }

    @Test
    public void listNth() throws Exception {
        assertEvalTrue("{ List<Int> list = list[1, 2, 3]; Bool testresult = nth(list,2) == 3; }");
    }

    @Test
    public void setElements() throws Exception {
        assertEvalTrue("{ Set<Int> s = set[1, 2, 3]; Bool testresult = elements(s) == list[1, 2, 3]; }");
    }

    @Test
    public void setElementsEmpty() throws Exception {
        assertEvalTrue("{ Set<Int> s = set[]; Bool testresult = elements(s) == list[]; }");
    }

    @Test
    public void setContains1() throws Exception {
        assertEvalTrue("{ Set<Int> s = set[1, 2, 3]; Bool testresult = contains(s, 3); }");
    }

    @Test
    public void setContains2() throws Exception {
        assertEvalTrue("{ Set<Int> s = set[1, 2, 3]; Bool testresult = !contains(s, 4); }");
    }

    @Test
    public void setUnion() throws Exception {
        assertEvalTrue(" { Set<Int> s1 = set[1,2,3]; Set<Int> s2 = set[1,2,4]; Set<Int> res = union(s1,s2);"
                + "Bool testresult = (size(res) == 4) && contains(res,4) && contains(res,3); }");
    }

    @Test
    public void setIsSubset() throws Exception {
        assertEvalTrue(" { Set<Int> s1 = set[1,2,3]; Set<Int> s2 = set[1,2,4]; Set<Int> res = union(s1,s2);"
                + "Bool testresult = isSubset(s1, s2) == False && isSubset(s1, res) == True; }");
    }

    @Test
    public void setRemove() throws Exception {
        assertEvalTrue("{ Set<Int> set = set[1, 2, 3]; Bool testresult = !contains(remove(set, 3), 3); }");
    }

    @Test
    public void setTake() throws Exception {
        assertEvalTrue("{ Set<Int> set = set[1]; Bool testresult = take(set) == 1; }");
    }

    @Test
    public void setTakeMaybe() throws Exception {
        assertEvalTrue("{ Set<Int> set = set[1]; Bool testresult = takeMaybe(set) == Just(1); }");
    }

    @Test
    public void setTakeMaybeEmpty() throws Exception {
        assertEvalTrue("{ Set<Int> set = set[]; Bool testresult = takeMaybe(set) == Nothing; }");
    }

    @Test
    public void mapLookup() throws Exception {
        assertEvalTrue("{ Map<Int, Int> map = map[Pair(1, 100), Pair(2, 200), Pair(3, 300)]; Bool testresult = lookup(map, 3) == Just(300); }");
    }

    @Test
    public void mapLookupDefault1() throws Exception {
        assertEvalTrue("{ Map<Int, Int> map = map[Pair(1, 100), Pair(2, 200), Pair(3, 300)]; Bool testresult = lookupDefault(map, 3, -1) == 300; }");
    }

    @Test
    public void mapLookupDefault2() throws Exception {
        assertEvalTrue("{ Map<Int, Int> map = map[Pair(1, 100), Pair(2, 200), Pair(3, 300)]; Bool testresult = lookupDefault(map, 5, -1) == -1; }");
    }

    @Test
    public void mapPut1() throws Exception {
        assertEvalTrue("{ Map<Int, Int> map = map[Pair(1, 100), Pair(2, 200), Pair(3, 300)]; Bool testresult = put(map, 2, -1) == map[Pair(1, 100), Pair(2, -1), Pair(3, 300)]; }");
    }

    @Test
    public void mapPut2() throws Exception {
        assertEvalTrue("{ Map<Int, Int> map = map[Pair(1, 100), Pair(2, 200), Pair(3, 300)]; Bool testresult = put(map, 4, 400) == map[Pair(1, 100), Pair(2, 200), Pair(3, 300), Pair(4, 400)]; }");
    }

    @Test
    public void mapKeys() throws Exception {
        assertEvalTrue("{ Map<Int, Int> map = map[Pair(1, 100), Pair(2, 200), Pair(3, 300)]; Bool testresult = keys(map) == set[1, 2, 3]; }");
    }

    @Test
    public void mapValues() throws Exception {
        assertEvalTrue("{ Map<Int, Int> map = map[Pair(1, 100), Pair(2, 200), Pair(3, 300)]; Bool testresult = values(map) == list[100, 200, 300]; }");
    }

    @Test
    public void mapEntries() throws Exception {
        assertEvalTrue("{ Map<Int, Int> map = map[Pair(1, 100), Pair(2, 200), Pair(3, 300)]; Bool testresult = entries(map) == list[Pair(1, 100), Pair(2, 200), Pair(3, 300)]; }");
    }

    // BUILT-IN FUNCTIONS

    @Test
    public void stringSubstr() throws Exception {
        assertEvalTrue("{ Bool testresult = substr(\"foobar\", 1, 3) == \"oob\"; }");
    }

    @Test
    public void stringLength() throws Exception {
        assertEvalTrue("{ Bool testresult = strlen(\"foobar\") == 6; }");
    }

    @Test
    public void stringLength1WS() throws Exception {
        assertEvalTrue("{ Bool testresult = strlen(\" \") == 1; }");
    }

    @Test
    public void truncate() throws Exception {
        assertEvalTrue("{ Bool testresult = truncate(11/2) == 5; }");
    }

    @Test
    public void truncate2() throws Exception {
        assertEvalTrue("{ Bool testresult = truncate(5) == 5; }");
    }

    @Test
    public void numerator1() throws Exception {
        assertEvalTrue("{ Bool testresult = numerator(-2/3) == -2; }");
    }

    @Test
    public void numerator2() throws Exception {
        assertEvalTrue("{ Bool testresult = numerator(-5) == -5; }");
    }

    @Test
    public void denominator1() throws Exception {
        assertEvalTrue("{ Bool testresult = denominator(-2/3) == 3; }");
    }

    @Test
    public void denominator2() throws Exception {
        assertEvalTrue("{ Bool testresult = denominator(-5) == 1; }");
    }

    @Test
    public void float1() throws Exception {
        assertEvalTrue("{ Bool testresult = float(5/2) == 2.5; }");
    }

    @Test
    public void float2() throws Exception {
        assertEvalTrue("{ Bool testresult = float(-5/2) == -2.5; }");
    }

    @Test
    public void float3() throws Exception {
        assertEvalTrue("{ Bool testresult = float(0) == 0.0; }");
    }

    @Test
    public void float4() throws Exception {
        assertEvalTrue("{ Bool testresult = float(1) == 1.0; }");
    }

    @Test
    public void rat1() throws Exception {
        assertEvalTrue("{ Bool testresult = rat(2.5) == 5/2; }");
    }

    @Test
    public void rat2() throws Exception {
        assertEvalTrue("{ Bool testresult = rat(-2.5) == -5/2; }");
    }

    @Test
    public void rat3() throws Exception {
        assertEvalTrue("{ Bool testresult = rat(0.0) == 0; }");
    }

    @Test
    public void rat4() throws Exception {
        assertEvalTrue("{ Bool testresult = rat(1.0) == 1; }");
    }

    @Test
    public void floor1() throws Exception {
        assertEvalTrue("{ Bool testresult = floor(2.5) == 2;}");
    }

    @Test
    public void floor2() throws Exception {
        assertEvalTrue("{ Bool testresult = floor(-2.5) == -3;}");
    }

    @Test
    public void ceil1() throws Exception {
        assertEvalTrue("{ Bool testresult = ceil(2.5) == 3;}");
    }

    @Test
    public void ceil2() throws Exception {
        assertEvalTrue("{ Bool testresult = ceil(-2.5) == -2;}");
    }

    @Test
    public void maxInt() throws Exception {
        assertEvalTrue("{Bool testresult = max(-2, 3) == 3;}");
    }

    @Test
    public void minInt() throws Exception {
        assertEvalTrue("{Bool testresult = min(-2, 3) == -2;}");
    }

    @Test
    public void maxRat() throws Exception {
        assertEvalTrue("{Bool testresult = max(-2/5, 3/5) == 3/5;}");
    }

    @Test
    public void minRat() throws Exception {
        assertEvalTrue("{Bool testresult = min(-2/5, 3/5) == -2/5;}");
    }

    @Test
    public void maxFloat() throws Exception {
        assertEvalTrue("{Bool testresult = max(-2.0, 3.0) == 3.0;}");
    }

    @Test
    public void minFloat() throws Exception {
        assertEvalTrue("{Bool testresult = min(-2.0, 3.0) == -2.0;}");
    }

    @Test
    public void sqrt1() throws Exception {
        assertEvalTrue("{Bool testresult = sqrt(4.0) == 2.0;}");
    }

    @Test
    public void logexp1() throws Exception {
        assertEvalTrue("{Bool testresult = log(exp(1.0)) == 1.0;}");
    }

    @Test
    public void map1() throws Exception {
	assertEvalTrue("{ Bool testresult = map((Int x) => x + 1)(list[1, 2, 3]) == list[2, 3, 4]; }");
    }

    @Test
    public void filter1() throws Exception {
	assertEvalTrue("{ Bool testresult = filter((Int x) => x % 2 == 0)(list[1, 2, 3]) == list[2]; }");
    }

    @Test
    public void foldl1() throws Exception {
	assertEvalTrue("{ Bool testresult = foldl((Int elem, Int acc) => elem + acc)(list[1, 2, 3], 0) == 6; }");
    }

    @Test
    public void foldr1() throws Exception {
	assertEvalTrue("{ Bool testresult = foldr((Int elem, Int acc) => elem + acc)(list[1, 2, 3], 0) == 6; }");
    }

    @Test
    public void durationLessThan() throws Exception {
        assertEvalTrue("{ Bool testresult = durationLessThan(Duration(5), InfDuration); }");
    }
}
