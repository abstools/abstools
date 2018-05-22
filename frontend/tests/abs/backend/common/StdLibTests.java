/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.common;

import org.junit.Ignore;
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
    public void setElements() {
        assertEvalTrue("{ Set<Int> s = set[1, 2, 3]; Bool testresult = elements(s) == list[1, 2, 3]; }");
    }

    @Test
    public void setElementsEmpty() {
        assertEvalTrue("{ Set<Int> s = set[]; Bool testresult = elements(s) == list[]; }");
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
    public void setIsSubset() {
        assertEvalTrue(" { Set<Int> s1 = set[1,2,3]; Set<Int> s2 = set[1,2,4]; Set<Int> res = union(s1,s2);"
                + "Bool testresult = isSubset(s1, s2) == False && isSubset(s1, res) == True; }");
    }

    @Test
    public void setRemove() {
        assertEvalTrue("{ Set<Int> set = set[1, 2, 3]; Bool testresult = !contains(remove(set, 3), 3); }");
    }

    @Test
    public void setTake() {
        assertEvalTrue("{ Set<Int> set = set[1]; Bool testresult = take(set) == 1; }");
    }

    @Test
    public void setTakeMaybe() {
        assertEvalTrue("{ Set<Int> set = set[1]; Bool testresult = takeMaybe(set) == Just(1); }");
    }

    @Test
    public void setTakeMaybeEmpty() {
        assertEvalTrue("{ Set<Int> set = set[]; Bool testresult = takeMaybe(set) == Nothing; }");
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

    @Test
    public void mapKeys() {
        assertEvalTrue("{ Map<Int, Int> map = map[Pair(1, 100), Pair(2, 200), Pair(3, 300)]; Bool testresult = keys(map) == set[1, 2, 3]; }");
    }

    @Test
    public void mapValues() {
        assertEvalTrue("{ Map<Int, Int> map = map[Pair(1, 100), Pair(2, 200), Pair(3, 300)]; Bool testresult = values(map) == list[100, 200, 300]; }");
    }

    @Test
    public void mapEntries() {
        assertEvalTrue("{ Map<Int, Int> map = map[Pair(1, 100), Pair(2, 200), Pair(3, 300)]; Bool testresult = entries(map) == list[Pair(1, 100), Pair(2, 200), Pair(3, 300)]; }");
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

    @Test
    public void numerator1() {
        assertEvalTrue("{ Bool testresult = numerator(-2/3) == -2; }");
    }

    @Test
    public void numerator2() {
        assertEvalTrue("{ Bool testresult = numerator(-5) == -5; }");
    }

    @Test
    public void denominator1() {
        assertEvalTrue("{ Bool testresult = denominator(-2/3) == 3; }");
    }

    @Test
    public void denominator2() {
        assertEvalTrue("{ Bool testresult = denominator(-5) == 1; }");
    }

    @Test
    public void float1() {
        assertEvalTrue("{ Bool testresult = float(5/2) == 2.5; }");
    }

    @Test
    public void float2() {
        assertEvalTrue("{ Bool testresult = float(-5/2) == -2.5; }");
    }

    @Test
    public void float3() {
        assertEvalTrue("{ Bool testresult = float(0) == 0.0; }");
    }

    @Test
    public void float4() {
        assertEvalTrue("{ Bool testresult = float(1) == 1.0; }");
    }

    @Test
    public void rat1() {
        assertEvalTrue("{ Bool testresult = rat(2.5) == 5/2; }");
    }

    @Test
    public void rat2() {
        assertEvalTrue("{ Bool testresult = rat(-2.5) == -5/2; }");
    }

    @Test
    public void rat3() {
        assertEvalTrue("{ Bool testresult = rat(0.0) == 0; }");
    }

    @Test
    public void rat4() {
        assertEvalTrue("{ Bool testresult = rat(1.0) == 1; }");
    }

    @Test
    public void floor1() {
        assertEvalTrue("{ Bool testresult = floor(2.5) == 2;}");
    }

    @Test
    public void floor2() {
        assertEvalTrue("{ Bool testresult = floor(-2.5) == -3;}");
    }

    @Test
    public void ceil1() {
        assertEvalTrue("{ Bool testresult = ceil(2.5) == 3;}");
    }

    @Test
    public void ceil2() {
        assertEvalTrue("{ Bool testresult = ceil(-2.5) == -2;}");
    }

    @Test
    public void maxInt() {
        assertEvalTrue("{Bool testresult = max(-2, 3) == 3;}");
    }

    @Test
    public void minInt() {
        assertEvalTrue("{Bool testresult = min(-2, 3) == -2;}");
    }

    @Test
    public void maxRat() {
        assertEvalTrue("{Bool testresult = max(-2/5, 3/5) == 3/5;}");
    }

    @Test
    public void minRat() {
        assertEvalTrue("{Bool testresult = min(-2/5, 3/5) == -2/5;}");
    }

    @Test
    public void maxFloat() {
        assertEvalTrue("{Bool testresult = max(-2.0, 3.0) == 3.0;}");
    }

    @Test
    public void minFloat() {
        assertEvalTrue("{Bool testresult = min(-2.0, 3.0) == -2.0;}");
    }

    @Test
    public void sqrt1() {
        assertEvalTrue("{Bool testresult = sqrt(4.0) == 2.0;}");
    }

    @Test
    public void logexp1() {
        assertEvalTrue("{Bool testresult = log(exp(1.0)) == 1.0;}");
    }

    @Test
    public void map1() {
	assertEvalTrue("{ Bool testresult = map((Int x) => x + 1)(list[1, 2, 3]) == list[2, 3, 4]; }");
    }

    @Test
    public void filter1() {
	assertEvalTrue("{ Bool testresult = filter((Int x) => x % 2 == 0)(list[1, 2, 3]) == list[2]; }");
    }

    @Test
    public void foldl1() {
	assertEvalTrue("{ Bool testresult = foldl((Int elem, Int acc) => elem + acc)(list[1, 2, 3], 0) == 6; }");
    }

    @Test
    public void foldr1() {
	assertEvalTrue("{ Bool testresult = foldr((Int elem, Int acc) => elem + acc)(list[1, 2, 3], 0) == 6; }");
    }
}
