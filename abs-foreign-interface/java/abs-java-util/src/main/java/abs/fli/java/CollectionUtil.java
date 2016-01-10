package abs.fli.java;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;

import ABS.StdLib.List;
import ABS.StdLib.List_Cons;
import ABS.StdLib.List_Nil;
import ABS.StdLib.Map;
import ABS.StdLib.Map_EmptyMap;
import ABS.StdLib.Map_InsertAssoc;
import ABS.StdLib.Pair_Pair;
import ABS.StdLib.Set;
import ABS.StdLib.Set_EmptySet;
import ABS.StdLib.Set_Insert;
import ABS.StdLib.reverse_f;
import abs.backend.java.lib.types.ABSValue;

/**
 * 
 * @author pwong
 *
 */
public class CollectionUtil {
	
    public <A extends ABSValue,B> java.util.List<B> convert(Fun<A,B> f, List<A> absList) {
        java.util.List<B> list = new ArrayList<B>();
        while (absList.isCons()) {
            List_Cons<A> cons = absList.toCons();
            list.add(f.evaluate(cons.getArg0()));
            absList = cons.getArg1();
        }
        return list;
    }

    public <A extends ABSValue,B> List<A> convert(Fun<B,A> f, java.util.List<B> List) {
        List<A> abslist = new List_Nil<A>();
        for (B v : List) {
            abslist = new List_Cons<A>(f.evaluate(v), abslist);
        }
        return reverse_f.apply(abslist);
    }
    
    public <A extends ABSValue,B> java.util.Set<B> convert(Fun<A,B> f, Set<A> absSet) {
        java.util.Set<B> set = new HashSet<B>();
        while (absSet.isInsert()) {
            Set_Insert<A> insert = absSet.toInsert();
            set.add(f.evaluate(insert.getArg0()));
            absSet = insert.getArg1();
        }
        return set;
    }

    public <A extends ABSValue,B> Set<A> convert(Fun<B,A> f, java.util.Set<B> set) {
        Set<A> absSet = new Set_EmptySet<A>();
        for (B v : set) {
            absSet = new Set_Insert<A>(f.evaluate(v), absSet);
        }
        return absSet;
    }

    public <A extends ABSValue,B,C extends ABSValue,D> java.util.Map<B, D> convert(Fun<A,B> f, Fun<C,D> g, Map<A, C> absMap) {
        java.util.Map<B, D> map = new HashMap<B, D>();
        while (absMap.isInsertAssoc()) {
            Map_InsertAssoc<A, C> insert = absMap.toInsertAssoc();
            Pair_Pair<A, C> pair = insert.getArg0().toPair();
            map.put(f.evaluate(pair.getArg0()), g.evaluate(pair.getArg1()));
            absMap = insert.getArg1();
        }
        return map;
    }

    public <A extends ABSValue,B,C extends ABSValue,D>  Map<A, C> convert(Fun<B,A> f, Fun<D,C> g, java.util.Map<B, D> map) {
        Map<A, C> absMap = new Map_EmptyMap<A, C>();
        for (B v : map.keySet()) {
            absMap = new Map_InsertAssoc<A, C>(
                        new Pair_Pair<A, C>(f.evaluate(v), g.evaluate(map.get(v))), 
                        absMap);
        }
        return absMap;
    }

    public <A extends ABSValue> java.util.List<A> convert(List<A> absList) {
        java.util.List<A> list = new ArrayList<A>();
        while (absList.isCons()) {
            List_Cons<A> cons = absList.toCons();
            list.add(cons.getArg0());
            absList = cons.getArg1();
        }
        return list;
    }
    
    public <A extends ABSValue> List<A> convert(java.util.List<A> List) {
        List<A> abslist = new List_Nil<A>();
        for (A v : List) {
            abslist = new List_Cons<A>(v, abslist);
        }
        return reverse_f.apply(abslist);
    }

    public <A extends ABSValue> java.util.Set<A> convert(Set<A> absSet) {
        java.util.Set<A> set = new HashSet<A>();
        while (absSet.isInsert()) {
            Set_Insert<A> insert = absSet.toInsert();
            set.add(insert.getArg0());
            absSet = insert.getArg1();
        }
        return set;
    }

    public <A extends ABSValue> Set<A> convert(java.util.Set<A> set) {
        Set<A> absSet = new Set_EmptySet<A>();
        for (A v : set) {
            absSet = new Set_Insert<A>(v, absSet);
        }
        return absSet;
    }

    public <A extends ABSValue,B extends ABSValue> java.util.Map<A, B> convert(Map<A, B> absMap) {
        java.util.Map<A, B> map = new HashMap<A, B>();
        if (absMap.isInsertAssoc()) {
            Map_InsertAssoc<A, B> insert = absMap.toInsertAssoc();
            Pair_Pair<A, B> pair = insert.getArg0().toPair();
            map.put(pair.getArg0(), pair.getArg1());
            map.putAll(convert(insert.getArg1()));
        }
        return map;
    }

    public <A extends ABSValue,B extends ABSValue> Map<A, B> convert(java.util.Map<A, B> map) {
        Map<A, B> absMap = new Map_EmptyMap<A, B>();
        for (A v : map.keySet()) {
            absMap = new Map_InsertAssoc<A, B>(
                        new Pair_Pair<A, B>(v, map.get(v)), 
                        absMap);
        }
        return absMap;
    }

}
