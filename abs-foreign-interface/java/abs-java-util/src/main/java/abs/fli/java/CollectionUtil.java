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
import abs.backend.java.lib.types.ABSValue;

/**
 * 
 * @author pwong
 *
 */
@SuppressWarnings("unchecked")
public class CollectionUtil {
    
    public <A extends ABSValue,B> java.util.List<B> convert(Fun<A,B> f, List<A> absList) {
        java.util.List<B> list = new ArrayList<B>();
        if (absList.isCons()) {
            List_Cons<A> cons = absList.toCons();
            list.add(f.evaluate(cons.getArg0()));
            list.addAll(convert(f,cons.getArg1()));
        }
        return list;
    }

    public <A extends ABSValue,B> List<A> convert(Fun<B,A> f, java.util.List<B> List) {
        List<A> abslist = new List_Nil<A>();
        for (B v : List) {
            abslist = new List_Cons<A>(f.evaluate(v), abslist);
        }
        return abslist;
    }
    
    public <A extends ABSValue,B> java.util.Set<B> convert(Fun<A,B> f, Set<A> absSet) {
        java.util.Set<B> set = new HashSet<B>();
        if (absSet.isInsert()) {
            Set_Insert<A> insert = absSet.toInsert();
            set.add(f.evaluate(insert.getArg0()));
            set.addAll(convert(f,insert.getArg1()));
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
        if (absMap.isInsertAssoc()) {
            Map_InsertAssoc<A, C> insert = absMap.toInsertAssoc();
            Pair_Pair<A, C> pair = insert.getArg0().toPair();
            map.put(f.evaluate(pair.getArg0()), g.evaluate(pair.getArg1()));
            map.putAll(convert(f,g,insert.getArg1()));
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

    public java.util.List<ABSValue> convert(List<ABSValue> absList) {
        java.util.List<ABSValue> list = new ArrayList<ABSValue>();
        if (absList.isCons()) {
            List_Cons<ABSValue> cons = absList.toCons();
            list.add(cons.getArg0());
            list.addAll(convert(cons.getArg1()));
        }
        return list;
    }
    
    public List<ABSValue> convert(java.util.List<ABSValue> List) {
        List<ABSValue> abslist = new List_Nil<ABSValue>();
        for (ABSValue v : List) {
            abslist = new List_Cons<ABSValue>(v, abslist);
        }
        return abslist;
    }

    public java.util.Set<ABSValue> convert(Set<ABSValue> absSet) {
        java.util.Set<ABSValue> set = new HashSet<ABSValue>();
        if (absSet.isInsert()) {
            Set_Insert<ABSValue> insert = absSet.toInsert();
            set.add(insert.getArg0());
            set.addAll(convert(insert.getArg1()));
        }
        return set;
    }

    public Set<ABSValue> convert(java.util.Set<ABSValue> set) {
        Set<ABSValue> absSet = new Set_EmptySet<ABSValue>();
        for (ABSValue v : set) {
            absSet = new Set_Insert<ABSValue>(v, absSet);
        }
        return absSet;
    }

    public java.util.Map<ABSValue, ABSValue> convert(Map<ABSValue, ABSValue> absMap) {
        java.util.Map<ABSValue, ABSValue> map = new HashMap<ABSValue, ABSValue>();
        if (absMap.isInsertAssoc()) {
            Map_InsertAssoc<ABSValue, ABSValue> insert = absMap.toInsertAssoc();
            Pair_Pair<ABSValue, ABSValue> pair = insert.getArg0().toPair();
            map.put(pair.getArg0(), pair.getArg1());
            map.putAll(convert(insert.getArg1()));
        }
        return map;
    }

    public Map<ABSValue, ABSValue> convert(java.util.Map<ABSValue, ABSValue> map) {
        Map<ABSValue, ABSValue> absMap = new Map_EmptyMap<ABSValue, ABSValue>();
        for (ABSValue v : map.keySet()) {
            absMap = new Map_InsertAssoc<ABSValue, ABSValue>(
                        new Pair_Pair<ABSValue, ABSValue>(v, map.get(v)), 
                        absMap);
        }
        return absMap;
    }

}
