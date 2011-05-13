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
