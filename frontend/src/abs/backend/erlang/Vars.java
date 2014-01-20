/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.erlang;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Set;

import abs.frontend.ast.ParamDecl;

import com.google.common.collect.Sets;

public class Vars extends LinkedHashMap<String, Var> {

    private static final long serialVersionUID = 1L;
    private int temp = 1;

    public Vars(Vars vars) {
        super(vars);
        temp = vars.temp;
    }

    public Vars() {
    }

    public static Vars n(abs.frontend.ast.List<ParamDecl> args) {
        Vars l = new Vars();
        for (ParamDecl n : args)
            l.put(n.getName(), new Var());
        return l;
    }

    public static Vars n(String... name) {
        Vars l = new Vars();
        for (String n : name)
            l.put(n, new Var());
        return l;
    }

    public String getTemp() {
        return "T_" + temp++;
    }

    public String nV(String name) {
        Var v = get((Object) name);
        if (v != null)
            if (v.isSet())
                throw new RuntimeException("Var already exists");
            else
                put(name, v.inc());
        else
            put(name, new Var());
        return get(name);
    }

    public String nVignoreOverload(String name) {
        put(name, new Var());
        return get(name);
    }

    public static final String PREFIX = "V_";

    public String inc(String name) {
        if (containsKey(name))
            put(name, super.get(name).inc());
        else
            put(name, new Var());
        return get(name);
    }

    public void incAll() {
        for (java.util.Map.Entry<String, Var> a : this.entrySet())
            if (a.getValue().isSet())
                a.setValue(a.getValue().inc());
    }

    public String get(String name) {
        if (!super.get(name).isSet())
            throw new RuntimeException("Tried to access protected but not set var");
        int c = super.get(name).getCount();
        return PREFIX + name + "_" + c;
    }

    public Vars pass() {
        return new Vars(this);
    }

    public String toParamList() {
        StringBuilder sb = new StringBuilder("[");
        boolean first = true;
        for (java.util.Map.Entry<String, Var> a : this.entrySet()) {
            if (a.getValue().isSet()) {
                if (!first)
                    sb.append(',');
                first = false;
                sb.append(PREFIX).append(a.getKey()).append("_").append(a.getValue().getCount());
            }
        }
        return sb.append("]").toString();
    }

    /**
     * Removes all vars,which are not contained in vars
     */
    public void retainAll(Vars vars) {
        for (String k : Sets.difference(this.keySet(), vars.keySet()).immutableCopy())
            remove(k);

    }

    public List<String> merge(List<Vars> vars) {
        List<StringBuilder> mergeLines = new ArrayList<StringBuilder>(vars.size());
        for (int i = 0; i < vars.size(); i++)
            mergeLines.add(new StringBuilder(","));
        Set<String> used = new HashSet<String>();
        for (java.util.Map.Entry<String, Var> a : this.entrySet()) {
            if (a.getValue().isSet()) {
                used.add(a.getKey());
                Var max = Var.max(vars, a.getKey());
                Iterator<Vars> itV = vars.iterator();
                Iterator<StringBuilder> itM = mergeLines.iterator();
                while (itV.hasNext()) {
                    Var v = itV.next().get((Object) a.getKey());
                    if (v.getCount() < max.getCount())
                        itM.next()
                                .append(String.format("V_%s_%s=V_%s_%s,", a.getKey(), max.getCount(), a.getKey(),
                                        v.getCount()));
                    else
                        itM.next();
                }
                a.setValue(new Var(max.getCount(), true));
            }
        }
        for(Vars v : vars)
            temp=Math.max(temp,v.temp);
        Set<String> allVars = new HashSet<String>();
        for (Vars v : vars)
            allVars.addAll(v.keySet());

        for (String k : Sets.difference(allVars, used))
            this.put(k, new Var(Var.max(vars, k).getCount(), false));
        List<String> res = new ArrayList<String>(vars.size());
        for (StringBuilder sb : mergeLines) {
            sb.deleteCharAt(sb.length() - 1);
            res.add(sb.toString());
        }
        return res;
    }

    /**
     * Simplified version of merge
     */
    public void hideIntroduced(Vars child) {
        for (java.util.Map.Entry<String, Var> k : child.entrySet())
            if (!containsKey(k.getKey()) || !k.getValue().isSet())
                this.put(k.getKey(), new Var(k.getValue().getCount(), false));
        temp = Math.max(temp, child.temp);
    }
}

class Var {
    static int max(Var v1, Var v2) {
        if (v1 == null)
            return v2.count;
        if (v2 == null)
            return v1.count;
        return Math.max(v1.count, v2.count);
    }

    static Var max(Collection<Vars> varsC, String name) {
        Var max = null;
        for (Vars vars : varsC)
            if (vars.containsKey(name) && (max == null ? -1 : max.getCount()) < vars.get((Object) name).getCount())
                max = vars.get((Object) name);
        return max;
    }

    private final int count;
    private final boolean set;

    public Var(int count, boolean set) {
        super();
        this.count = count;
        this.set = set;
    }

    public Var() {
        count = 0;
        set = true;
    }

    public Var inc() {
        return new Var(count + 1, true);
    }

    public int getCount() {
        return count;
    }

    public boolean isSet() {
        return set;
    }

}
