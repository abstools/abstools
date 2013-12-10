/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.erlang;

import java.util.HashSet;
import java.util.LinkedHashMap;
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

    public void retainAll(Vars vars) {
        for (String k : Sets.difference(this.keySet(), vars.keySet()).immutableCopy())
            remove(k);

    }

    public String[] merge(Vars var1, Vars var2) {
        StringBuilder left = new StringBuilder(",");
        StringBuilder right = new StringBuilder(",");
        Set<String> used = new HashSet<String>();
        for (java.util.Map.Entry<String, Var> a : this.entrySet()) {
            if (a.getValue().isSet()) {
                used.add(a.getKey());
                int leftN = var1.get((Object) a.getKey()).getCount();
                int rightN = var2.get((Object) a.getKey()).getCount();
                if (a.getValue().getCount() != leftN || a.getValue().getCount() != rightN) {
                    if (leftN > rightN)
                        right.append(String.format("%s=%s,", var1.get(a.getKey()), var2.get(a.getKey())));
                    if (leftN < rightN)
                        left.append(String.format("%s=%s,", var2.get(a.getKey()), var1.get(a.getKey())));
                    a.setValue(new Var(Math.max(leftN, rightN), true));
                }
            }
        }
        temp = Math.max(var1.temp, var2.temp);
        for (String k : Sets.union(Sets.difference(var1.keySet(), used), Sets.difference(var2.keySet(), used)))
            this.put(k, new Var(Var.max(var1.get((Object) k), var2.get((Object) k)), false));
        left.deleteCharAt(left.length() - 1);
        right.deleteCharAt(right.length() - 1);
        return new String[] { left.toString(), right.toString() };
    }

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