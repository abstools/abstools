/**
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.erlang;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.List;
import java.util.Set;

import abs.frontend.ast.ParamDecl;
import abs.frontend.ast.VarOrFieldDecl;

import com.google.common.collect.Sets;

/**
 * Used for tracking variables through different scopes
 * 
 * @author Georg Göri
 * 
 */
public class Vars extends LinkedHashMap<String, Var> {
    public static final String PREFIX = "V_";
    private static final long serialVersionUID = 1L;
    private int temp = 1;

    /**
     * Copy Constructor
     */
    public Vars(Vars vars) {
        super(vars);
        temp = vars.temp;
    }

    public Vars() {
    }

    public static Vars n(abs.frontend.ast.List<ParamDecl> args) {
        Vars l = new Vars();
        for (ParamDecl n : args) {
            l.put(n.getName(), new Var(n.hasReferences()));
        }
        return l;
    }

    public static Vars n(String... name) {
        Vars l = new Vars();
        for (String n : name) {
            // Suspect that references may exist when we don't know the type.
            l.put(n, new Var(true));
        }
        return l;
    }

    public String getTemp() {
        return "T_" + temp++;
    }

    /**
     * Introduce a new variable
     */
    public String nV(VarOrFieldDecl d) {
        String name = d.getName();
        Var v = get((Object) name);
        if (v != null)
            if (v.isSet())
                throw new RuntimeException("Var already exists:"+name+":" +d.getFileName()+"@" + d.getStartLine()+"/"+d.getStartColumn());
            else
                put(name, v.inc());
        else
            put(name, new Var(d.hasReferences()));
        return get(name);
    }

    /**
     * Introduces a new variable and ignores if it overloads a previous one
     * (used for let)
     */
    public String nVignoreOverload(VarOrFieldDecl d) {
        String name = d.getName();
        put(name, new Var(d.hasReferences()));
        return get(name);
    }

    /**
     * Increase counter of variable
     */
    public String inc(String name) {
        // TODO: Review, can you get(name) directly and test for null, avoiding the duplicate lookup? [stolz]
        if (containsKey(name)) {
            put(name, super.get(name).inc());
        } else {
            // Suspect that references may exist when we don't know the type.
            put(name, new Var(true));
        }
        return get(name);
    }

    public void incAll() {
        for (java.util.Map.Entry<String, Var> a : this.entrySet())
            if (a.getValue().isSet())
                a.setValue(a.getValue().inc());
    }

    /**
     * Mark a variable as awaited (i.e. a get will not block).
     */
    public void await(String name) {
        Var v = super.get(name);
        if (v != null) {
            put(name, v.await());
        }
    }

    public boolean canBlock(String name) {
        Var v = super.get(name);

        if (v != null) {
            return v.canBlock();
        }

        return true;
    }

    /**
     * Get Erlang name of a variable
     */
    public String get(String name) {
        if (!super.get(name).isSet())
            throw new RuntimeException("Tried to access protected but not set var");
        int c = super.get(name).getCount();
        return PREFIX + name + "_" + c;
    }

    /**
     * Return a instance for a new branch
     */
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

    public String toStack() {
        StringBuilder sb = new StringBuilder();
        sb.append("[O");

        for (Map.Entry<String, Var> a : this.entrySet()) {
            Var v = a.getValue();
            if (!v.isSet() || !v.hasReferences()) {
                continue;
            }

            sb.append(",");

            sb.append(PREFIX).append(a.getKey()).append("_").append(v.getCount());
        }

        sb.append("|Stack]");

        return sb.toString();
    }

    /**
     * Removes all vars,which are not contained in vars
     */
    public void retainAll(Vars vars) {
        for (String k : Sets.difference(this.keySet(), vars.keySet()).immutableCopy())
            remove(k);

    }

    /**
     * Merges multiple Var instance in that one, so counters of variables are
     * set to maximum. To have this variables also bound, in each branch, this
     * method will return necessary code lines which set this variables
     * 
     */
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

                a.setValue(new Var(max.getCount(), true, a.getValue().hasReferences()));
            }
        }
        // Set temp to max
        for (Vars v : vars)
            temp = Math.max(temp, v.temp);

        // Hide all variables, which are not used in this instance
        Map<String, Boolean> allVars = new HashMap<String, Boolean>();
        for (Vars vs : vars) {
            for (Map.Entry<String, Var> v : vs.entrySet()) {
                if (!allVars.containsKey(v.getKey()) || !allVars.get(v.getKey())) {
                    allVars.put(v.getKey(), v.getValue().hasReferences());
                }
            }
        }

        for (String k : Sets.difference(allVars.keySet(), used))
            this.put(k, new Var(Var.max(vars, k).getCount(), false, allVars.get(k)));

        // Now that we know all vars across all branches, we know whether they may block
        for (Map.Entry<String, Var> v : entrySet()) {
            boolean canBlock = false;

            for (Vars vs : vars) {
                if (vs.containsKey(v.getKey())) {
                    canBlock |= vs.get((Object) v.getKey()).canBlock();
                }
            }

            if (!canBlock) {
                v.setValue(v.getValue().await());
            }
        }

        // Built return val
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
                this.put(k.getKey(), new Var(k.getValue().getCount(), false, k.getValue().hasReferences(), k.getValue().canBlock()));
        temp = Math.max(temp, child.temp);
    }

    public void updateTemp(Vars child) {
        temp = Math.max(temp, child.temp);
    }
}

/**
 * Variable tracking information
 */
class Var {

    static Var max(Collection<Vars> varsC, String name) {
        Var max = null;
        for (Vars vars : varsC)
            if (vars.containsKey(name) && (max == null ? -1 : max.getCount()) < vars.get((Object) name).getCount())
                max = vars.get((Object) name);
        return max;
    }

    private final int count;
    // False if it was used in an previous scope, so it has to be remembered but
    // not used
    private final boolean set;

    private final boolean hasReferences;

    private final boolean canBlock;

    public Var(int count, boolean set, boolean hasReferences, boolean canBlock) {
        this.count = count;
        this.set = set;
        this.hasReferences = hasReferences;
        this.canBlock = canBlock;
    }

    public Var(int count, boolean set, boolean hasReferences) {
        this(count, set, hasReferences, true);
    }

    public Var(boolean hasReferences) {
        this(0, true, hasReferences, true);
    }

    public Var await() {
        return new Var(count, set, hasReferences, false);
    }

    public Var inc() {
        return new Var(count + 1, true, hasReferences);
    }

    public int getCount() {
        return count;
    }

    public boolean isSet() {
        return set;
    }

    public boolean hasReferences() {
        return hasReferences;
    }

    public boolean canBlock() {
        return canBlock;
    }
}
