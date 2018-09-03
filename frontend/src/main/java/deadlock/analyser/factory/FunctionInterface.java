/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package deadlock.analyser.factory;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.gzoumix.semisolver.term.Term;
import com.gzoumix.semisolver.term.TermVariable;
import com.gzoumix.semisolver.term.Variable;

public class FunctionInterface extends GenericStructuredTerm {

    public final static String name = "functionInterface";

    public FunctionInterface(List<Term> l) { super(name, l); }
    public FunctionInterface(List<Term> l, Term rreturn) {
        super(name, new LinkedList<>());
        this.subterms.addAll(l);
        this.subterms.add(rreturn);
    }

//    public FunctionInterface generalize(Factory df) {
//        Set<TermVariable> s = this.fvTerm();
//        HashMap<Variable, TermVariable> map = new HashMap<>();
//
//        for(TermVariable v: s)
//            map.put(v.getVariable(), df.freshTermVariableFromTerm(v));
//
//        FunctionInterface res = df.clone(this);
//        for(Map.Entry<Variable, TermVariable>entry : map.entrySet()) {
//            res.substitute(entry.getKey(), entry.getValue()));
//
//        }
//        return res;
//    }

}
