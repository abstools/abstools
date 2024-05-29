/**************************************************************************/
/*  Implementation of a simple semi-unification algorithm (Henglein 1993) */
/*  Copyright (C) 2012. Michael Lienhardt                                 */
/*                                                                        */
/*  This program is free software; you can redistribute it and/or modify  */
/*  it under the terms of the GNU General Public License as published by  */
/*  the Free Software Foundation; version 2 of the License.               */
/*                                                                        */
/*  This program is distributed in the hope that it will be useful, but   */
/*  WITHOUT ANY WARRANTY; without even the implied warranty of            */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     */
/*  General Public License for more details.                              */
/*                                                                        */
/*  You should have received a copy of the GNU General Public License     */
/*  along with this program; if not, write to the Free Software           */
/*  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA         */
/*  02110-1301 USA                                                        */
/*                                                                        */
/**************************************************************************/

package com.gzoumix.semisolver.substitution;


import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;

import com.gzoumix.semisolver.factory.Factory;
import com.gzoumix.semisolver.term.Term;
import com.gzoumix.semisolver.term.TermVariable;
import com.gzoumix.semisolver.term.TermStructured;
import com.gzoumix.semisolver.term.Variable;

import com.gzoumix.semisolver.constraint.Constraint;

public class Substitution {

  private Factory factory;
  private Map<Variable,Term> data;
  
  public Substitution(Factory f) { factory = f; data = new HashMap<Variable,Term>(); }

  public Substitution(Factory f, Map<Variable,Term> m) { factory = f; data = m; }
    
  public Term apply(Term t) {
    Term res;
    if(t instanceof TermVariable) {
      Term tv = data.get(((TermVariable)t).getVariable());
      if(tv == null) res = t;
      else res = tv;
    } else {
      String c = ((TermStructured)t).getConstructor();
      List<Term> l = ((TermStructured)t).getSubTerms();
      List<Term> resl = new LinkedList<Term>();
      for(Term ts : l) resl.add(this.apply(ts));
      res = factory.newTerm(c, resl);
    }
    return res;
  }
  
  public Set<Variable> dom() {
    Set<Variable> res = new HashSet<Variable>();
    for(Map.Entry<Variable, Term> entry : data.entrySet()) { res.add(entry.getKey()); }
    return res;
  }
  // puts (this o s) in this
  public void compose(Substitution s) {
    Substitution tmp = new Substitution(factory);
    for(Map.Entry<Variable, Term> entry : s.data.entrySet()) {
      tmp.data.put(entry.getKey(), this.apply(entry.getValue()));
    }
    merge(tmp);
  }
  
  public String toString() {
    String res = "\n";
    Iterator<Map.Entry<Variable,Term>> i = data.entrySet().iterator();
    Map.Entry<Variable, Term> tmp;
    while (i.hasNext()) {
      tmp = i.next();
      res = res + "  " + tmp.getKey().toString() + " -> " + tmp.getValue().toString() + "\n";
    }
    return res;
  }
  
  /*  */
  private void merge(Substitution s) {
    for(Map.Entry<Variable, Term> entry : s.data.entrySet()) {
      data.put(entry.getKey(), entry.getValue());
    }
  }
  
}
