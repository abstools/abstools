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

package com.gzoumix.semisolver.constraint;


import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import com.gzoumix.semisolver.factory.Factory;
import com.gzoumix.semisolver.substitution.Substitution;
import com.gzoumix.semisolver.term.Term;
import com.gzoumix.semisolver.term.TermStructured;
import com.gzoumix.semisolver.term.TermVariable;
import com.gzoumix.semisolver.term.Variable;

class EquivClass {

  private Factory factory;
  private Constraint constraint;
  
  private Term im;
  private boolean isStructured;
  private Set<Variable> dom;
  private Map<Integer,Term> semiim;
  
  private Dependency depends;

  private HistoryEquivClass histories;
  
  /* 1. Constructors */
  
  public EquivClass(Factory f, Constraint c, Variable v) {
    factory = f;
    constraint = c;
    
    im = factory.termVariable(v);
    dom = new HashSet<Variable>();
    dom.add(v);
    isStructured = false;
    
    semiim = new HashMap<Integer, Term>();
    
    //depends = null;
    depends = new Dependency(constraint, this);

    histories = new HistoryEquivClass(this);
  }

/*
  public EquivClass(Factory f, Constraint c, Variable v, TermStructured t) {
    factory = f;
    
    im = t;
    dom = new HashSet<Variable>();
    dom.add(v);
    isStructured = true;
    
    semiim = new HashMap<Integer,Term>();
    
    depends = null;
    //solves = new HashSet<EquivClass>();
  }
*/


  /* 2. Utility Methods */

  public Term getIm() { return im; }
  public boolean isStructured() { return isStructured; }
  public Set<Variable> dom() { return dom; }
  public int size() { return dom.size(); }

  public Term getSemiIm(Integer i) { return semiim.get(i); }

  //public void initDependencies() throws Dependency.LoopException { depends = new Dependency(constraint, this); depends.init(); }
  public Dependency getDepends() { return depends; }

  
  /* 4. Equivalence Class Extension */
  
  
  public List<Edge> addSemiIm(History h, Variable v, Integer i, Term t) throws Dependency.LoopException {
    List<Edge> res = new LinkedList<Edge>();    
    depends.addSemiIm(h, i, t);
  
    Term sim = semiim.get(i);
    if(sim == null) {
      histories.put(v, h);
      semiim.put(i, t);
      if(isStructured) {
        History hres = HistoryFactory.addSemiImStepOne(h, histories.findHistory(v)); // [this.im] ~ [v] <_i [t]
        res.add(new SemiEdge(hres, im, i, t));
      }
    } else {
      History hres = HistoryFactory.addSemiImStepTwo(h, histories.findHistory(v, i)); // [v] ~<_i [t'] /\ [v] <_i [t]
      res.add(new UnifEdge(hres, sim, t));
    }
    
    return res;
  }

  public List<Edge> addSemiInf(History h, TermStructured t, Integer i, Variable v) throws Dependency.LoopException {
    List<Edge> res = new LinkedList<Edge>();    
    depends.addSemiInf(h, t, i);
    
    if(im instanceof TermStructured) {
      History hres = HistoryFactory.addSemiInfStepOne(h, histories.findHistory(v)); // [t] <_i [v] ~ [this.im]
      res.add(new SemiEdge(hres, t, i, im));
    } else {
      List<Term> subs = new LinkedList<Term>();
      TermVariable tmp;
      for(Term st : t.getSubTerms()) { subs.add(factory.freshTermVariableFromTerm(st)); }
      im = factory.newTerm(t.getConstructor(), subs); // we need to say that this comes from the history h
      isStructured = true;

      History hres1 = HistoryFactory.addSemiInfStepTwo(h);
      new UnifEdge(hres1, h.getEdge().getRight(), im); // set the edge in the history
      histories.put(v, hres1);
      depends.addUnif(hres1, im); // update dependency and solve
      
      History hres2 = HistoryFactory.addSemiInfStepThree(h, hres1); // [t] <_i [v] = c(t_j)
      res.add(new SemiEdge(hres2, t, i, im));


      // add the semi-unification constraints.
      for(Map.Entry<Integer, Term> entry : semiim.entrySet()) {
        History hres = HistoryFactory.addSemiInfStepFour(hres1, histories.findHistory(v, entry.getKey())); // c(t_j) = [v] ~<_i [t']
        res.add(new SemiEdge(hres, im, entry.getKey(), entry.getValue()));
      }
    }
    return res;
  }

  
  public List<Edge> addUnif(History h, Variable v, Term t) throws Dependency.LoopException {
    List<Edge> res = new LinkedList<Edge>();    
    depends.addUnif(h, t);
     
    if(t instanceof TermStructured) {
      if(im instanceof TermStructured) {
        History hres = HistoryFactory.addUnifStepOne(h, histories.findHistory(v)); // [t] = [v] ~ [this.im]
        res.add(new UnifEdge(hres, im, t));
      } else {
        histories.put(v, h); // we know why
        im = t;
        isStructured = true;
        for(Map.Entry<Integer, Term> entry : semiim.entrySet()) {
          History hres = HistoryFactory.addUnifStepTwo(h, histories.findHistory(v, entry.getKey())); // [t] = [v] ~<_i [t']
          res.add(new SemiEdge(hres, t, entry.getKey(), entry.getValue()));
        }
      }
    } else {
      EquivClass eq = constraint.getEquivClass((TermVariable)t);
      if(this.size() < eq.size()) res = this.add(h, v, ((TermVariable)t).getVariable(),  eq);
      else res = eq.add(h, ((TermVariable)t).getVariable(), v, this);
    }
    return res;
  }
  
  
  /* 5. Merge of Equivalence Classes */
  
    public List<Edge> add(History h, Variable here, Variable there, EquivClass eq) throws Dependency.LoopException {
    List<Edge> res = new LinkedList<Edge>();    
    if(eq == this) return res;
    if(this.constraint != eq.constraint) return res; //throw new Exception("EquivClass.add(EquivClass): classes do not match");
    
    depends.add(h, eq);
    dom.addAll(eq.dom); // set the domain
    if (isStructured && eq.isStructured) { // set the image
      History hres = HistoryFactory.addEquivClassStepOne(histories.findHistory(here), h, eq.histories.findHistory(there)); // why here = this.im and why there = eq.im?
      res.add(new UnifEdge(hres, this.im, eq.im));
    } else if (eq.isStructured) {
      im = eq.im;
      isStructured = true;
    }
    // set the semiim
    for(Map.Entry<Integer,Term> entry: eq.semiim.entrySet()) {
      Term t = semiim.get(entry.getKey());
      if (t != null) {
        History hres = HistoryFactory.addEquivClassStepTwo(histories.findHistory(here, entry.getKey()), h, eq.histories.findHistory(there, entry.getKey()));
                                                                                  // why here <_i this.semiim and why there <_i eq.semiim?
        res.add(new UnifEdge(hres, entry.getValue(), t));
      } else semiim.put(entry.getKey(), entry.getValue());
    }
    // and finally, update the reference to eq
    Map<Variable, EquivClass> equiv = constraint.getEquivClassMapping();
    for(Variable v : eq.dom()) equiv.put(v, this);
    for(EquivClass tmp : constraint.getEquivClass()) tmp.depends.replace(eq, this);

    // extend history
    histories.put(here, h);
    histories.putAll(eq.histories);

    return res;
  }
  
  public List<Edge> putIn(Constraint c, Map<EquivClass, EquivClass> mapRes) throws Dependency.LoopException {
    List<Edge> res = new LinkedList<Edge>();
    if(c == this.constraint) return res;
    
    // 1. Compute the set of EquivClass with which we fuse
    Set<EquivClass> seq = c.getPartialEquivClass(this.dom);

    // 2. find one EquivClass where to put [this]
    EquivClass newthis;
    if(seq.isEmpty()) { newthis = c.getEquivClass(this.dom.iterator().next()); }
    else { newthis = seq.iterator().next(); }

    // 3. Update [seq]
    seq.remove(newthis);
    seq.add(this);

    // 4. Add all equiv class in [seq] to [newthis]
    for(EquivClass eq : seq) {
      // 4.1. dom
      newthis.dom.addAll(eq.dom);
      // 4.2. histories
      newthis.histories.putAll(eq.histories);
      // 4.3. im and (semiim vs im)
      if(newthis.isStructured && eq.isStructured) {
        History hnt = newthis.histories.getIm();
        History heq = eq.histories.getIm();
        History hres = HistoryFactory.putInCoStepOne(hnt, newthis.histories.findHistory(hnt.getVariable(), heq.getVariable()), heq); // ([t] = [a]) ~ ([b] = [t'])
        res.add(new UnifEdge(hres, newthis.im, eq.im));
      } else if(eq.isStructured) {
        newthis.im = eq.im;
        newthis.isStructured = true;
        History heq = eq.histories.getIm();
        for(Map.Entry<Integer, Term> entry : newthis.semiim.entrySet()) {
          History hnt = newthis.histories.getSemiIm(entry.getKey());
          History hres = HistoryFactory.putInCoStepTwo(hnt, newthis.histories.findHistory(hnt.getVariable(), heq.getVariable()), heq); // ([t] = [a]) ~ ([b] <_i [t'])
          res.add(new SemiEdge(hres, eq.getIm(), entry.getKey(), entry.getValue()));
        }
      } else if(newthis.isStructured) {
        History hnt = newthis.histories.getIm();
        for(Map.Entry<Integer, Term> entry : eq.semiim.entrySet()) {
          History heq = eq.histories.getSemiIm(entry.getKey());
          History hres = HistoryFactory.putInCoStepThree(hnt, newthis.histories.findHistory(hnt.getVariable(), heq.getVariable()), heq); // ([t] = [a]) ~ ([b] <_i [t'])
          res.add(new SemiEdge(hres, newthis.getIm(), entry.getKey(), entry.getValue()));
        }
      }
      // 4.4. semiim
      for(Map.Entry<Integer, Term> entry : eq.semiim.entrySet()) {
        if(newthis.semiim.containsKey(entry.getKey())) {
          History hnt = newthis.histories.getSemiIm(entry.getKey());
          History heq = eq.histories.getSemiIm(entry.getKey());
          History hres = HistoryFactory.putInCoStepFour(hnt, newthis.histories.findHistory(hnt.getVariable(), heq.getVariable()), heq); // ([a] <_i [t]) ~ ([b] <_i [t'])
          res.add(new UnifEdge(hres, newthis.semiim.get(entry.getKey()), entry.getValue()));
        } else { newthis.semiim.put(entry.getKey(), entry.getValue()); }
      }
      // 4.5. depends
      newthis.depends.add(null, eq);
      // 4.6. update references in [c]
      Map<Variable, EquivClass> map = c.getEquivClassMapping();
      for(Variable v : eq.dom) { map.put(v, newthis); }
      // 4.7. finally set the map in parameter
      mapRes.put(this, newthis);
    }

    return res;
  }

/*
    // 1. Create a "copy" in c
    TermVariable idv = factory.freshTermVariableFromTerm(this.im);
    EquivClass ideq = c.getEquivClass(idv);
    ideq.im = this.im;
    ideq.isStructured = this.isStructured;
    ideq.semiim.putAll(this.semiim);
    ideq.depends = this.depends.copy();
    mapRes.put(this, ideq);
    
    // 1.2. generate the history for ideq
    ideq.histories.putAll(histories); // we put why the variables in ideq have the current im and semiim
    History hres = HistoryFactory.putInCoStepOne(); // create an empty history for idv = v
    Iterator<Variable> ith = dom.iterator(); Variable testy = ith.next();
    new UnifEdge(hres, idv, testy); // set the edge in the history
    ideq.histories.put(idv, hres); // and put the equivalence between the two variable in the class
    ideq.histories.put(testy, hres);


    // 2. link this copy to the other equivalence class in c
    Set<Variable> svc = c.dom();
    Set<Variable> tmp = new HashSet<Variable>(); tmp.addAll(this.dom);
    Iterator<Variable> it = tmp.iterator();
    while(it.hasNext()) {
      Variable v = it.next();
      if(dom.contains(v)) {
        History hresNext = HistoryFactory.putInCoStepTwo(ideq.histories.findHistory()); // <- cosa metterci ??
        res.add(new UnifEdge(idv, factory.termVariable(v))); // <- viene dal fatto che abbiamo già il vincolo. Si deve quindi trovare, 
        tmp.removeAll(c.getEquivClass(v).dom);
      } else {
        ideq.dom.add(v);
      }
    }
    return res;
  }*/



  public String toString(Map<EquivClass, Integer> m) {
    String res = "      Image = " + (isStructured? "[s]" : "[v]") + (im.toString()) + "\n";
    res       += "      Domain = "; for(Variable v : dom) { res += v.toString() + " "; } res += "\n";
    res       += "      Semi Image = "; if(semiim.isEmpty()) { res += "Empty\n"; } else {
      for(Map.Entry<Integer, Term> entry : semiim.entrySet()) { res += "(" + (entry.getKey()) + " = " + (entry.getValue().toString()) + ")";
      } res += "\n"; }
    res       += "      Depends on = \n" + depends.toString(m); 
    return res;
  }
}



