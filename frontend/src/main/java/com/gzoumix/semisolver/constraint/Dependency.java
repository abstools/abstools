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

import java.util.Map;
import java.util.HashMap;
//import java.util.Set;
//import java.util.HashSet;

import com.gzoumix.semisolver.term.Variable;
import com.gzoumix.semisolver.term.Term;
import com.gzoumix.semisolver.term.TermVariable;
import com.gzoumix.semisolver.term.TermStructured;

public class Dependency {

  private Map<EquivClass, HistoryDependency> shallow; // dependency that is not erroneous
  private Map<EquivClass, HistoryDependency> deep;    // loops there is an error

  private Map<Dependency, History> revShallow; // use to compute transitive closure of dependencies
  private Map<Dependency, History> revDeep;    // idem

  private EquivClass owner;
  private Constraint constraint;

  private boolean recursive;

  /* 1. Constructor */
  
  public Dependency(Constraint c, EquivClass eq) {
    shallow = new HashMap<EquivClass, HistoryDependency>();
    deep = new HashMap<EquivClass, HistoryDependency>();
    
    revShallow = new HashMap<Dependency, History>();
    revDeep = new HashMap<Dependency, History>();

    owner = eq;
    constraint = c;
    recursive = false;
  }
  
  /* 2. Exception for when we foud a loop */
  public class LoopException extends Exception {
    private HistoryDependency loop;
    public LoopException(HistoryDependency h) { loop = h; }
    public HistoryDependency getLoop() { return loop; } }

  
  /* 3. Simple Managing */
  
  public Dependency copy() {
    Dependency res = new Dependency(this.constraint, this.owner);
    res.shallow.putAll(this.shallow);
    res.deep.putAll(this.deep);
    res.revShallow.putAll(this.revShallow);
    res.revDeep.putAll(this.revDeep);
    res.recursive = this.recursive;
    return res;
  }

  public void addSimpleDeep(HistoryDependency h, EquivClass neq) throws LoopException {
    if(neq == owner) { raiseErrorLoop(h); }
    else if(!deep.containsKey(neq)) {
      deep.put(neq, h);
      if(h.size() == 1) { neq.getDepends().revDeep.put(this, h.head()); }
      for(Map.Entry<Dependency, History> entry : revShallow.entrySet()) { entry.getKey().addSimpleDeep(new HistoryDependency(entry.getValue(), h), neq); }
      for(Map.Entry<Dependency, History> entry : revDeep.entrySet())    { entry.getKey().addSimpleDeep(new HistoryDependency(entry.getValue(), h), neq); }
    }
  }

  public void addSimpleShallow(HistoryDependency h, EquivClass neq) throws LoopException {
    if(!(shallow.containsKey(neq) || deep.containsKey(neq))) {
      shallow.put(neq, h);
      if(h.size() == 1) { neq.getDepends().revShallow.put(this, h.head()); }
      for(Map.Entry<Dependency, History> entry : revShallow.entrySet()) { entry.getKey().addSimpleShallow(new HistoryDependency(entry.getValue(), h), neq); }
      for(Map.Entry<Dependency, History> entry : revDeep.entrySet())    { entry.getKey().addSimpleDeep(new HistoryDependency(entry.getValue(), h), neq); }
    }
  }

  /* 4. Complete Managing */

  // for adding a non structured semiinf
  private void addNormal(History h, EquivClass eq) throws LoopException {
    Dependency d = eq.getDepends();
    addSimpleShallow(new HistoryDependency(h), eq);
    for(Map.Entry<EquivClass, HistoryDependency> entry : d.shallow.entrySet()) { addSimpleShallow(new HistoryDependency(h, entry.getValue()), entry.getKey()); }
    for(Map.Entry<EquivClass, HistoryDependency> entry : d.deep.entrySet()) { addSimpleDeep(new HistoryDependency(h, entry.getValue()), entry.getKey()); }
  }

  // for adding an Image, or a structured semiinf
  private void addDeep(History h, EquivClass eq) throws LoopException {
    Dependency d = eq.getDepends();
    addSimpleDeep(new HistoryDependency(h), eq);
    for(Map.Entry<EquivClass, HistoryDependency> entry : d.shallow.entrySet()) { addSimpleDeep(new HistoryDependency(h, entry.getValue()), entry.getKey()); }
    for(Map.Entry<EquivClass, HistoryDependency> entry : d.deep.entrySet()) { addSimpleDeep(new HistoryDependency(h, entry.getValue()), entry.getKey()); }
  }

  // for adding an EquivClass
  private void addAll(History h, EquivClass eq) throws LoopException {
    Dependency d = eq.getDepends();
    HistoryDependency hd;
    for(Map.Entry<EquivClass, HistoryDependency> entry : d.shallow.entrySet()) {
      hd = entry.getValue(); hd.addEquiv(h); addSimpleShallow(hd, entry.getKey()); }
    for(Map.Entry<EquivClass, HistoryDependency> entry : d.deep.entrySet()) {
      hd = entry.getValue(); hd.addEquiv(h); addSimpleDeep(hd, entry.getKey()); }
    for(Map.Entry<Dependency, History> entry : d.revShallow.entrySet()) {
      if(!revShallow.containsKey(entry.getKey())) { revShallow.put(entry.getKey(), entry.getValue()); } }
    for(Map.Entry<Dependency, History> entry : d.revDeep.entrySet())    {
      if(!revDeep.containsKey(entry.getKey())) { revDeep.put(entry.getKey(), entry.getValue()); } }
  }


  private void raiseErrorLoop(HistoryDependency h) throws LoopException {
    recursive = true; throw new LoopException(h); 
  }
  
  /* 5. Mapping to EquivClass */
  
/*  public void init() throws LoopException {
    if(owner.isStructured()) {
      for(Variable v : owner.getIm().fv()) {
        EquivClass eqv = constraint.getEquivClass(v);
        addDeep(eqv);
      }
    }
  }*/
  
  
  public void addSemiIm(History h, Integer i, Term t) throws LoopException {
    if(t instanceof TermVariable) {
      constraint.getEquivClass((TermVariable)t).getDepends().addNormal(h, owner);
    }
  }

  public void addSemiInf(History h, TermStructured t, Integer i) throws LoopException {
    for(EquivClass eq : constraint.getEquivClass(t.fv())) { addDeep(h, eq); }
  }

  public void addUnif(History h, Term t) throws LoopException {
    if(t instanceof TermStructured) { // we depends only if structured
      for(EquivClass eq : constraint.getEquivClass(t.fv())) { addDeep(h, eq); }
    }
  }

  public void add(History h, EquivClass eq) throws LoopException { addAll(h, eq); }
  
  public void replace(EquivClass prev, EquivClass post) throws LoopException {
    if(prev == owner) return;
    
    boolean inShallow = shallow.containsKey(prev);
    boolean inDeep = deep.containsKey(prev);
    HistoryDependency hd;

    if((inDeep) && (post == this.owner)) { raiseErrorLoop(deep.get(prev)); }
    if(inShallow) { hd = shallow.get(prev); shallow.remove(prev); shallow.put(post, hd); }
    if(inDeep) { hd = deep.get(prev); deep.remove(prev); deep.put(post, hd); }
  }
  

  /* 6. toString */

  public String toString(Map<EquivClass, Integer> m) {
    String res = "        shallow = "; for(EquivClass eq : shallow.keySet()) { res += m.get(eq) + " "; } res += "\n";
    res       += "        deep    = "; for(EquivClass eq : deep.keySet())    { res += m.get(eq) + " "; } res += "\n";
    return res;
  }

}


