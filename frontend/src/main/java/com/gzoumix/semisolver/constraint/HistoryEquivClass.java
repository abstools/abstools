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
import java.util.Map;
import java.util.Set;

import com.gzoumix.semisolver.term.Term;
import com.gzoumix.semisolver.term.TermVariable;
import com.gzoumix.semisolver.term.Variable;

public class HistoryEquivClass {

  private History im;
  private Map<Variable, Set<History>> dom;
  private Map<Integer, History> semiim;
  private EquivClass owner;

  /* 1. Constructor */
  public HistoryEquivClass(EquivClass eq) { im = null; dom = new HashMap<Variable, Set<History>>(); semiim = new HashMap<Integer, History>(); owner = eq; }

  /* 2. Basic Extension */
  public void put(Variable v, History h) {
    this.get(v).add(h);
    if(h.isSemiEdge()) { semiim.put(h.getI(), h); }
    else if(h.hasStructured()) { im = h; }
    else if(h.isVariableHistory()) {
      Variable v2 = h.getFollowing(v);
      this.get(v2).add(h); 
    }
  }

  public void putAll(HistoryEquivClass heq) {
    if(im == null) { im = heq.im; }
    for(Map.Entry<Variable, Set<History>> entry : heq.dom.entrySet()) {
      Set<History> res = this.dom.get(entry.getKey());
      if(res == null) { res = new HashSet<History>(); this.dom.put(entry.getKey(), res); }
      res.addAll(entry.getValue());
    }
    for(Map.Entry<Integer, History> entry : heq.semiim.entrySet()) {
      if(!this.semiim.containsKey(entry.getKey())) { this.semiim.put(entry.getKey(), entry.getValue()); }
    }
  }

  /* 3. Getters */
  public History getIm() { return im; }
  public History getSemiIm(Integer i) { return semiim.get(i); }

  public Set<History> get(Variable v) {
    if(dom.containsKey(v)) { return dom.get(v); }
    else { Set<History> res = new HashSet<History>(); dom.put(v, res); return res; } }



  /* 4. Path Finding */
  HistoryPath findHistory(TermVariable v1, TermVariable v2) { return findHistory(v1.getVariable(), HistoryType.unif(v2)); }
  HistoryPath findHistory(Variable v) { return findHistory(v, HistoryType.unif(owner.getIm())); }
  HistoryPath findHistory(Variable v, Integer i) { return findHistory(v, HistoryType.semi(i, owner.getSemiIm(i))); }


  HistoryPath findHistory(Variable var, HistoryType t) { // follow the graph of variables from [v], to find the term in t.
    Set<Variable> checked = new HashSet<Variable>();
    Set<Variable> toCheck = new HashSet<Variable>();
    toCheck.add(var);
    
    Map<Variable, HistoryPath> map = new HashMap<Variable, HistoryPath>();

    try{
    while(!toCheck.isEmpty()) {
      Set<Variable> tmp = toCheck;
      toCheck = new HashSet<Variable>();
      for(Variable v : tmp) {
        HistoryPath path = map.get(v);
        for(History h : dom.get(v)) {
          if(t.validates(h)) { return new HistoryPath(path, h); }
          Variable follow = h.getFollowing(v);
          if((follow != null) && (!checked.contains(follow))) {toCheck.add(follow); map.put(follow, new HistoryPath(path, h)); }
        }
        checked.add(v);
      }
    }
    } catch(Exception e) { e.printStackTrace(); } 
    System.out.println("Error while looking for a path for \"" + var.toString() + t.toString() + "\"");
    System.out.println("Available edges:");
    for(Map.Entry<Variable, Set<History>> entry : dom.entrySet()) {
      System.out.print("  " + entry.getKey().toString() + " => ");
      Iterator<History> ite = entry.getValue().iterator();
      while(ite.hasNext()) {
        System.out.print(ite.next().getEdge().toString());
        if(ite.hasNext()) { System.out.print(" /\\ "); }
      } System.out.println("");

    }
    System.out.println(owner.toString(new HashMap<EquivClass, Integer>()));
    
    return null;
  }



}


