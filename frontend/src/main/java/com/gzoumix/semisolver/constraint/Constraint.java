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
import java.util.Queue;
import java.util.Set;

import java.io.PrintStream;

import com.gzoumix.semisolver.factory.Factory;
import com.gzoumix.semisolver.term.Term;
import com.gzoumix.semisolver.term.TermStructured;
import com.gzoumix.semisolver.term.TermVariable;
import com.gzoumix.semisolver.term.Variable;
import com.gzoumix.semisolver.substitution.Substitution;

public class Constraint {

  private static int semiCounter = 0;

  private Factory factory;
  private boolean hasRecursion;
  
  private Set<Edge> graphEdges;
  private Map<Variable,EquivClass> equiv;
  
  private List<Edge> queue;
  
  private List<SolvingError> bot;

  private PrintStream debugFile;
  
  public Constraint(Factory f) {
    factory = f;
    hasRecursion = false;
    
    graphEdges = new HashSet<Edge>(); 
    equiv = new HashMap<Variable,EquivClass>();
    
    queue = new LinkedList<Edge>();
    
    bot = new LinkedList<SolvingError>();

    debugFile = null;
  }
  
  /*********************************/
  /* First Part: Utility Functions */
  /*********************************/

  
  public List<SolvingError> getErrors() { return bot; }
  public boolean hasRecursion() { return hasRecursion; }
  
  public Substitution getSubstitution() {
    SubstitutionGraph g = new SubstitutionGraph(this);
    return g.getSubstitution(factory);
  }

  public Set<Variable> dom() {
    Set<Variable> res = new HashSet<Variable>();  
    for(Map.Entry<Variable, EquivClass> entry : equiv.entrySet()) { res.add(entry.getKey()); }
    return res;
  }

  public Set<EquivClass> getEquivClass() {
    Set<EquivClass> res = new HashSet<EquivClass>();
    for(Map.Entry<Variable, EquivClass> entry : equiv.entrySet()) {
      res.add(entry.getValue()); }
    return res;
  }

  public EquivClass getEquivClass(TermVariable t) { return getEquivClass(t.getVariable()); }
  public EquivClass getEquivClass(Variable v) {
    EquivClass eq = equiv.get(v);
    if(eq == null) {
      eq = new EquivClass(factory, this, v);
      equiv.put(v, eq);
      //try { eq.initDependencies(); } catch(Dependency.LoopException exp) {
      //  System.err.println("Dependency.LoopException caught in \"getEquivClass\": couldn't occur here!!??"); }
    }
    return eq;
  }

  public Set<EquivClass> getEquivClass(Set<Variable> vs) {
    Set<EquivClass> res = new HashSet<EquivClass>();
    for(Variable v : vs) res.add(this.getEquivClass(v));
    return res;
  }

  public Set<EquivClass> getPartialEquivClass(Set<Variable> svar) {
    Set<EquivClass> res = new HashSet<EquivClass>();
    EquivClass tmp;
    for(Variable v : svar) { tmp = equiv.get(v); if(tmp != null) { res.add(tmp); } }
    return res;
  }


  public void setDebugFile(PrintStream s) { debugFile = s; }

  Map<Variable, EquivClass> getEquivClassMapping() { return equiv; }


  private void debug (String s) { if(debugFile != null) {debugFile.println(s); } }
  
  
  /******************************/
  /* Second Part: Edge addition */
  /******************************/

  
  private void addEdge(Edge e) {
    debug("  Adding edge: " + e.toString());
    if (graphEdges.contains(e)) { }
    else { graphEdges.add(e); queue.add(e); }
  }



  
  public void addEquation(Information info, Term t1, Term t2) {
    if (t1 == t2) return;
    
    if ((t1 != null) && (!t1.hasNull())) {
      if ((t2 != null) && (!t2.hasNull())) {
        addEdge(new UnifEdge(info, t1, t2));
      } else throw new NullPointerException("Constraint.addEquation: The second term contains a forbidden null pointer");
    } else throw new NullPointerException("Constraint.addEquation: The first term contains a forbidden null pointer");
  }
  
  public void addSemiEquation(Information info, Term t1, Term t2) {
    if (t1 == t2) return;

    if ((t1 != null) && (!t1.hasNull())) {
      if ((t2 != null) && (!t2.hasNull())) {
        Integer i = new Integer(semiCounter);
        semiCounter += 1;
        addEdge(new SemiEdge(info, t1, i, t2));
      } else throw new NullPointerException("Constraint.addSemiEquation: The second term contains a forbidden null pointer");
    } else throw new NullPointerException("Constraint.addSemiEquation: The first term contains a forbidden null pointer");
  }

  void addAllEquation(List<Edge> l) { for(Edge e: l) { addEdge(e); } }
  
  public void add(Constraint c) {
    
    // For simple structure, simple union
    graphEdges.addAll(c.graphEdges);
    queue.addAll(c.queue);
    bot.addAll(c.bot);

    // Equivalence Class
    try {
    //  1. add the classes in the constraint
      Map<EquivClass, EquivClass> map = new HashMap<EquivClass, EquivClass>();
      for(EquivClass eq: c.getEquivClass()) { this.addAllEquation(eq.putIn(this, map)); }
    //  2. Update the references in the dependencies
      for(EquivClass ideq: map.values()) {
        for(Map.Entry<EquivClass, EquivClass> entry : map.entrySet()) {
          ideq.getDepends().replace(entry.getKey(), entry.getValue());
        }
      }
    } catch(Dependency.LoopException exp) {
      System.err.println("Dependency.LoopException caught in \"add(Constraint)\": couldn't occur here!!??"); }
  }
  
  /*****************************/
  /* Third Part: Closure Rules */
  /*****************************/

  private void ruleOne(History h, TermStructured t1, TermStructured t2) {
    if ((t1.getConstructor().contentEquals(t2.getConstructor())) && (t1.getSubTerms().size() == t2.getSubTerms().size())) {
      Iterator<Term> i1 = t1.getSubTerms().iterator();
      Iterator<Term> i2 = t2.getSubTerms().iterator();
      while(i1.hasNext()) { addEdge(new UnifEdge(HistoryFactory.ruleOne(h), i1.next(), i2.next())); }
    } else {
      bot.add(new SolvingErrorUnif(h, t1, t2));
      debug("  Error: Impossible to unify!");
    }
  }


  private void ruleTwo(History h, TermStructured t1, Integer i, TermStructured t2) {
    if ((t1.getConstructor().contentEquals(t2.getConstructor())) && (t1.getSubTerms().size() == t2.getSubTerms().size())) {
      Iterator<Term> i1 = t1.getSubTerms().iterator();
      Iterator<Term> i2 = t2.getSubTerms().iterator();
      while(i1.hasNext()) { addEdge(new SemiEdge(HistoryFactory.ruleTwo(h), i1.next(), i, i2.next())); }
    } else {
      bot.add(new SolvingErrorUnif(h, t1,i,t2));
      debug("  Error: Impossible to unify!");
    }
  }
    
  /**********************************/
  /* Fourth Part: Solving Algorithm */
  /**********************************/

  private void solveEdge(Edge e) {
    History h = e.getHistory();
    Term t1 = e.getLeft();
    Term t2 = e.getRight();

   debug("Solve: Dealing with the edge: " + e.toString() + " ...     ");
    
    try {
      if(e instanceof UnifEdge) {  
        if(t1 instanceof TermVariable) { addAllEquation(getEquivClass((TermVariable)t1).addUnif(h, ((TermVariable)t1).getVariable(), t2)); }
        else if(t2 instanceof TermVariable) { addAllEquation(getEquivClass((TermVariable)t2).addUnif(h, ((TermVariable)t2).getVariable(), t1)); }
        else { ruleOne(h, (TermStructured)t1, (TermStructured)t2); }
      } else {
        Integer i = ((SemiEdge)e).getI();
        if(t1 instanceof TermVariable) { addAllEquation(getEquivClass((TermVariable)t1).addSemiIm(h, ((TermVariable)t1).getVariable(), i, t2)); }
        else if(t2 instanceof TermVariable) { addAllEquation(getEquivClass((TermVariable)t2).addSemiInf(h, (TermStructured)t1, i, ((TermVariable)t2).getVariable())); }
        else { ruleTwo(h, (TermStructured)t1, i, (TermStructured)t2); }
      }
    } catch(Dependency.LoopException exp) {
      debug("  Error: Recursive definition found!");
      hasRecursion = true;
      bot.add(new SolvingErrorLoop(e, exp.getLoop()));
    }
  }
  
  public void solve() {
    while(!queue.isEmpty()) {
      List<Edge> tmp = queue;
      queue = new LinkedList<Edge>();
      for(Edge e : tmp) {
        solveEdge(e);
      }
    }
  }

  /************************/
  /* Sixth Part: ToString */
  /************************/

  
  public String toString() {
    
    String res =  "Constraint: " + (bot.isEmpty()? "[true]\n" : "[false]\n");
    // 1. print the queue
    res = res + "  Queue = " ;
    Iterator<Edge> ite = queue.iterator();
    while(ite.hasNext()) {
      res += ite.next().toString();
      if(ite.hasNext()) res = res + " /\\ ";
    } res = res + "\n";
    /*// 2. print the unification Graph
    res = res + "  Equation Graph = ";
    Iterator<UnifEdge> itu = unifEdges.iterator();
    while(itu.hasNext()) {
      res += itu.next().toString();
      if(itu.hasNext()) res = res + " /\\ ";
    }
    // 3. print the semi-unification Graph
    res = res + "  Semi-Equation Graph = \n     ";
    Iterator<SemiEdge> its = semiEdges.iterator();
    while(its.hasNext()) {
      res += its.next().toString();
      if(its.hasNext()) res = res + " /\\ ";
    }*/
    // 4. finally, the equivalence class
    int id = 0;
    Map<EquivClass, Integer> m = new HashMap<EquivClass, Integer>();
    Set<EquivClass> tmp = this.getEquivClass();
    for(EquivClass eq : tmp) { 
        m.put(eq, new Integer(id)); id += 1; 
    }
 
    res = res + "  Equivalence Classes = "; 
    if(tmp.isEmpty()) { 
        res += "Empty"; 
    } else {
        for(EquivClass eq : tmp) {
        res += "\n    Class " + m.get(eq).toString() + "\n" + eq.toString(m);
        }
    } 
    
    res += "\n";    
    return res;
  }
  
}


