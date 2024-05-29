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

package com.gzoumix.semisolver.term;

import java.util.Set;
import java.util.TreeSet;

public class TermVariable implements Term, Comparable<TermVariable>{

  protected Variable v;
  
  public TermVariable(Variable v) { 
      this.v = v; 
      if(v!=null)
          v.setKind(this); 
  }
  
  public Variable getVariable() { return v; }
    
  public boolean strictlyContains(Term t) { return false; } // not possible to be deep into a variable

  public Set<Variable> fv() {
    TreeSet<Variable> res = new TreeSet<Variable>();
    res.add(v);
    return res;
  }

  public Set<TermVariable> fvTerm() {
    TreeSet<TermVariable> res = new TreeSet<TermVariable>();
    res.add(this);
    return res;
  }

  @Override
  public boolean equals(Object obj) {
      if (obj == this) {
          return true;
      }
      if (obj == null || ! (obj instanceof TermVariable )){
          return false;
      }

     return this.v.compareTo(((TermVariable)obj).v) == 0;
  }
 
  @Override
  public int hashCode() {
      return this.v.hashCode();
  }

  @Override
  public boolean equals(Term t) {
    return this.equals((Object)t);
  }

  public boolean equals(Variable var) { return (v.compareTo(var) == 0); }

  @Override
  public int compareTo(TermVariable tv) { return v.compareTo(tv.v);  }
  
  public String toString() { return v.toString(); }

 
  public int compareTo(Term t) {
    if(t instanceof TermVariable) {
      return this.v.compareTo(((TermVariable) t).v);
    } else return 1;
  }
  
  public boolean hasNull() { return false; }

}
