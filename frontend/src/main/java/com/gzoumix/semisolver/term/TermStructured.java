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

import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

public class TermStructured implements Term {

  protected String constructor;
  protected List<Term> subterms;
  
  public TermStructured(String c, List<Term> l) {
    constructor = c; subterms = l;
  }
  
  public String getConstructor() { return constructor; }
  public List<Term> getSubTerms() { return subterms; }
  
  public boolean equals(Term t) {
    if (t == this) return true;
    else if (t instanceof TermStructured) {
      TermStructured ts = (TermStructured)t;
      boolean b = (this.constructor.contentEquals(ts.constructor)) && (this.subterms.size() == ts.subterms.size());
      if (b) {
        Iterator<Term> i1 = this.subterms.iterator();
        Iterator<Term> i2 = ts.subterms.iterator();
        while (i1.hasNext()) {
          if(!i2.hasNext()) return false;
          if (!(i1.next()).equals(i2.next())) return false;
        } return true;
      } else return false;
    } else return false;
  }
  

  public boolean strictlyContains(Term t) {
    for(Term ts: subterms) { if (t == ts) return true; }
    for(Term ts: subterms) { if (ts.strictlyContains(t)) return true; }
    return false;
  }

  public Set<Variable> fv() {
    TreeSet<Variable> res = new TreeSet<Variable>();
    for(Term ts: subterms) { res.addAll(ts.fv()); }
    return res;
  }

  public Set<TermVariable> fvTerm() {
    TreeSet<TermVariable> res = new TreeSet<TermVariable>();
    for(Term ts: subterms) { res.addAll(ts.fvTerm()); }
    return res;
  }


  public String toString() {
    String res = constructor + '(';
    Iterator<Term> i = this.subterms.iterator();
    while (i.hasNext()) {
      res = res + (i.next().toString());
      if(i.hasNext()) res += ", ";
    }
    res = res + ")";
    return res;
  }

/*
  public int compareTo(Term t) {
    if(t instanceof TermStructured) {
      String tc = ((TermStructured) t).constructor;
      List<Term> ts = ((TermStructured) t).subterms;
      int res = this.constructor.compareTo(tc);
      if(res != 0) return res;
      else {
        Iterator<Term> ithis = subterms.iterator();
        Iterator<Term> it    = ts.iterator();
        while(ithis.hasNext()) {
          if(!it.hasNext()) return 1;
          res = ithis.next().compareTo(it.next());
          if(res != 0) return res;
        }
        return 0;
      }
      
    } else return -1;    
  }
*/
  public boolean hasNull() {
    for(Term ts: subterms) {
      if (ts == null) return true;
      else if (ts.hasNull()) return true;
    }
    return false;
  }


}
