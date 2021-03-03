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

package deadlock.analyser.factory;

import java.util.List;
import java.util.LinkedList;
import java.util.Iterator;

import com.gzoumix.semisolver.term.Term;
import com.gzoumix.semisolver.term.TermStructured;

public class MethodInterface extends TermStructured {

  public final static String name = "MethodInterface";

  /* Constructors */
  public MethodInterface(List<Term> l) { super(name, l); }
  public MethodInterface(IRecord r, List<IRecord> s, IRecord res){
    super(name, new LinkedList<>());
    subterms.add(r); subterms.addAll(s); subterms.add(res);
  }

  /* Basic Get */
  public IRecord getThis() { return (IRecord)(subterms.get(0)); }
  public IRecord getResult() {
      return (IRecord)(subterms.get(subterms.size() -1));
  }
  public List<IRecord> getParameters() {
    List<IRecord> res = new LinkedList<>();
    for(Term t : subterms.subList(1, subterms.size() -1)) { res.add((IRecord)t); }
    return res;
  }

  /* toString */
  public String toString(){
    Iterator<Term> i = subterms.iterator();
    String res = (i.next().toString()) + "(";
    Term tmp = i.next();
    if(i.hasNext()) res = res + " ";
    while (i.hasNext()) {
      res = res + (tmp.toString());
      tmp = i.next();
      if(i.hasNext()) res = res + ", ";
      else res = res + " ";
    }
    return res + ") -> " + tmp.toString();
  }

}


