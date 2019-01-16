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
import java.util.Set;
import java.util.HashSet;


import com.gzoumix.semisolver.term.Term;
import com.gzoumix.semisolver.term.TermStructured;
import com.gzoumix.semisolver.term.TermVariable;

public class GenericStructuredTerm extends TermStructured {


  /* Constructors */
  public GenericStructuredTerm(String name, List<Term> l) { super(name, l); }

  /* Basic Get */
  public Set<GroupName> fn() {
    Set<TermVariable> set = this.fvTerm();
    Set<GroupName> res = new HashSet<>();
    for(TermVariable v : set) { if(v instanceof GroupName) { res.add((GroupName)v); } }
    return res;
  }

}


