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
import java.util.Set;
import java.util.HashMap;
import java.util.HashSet;

import com.gzoumix.semisolver.term.Term;
import com.gzoumix.semisolver.term.Variable;
import com.gzoumix.semisolver.factory.Factory;
import com.gzoumix.semisolver.substitution.Substitution;

class SubstitutionGraphNode {

  private Term im;
  private Set<Variable> dom;
  private Set<SubstitutionGraphNode> depends;
  private Set<SubstitutionGraphNode> solves;

  public SubstitutionGraphNode(EquivClass eq) {
    im = eq.getIm();
    dom = eq.dom();

    depends = new HashSet<SubstitutionGraphNode>();
    solves  = new HashSet<SubstitutionGraphNode>();
  }
  
  public void addDepends(SubstitutionGraphNode n) { depends.add(n); }
  public void addSolves(SubstitutionGraphNode n) { solves.add(n); }
  public void removeDepends(SubstitutionGraphNode n) { depends.remove(n); }
  
  public void apply(Substitution s) { im = s.apply(im); }

  public Set<SubstitutionGraphNode> getDepends() { return depends; }
  public Set<SubstitutionGraphNode> getSolves() { return solves; }

  public Substitution getSubstitution(Factory factory) {
    Map<Variable, Term> m = new HashMap<Variable, Term>();
    for(Variable v : dom) m.put(v, im);
    return new Substitution(factory, m);
  }
  
}
