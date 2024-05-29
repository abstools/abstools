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
import java.util.Queue;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;

import com.gzoumix.semisolver.factory.Factory;
import com.gzoumix.semisolver.substitution.Substitution;

class SubstitutionGraph {

  private Set<SubstitutionGraphNode> graph;

  public SubstitutionGraph(Constraint c) {
    Map<EquivClass, SubstitutionGraphNode> init = new HashMap<EquivClass, SubstitutionGraphNode>();
    graph = new HashSet<SubstitutionGraphNode>();
    Set<EquivClass> seq = c.getEquivClass();

    for(EquivClass eq : seq) { // compute the set of nodes
      SubstitutionGraphNode n = new SubstitutionGraphNode(eq);
      graph.add(n);
      init.put(eq, n);
    }
    for(EquivClass eq : seq) { // compute solves and depends
      SubstitutionGraphNode n = init.get(eq);
      if(eq.isStructured()) {
        for(EquivClass deponEQ : c.getEquivClass(eq.getIm().fv())) {
          SubstitutionGraphNode depon = init.get(deponEQ);
          n.addDepends(depon);
          depon.addSolves(n);
        }
      }
    }
  }

  public Substitution getSubstitution(Factory factory) {
    Substitution res = new Substitution(factory);
    Queue<SubstitutionGraphNode> q = new LinkedList<SubstitutionGraphNode>();

    for(SubstitutionGraphNode n : graph) {// take the nodes without dependencies
      if(n.getDepends().isEmpty()) { q.add(n); } }

    while(!q.isEmpty()) {
      SubstitutionGraphNode n = q.poll();
      Substitution resNode = n.getSubstitution(factory);
      res.compose(resNode);
      for(SubstitutionGraphNode sol : n.getSolves()) {
        sol.apply(res);
        sol.removeDepends(n);
        if(sol.getDepends().isEmpty()) { q.add(sol); }
      }
    }
    return res;
  }  
}
