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

import java.util.List;

import com.gzoumix.semisolver.term.Term;
import com.gzoumix.semisolver.term.TermVariable;
import com.gzoumix.semisolver.term.Variable;

public abstract class History {

  private Edge edge;

  /* 1. Basic Set/Get */
  void setEdge(Edge e) { edge = e; }
  public Edge getEdge() { return edge; }

  /* 2. Basic Test */
  public boolean isType(HistoryType t) { return t.validates(this); }
  public boolean isVariableHistory() { return (edge.getLeft() instanceof TermVariable) && (edge.getRight() instanceof TermVariable); }
  public boolean isSemiEdge() { return (edge instanceof SemiEdge); }
  public boolean isUnifEdge() { return (edge instanceof UnifEdge); }
  public boolean hasStructured() { return !this.isVariableHistory(); }


  /* 3. Basic Retrival */
  public Variable getFollowing(Variable v) {
    if((this.isVariableHistory()) && (this.isUnifEdge())) {
      Variable vl = ((TermVariable)edge.getLeft()).getVariable();
      Variable vr = ((TermVariable)edge.getRight()).getVariable();
      if(vl == v) { return vr; }
      if(vr == v) { return vl; }
    }
    return null;
  }

  public TermVariable getVariable() {
    if(edge.getLeft() instanceof TermVariable) { return (TermVariable)edge.getLeft(); }
    if(edge.getRight() instanceof TermVariable) { return (TermVariable)edge.getRight(); }
    return null;
  }

  public Integer getI() { return ((SemiEdge)edge).getI(); }

  public abstract List<Information> getInformations();

}


