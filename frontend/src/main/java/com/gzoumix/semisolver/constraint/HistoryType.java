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

import com.gzoumix.semisolver.term.Term;

public class HistoryType {

  enum Type { SEMI, UNIF };

  private Type type;
  private int semi;
  private Term term;

  public static HistoryType unif(Term t) { return new HistoryType(t); }
  public static HistoryType semi(Integer i, Term t) { return new HistoryType(i, t); }


  private HistoryType(Term t) { type = Type.UNIF; semi = 0; term = t; }
  private HistoryType(Integer i, Term t) { type = Type.SEMI; semi = i.intValue(); term = t; }

  public boolean validates(History h) {
    Edge edge= h.getEdge();
    boolean has = edge.getLeft().equals(term) || edge.getRight().equals(term);

    if(has && (type == Type.UNIF)) { return true; }
    else if(has && (edge instanceof SemiEdge) && (((SemiEdge)edge).getI().intValue() == semi)) { return true; }
    return false;
  }

  public String toString() {
    if(type == Type.UNIF) { return " = " + term.toString(); }
    else { return " <_" + semi + " " + term.toString(); }
  }

}


