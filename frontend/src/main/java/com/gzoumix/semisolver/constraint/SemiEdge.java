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

public class SemiEdge implements Edge {

  private Term t1, t2;
  private Integer i;
  private History history;
  
  public SemiEdge(History h, Term t1, Integer i, Term t2) {
    this.history = h; this.t1 = t1; this.i = i; this.t2 = t2; history.setEdge(this);
  }
  
  public Term getLeft() { return t1; }
  public Term getRight() { return t2; }
  public Integer getI() { return i; }

  public boolean equals(SemiEdge e) { return ((t1.equals(e.t1)) && (t2.equals(e.t2)) && (i.equals(e.i))); }

  public History getHistory() { return history; }

  public String toString() { return (t1.toString()) + " <_" + i + " " + (t2.toString()); }

}
