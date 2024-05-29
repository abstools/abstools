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

public class SolvingErrorUnif implements SolvingError {

  private Edge edge;

  /* Constructors */

  public SolvingErrorUnif(History h, Term t1, Term t2) {
    edge = new UnifEdge(h, t1, t2);
  }

  public SolvingErrorUnif(History h, Term t1, Integer i, Term t2) {
    edge = new SemiEdge(h, t1, i, t2);
  }

  /* Basic Access */
  public Edge getEdge() { return edge; }

}


