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

public class Variable implements Comparable<Variable> {
  
  public static int varCounter = 0;
  private int id;

  private TermVariable kind;
  
  public Variable() {
    id = varCounter;
    varCounter += 1;
    kind = null;
  }

  public void setKind(TermVariable k) { kind = k; }
  public TermVariable getKind() { return kind; }

  public String toString() { 
    String res = "";
    int tmp = id + 1;
    
    do {
      tmp = tmp - 1;
      res = ((char)((tmp % 26) + 97)) + res;
      tmp = tmp / 26;
    } while (tmp != 0);
    
    return "'" + res;
  }

  
  public int compareTo(Variable v) {
    return this.id - v.id;
  }

}
