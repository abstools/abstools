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


package deadlock.constraints.factory;


import java.util.List;
import java.util.Map;
import java.util.TreeSet;

import deadlock.constraints.constraint.Constraint;
import deadlock.constraints.substitution.Substitution;
import deadlock.constraints.term.Term;
import deadlock.constraints.term.TermStructured;
import deadlock.constraints.term.TermVariable;
import deadlock.constraints.term.Variable;


public class Factory {

  
  public Factory() { }
  
  // Terms and Variables
  
  
  public TermVariable termVariable(Variable v) {
    return v.getKind();
  }

/* 
  public TermVariable freshTermVariable() {
    TermVariable res = new TermVariable(new Variable());
    return res;
  }
*/
  public TermVariable freshTermVariableFromTerm(Term t) {
    TermVariable res = new TermVariable(new Variable());
    return res;
  }
  
  public Term newTerm(String c, List<Term> l) {
    Term res = new TermStructured(c, l);
    return res;
  }
  
  // Constraints and Substitutions
  
  public Constraint newConstraint() { return new Constraint(this); }
  public Substitution newSubstitution() { return new Substitution(this); }
}



