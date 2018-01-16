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

import abs.frontend.ast.ASTNode;

import com.gzoumix.semisolver.term.Term;

public class ContractElementUnion extends ContractElement {

  public static final String name = "ContractUnion";

  private Contract branchOne;
  private Contract branchTwo;

  /* Constructors */
  public ContractElementUnion(List<Term> l) { super(name, l); }

  public ContractElementUnion(ASTNode pos, Contract a, Contract b){
    super(pos, name, new LinkedList<>());
    subterms.add(a);
    subterms.add(b);
  }

  /* Basic Functions */
  public Contract getBranchOne() { return (Contract)subterms.get(0); }
  public Contract getBranchTwo() { return (Contract)subterms.get(1); }


  public String toString() {
    return "(" + (getBranchOne().toString()) + " + " + (getBranchTwo().toString()) + ")";
  }
}




