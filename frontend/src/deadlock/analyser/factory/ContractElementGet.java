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
import java.util.ArrayList;

import abs.frontend.ast.ASTNode;

import com.gzoumix.semisolver.term.Term;

public class ContractElementGet extends ContractElement {

  public static String name = "ContractGet";

  /* Constructors */
  public ContractElementGet(List<Term> l) { super(name, l); }
  public ContractElementGet(ASTNode pos, GroupName a, GroupName b){
    super(pos, name, new ArrayList<>(2));
    subterms.add(a);
    subterms.add(b);
  }

  /* Basic Get */
  public GroupName whosWaiting() { return (GroupName)subterms.get(0); }
  public GroupName whosWaited() { return (GroupName)subterms.get(1); }

  /* toString */
  public String toString() {
    return "(" + (this.whosWaiting().toString()) + "," + (this.whosWaited().toString()) + ")";
  }


}

