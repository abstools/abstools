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

import org.abs_models.frontend.ast.ASTNode;

import com.gzoumix.semisolver.term.Term;

public class ContractElementInvk extends ContractElement {

  public static String prefix = "ContractInvk-";

  /* Constructors */
  public ContractElementInvk(String name, List<Term> l) { super(name, l); } // DO NOT USE !!!
  public ContractElementInvk(ASTNode pos, String nameClass, String nameMethod, MethodInterface mi){
    super(pos, prefix + nameClass + "." + nameMethod, new ArrayList<>(1));
    subterms.add(mi);
  }

  /* Basic Get */
  public String getClassName() {
    int begin = this.getConstructor().lastIndexOf("-") + 1;
    int end = this.getConstructor().lastIndexOf(".");
    return this.getConstructor().substring(begin, end);
  }

  public String getMethodName() {
    int begin = this.getConstructor().lastIndexOf(".") + 1;
    return this.getConstructor().substring(begin);
  }

  public MethodInterface getMethodInterface() { return (MethodInterface)this.getSubTerms().get(0); }

  /* toString */
  public String toString() { return this.getClassName() + "!" + this.getMethodName() + ": " + this.getMethodInterface().toString(); }
}


