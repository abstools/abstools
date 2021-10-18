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

import java.util.LinkedList;
import java.util.List;
import java.util.ArrayList;

import com.gzoumix.semisolver.term.Term;
import com.gzoumix.semisolver.term.TermStructured;

public class MethodContract extends TermStructured {

  public final static String name = "MethodContract";

  /* Constructors */
  public MethodContract(List<Term> l) { super(name, l); }

  public MethodContract(MethodInterface mi, Contract cp, Contract cf){
    super(name, new ArrayList<>(2));
    subterms.add(mi);
    subterms.add(cp);
    subterms.add(cf);
  }

  /* Basic Get */
  public MethodInterface getMethodInterface() { return (MethodInterface)subterms.get(0); }
  public Contract getContractPresent() { return (Contract)subterms.get(1); }
  public Contract getContractFuture() { return (Contract)subterms.get(2); }

//  public Contract getContract(){
//      List<Term> pairSeq = new LinkedList<Term>();
//      pairSeq.add(getContractPresent());
//      pairSeq.add(getContractFuture());
//      return new Contract(pairSeq);
//  }

  /* Basic Manipulation */
  //public void clean() { getContractPresent().clean(); getContractFuture().clean(); }

  /* toString */
  @Override
  public String toString() {
    return (subterms.get(0).toString()) + " < " + (subterms.get(1).toString()) + " , " + (subterms.get(2).toString()) + " > ";
  }

}


