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
import java.util.Iterator;

import com.gzoumix.semisolver.term.Term;
import com.gzoumix.semisolver.term.TermStructured;

public class Contract extends GenericStructuredTerm {


  public static final String name = "Contract";

  /* Constructors */
  public Contract(List<Term> l) { super(name, l); }
  public Contract() { super(name, new LinkedList()); }

  public Contract(ContractElement e) { this(); this.getSubTerms().add(e); }

  /* Basic Extension */
  public void add(Contract c) { subterms.addAll(c.subterms); }

  public void fusion(Contract c) {
    if(c.subterms.isEmpty()) { return; }
    if(this.subterms.isEmpty()) { this.add(c); return; }

    Term end = this.subterms.get(this.subterms.size() -1);
    Term first = c.subterms.get(0);
    Term res = null;
    if(end instanceof ContractElementInvk) {
      if(first instanceof ContractElementAwait) {
       res = new ContractElementInvkA(((ContractElementInvk)end).getPosition(), (ContractElementInvk)end, (ContractElementAwait)first);
      } else if(first instanceof ContractElementGet) {
       res = new ContractElementInvkG(((ContractElementInvk)end).getPosition(), (ContractElementInvk)end, (ContractElementGet)first);
      } }
    if(res != null) {
      this.subterms.set(this.subterms.size() - 1, res);
      this.subterms.addAll(c.subterms.subList(1, c.subterms.size()));
    } else { this.add(c); }
  }

  // TODO: remove because uselless now.
/*  public void clean() { // remove extra dependences
    LinkedList<Term>subterms_cleaned = new LinkedList<Term>();

    // 1. Recursively clean
    for(Term t : this.getSubTerms()) {
      if(t instanceof ContractElementUnion) {
         ContractElementUnion u = (ContractElementUnion)t;
         u.getBranchOne().clean();
         u.getBranchTwo().clean();
      }
    }

    // 2. local clean
    Iterator<Term> i = getSubTerms().iterator();
    if(!i.hasNext()) return; // no subterms, nothing to do

    Term first = i.next();
    Term second;
    while(i.hasNext()) {
      second = i.next();
      if((first instanceof ContractElementInvkA) && (second instanceof ContractElementGet)) {
        GroupName afirst  = ((ContractElementInvkA)first).getAwait().whosWaiting();
        GroupName asecond = ((ContractElementGet)second).whosWaiting();
        GroupName bfirst = ((ContractElementInvkA)first).getAwait().whosWaited();
        GroupName bsecond = ((ContractElementGet)second).whosWaited();

        if(!((afirst.equals(asecond)) && (bfirst.equals(bsecond)))) {
          subterms_cleaned.addLast(first);
          first = second;
        }
      } else { subterms_cleaned.addLast(first); first = second; }
    }

    subterms_cleaned.addLast(first);
    this.subterms = subterms_cleaned;
  }*/



  /* Basic Get */
  public List<ContractElement> getList() {
    List<ContractElement> res = new LinkedList<>();
    for(Term t : getSubTerms()) {
        res.add((ContractElement)t);
    }
    return res;
  }

  /* toString */
  public String toString(){
    if(subterms.isEmpty()) return "0";

    Iterator<Term> i = subterms.iterator();
    String res = "";
    while (i.hasNext()) {
      res = res + (i.next().toString());
      if(i.hasNext()) res = res + "; ";
    }
    return res;
  }


}

