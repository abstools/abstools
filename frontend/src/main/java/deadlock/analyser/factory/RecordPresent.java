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

public class RecordPresent extends GenericStructuredTerm implements IRecord {

  public static String name = "RecordPresent";

  /* Constructors */
  public RecordPresent(List<Term> l) { super(name, l); }
  public RecordPresent(GroupName a, List<RecordField> fields) {
    super(name, new LinkedList<>());
    subterms.add(a);
    subterms.addAll(fields);
  }


  /* Basic Get */
  public GroupName getRoot() {
    return ((GroupName)(getSubTerms().get(0)));
  }

  public IRecord getField(String name) {
    IRecord res;
    for(Term sub : subterms.subList(1, subterms.size())) {
      res = ((RecordField)sub).getField(name);
      if(res != null) return res;
    }
    return null;
  }

  /* toString */
  public String toString(){
    Iterator<Term> i = subterms.iterator();
    String res = (i.next().toString()) + "[";
    if(i.hasNext()) res = res + " ";
    while (i.hasNext()) {
      res = res + (i.next().toString());
      if(i.hasNext()) res = res + "; ";
      else res = res + " ";
    }
    return res + "]";
  }

}

