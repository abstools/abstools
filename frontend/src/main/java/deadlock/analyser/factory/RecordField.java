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

import com.gzoumix.semisolver.term.Term;
import com.gzoumix.semisolver.term.TermStructured;

public class RecordField extends GenericStructuredTerm implements IRecord { // not really a record, but it helps the debug

  public final static String prefix = "Field-";

  /* Constructors */
  public  RecordField(String name, List<Term> fields) { super(name, fields); }
  public RecordField(String name, IRecord r) {
    super(prefix + name, new LinkedList<>());
    subterms.add(r);
  }

  /* Basic Get */
  public String getName() {
    int begin = this.getConstructor().lastIndexOf("-") + 1;
    return getConstructor().substring(begin);
  }

  public IRecord getField(String name) {
    if(constructor.equals(prefix + name)) { return (IRecord)subterms.get(0); }
    else return null;
  }

  public String toString() { return this.getName() + ": " + this.subterms.get(0).toString(); }

}

