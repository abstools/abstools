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
import com.gzoumix.semisolver.term.TermVariable;
import com.gzoumix.semisolver.term.Variable;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

public class HistoryDependency {

  private List<History> hlist;
  private List<History> equivs;

  /* Constructors */
  public HistoryDependency(History h) {
    hlist = new ArrayList<History>(1);
    hlist.add(h);
    equivs = new LinkedList<History>();
  }

  public HistoryDependency(History h, HistoryDependency hs) {
    hlist = new ArrayList<History>(hs.hlist.size() + 1);
    hlist.add(h);
    hlist.addAll(hs.hlist);
    equivs = new LinkedList<History>();
    equivs.addAll(hs.equivs);
  }

  /* Basic Access */
  public int size() { return hlist.size(); }
  public History head() { return hlist.get(0); }
  public List<History> getChain() { return hlist; }

  /* Basic Addition */
  public void addEquiv(History h) { if(h != null) { equivs.add(h); } }

  public List<History> getList() { return hlist; }


}


