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

import java.util.List;
import java.util.ArrayList;
import java.util.LinkedList;

public class HistoryPath {

  private List<History> list;

  /* Constructor */
  public HistoryPath(HistoryPath p, History h) {
    if(p == null) { list = new ArrayList<History>(1); }
    else {
      list = new ArrayList<History>(p.list.size() + 1);
      list.addAll(p.list);
    }
      list.add(h);
  }

  public List<Information> getInformations() {
    List<Information> res = new LinkedList<Information>();
    for(History h : list) { res.addAll(h.getInformations()); }
    return res;
  }

}


