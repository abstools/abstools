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
import java.util.LinkedList;

public class HECAddECTwo extends History {

  private HistoryPath pathToSemiHere;
  private History history;
  private HistoryPath pathToSemiThere;

  public HECAddECTwo(HistoryPath psemihere, History h, HistoryPath psemithere) { super(); pathToSemiHere = psemihere; history = h; pathToSemiThere = psemithere; }

  public List<Information> getInformations() {
    List<Information> res = new LinkedList<Information>();
    res.addAll(pathToSemiHere.getInformations());
    res.addAll(history.getInformations());
    res.addAll(pathToSemiThere.getInformations());
    return res;
  }

}

