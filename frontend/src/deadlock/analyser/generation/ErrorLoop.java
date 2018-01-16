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

package deadlock.analyser.generation;

import java.util.List;
import java.util.Iterator;
import java.util.LinkedList;

import com.gzoumix.semisolver.constraint.SolvingErrorLoop;
import com.gzoumix.semisolver.constraint.Edge;
import com.gzoumix.semisolver.constraint.History;
import com.gzoumix.semisolver.constraint.HistoryPath;
import com.gzoumix.semisolver.constraint.Information;

public class ErrorLoop implements GenerationError {

  class Inner { public Edge edge; public List<ErrorEdge> origin; public Inner(Edge e, List<ErrorEdge> o) { edge = e; origin = o; } }
  private List<Inner> origin;
  private Edge edge;

  public ErrorLoop(SolvingErrorLoop loop) {
    edge = loop.getEdge();
    origin = new LinkedList<>();

    List<ErrorEdge> o;
    for(History h : loop.getHistory().getList()) {
      o = new LinkedList<>();
      for(Information info : h.getInformations()) { o.add(new ErrorEdge((ASTNodeInformation)info)); }
      origin.add(new Inner(h.getEdge(), o));
    }
  }

  public String getHelpMessage() {
    String res = "*********************\n";
    res = res  + "* Cannot generate contract in presence of Recursive Structure\n";
    res = res  + "* Structure defined with the following edges:\n*****\n";
    Iterator<Inner> itin = origin.iterator();
    while(itin.hasNext()) {
      Inner in = itin.next();
      res = res + "* edge: " + in.edge.toString() + "\n***\n";
      Iterator<ErrorEdge> it = in.origin.iterator();
      while(it.hasNext()) {
        res = res + it.next().getHelpMessage();
        if(it.hasNext()) { res = res + "\n***\n"; } else { res = res + "\n"; }
      }
      if(itin.hasNext()) { res = res + "\n*****\n"; } else { res = res + "\n"; }
    }
    res = res  + "*********************";
    return res;
  }

}


