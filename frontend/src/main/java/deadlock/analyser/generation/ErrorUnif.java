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

import com.gzoumix.semisolver.constraint.SolvingErrorUnif;
import com.gzoumix.semisolver.constraint.Edge;
import com.gzoumix.semisolver.constraint.History;
import com.gzoumix.semisolver.constraint.Information;

public class ErrorUnif implements GenerationError {

  private Edge edge;
  private List<ErrorEdge> origin;

  public ErrorUnif(SolvingErrorUnif err) {
    edge = err.getEdge();
    origin = new LinkedList<>();
    for(Information info : edge.getHistory().getInformations()) { origin.add(new ErrorEdge((ASTNodeInformation)info)); }
  }

  public String getHelpMessage() {
    String res = "*********************\n";
    res = res  + "* Cannot solve edge: " + edge.toString() + "\n";
    res = res  + "* Generated from:\n***\n";
    Iterator<ErrorEdge> it = origin.iterator();
    while(it.hasNext()) {
      res = res + it.next().getHelpMessage();
      if(it.hasNext()) { res = res + "\n***\n"; } else { res = res + "\n"; }
    }
    res = res  + "*********************";
    return res;
  }

}


