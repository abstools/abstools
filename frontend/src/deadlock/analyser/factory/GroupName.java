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
//TODO ABEL: ERASE THIS CLASS, the only reason why this class exists is the visited flag, which is not used anymore
//it can be deleted until the contract classes stop using this (these classes dont use the flag either)
package deadlock.analyser.factory;

//TODO ABEL: Review DONE

import deadlock.constraints.term.TermVariable;
import deadlock.constraints.term.Variable;

public class GroupName extends TermVariable {
    
    //public boolean visited = false;

  public GroupName(Variable v) { super(v); }

  //TODO ABEL: ERASE THIS
  //public Set<GroupName> fn() { Set<GroupName> res = new HashSet<GroupName>(); res.add(this); return res; }

 

}

