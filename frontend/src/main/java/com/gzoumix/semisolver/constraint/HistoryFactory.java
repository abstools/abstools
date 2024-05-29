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

public class HistoryFactory {

  // Constraints
  public static History ruleOne(History h) { return new HCRuleOne(h); }
  public static History ruleTwo(History h) { return new HCRuleTwo(h); }

  // EquivClass
    // addSemiIm
  public static History addSemiImStepOne(History h, HistoryPath pim) { return new HECSemiImOne(h, pim); }
  public static History addSemiImStepTwo(History h, HistoryPath psemi) { return new HECSemiImTwo(h, psemi); }
    // addSemiInf
  public static History addSemiInfStepOne(History h, HistoryPath pim) { return new HECSemiInfOne(h, pim); }
  public static History addSemiInfStepTwo(History hgenterm) { return new HECSemiInfTwo(hgenterm); }
  public static History addSemiInfStepThree(History h, History hnterm) { return new HECSemiInfThree(h, hnterm); }
  public static History addSemiInfStepFour(History hgenterm, HistoryPath psemi) { return new HECSemiInfFour(hgenterm, psemi); }
    // addUnif
  public static History addUnifStepOne(History h, HistoryPath pim) { return new HECUnifOne(h, pim); }
  public static History addUnifStepTwo(History h, HistoryPath psemi) { return new HECUnifTwo(h, psemi); }
    // add(EquivClass)
  public static History addEquivClassStepOne(HistoryPath pimhere, History h, HistoryPath pimthere) { return new HECAddECOne(pimhere, h, pimthere); }
  public static History addEquivClassStepTwo(HistoryPath psemihere, History h, HistoryPath psemithere) { return new HECAddECTwo(psemihere, h, psemithere); }
    // putIn(Constraint)
  public static History putInCoStepOne(History himhere, HistoryPath peq, History himthere) { return null; }
  public static History putInCoStepTwo(History hsemihere, HistoryPath peq, History himthere) { return null; }
  public static History putInCoStepThree(History himhere, HistoryPath peq, History hsemithere) { return null; }
  public static History putInCoStepFour(History hsemihere, HistoryPath peq, History hsemithere) { return null; }

}


