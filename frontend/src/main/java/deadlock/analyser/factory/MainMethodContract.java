/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package deadlock.analyser.factory;

import java.util.List;

import com.gzoumix.semisolver.term.Term;

public class MainMethodContract extends MethodContract {

    public MainMethodContract(Contract cp, Contract cf) {
        super(null, cp, cf);
        // TODO Auto-generated constructor stub
    }

    @Override
    public String toString() {
      return "MAIN method" + " < " + (subterms.get(1).toString()) + " , " + (subterms.get(2).toString()) + " > ";
    }

}
