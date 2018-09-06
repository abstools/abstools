/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package deadlock.analyser.factory;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import abs.frontend.ast.ASTNode;
import com.gzoumix.semisolver.term.Term;

public class ContractElementParallel extends ContractElement {

    public static final String name = "ContractParallel";

    public ContractElementParallel( List<Term> l) {
        super(name, l);
    }


    public ContractElementParallel(ASTNode pos, List<Contract> contracts){
        super(pos, name, new LinkedList<>());
        subterms.addAll(contracts);

      }

    public List<Contract> getContracts(){
        List<Contract> result = new LinkedList<>();
        for(Term t : getSubTerms())
            result.add((Contract)t);

        return result;
    }

    public String toString(){
        Iterator<Term> i = subterms.iterator();
        String res =  "(";
        if(i.hasNext()) res = res + " ";
        while (i.hasNext()) {
          res = res + (i.next().toString());
          if(i.hasNext()) res = res + " || ";
          else res = res + " ";
        }
        return res + ")";
      }
}
