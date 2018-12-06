/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package deadlock.analyser.factory;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.abs_models.frontend.typechecker.DataTypeType;
import com.gzoumix.semisolver.term.Term;

public class RecordDataType extends GenericStructuredTerm implements IRecord{

    public static final String _prefix = "DataType_";

    public RecordDataType(String name, List<Term> l) {
        super(name, l);

    }

    public RecordDataType(DataTypeType type, List<IRecord> r){
        super(_prefix + (type.getModuleName() == null ? type.getSimpleName():(type.getModuleName() + "." + type.getSimpleName())),
            new LinkedList<>());

        subterms.addAll(r);
    }

    public String getDataTypeName(){
        return getConstructor().substring(_prefix.length());
    }


    public List<IRecord> getSubRecords(){
        List<IRecord> result = new LinkedList<>();
        for(Term t : getSubTerms())
            result.add((IRecord)t);

        return result;
    }

    public String toString(){
        Iterator<Term> i = subterms.iterator();
        String res = getDataTypeName() + "[";
        if(i.hasNext()) res = res + " ";
        while (i.hasNext()) {
          res = res + (i.next().toString());
          if(i.hasNext()) res = res + "; ";
          else res = res + " ";
        }
        return res + "]";
      }

}
