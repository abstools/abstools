package org.abs_models.frontend.variablechecker;

import org.abs_models.frontend.ast.AppCond;

import java.util.ArrayList;
import java.util.HashMap;

public class ApplicationConstraints {
    private HashMap<String, AppCond> theta;

    public ApplicationConstraints() {
        this.theta = new HashMap<>();
    }

    /*public void addConjunct(String key, AppCond cond){
        AppCond res = theta.get(key);
        if(res == null) res = new ArrayList<>();
        res.add(cond);
        theta.put(key, res);
    }*/

}
