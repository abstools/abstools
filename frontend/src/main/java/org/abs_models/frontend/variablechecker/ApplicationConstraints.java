package org.abs_models.frontend.variablechecker;

import org.abs_models.frontend.ast.*;

import java.util.ArrayList;
import java.util.HashMap;

public class ApplicationConstraints {
    public static AppCond imply(AppCond a, AppCond b){
        return new AppCondOr(new AppCondNot(a),b);
    }

    private HashMap<String, AppCond> theta;
    private AppCond psi;
    private HashMap<String, AppCond> deltaTheta = null;

    public void computeDeltaTheta(LocalProductLine lpl){
        deltaTheta = new HashMap<>();
        for(DeltaClause clause : lpl.getDeltaClauses()){
            deltaTheta.put(clause.getDeltaspec().getDeltaID(), clause.getAppCond());
        }
    }

    public AppCond getDeltaTheta(String delta){
        if(deltaTheta == null) return new AppCondFalse();
        AppCond res = deltaTheta.get(delta);
        if (res == null) res = new AppCondFalse();
        return res;
    }

    public ApplicationConstraints() {
        this.theta = new HashMap<>();
        this.psi = new AppCondTrue();
    }
    public ApplicationConstraints(ApplicationConstraints old) {
        this.theta = old.theta; // use reference: all ACs manage the same map
        this.psi = new AppCondTrue();
    }
    public ApplicationConstraints(ApplicationConstraints old, AppCond psi) {
        this.theta = old.theta; // use reference: all ACs manage the same map
        this.psi = psi;
    }

    public AppCond getTheta(String path) {
        AppCond res = theta.get(path);
        if(res == null) res = new AppCondFalse();
        return res;
    }

    public void setTheta(String path, AppCond appCond) {
        theta.put(path, appCond);
    }

    public AppCond getPsi() {
        return psi;
    }

    public void setPsi(AppCond psi) {
        this.psi = psi;
    }
}
