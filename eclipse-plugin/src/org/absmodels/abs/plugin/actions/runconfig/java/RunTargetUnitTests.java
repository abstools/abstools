package org.absmodels.abs.plugin.actions.runconfig.java;


public class RunTargetUnitTests implements RunTarget {


    @Override
    public String toString() {
        return "Run all unit tests";
    }

    @Override
    public void setConfig(JavaLaunchConfig cfg) {
        cfg.setRunTarget(toString());
        cfg.setTestExecution(true);
    }
}
