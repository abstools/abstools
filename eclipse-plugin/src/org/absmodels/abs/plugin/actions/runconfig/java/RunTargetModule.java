package org.absmodels.abs.plugin.actions.runconfig.java;

import abs.frontend.ast.ModuleDecl;

public class RunTargetModule implements RunTarget {

    private ModuleDecl module;

    public RunTargetModule(ModuleDecl module) {
        this.module = module;
    }

    public ModuleDecl getModule() {
        return module;
    }

    @Override
    public String toString() {
        return module.getName();
    }

    @Override
    public void setConfig(JavaLaunchConfig cfg) {
    	cfg.setTestExecution(false);
        cfg.setRunTarget(module.getName());        
    }
}
