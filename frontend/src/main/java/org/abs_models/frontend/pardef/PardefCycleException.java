package org.abs_models.frontend.pardef;

import org.abs_models.frontend.ast.PartialFunctionDecl;

public final class PardefCycleException extends PardefModellingException {

    private static final String ERROR_MESSAGE = "Cycle in module %s detected: %s";

    private final String module;
    private final String cycle;

    public PardefCycleException(PartialFunctionDecl current) {
        super(String.format(ERROR_MESSAGE, current.getModuleDecl().getName(), current.getName()));
        this.cycle = current.getName();
        this.module = current.getModuleDecl().getName();
    }

    private PardefCycleException(String cycle, String module) {
        super(String.format(ERROR_MESSAGE, module, cycle));
        this.cycle = cycle;
        this.module = module;
    }

    public PardefCycleException calledBy(PartialFunctionDecl caller) {
        String cycle = caller.getName() + " -> " + this.cycle;
        return new PardefCycleException(cycle, module);
    }
}
