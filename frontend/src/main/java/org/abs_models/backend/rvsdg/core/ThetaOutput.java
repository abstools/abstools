package org.abs_models.backend.rvsdg.core;

public class ThetaOutput extends Output {
    public final ThetaArgument argument;

    public ThetaOutput(Node node, ThetaArgument argument) {
        super(argument.getType());
        this.argument = argument;
    }
}
