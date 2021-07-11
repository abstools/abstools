package org.abs_models.backend.rvsdg.core;

import org.abs_models.frontend.typechecker.Type;

/**
 * An argument representing a looping value.
 */
public class ThetaArgument extends Input {
    /**
     * The initial value (outer region).
     */
    public final Output initialValue;

    /**
     * The new value after one iteration (inner region).
     */
    public Output loopValue;

    /**
     * The resulting value after the loop has stopped (outer region).
     */
    public final ThetaOutput resultValue;

    public ThetaArgument(ThetaNode thetaNode, Output initialValue) {
        this.initialValue = initialValue;
        this.resultValue = new ThetaOutput(thetaNode, this);
    }

    public void setLoopResult(Output output) {
        loopValue = output;
    }

    @Override
    public Type getType() {
        return initialValue.type;
    }
}
