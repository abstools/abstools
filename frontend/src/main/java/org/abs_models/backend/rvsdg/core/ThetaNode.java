package org.abs_models.backend.rvsdg.core;

/**
 * A theta node represent tail-controlled loops (do-while loops).
 *
 * It has a single child region which is executed repeatedly as long as the predicate returns true.
 */
public class ThetaNode extends Node {
    final public Region loopRegion = new Region();
    public ThetaArgument predicate;

    public ThetaNode(Region region) {
        super(region);
    }

    public ThetaArgument createArgument(Output initialValue) {
        ThetaArgument argument = new ThetaArgument(this, initialValue);
        this.inputs.add(argument);
        this.outputs.add(argument.resultValue);
        return argument;
    }

    public void setPredicate(ThetaArgument predicate) {
        this.predicate = predicate;
    }
}
