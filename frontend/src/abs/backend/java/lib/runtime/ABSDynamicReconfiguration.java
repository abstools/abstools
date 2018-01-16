/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

import java.util.ArrayList;
import java.util.List;

import abs.backend.java.lib.runtime.metaABS.Reconfiguration;

public class ABSDynamicReconfiguration extends ABSDynamicObject {

    public ABSDynamicReconfiguration() {
        super(Reconfiguration.singleton());
    }

    private String name;
    private ABSDynamicProduct currentP;
    private ABSDynamicProduct targetP;
    private List<ABSDynamicDelta> deltas = new ArrayList<>();
    private ABSDynamicUpdate update = null;

    public String getName() {
        return name;
    }
    public void setName(String name) {
        this.name = name;
    }

    public ABSDynamicProduct getCurrentProduct() {
        return currentP;
    }
    public void setCurrentProduct(ABSDynamicProduct p) {
        this.currentP = p;
    }

    public ABSDynamicProduct getTargetProduct() {
        return targetP;
    }
    public void setTargetProduct(ABSDynamicProduct p) {
        this.targetP = p;
    }

    public void setDeltas(List<ABSDynamicDelta> deltaList) {
        this.deltas.addAll(deltaList);
    }

    public List<ABSDynamicDelta> getDeltas() {
            return deltas;
    }

    public void setUpdate(ABSDynamicUpdate upd) {
        this.update = upd;
    }
    public ABSDynamicUpdate getUpdate() {
        return update;
    }

}
