package abs.backend.java.lib.runtime;

import abs.backend.java.lib.types.ABSBool;
import abs.backend.java.lib.types.ABSRef;
import static abs.backend.java.lib.runtime.ABSRuntime.*;

public class ABSObject {
    private final COG cog;
    
    public ABSObject() {
        cog = getCurrentCOG();
    }
    
    public COG getCOG() {
        return cog;
    }

    protected final void __ABS_checkSameCOG() {
        if (cog != getCurrentCOG()) {
            throw new ABSIllegalSynchronousCallException();
        }
    }
    
    public ABSBool eq(ABSRef o) {
        return ABSBool.fromBoolean(this == o);
    }
}
