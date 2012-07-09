package ABS.Meta;

import abs.backend.java.lib.types.*;
import abs.backend.java.lib.runtime.*;

public class Meta_fli extends Meta_c {

    ObjectMirror fli_reflect(ABSDynamicObject o) {
        return new ObjectMirror(o);
        
        // TODO implement ObjectMirror in abs.backend.java.lib.types
    }
}