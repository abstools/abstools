package Env;

import java.util.Random;
import org.abs_models.backend.java.lib.types.*;

public class Random_fli extends Random_c {
    
    public Apint fli_random(Apint max) {
        Random rnd = new Random();
        int n = rnd.nextInt(max.intValue());
        return new Apint(n);
    }
}

/* useful:
 * Tools/ABS/trunk/abs-foreign-interface/java/abs-java-util/src/main/java/abs/fli/java/PrimitiveUtil.java
 */
