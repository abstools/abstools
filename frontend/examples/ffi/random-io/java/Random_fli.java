package Env;

import java.util.Random;

public class Random_fli extends Random_c {
    
    public ABSInteger fli_random(ABSInteger max) {
        Random rnd = new Random();
        int n = rnd.nextInt(max.toInt());
        return ABSInteger.fromInt(n);
    }
}

/* useful:
 * Tools/ABS/trunk/abs-foreign-interface/java/abs-java-util/src/main/java/abs/fli/java/PrimitiveUtil.java
 */
