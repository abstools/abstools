package Env;
import org.abs_models.backend.java.lib.types.*;

public class IO_fli extends IO_c {
    
    public ABSUnit fli_println(String s) {
        System.out.println(s);
        return ABSUnit.UNIT;
    }
}

