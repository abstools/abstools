package abs.backend.java.lib.types;


public class ABSUnit extends ABSBuiltInDataType {
    public static ABSUnit UNIT = new ABSUnit();
    
    private ABSUnit() { 
        super("Unit");
    }

}
