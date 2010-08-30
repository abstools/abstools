package abs.backend.java.lib.types;

import java.math.BigInteger;



/**
 * Implementation of an ABS integer
 * 
 * @author Jan Schäfer
 *
 */
public class ABSInteger extends ABSBuiltInDataType {
    public static final ABSInteger ZERO = new ABSInteger(0);
    public static final ABSInteger ONE = new ABSInteger(1);

    private BigInteger value;
    
    private ABSInteger(int i) { 
        this(BigInteger.valueOf(i));
    }

    private ABSInteger(BigInteger i) {
        super("");
        this.value = i;
    }
    
    public ABSInteger add(ABSInteger i) {
        return fromBigInt(this.value.add(i.value)); 
    }
    
    public ABSInteger subtract(ABSInteger i) {
        return fromBigInt(this.value.subtract(i.value)); 
    }
    
    public ABSInteger multiply(ABSInteger i) {
        return fromBigInt(this.value.multiply(i.value)); 
    }

    public ABSInteger divide(ABSInteger i) {
        return fromBigInt(this.value.divide(i.value)); 
    }
    
    public ABSInteger mod(ABSInteger i) {
        return fromBigInt(this.value.mod(i.value)); 
    }
    
    public ABSInteger negate() {
        return fromBigInt(this.value.negate());
    }

    @Override
    public ABSBool eq(ABSValue o) {
   	  if (!super.eq(o).toBoolean())
   		  return ABSBool.FALSE;
   	  ABSInteger oi = (ABSInteger) o;
        return ABSBool.fromBoolean(oi.value.compareTo(this.value) == 0);
    }

    public ABSBool gt(ABSInteger i) {
        if (i == null)
            return ABSBool.FALSE;
        return ABSBool.fromBoolean(this.value.compareTo(i.value) == 1);
    }
    
    public ABSBool lt(ABSInteger i) {
        if (i == null)
            return ABSBool.FALSE;
        return ABSBool.fromBoolean(this.value.compareTo(i.value) == -1);
    }
    
    public ABSBool gtEq(ABSInteger i) {
        if (i == null)
            return ABSBool.FALSE;
        int res = this.value.compareTo(i.value);
        return ABSBool.fromBoolean(res == 0 || res == 1);
    }
    
    public ABSBool ltEq(ABSInteger i) {
        if (i == null)
            return ABSBool.FALSE;
        int res = this.value.compareTo(i.value);
        return ABSBool.fromBoolean(res == 0 || res == -1);
    }

    public static ABSInteger fromBigInt(BigInteger i) {
        return new ABSInteger(i);
    }
    
    public static ABSInteger fromString(String value) {
        return fromBigInt(new BigInteger(value));
    }
    
    public static ABSInteger fromInt(int i) {
        switch (i) {
        case 0: return ZERO;
        case 1: return ONE;
        default: return fromBigInt(BigInteger.valueOf(i));
        }
    }

    public int toInt() {
        return value.intValue();
    }
    
    @Override
    public String toString() {
        return value.toString();
    }
    
}
