package Foreign;

public class Printer_fli extends Printer_c {
    
    public ABSUnit fli_printS(ABSString text) {
        System.out.println(text.getString());
        return ABSUnit.UNIT;
    }   
    public ABSUnit fli_printI(ABSInteger integer) {
        System.out.println(integer.toInt());
        return ABSUnit.UNIT;
    }   
    public ABSUnit fli_printB(boolean bool) {
        System.out.println(bool);
        return ABSUnit.UNIT;
    }
}
