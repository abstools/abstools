/*
 * This files shows an idea of how
 * Java code could look like that
 * implements foreign classes.
 * These are only ideas, nothing is fixed yet.\
 */
package example;

// imports interfaces defined in the FLDefs ABS module
// in the Example.abs file.
// As interfaces generated from ABS are identical to
// Java interfaces, this is no problem at all.
import ABS.StdLib.List;
import ABS.StdLib.List_Cons;
import ABS.StdLib.isEmpty_f;
import FLDefs.*;

// Import classes and interface to interact with ABS
// from Java
import abs.backend.java.fli.*;
import abs.backend.java.lib.runtime.ABSFut;
import abs.backend.java.lib.types.ABSUnit;


/* one idea to specify the connection between
 * Java classes and ABS classes is to use Annotations.
 * 
 */
//@RealizesABSClass("FLDefs.ForeignClass")
/*
 * also possible is to directly inherit from the foreign
 * class and implement the corresponding methods 
 */
public class ForeignClassImpl extends ForeignClass_c {

   @Override
   public ABSUnit fli_m(String s) {
      System.out.println("Hello ABS, just got "+s+ " from you!");
      return ABSUnit.UNIT;
   }

   @Override
   public String fli_foo(Test t) {
       if (t.isBar()) {
           System.out.println("Found a Bar with argument "+t.toBar().getArg0());
           return t.toBar().getArg0();
       }
      return "This is Java";
   }
   
   @Override
   public ABSUnit fli_visit(List<String> planets) {
       if (planets.isNil())
           System.out.println("Planet list is Empty");
       else {
           List_Cons<String> cons = planets.toCons();
           String s = cons.getArg0();
           System.out.println("Found planet "+s);
       }
      return null;
   }

   @Override
   public ABSUnit fli_aPing(Ping_i p) {
       ABSFut<String> fut = p.async_ping("Hi Ping!!!");
       System.out.println("Waiting for future....");
       System.out.println("Got future: "+fut.get());
      return ABSUnit.UNIT;
   }
}


