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
import FLDefs.*;

// Import classes and interface to interact with ABS
// from Java
import abs.backend.java.fli.*;
import abs.backend.java.lib.types.ABSString;
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
public class ForeignClassImpl extends ABSForeignObject implements ForeignInterface_i {

   @Override
   public ABSUnit m(ABSString s) {
      System.out.println("Hello ABS, just got "+s+ " from you!");
      return ABSUnit.UNIT;
   }

   @Override
   public ABSString foo(Test t) {
       if (t.isBar()) {
           System.out.println("Found a Bar with argument "+t.toBar().getArg0());
           return t.toBar().getArg0();
       }
      return null;
   }

   @Override
   public ABSUnit visit(List planets) {
       if (planets.isNil())
           System.out.println("Planet list is Empty");
       else {
           List_Cons<ABSString> cons = planets.toCons();
           ABSString s = cons.getArg0();
           System.out.println("Found planet "+s);
       }
      return null;
   }

}


