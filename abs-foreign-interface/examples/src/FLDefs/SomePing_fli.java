package FLDefs;

import abs.backend.java.lib.types.ABSString;
import FLDefs.SomePing_c;

public class SomePing_fli extends SomePing_c {
   @Override
   public ABSString fli_ping(ABSString s) {
      System.out.println("SomePing_fli received message");
      return ABSString.fromString("SomePing from Java");
   }

}
