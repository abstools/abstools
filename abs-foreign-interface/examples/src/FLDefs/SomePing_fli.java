package FLDefs;

import FLDefs.SomePing_c;

public class SomePing_fli extends SomePing_c {
   @Override
   public String fli_ping(String s) {
      System.out.println("SomePing_fli received message");
      return "SomePing from Java";
   }

}
