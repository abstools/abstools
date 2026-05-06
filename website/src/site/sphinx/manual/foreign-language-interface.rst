.. _sec:fli:

******************************
The foreign language interface
******************************

The Java backend supports calling Java code from ABS.  This works on a
per-class level: at compile time, an ABS class is replaced with a Java
class (the "native" class) that implements the same interface.
Therefore, the Java class needs to be written in such a way that the
methods that are defined in the ABS interface accept ABS datatypes.
The foreign code must translate between ABS and Java-native datatypes.

In order to facilitate code analysis, every ABS model is written to be
complete even without the foreign code.  “Complete” means that the
model must type-check and ideally contains an ABS implementation of
the class that is replaced by a Java-native class.  Typically, there
is a “stub” ABS class that can be used for static analysis and other
scenarios where the native class is not available.

Classes to be replaced are annotated with the ``[Foreign]``
annotation.  At runtime, when instantiating an ABS class ``M.X``, the
system will attempt to create a class ``M.X_fli`` instead, i.e., a
Java class in the same package, with the same name with an appendix
``_fli``.  (This naming scheme can be overridden per class by setting
a system property ``abs.fli.class.M.X`` to the desired name of the
native class.)

This foreign Java class ``M.X_fli`` must be public, with a public
constructor.  The constructor of the native class needs to take the
same arguments as the generated ABS class, in the same order.
Furthermore, the foreign Java class must extend the stub Java class
that was generated from the ABS class definition.  This generated Java
class will have the same name as the ABS class with a suffix ``_c``,
e.g., ``M.X_c`` for an ABS class ``M.X``.  Extending the generated
class guarantees that the foreign class implements all necessary
interfaces and has the necessary mechanisms for blocking and
non-blocking calls in place.

The foreign class can override zero or more methods from the ABS
class.  All methods that the foreign Java class wants to override from
the ABS class must be named with the prefix ``fli_``.

Method calls to the native class receive ABS-native datatypes.

A simple ABS model declaring a class to be foreign::

  module FLITest;

  import Foreign from ABS.FLI;

  interface I {
    String message();
  }

  [Foreign]
  class C implements I {
    String message() {
      return "Hello from ABS";
    }
  }

  {
    I o = new C();
    String message = await o!message();
    println(`The message: $message$`);
  }


The corresponding Java class::

   package FLITest;

   public class C_fli extends C_c {

     public String fli_message() {
       return "Hello from Java";
     }
   }

With both ABS and Java compiled files in the Java classpath, the model
will print ``Hello from Java`` when run.  If the foreign class
``C_fli`` is not in the classpath, the model will print ``Hello from
ABS`` instead.
