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

Method calls to the native class receive and return ABS-native
datatypes.

..
   TODO fill in those details

   Table TK shows how to use these datatypes.

   See ``abs-foreign-interface/examples``,
   ``frontend/examples/SmartHome/`` for worked examples.

   Grep ``ClassDeclGenerator.java`` for "Foreign"

Here is a simple ABS model with a class ``FLITest.C`` that is declared
as foreign::

   module FLITest;

   import Foreign from ABS.FLI;

   interface I {
       Unit printMessage(String message);
   }

   [Foreign]
   class C implements I {
       Unit printMessage(String message) {
           println(message);
       }
   }

   {
       I o = new C();
       await o!printMessage("Hello world!");
       await o!printMessage("A second message appears!");
   }

When run, this model will print two messages on the terminal, since
the ABS implementation of ``C`` implements this behavior.  However,
the following Java class can be added to the model to override the
``printMessage`` method:

.. code-block:: java

   package FLITest;
   import javax.swing.*;
   import org.abs_models.backend.java.lib.types.ABSUnit;

   public class C_fli extends C_c {

       JLabel label = new JLabel("<html>"); // for multi-line text
       String lineSeparator = "";
       JFrame frame = new JFrame("ABSForeignClass");

       public C_fli() {
           super();
           frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
           frame.getContentPane().add(label);
           frame.pack();
           frame.setVisible(true);
       }

       /* override the base class method */
       public ABSUnit fli_printMessage(String message) {
           javax.swing.SwingUtilities.invokeLater(() -> {
               label.setText(label.getText()
                             + lineSeparator + message);
               frame.pack();
           });
           lineSeparator = "<br>";
           return ABSUnit.UNIT;
       }
   }


To include the foreign class, place both ``fli.abs`` and
``C_fli.java`` in the current directory, then compile the model as
follows (with the ABS compiler ``absfrontend.jar`` added to the
classpath)::

   absc --java fli.abs
   javac -d gen/ --class-path gen:absfrontend.jar C_fli.java
   java -cp gen:absfrontend.jar FLITest.Main

When compiled and run like this, the model will not print to the
console.  Instead, the messages will be shown in a window that is
created by the foreign class.
