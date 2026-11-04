
.. _sec:interfaces:

**********
Interfaces
**********

Interfaces in ABS are similar to interfaces in Java.  They have a
name, which defines a nominal type, and they can extend one or more
interfaces.  If no explicit ``extends`` clause is given, the interface
will extend the standard library interface ``ABS.StdLib.Object`` (see
:ref:`type-object`).  The ``ABS.StdLib.Object`` interface does not
specify any methods.

The interface body consists of a list of method signature
declarations.  Method names start with a lowercase letter.

.. table:: Syntax
   :align: left
   :class: syntax

   ===================   ==================
   *InterfaceDecl* ::=   ``interface`` *SimpleTypeIdentifier* [ ``extends`` *InterfaceList* ]
   \                     ``{`` { *MethSig* } ``}``
   *InterfaceList* ::=   *TypeIdentifier* { ``,`` *TypeIdentifier* }
   *MethSig* ::=         *Type* *SimpleIdentifier*
   \                     ``(`` [ *Type* *SimpleIdentifier* { ``,`` *Type* *SimpleIdentifier* } ] ``)`` ``;``
   ===================   ==================


If a method declarations in an interface has an ``[Atomic]``
annotation, the compiler will statically check that any definitions
for this method contain no suspension points (``suspend`` and
``await`` statements), i.e., that running that method will complete
without scheduling.

If a method declarations in an interface has a ``[Readonly]``
annotation, the compiler will statically check that any definitions
for this method contain no assignments to fields.

.. note:: For ease of reasoning and analysis, ABS methods differ only
          by name.  It is an error for an interface to declare two
          methods with the same name, either explicitly or via
          extending another interface.

The interfaces in the example below represent a database system,
providing functionality to store and retrieve files, and a node of a
peer-to-peer file sharing system.  Each node of a peer-to-peer system
plays both the role of a server and a client::

  interface DB {
    File getFile(Filename fId);
    Int getLength(Filename fId);
    Unit storeFile(Filename fId, File file);
    Filenames listFiles();
  }

  interface Client {
    List<Pair<Server,Filenames>> availFiles(List<Server> sList);
    Unit reqFile(Server sId, Filename fId);
  }

  interface Server {
    Filenames inquire();
    Int getLength(Filename fId);
    Packet getPack(Filename fId, Int pNbr);
  }

  interface Peer extends Client, Server {
    List<Server> getNeighbors();
  }
