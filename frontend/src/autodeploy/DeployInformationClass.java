package autodeploy;

import abs.frontend.ast.*;
import abs.frontend.typechecker.DataTypeType;

import java.io.PrintWriter;
import java.util.*;
import java.util.List;

public class DeployInformationClass {

  private Map<String, Integer> _cost;
  private List<DeployInformationClassSpecification> _spec;

  private List<String> _paramList;        // parameter list, in order
  private Map<String, String> _paramType; // type of every parameter

  public DeployInformationClass(abs.frontend.ast.List<ParamDecl> l) {
    _cost = new HashMap<String, Integer>();
    _spec = new LinkedList<DeployInformationClassSpecification>();

    _paramList = new LinkedList<String>();
    _paramType = new HashMap<String, String>();


    for(ParamDecl decl: l) { // init the list of parameters with their respective types
      String param = decl.getName();
      _paramList.add(param);
      String port = decl.getType().getQualifiedName();
      assert port != null : "Error: port for " + decl.getType() + " is null";
      if(port.equals("ABS.StdLib.List"))
        port = ((DataTypeType) (decl.getType())).getTypeArgs().get(0).getQualifiedName();
      _paramType.put(param, port);
    }

    System.out.println("Scenario Specification found");
  }


  public void addAnn(PureExp exp) {

    // A specification is a list (hence a FnApp with first argument being a list)
    DeployInformationClassSpecification info = new DeployInformationClassSpecification(_paramList, _paramType);
    PureExp list = ((FnApp)exp).getParam(0);

    while(((DataConstructorExp)list).hasParam() && (((DataConstructorExp)list).getParam(0) != null)) { // means we have a cons
      PureExp el = ((DataConstructorExp)list).getParam(0);
      list = ((DataConstructorExp)list).getParam(1);

      if(((DataConstructorExp)el).getDataConstructor().getName().equals("Cost")) {
        String name = ((StringLiteral) ((DataConstructorExp) el).getParam(0)).getContent();
        int cost = Integer.parseInt(((IntLiteral) ((DataConstructorExp) el).getParam(1)).getContent());
        info.addCost(name, cost);

        System.out.println("  Annotation is Cost(\"" + name + "\", " + cost + ")");

      } else if(((DataConstructorExp)el).getDataConstructor().getName().equals("MaxUse")) {
        int arity = Integer.parseInt(((IntLiteral) ((DataConstructorExp) el).getParam(0)).getContent());
        info.setProvide(arity);

        System.out.println("  Annotation is MaxUse("  + arity + ")");

      } else if(((DataConstructorExp)el).getDataConstructor().getName().equals("Name")) {
        String name = ((StringLiteral) ((DataConstructorExp) el).getParam(0)).getContent();
        info.addScenarioName(name);

        System.out.println("  Annotation is Name("  + name + ")");

      } else if(((DataConstructorExp)el).getDataConstructor().getName().equals("Param")) {
        String param = ((StringLiteral) ((DataConstructorExp) el).getParam(0)).getContent();
        String port = _paramType.get(param);
        PureExp spec = ((DataConstructorExp)el).getParam(1);
        if(((DataConstructorExp)spec).getDataConstructor().getName().equals("Req")) {
          System.out.print("Req");
          info.addRequirement(param);

          System.out.println("  Annotation is Req(\"" + param + "\")");

        } else if(((DataConstructorExp)spec).getDataConstructor().getName().equals("List")) {
          int arity = Integer.parseInt(((IntLiteral) ((DataConstructorExp) spec).getParam(0)).getContent());
          info.addList(param, arity);

          System.out.println("  Annotation is List(\"" + port + "\", " + arity + ")");

				} else if(((DataConstructorExp)spec).getDataConstructor().getName().equals("OptList")) {
          String value = ((StringLiteral) ((DataConstructorExp) spec).getParam(0)).getContent();
          info.addOptList(param, value);

          System.out.println("  Annotation is OptList(\"" + port + "\", " + value + ")");

        } else if(((DataConstructorExp)spec).getDataConstructor().getName().equals("Default")) {
          System.out.print("Default");
          String value = ((StringLiteral) ((DataConstructorExp) spec).getParam(0)).getContent();
          if(port != null) System.out.print("(\"" + port + "\", " + value + ")");
          info.addDefault(param, value);

          System.out.println("  Annotation is Default(\"" + port + "\", " + value + ")");

        } else if(((DataConstructorExp)spec).getDataConstructor().getName().equals("User")){
          System.out.print("User");
          if(port != null) System.out.print("(\"" + port + "\")");
          info.addUser(param, port);

          System.out.println("  Annotation is User(\"" + port + "\", " + param + ")");

        }
      }
    }
    _spec.add(info);
  }

  public void generateJSON(String className, PrintWriter f) {
    // 1: the cost that is shared between all the instances of that class
    f.write("    {\n");
    f.write("      \"name\": \"" + className + "\",\n");f.write("      \"activates\": [\n");

    Iterator<DeployInformationClassSpecification> iSpec = _spec.iterator();
    while(iSpec.hasNext()) {
      DeployInformationClassSpecification spec = iSpec.next();
      spec.generateJSON(f);
      if(iSpec.hasNext()) f.write(",\n");
    }
    f.write("\n      ]\n");
    f.write("    }");
  }

  public boolean isEmpty() { return _spec.isEmpty(); }
}
