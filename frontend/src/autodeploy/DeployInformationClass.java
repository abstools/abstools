package autodeploy;

import abs.frontend.ast.*;
import abs.frontend.typechecker.DataTypeType;

import java.io.PrintWriter;
import java.util.*;
import java.util.List;

public class DeployInformationClass {

  private Map<String, Integer> _cost;
  private List<DeployInformationClassSpecification> _spec;
  //private int _provide;

  private abs.frontend.ast.List<ParamDecl> _params;

  public DeployInformationClass(abs.frontend.ast.List<ParamDecl> l) {
    _cost = new HashMap<String, Integer>();
    _spec = new LinkedList<DeployInformationClassSpecification>();
    _params = l;
  }


  private void addCost(String name, int cost) { _cost.put(name, new Integer(cost)); }
  private void addRequirement(DeployInformationClassSpecification l) { _spec.add(l); }
  //private void setProvide(int p) { _provide = p; }


  public void addAnn(PureExp exp) {

    if(((DataConstructorExp)exp).getDataConstructor().getName().equals("Cost")) {
      String name = ((StringLiteral) ((DataConstructorExp) exp).getParam(0)).getContent();
      int cost = Integer.parseInt(((IntLiteral) ((DataConstructorExp) exp).getParam(1)).getContent());
      System.out.println("Cost = (\"" + name + "\", " + cost + ")");
      addCost(name, cost);
    } else if(((DataConstructorExp)exp).getDataConstructor().getName().equals("Spec")) {
      System.out.print("Spec = (");
      DeployInformationClassSpecification spec = new DeployInformationClassSpecification();
      int p = Integer.parseInt(((IntLiteral) ((DataConstructorExp) exp).getParam(0)).getContent());
      System.out.print(p + ", [");
      spec.setProvide(p);
      PureExp l = ((FnApp)(((DataConstructorExp) exp).getParam(1))).getParam(0);
      Iterator<ParamDecl> i = _params.iterator();
      while(((DataConstructorExp)l).hasParam() && (((DataConstructorExp)l).getParam(0) != null)) { // means we have a cons
        PureExp el = ((DataConstructorExp)l).getParam(0);
        l = ((DataConstructorExp)l).getParam(1);
        if(((DataConstructorExp)el).getDataConstructor().getName().equals("P")) {
          System.out.print("P");
            int arity = 1;
            String port = i.next().getType().getQualifiedName();
            System.out.print("(\"" + port + "\", " + arity + ")");
            spec.addRequirement(port, arity);
        } else if(((DataConstructorExp)el).getDataConstructor().getName().equals("A")){
          System.out.println("A(" + i.next().getType().toString() + ")");// i.next();
        } else if(((DataConstructorExp)el).getDataConstructor().getName().equals("L")) {
          System.out.print("L");
          int arity = Integer.parseInt(((IntLiteral) ((DataConstructorExp) el).getParam(0)).getContent());
          String port = ((DataTypeType) (i.next().getType())).getTypeArgs().get(0).getQualifiedName();
          if(port != null) System.out.print("(\"" + port + "\", " + arity + ")");
          spec.addRequirement(port, arity);
        }
      }
      System.out.println("])");
      addRequirement(spec);
    }
  }

  public void generateJSON(String className, PrintWriter f) {
    // 1: the cost that is shared between all the instances of that class
    f.write("    {\n");
    f.write("      \"name\": \"" + className + "\",\n");
    f.write("      \"cost\": {\n");
    Iterator<Map.Entry<String, Integer>> iCost = _cost.entrySet().iterator();
    while(iCost.hasNext()) {
      Map.Entry<String, Integer> entry = iCost.next();
      f.write("        \"" + entry.getKey() + "\": " + entry.getValue());
      if(iCost.hasNext()) f.write(",\n");
    }
    f.write("\n      },\n");
    f.write("      \"activates\": [\n");

    Iterator<DeployInformationClassSpecification> iSpec = _spec.iterator();
    while(iSpec.hasNext()) {
      DeployInformationClassSpecification spec = iSpec.next();
      f.write("        {\n");
      f.write("          \"provide\": " + spec.getProvide() + ",\n");
      f.write("          \"require\": {");
      Iterator<Map.Entry<String, Integer>> iReq = spec.reqEntrySet().iterator();
      while(iReq.hasNext()) {
        Map.Entry<String, Integer> req = iReq.next();
        f.write("\"" + req.getKey() + "\": " + req.getValue());
        if(iReq.hasNext()) f.write(", ");
      }
      f.write("}\n"); // end of the require
      f.write("        }");
      if(iSpec.hasNext()) f.write(",\n");
    }
    f.write("\n      ]\n");
    f.write("    }");
  }


}
