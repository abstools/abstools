package autodeploy;

import java.io.PrintWriter;
import java.util.*;

public class DeployInformationClassSpecification {



  private interface IDeployInformationClassSpecificationRequirement { String toString(); }

  private class DeployInformationClassSpecificationRequirementReq implements  IDeployInformationClassSpecificationRequirement {
    private String _port;
    public DeployInformationClassSpecificationRequirementReq(String port) { _port = port; }
    public String toString() { return "{ \"type\": \"require\", \"arity\": 1, \"value\": \"" + _port + "\" }"; }
  }

  private class DeployInformationClassSpecificationRequirementList implements  IDeployInformationClassSpecificationRequirement {
    private String _port;
    private int _arity;
    public DeployInformationClassSpecificationRequirementList(String port, int arity) { _port = port; _arity = arity; }
    public String toString() { return "{ \"type\": \"list\", \"arity\": " + _arity + ", \"value\": \"" + _port + "\" }"; }
  }

  private class DeployInformationClassSpecificationRequirementDefault implements  IDeployInformationClassSpecificationRequirement {
    private String _port;
    private String _value;
    public DeployInformationClassSpecificationRequirementDefault(String port, String value) { _port = port; _value = value; }
    public String toString() { return "{ \"type\": \"default\", \"arity\": 1, \"value\": \"" + _value + "\" }"; }
  }

  private class DeployInformationClassSpecificationRequirementFill implements  IDeployInformationClassSpecificationRequirement {
    private String _port;
    private String _field;
    public DeployInformationClassSpecificationRequirementFill(String port, String field) { _port = port; _field = field; }
    public String toString() { return "{ \"type\": \"user\", \"arity\": 1, \"value\": \"" + _field + "\" }"; }
  }


  private List<IDeployInformationClassSpecificationRequirement> _req;
  private int _provide;

  public DeployInformationClassSpecification() {_req = new LinkedList<IDeployInformationClassSpecificationRequirement>(); }

  public void setProvide(int p) { _provide = p; }
  public int getProvide() { return _provide; }


  public void addRequirement(String port) { _req.add(new DeployInformationClassSpecificationRequirementReq(port)); }
  public void addList(String port, int arity) { _req.add(new DeployInformationClassSpecificationRequirementList(port, arity)); }
  public void addDefault(String port, String value) { _req.add(new DeployInformationClassSpecificationRequirementDefault(port, value)); }
  public void addFill(String port, String field) { _req.add(new DeployInformationClassSpecificationRequirementFill(port, field)); }



  public void generateJSON(PrintWriter f) {
    Iterator<IDeployInformationClassSpecificationRequirement> iReq = _req.iterator();
    while(iReq.hasNext()) {
      f.write("            ");
      f.write(iReq.next().toString());
      if(iReq.hasNext()) f.write(",");
      f.write("\n");
    }
  }


}
