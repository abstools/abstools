package autodeploy;

import java.io.PrintWriter;
import java.util.*;

public class DeployInformationClassSpecification {


  private interface IDeployInformationClassSpecificationRequirement { String toString(String _port); }

  private class DeployInformationClassSpecificationRequirementReq implements  IDeployInformationClassSpecificationRequirement {
    public DeployInformationClassSpecificationRequirementReq() {  }
    public String toString(String _port) { return "{ \"type\": \"require\", \"arity\": 1, \"value\": \"" + _port + "\" }"; }
  }

  private class DeployInformationClassSpecificationRequirementList implements  IDeployInformationClassSpecificationRequirement {
    private int _arity;
    public DeployInformationClassSpecificationRequirementList(int arity) { _arity = arity; }
    public String toString(String _port) { return "{ \"type\": \"list\", \"arity\": " + _arity + ", \"value\": \"" + _port + "\" }"; }
  }

	private class DeployInformationClassSpecificationRequirementOptList implements  IDeployInformationClassSpecificationRequirement {
    private String _value;
    public DeployInformationClassSpecificationRequirementOptList(String value) { _value = value; }
    public String toString(String _port) { return "{ \"method\": \"" + _value + "\", \"interface\": \"" + _port + "\" }"; }
  }

  private class DeployInformationClassSpecificationRequirementDefault implements  IDeployInformationClassSpecificationRequirement {
    private String _value;
    public DeployInformationClassSpecificationRequirementDefault(String value) { _value = value; }
    public String toString(String _port) { return "{ \"type\": \"default\", \"arity\": 1, \"value\": \"" + _value + "\" }"; }
  }

  private class DeployInformationClassSpecificationRequirementUser implements  IDeployInformationClassSpecificationRequirement {
    String _field;
    public DeployInformationClassSpecificationRequirementUser(String field) { _field = field; }
    public String toString(String _port) { return "{ \"type\": \"user\", \"arity\": 1, \"value\": \"" + _field + "\" }"; }
  }


  private List<String> _paramList;
  private Map<String, String> _paramType;

  private int _provide;
  private Map<String, Integer> _cost;
  private Map<String, IDeployInformationClassSpecificationRequirement> _params;
  private Set<String> _names;

  public DeployInformationClassSpecification(List<String> paramList, Map<String, String> paramType) {
    _paramList = paramList;
    _paramType = paramType;
    _provide = -1;                   // per default, the provide is infinite
    _cost = new HashMap<String, Integer>();
    _params = new HashMap<String, IDeployInformationClassSpecificationRequirement>();
    for(String param: _paramList) {  // per default, all fields must be filled by the user.
      _params.put(param, new DeployInformationClassSpecificationRequirementUser(param));
    }
    _names = new HashSet<String>();
  }

  public void setProvide(int p) { _provide = p; }
  public int getProvide() { return _provide; }

  public void addCost(String resource, int cost) { _cost.put(resource, new Integer(cost)); }
  public int getCost(String resource) {
    Integer cost = _cost.get(resource);
    if(cost == null) { return 0; }
    else { return cost.intValue(); }
  }


  public void addRequirement(String param) { _params.put(param, new DeployInformationClassSpecificationRequirementReq()); }
  public void addList(String param, int arity) { _params.put(param, new DeployInformationClassSpecificationRequirementList(arity)); }
	public void addOptList(String param, String value) { _params.put(param, new DeployInformationClassSpecificationRequirementOptList(value)); }
  public void addDefault(String param, String value) { _params.put(param, new DeployInformationClassSpecificationRequirementDefault(value)); }
  public void addUser(String param, String field) { _params.put(param, new DeployInformationClassSpecificationRequirementUser(field)); }
  public void addScenarioName(String name) { _names.add(name); }



  public void generateJSON(PrintWriter f) {

    f.write("        {\n");
    // 1. Provide
    f.write("          \"provide\": " + this.getProvide() + ",\n");
    // 2. Cost
    f.write("          \"cost\": {\n");
    Iterator<Map.Entry<String, Integer>> iCost = _cost.entrySet().iterator();
    while(iCost.hasNext()) {
      Map.Entry<String, Integer> entry = iCost.next();
      f.write("            \"" + entry.getKey() + "\": " + entry.getValue());
      if(iCost.hasNext()) f.write(",\n");
    }
    f.write("\n         },\n");
    // 3. Scenario names
    f.write("          \"scenarios\": [ ");
    Iterator<String> iName = _names.iterator();
    while(iName.hasNext()) {
      String name = iName.next();
      f.write("\"" + name + "\"");
      if(iName.hasNext()) f.write(", ");
      else f.write(" ");
    }
    f.write("],\n");
    // 4, requirements
    f.write("          \"sig\": [\n");
    Iterator<String> iReq = _paramList.iterator();
    while(iReq.hasNext()) {
      String param = iReq.next();
      f.write("            ");
      f.write(_params.get(param).toString(_paramType.get(param)));
      if(iReq.hasNext()) f.write(", ");
      f.write("\n");
    }
    f.write("          ]\n"); // end of the require
		// 5, optional lists
		f.write("          ,\"optional_list\": [\n");
		Iterator it = _params.entrySet().iterator();
		Boolean first = true;
    while (it.hasNext()) {
        Map.Entry pair = (Map.Entry)it.next();
				String name = (String)pair.getKey();
				Boolean not_found = true;
				for(String s : _paramList)
    			if (s.equals(name)) { not_found = false; }
				if (not_found) {
					if (first) {
						first = false;
					} else {
						f.write(",");
					}
					f.write("            ");
					f.write( _params.get(name).toString( name ) );
				}
        it.remove(); // avoids a ConcurrentModificationException
    }
		f.write("          ]\n"); // end of the optional list
		// end of json object
    f.write("        }");


  }


}
