package autodeploy;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public class DeployInformationClassSpecification {

  private Map<String, Integer> _req;
  private int _provide;

  public DeployInformationClassSpecification() {_req = new HashMap<String, Integer>(); }

  public void setProvide(int p) { _provide = p; }
  public void addRequirement(String port, int arity) {
    Integer a = _req.get(port);
    if(a == null) { _req.put(port, new Integer(arity)); }
    else { _req.put(port, new Integer(a.intValue() + arity)); }
  }


  public int getProvide() { return _provide; }
  public Set<Map.Entry<String, Integer>> reqEntrySet() { return _req.entrySet(); }
}
