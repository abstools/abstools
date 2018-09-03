package deadlock.analyser.detection;

import java.util.HashMap;
import java.util.Map;

import deadlock.analyser.factory.GroupName;
//import com.gzoumix.semisolver.term.TermVariable;

public class VarSubstitution {

	  private Map<GroupName,GroupName> data;

	  public VarSubstitution(){
		  this.data = new HashMap<>();
	  }

	  //substitute v1 with v2
	  public void addSub(GroupName v1, GroupName v2){
		  if(v1.equals(v2)) return;
		  this.data.put(v1, v2);
	  }

	  public void addSub(HashMap<GroupName,GroupName> map2){
		  this.data.putAll(map2);
	  }

	  public void addSub(VarSubstitution s){
		  this.data.putAll(s.getMap());
	  }

	  public GroupName apply(GroupName v){
	          GroupName tv = data.get(v);
		  if(tv == null) return v;
		  else return tv;
	  }

	  public Map<GroupName,GroupName> getMap(){
		  return this.data;
	  }

	  public String toString(){
		  String res = "";
		  for(GroupName v : data.keySet())
			  res += "[" + data.get(v).toString()  + "/" + v.toString() +"] ";
		  return res;
	  }

}
