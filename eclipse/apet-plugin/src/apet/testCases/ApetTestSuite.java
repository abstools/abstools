package apet.testCases;


import java.util.ArrayList;
import java.util.HashMap;


public class ApetTestSuite extends HashMap<String,ArrayList<TestCase>>{

	private static final long serialVersionUID = 1L;
	
	public ApetTestSuite(){
		super();
		
	}
	
	public void put(TestCase test){
		ArrayList<TestCase> value;
		if (this.containsKey(test.method_name)){
			value = this.get(test.method_name);
		}else value = new ArrayList<TestCase>();
		value.add(test);
		this.put(test.method_name, value);
	}
	
	/*public boolean containsKey(String key){
		Set<String> keySet = this.keySet();
		Iterator<String> setIt = keySet.iterator();
		while (setIt.hasNext()){
			if (key.equalsIgnoreCase(setIt.next()))
				return true;
		}
		return false;
	}*/

}
