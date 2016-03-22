package apet.testCases;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;


public abstract class ABSData{
	String type;
	String value;
		
	static ABSData parseData(Element elem) throws Exception{
		if (elem.getNodeName().equalsIgnoreCase("ref"))
			return new ABSRef(elem);
		else if (elem.getNodeName().equalsIgnoreCase("term"))
			return new ABSTerm(elem);
		else return null;
	}
	
	ABSData(){}
	
	/*public ABSData(String type,String value) throws Exception{
		this.type = type;
		this.value = value;
	}*/
}
