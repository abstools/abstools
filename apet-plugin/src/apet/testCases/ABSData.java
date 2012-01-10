package apet.testCases;

import org.w3c.dom.Element;


public abstract class ABSData{
	String value;
		
	static ABSData parseData(Element elem){
		if (elem.getNodeName().equalsIgnoreCase("ref"))
			return new ABSRef(elem);
		else if (elem.getNodeName().equalsIgnoreCase("term"))
			return new ABSTerm(elem);
		else return null;
	}
		
	public ABSData(Element elem){
		value=elem.getTextContent();
	}
}
