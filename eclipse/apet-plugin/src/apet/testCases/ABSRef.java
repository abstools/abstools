package apet.testCases;


import org.w3c.dom.Element;

public class ABSRef extends ABSData{
	
	public ABSRef(Element elem) throws Exception{
		type = elem.getAttribute("type");
		value = elem.getTextContent();
	}
	
	public ABSRef(String s){
		value = s;
	}
}
