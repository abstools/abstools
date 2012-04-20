package apet.testCases;


import java.util.ArrayList;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;


public class ABSObject {
	String type;
	ArrayList<Field> fields;
	
	class Field{
		String fieldName;
		ABSData value;
		
		public Field(Element elem) throws Exception{
			fieldName = elem.getAttribute("name");
			
			NodeList childList = elem.getElementsByTagName("*");
			if(childList.getLength() != 1) throw new Exception();
			Element valueElem = (Element) childList.item(0);
			value = ABSData.parseData(valueElem);
		}
	}
	
	public ABSObject(Element elem) throws Exception{
		type = elem.getAttribute("type");
		
		NodeList fieldList = elem.getElementsByTagName("field");
		fields = new ArrayList<Field>();
		for (int i = 0; i < fieldList.getLength(); i++) {
			Element fieldElem = (Element) fieldList.item(i);
			fields.add(new Field(fieldElem));	
		}
	}
}
