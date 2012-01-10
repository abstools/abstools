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
			NodeList childList = elem.getElementsByTagName("*");
			if(childList.getLength() != 2) throw new Exception();
			Element nameElem = (Element) childList.item(0);
			fieldName = nameElem.getTextContent();
			
			Element valueElem = (Element) childList.item(1);
			value = ABSData.parseData(valueElem);
		}
	}
	
	public ABSObject(Element elem) throws Exception{
		NodeList typeList = elem.getElementsByTagName("type");
		if (typeList.getLength() != 1)throw new Exception();
		type = typeList.item(0).getTextContent();
	
		NodeList fieldsList = elem.getElementsByTagName("fields");
		if (fieldsList.getLength() != 1) throw new Exception();
		Element fieldsElem = (Element) fieldsList.item(0);
		
		NodeList fieldList = fieldsElem.getElementsByTagName("field");
		fields = new ArrayList<Field>();
		for (int i = 0; i < fieldList.getLength(); i++) {
			Element fieldElem = (Element) fieldList.item(i);
			fields.add(new Field(fieldElem));	
		}
	}
}
