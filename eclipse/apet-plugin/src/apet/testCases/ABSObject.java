package apet.testCases;


import java.util.ArrayList;

import javax.xml.soap.Node;

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
			boolean foundFirst = false;
			// We have to parse the field's value (in an ABSData object)
			NodeList childList = elem.getChildNodes();
			for (int i = 0; i < childList.getLength() && !foundFirst; i++) {
				if (childList.item(i).getNodeType() == Node.ELEMENT_NODE){			
					Element valueElem = (Element) childList.item(i);	
					value = ABSData.parseData(valueElem);
					foundFirst = true;
				}
			}
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
