package apet.testCases;


import java.util.ArrayList;

import javax.xml.soap.Node;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class ABSTerm extends ABSData{
	
	String functor;
	ArrayList<ABSData> args = new ArrayList<ABSData>();
	String typeName;
	String[] typeParams;
	
	public ABSTerm(Element elem) throws Exception{
		type = elem.getAttribute("type");
		int iPar = type.indexOf('(');
		if (iPar >= 0){
			typeName = type.substring(0,iPar);
			typeParams = type.substring(iPar+1).split("[(,)]");
		} else
			typeName = type;
		value = elem.getAttribute("value");
		functor = elem.getAttribute("functor");
		NodeList childList = elem.getChildNodes();
		for (int i = 0; i < childList.getLength(); i++) {
			if (childList.item(i).getNodeType() == Node.ELEMENT_NODE){
				Element arg = (Element) childList.item(i);
				args.add(ABSData.parseData(arg));
			}
		}
	}
	
}