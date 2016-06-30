package apet.testCases;

import java.util.ArrayList;

import javax.xml.soap.Node;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class PreviousCall {
	String method;
	ArrayList<ABSData> args = new ArrayList<ABSData>();
	
	public PreviousCall(Element elem) throws Exception{
		method = elem.getAttribute("method");
		NodeList childList = elem.getChildNodes();
		for (int i = 0; i < childList.getLength(); i++) {
			if (childList.item(i).getNodeType() == Node.ELEMENT_NODE){
				Element arg = (Element) childList.item(i);
				args.add(ABSData.parseData(arg));
			}
		}
	}
}
	