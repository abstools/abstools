package apet.testCases;


import java.util.ArrayList;
import java.util.HashMap;

import javax.xml.soap.Node;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;


public class TestCase {
	String method_name;
	ArrayList<ABSData> argsIn;
	HashMap<ABSRef,ABSObject> heapIn;
	ABSData returnData;
	HashMap<ABSRef,ABSObject> heapOut;
	ArrayList<PreviousCall> prevCalls;
	
	public TestCase(Element elem) throws Exception{
		// get method name
		this.parseMethodName(elem);
		
		//get args in
		this.parseArgsIn(elem);
		 
		//get heap in
		this.parseHeapIn(elem);
		
		//get return
		this.parseReturn(elem);
		
		//get heap out
		this.parseHeapOut(elem);
		
		//get previous calls (task interleavings)
		this.parsePrevCalls(elem);
	}
		
	private void parseMethodName(Element elem) throws Exception{ 	
    	NodeList methodList = elem.getElementsByTagName("method");
		if (methodList.getLength() != 1) throw new Exception();
		method_name = methodList.item(0).getTextContent();
    }
    
    private void parseArgsIn(Element elem) throws Exception{
    	this.argsIn = new ArrayList<ABSData>();
    	NodeList argsInList = elem.getElementsByTagName("args_in");
		Element argsIn;
		
		if (argsInList.getLength() != 1) throw new Exception();
		argsIn =(Element) argsInList.item(0);
		
		NodeList argList = argsIn.getChildNodes();
		for (int i = 0; i < argList.getLength(); i++) {
			if (argList.item(i).getNodeType() == Node.ELEMENT_NODE){
				Element argInElem = (Element) argList.item(i); 	
				ABSData argIn = ABSData.parseData(argInElem);
				this.argsIn.add(argIn);
			}
		}
    }
    
	private void parseHeapIn(Element elem) throws Exception {
		this.heapIn = new HashMap<ABSRef,ABSObject>();
		NodeList heapInList = elem.getElementsByTagName("heap_in");
		Element heapInElem;
		
		if (heapInList.getLength() != 1) throw new Exception();
		heapInElem = (Element) heapInList.item(0);
		
		NodeList cellList = heapInElem.getElementsByTagName("cell");
		for (int i = 0; i < cellList.getLength(); i++) {
			this.parseCell((Element) cellList.item(i), this.heapIn);
		}
	}
	
	private void parseCell(Element item, HashMap<ABSRef, ABSObject> heap) throws Exception {
		ABSRef ref = new ABSRef(item.getAttribute("ref"));
		NodeList objectList = item.getElementsByTagName("object");
		if (objectList.getLength() != 1) throw new Exception();
		Element objectElem = (Element) objectList.item(0);	
		ABSObject object = new ABSObject(objectElem);
		heap.put(ref, object);	
	}

	private void parseReturn(Element elem) throws Exception {
    	NodeList returnElemList = elem.getElementsByTagName("return");
		boolean foundFirst = false;
		if (returnElemList.getLength() != 1) throw new Exception();
		Element returnElem = (Element) returnElemList.item(0);
		NodeList childList = returnElem.getChildNodes();
		for (int i = 0; i < childList.getLength() && !foundFirst; i++) {
			if (childList.item(i).getNodeType() == Node.ELEMENT_NODE){			
				Element dataElem = (Element) childList.item(i);	
				this.returnData = ABSData.parseData(dataElem);
				foundFirst = true;
			}
		}
	}
	
	private void parseHeapOut(Element elem) throws Exception {
		this.heapOut = new HashMap<ABSRef,ABSObject>();
		NodeList heapOutList = elem.getElementsByTagName("heap_out");
		Element heapOutElem;
		if (heapOutList.getLength() != 1) throw new Exception();
		heapOutElem = (Element) heapOutList.item(0);
		
		NodeList cellList = heapOutElem.getElementsByTagName("cell");
		for (int i = 0; i < cellList.getLength(); i++) {
			this.parseCell((Element) cellList.item(i), this.heapOut);
		}
	}

	private void parsePrevCalls(Element elem) throws Exception{
		this.prevCalls = new ArrayList<PreviousCall>();
    	NodeList tasksElems = elem.getElementsByTagName("tasks");	
    	
    	if (tasksElems.getLength() != 1) throw new Exception();
		Element tasksElem = (Element) tasksElems.item(0);
		
		NodeList calls = tasksElem.getChildNodes();
		for (int i = 0; i < calls.getLength(); i++) {
			if (calls.item(i).getNodeType() == Node.ELEMENT_NODE) 		
				prevCalls.add(new PreviousCall((Element) calls.item(i)));
		}
    }

}
