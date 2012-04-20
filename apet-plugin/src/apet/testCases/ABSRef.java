package apet.testCases;


import org.w3c.dom.Element;

public class ABSRef extends ABSData{
	public ABSRef(String s){
		value = s;
	}
	public ABSRef(Element elem) throws Exception{
		super(elem);
	}
}
