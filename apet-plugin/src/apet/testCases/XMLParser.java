package apet.testCases;


import java.io.File;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;


public class XMLParser {
	private static final String ERROR_LOADING_FILE = "Xml file not loaded correctly.";

	/**
	 * This object contains the xml structure as a tree.
	 */
	private Document doc;
	static boolean debug = true;
	
	/**
	 * Open and read the structure of a xml file.
	 * @param xmlFile The xml file to parse.
	 */
	public XMLParser(String xmlFile) {
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance ( );
		doc = null;
		try {
		   DocumentBuilder builder = factory.newDocumentBuilder();
		   File file = new File(xmlFile);
		   doc = builder.parse(file);
		} catch (Exception spe) {
		   System.out.println(ERROR_LOADING_FILE);
		}
	}
	
	public ApetTestSuite read() throws Exception {
			ApetTestSuite suite = new ApetTestSuite();
			
			// get the root elemen of the xml tree ( <pet> )
			Element root = doc.getDocumentElement();
			// get the childs of the root (test cases)
			NodeList childList = root.getElementsByTagName("test_case");
			// for each test_case, parse it
			for (int i = 0; i < childList.getLength(); i++) {
				Element child = (Element) childList.item(i); 
				TestCase testCase = new TestCase(child);
				suite.put(testCase);
			}
			return suite;
	}
	
	public static void main(String[] args){
		XMLParser parser = new XMLParser("/tmp/pet/abs_testcases.xml");
		try {
			ApetTestSuite suite = parser.read();
			System.out.println("Done");
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}
