package costabs.structures;

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
	
	/**
	 * Open and read the structure of a xml file.
	 * @param xmlFile The xml file to parse.
	 */
	public XMLParser(String xmlFile) {
		
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance ( );
		doc = null;
		try {
		   DocumentBuilder builder = factory.newDocumentBuilder();
		   doc = builder.parse( new File(xmlFile) );
		}
		catch (Exception spe) {
		   System.out.println(ERROR_LOADING_FILE);

		}
		
	}
	
	public ResultTracker read() {
		
		ResultTracker resultMap = new ResultTracker();
		
		// get the list of result elements from the xml tree
		NodeList childList = doc.getElementsByTagName("elem");

		// for each result element, parse it
		for (int i=0; i< childList.getLength(); i++) {

			Element result = (Element) childList.item(i);
			
			TrackerValue r = loadResult(result);
			
			resultMap.addResult(r.getCallName(), 
								r.getHeader(), 
								r.getUb(), 
								r.getTermin(), 
								r.getLine());
		}
		
		return resultMap;
	}
	
	private TrackerValue loadResult(Element result) {
		
		TrackerValue value = new TrackerValue();
		
		NodeList resultNodes = result.getChildNodes();
		
		if (resultNodes == null) return value;
		
		for (int i = 0; i < resultNodes.getLength(); i++) {
			
			String nodeName = resultNodes.item(i).getNodeName();
			if (nodeName.equals("name")) {
				String header = resultNodes.item(i).getTextContent();
				if (header != null) {
					
					if (header.contains("/")) {
						
						int end = header.indexOf("/");
						
						String name = "";
						if (end >= 0) 
							name = header.substring(0,end);
						if (name!=null) {
							String nameEscaped = name.replace("'", "");
							value.setCallName(nameEscaped);		
						}
					}
					else {
						value.setHeader(header);
					
						int end = header.indexOf("(");
					
						String name = "";
						if (end >= 0) 
							name = header.substring(0,end);
						if (name!=null) {
							String nameEscaped = name.replace("'", "");
							value.setCallName(nameEscaped);		
						}
					}
				}
				
			}
			else if (nodeName.equals("ub")) {
				String ub = resultNodes.item(i).getTextContent();
				if (ub!=null) value.setUb(ub);		
			}
			else if (nodeName.equals("termin")) {
				String termin = resultNodes.item(i).getTextContent();
				if (termin != null) value.setTermin(termin);
			}
				
		}		
		
		return value;
	}
}
