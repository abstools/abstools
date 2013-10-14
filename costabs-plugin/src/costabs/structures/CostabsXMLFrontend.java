package costabs.structures;


import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.StringReader;
import java.io.StringWriter;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import costabs.beans.Analyses;
import costabs.beans.Analysis;
import costabs.beans.CostabsOutput;
import costabs.beans.Options;
import costabs.exceptions.CostabsException;

public class CostabsXMLFrontend {

	public static String toString (Object o) {
		JAXBContext context;
		try {
			context = JAXBContext.newInstance(o.getClass());
			Marshaller m = context.createMarshaller();
			m.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
			StringWriter stream = new StringWriter();
			m.marshal(o, stream);
			stream.close();
			return stream.toString();
		} catch (JAXBException e) {
			return "XML not generated: " + o.toString();
		} catch (IOException e) {
			return "XML not generated: " + o.toString(); 
		}
	}

	// Export
	public static void marshal(Object o, OutputStream writer)
			throws IOException, JAXBException {
		JAXBContext context;
		context = JAXBContext.newInstance(o.getClass());
		Marshaller m = context.createMarshaller();
		m.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
		m.marshal(o, writer);
	}

	public static Analyses readAnalyses(InputStream importFile) throws CostabsException {
		Analyses analyses;

		try {
			JAXBContext context = JAXBContext.newInstance(Analyses.class);
			Unmarshaller um = context.createUnmarshaller();
			analyses = (Analyses) um.unmarshal(importFile);

			for(Analysis analysis: analyses.getAnalyses()) {
				if (analysis.getOptionsCommand() != null) {
					String optString = CostabsXMLFrontend.execOptionsCommand(analysis.getOptionsCommand());
					
					Options opt = CostabsXMLFrontend.readOptions(optString, analysis.getOptionsCommand());
					analysis.setOptions(opt);
				}
			}

			return analyses;
		} catch (JAXBException e) {
			throw new CostabsException("Error " + e.getMessage() + " has ocurred while parsing the analyses file");
		}
	}

	public static String execOptionsCommand (String command) {
		try {
			String [] commands = command.split(";");
			StringBuffer output = new StringBuffer();
			for (String c: commands) {
				Process p = Runtime.getRuntime().exec(c);
				p.waitFor();
				BufferedReader buf = new BufferedReader(new InputStreamReader(
						p.getInputStream()));
				String line = "";
				while ((line = buf.readLine()) != null) {
					output.append(line + "\n");
				}
			}
			return output.toString();
		} catch (IOException e) {
			e.printStackTrace();
			return "";
		} catch (InterruptedException e) {
			e.printStackTrace();
			return "";
		}
	}

	public static Options readOptions(String content, String command) throws CostabsException {
		Options options;
		try {
			JAXBContext context = JAXBContext.newInstance(Options.class);
			Unmarshaller um = context.createUnmarshaller();
			StringReader reader = new StringReader (content);
			options = (Options) um.unmarshal(reader);
			return options;
		} catch (JAXBException e) {
			throw new CostabsException("Error " + e.getErrorCode() + " has ocurred while parsing the options printed by command: '" + command + "'");
		}
	}

	public static CostabsOutput readOutput(File importFile) throws CostabsException {
		try {
			CostabsOutput output;

			JAXBContext context = JAXBContext.newInstance(CostabsOutput.class);
			Unmarshaller um = context.createUnmarshaller();
			output = (CostabsOutput) um.unmarshal(importFile);

			System.out.println(output);
			
			return output;
		} catch (JAXBException e) {
			throw new CostabsException("Error '" + e.getMessage() + "' has ocurred while parsing the output file (" + CostabsConstants.OUTPUT_XML + ")",e);
		}

	}
	
	public static void cleanPrevResults() throws CostabsException {
		File f = new File (CostabsConstants.OUTPUT_XML);
		if (!f.exists()) {
			return;
		}
		f.delete();
	}

}

