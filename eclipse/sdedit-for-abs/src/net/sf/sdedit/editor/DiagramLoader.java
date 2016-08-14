// Copyright (c) 2006 - 2008, Markus Strauch.
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// 
// * Redistributions of source code must retain the above copyright notice, 
// this list of conditions and the following disclaimer.
// * Redistributions in binary form must reproduce the above copyright notice, 
// this list of conditions and the following disclaimer in the documentation 
// and/or other materials provided with the distribution.
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE 
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF 
// THE POSSIBILITY OF SUCH DAMAGE.

package net.sf.sdedit.editor;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.StringWriter;

import net.sf.sdedit.config.Configuration;
import net.sf.sdedit.config.ConfigurationManager;
import net.sf.sdedit.ui.components.configuration.Bean;
import net.sf.sdedit.ui.components.configuration.BeanConverter;
import net.sf.sdedit.util.DocUtil;
import net.sf.sdedit.util.Pair;
import net.sf.sdedit.util.DocUtil.XMLException;

import org.w3c.dom.CDATASection;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * Auxiliary class for saving diagrams as plain text or in XML format, along
 * with a configuration.
 * 
 * @author Markus Strauch
 */
public final class DiagramLoader {

	private DiagramLoader() {
		/* empty */
	}

	/**
	 * Loads a diagram from the text transmitted through the given
	 * <tt>stream</tt>. If the text contains a line that starts with
	 * <tt>&lt;?xml</tt>, it is interpreted as an XML file, containing the
	 * diagram source as a CDATA section along with a configuration. Otherwise
	 * the whole of the text is interpreted as a diagram source, and a default
	 * configuration is used.
	 * 
	 * @param stream
	 *            the stream from where the diagram specification is read
	 * @param encoding
	 *            the encoding of the diagram specification
	 * @return a pair of the diagram source and the configuration to be used for
	 *         generating the diagram
	 * 
	 * @throws IOException
	 * @throws DocUtil.XMLException
	 */
	public static Pair<String, Bean<Configuration>> load(InputStream stream,
			String encoding) throws IOException, DocUtil.XMLException {
		InputStreamReader reader = new InputStreamReader(stream, encoding);
		BufferedReader buffered = new BufferedReader(reader);
		StringWriter stringWriter = new StringWriter();
		PrintWriter writer = new PrintWriter(stringWriter);
		boolean xml = false;
		String line = buffered.readLine();
		while (line != null) {
			xml |= line.trim().startsWith("<?xml");
			writer.println(line);
			line = buffered.readLine();
		}
		writer.close();
		String source;
		Bean<Configuration> configuration = ConfigurationManager
				.createNewDefaultConfiguration();
		if (xml) {
			InputStream inputStream = new ByteArrayInputStream(stringWriter
					.toString().getBytes(encoding));
			try {
				Document document = DocUtil.readDocument(inputStream, encoding);
				source = DocUtil.evaluateCDATA(document, "/diagram/source");
				Element confElement = (Element) DocUtil.evalXPathAsNode(
						document, "/diagram/configuration");
				BeanConverter converter = new BeanConverter(configuration,
						document);
				converter.setValues(confElement);
			} finally {
				inputStream.close();
			}
		} else {
			source = stringWriter.toString();
		}
		return new Pair<String, Bean<Configuration>>(source, configuration);
	}

	/**
	 * Saves a diagram specification (and a configuration), using a stream.
	 * 
	 * @param source
	 *            the source text of the diagram
	 * @param configuration
	 *            a configuration of the diagram, or null if it is to be saved
	 *            without a configuration (as plain text)
	 * @param stream
	 *            the stream to use for saving the diagram source and
	 *            configuration
	 * @param encoding
	 *            the encoding to be used
	 * 
	 * @throws IOException
	 * @throws XMLException
	 */
	public static void saveDiagram(String source, Bean<Configuration> configuration,
			OutputStream stream, String encoding) throws IOException,
			XMLException {
		if (configuration != null) {
			Document document = DocUtil.newDocument();
			Element root = document.createElement("diagram");
			document.appendChild(root);
			BeanConverter converter = new BeanConverter(configuration, document);
			Element sourceElem = document.createElement("source");
			CDATASection sourceNode = document.createCDATASection(source);
			sourceElem.appendChild(sourceNode);
			root.appendChild(sourceElem);
			Element configurationNode = converter
					.createElement("configuration");
			root.appendChild(configurationNode);
			DocUtil.writeDocument(document, encoding, stream);
		} else {
			OutputStreamWriter osw = new OutputStreamWriter(stream, encoding);
			PrintWriter pw = new PrintWriter(osw);
			pw.print(source);
			pw.flush();
		}
	}
}
