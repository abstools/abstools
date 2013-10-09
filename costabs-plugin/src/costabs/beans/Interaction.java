package costabs.beans;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement(name = "interaction")
public class Interaction {
	
	public static final String NO_CLEAN_INTERACTION_VALUE = "no";
	
	@XmlAttribute		
	private String type; 
	
	@XmlAttribute	
	private String clean = "yes"; 
	
	private String file;
	
	private Lines lines;

	private Nodes nodes;
	
	@XmlElement(name = "eicommands", type = Commands.class)
	private Commands commands;

	public Commands getCommands() {
		return commands;
	}

	public void setCommands(Commands commands) {
		this.commands = commands;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public String getFile() {
		return file;
	}

	public void setFile(String file) {
		this.file = file;
	}

	public Lines getLines() {
		return lines;
	}

	public void setLines(Lines lines) {
		this.lines = lines;
	}

	public Nodes getNodes() {
		return nodes;
	}

	public void setNodes(Nodes nodes) {
		this.nodes = nodes;
	}

	public String getClean() {
		return clean;
	}

	public void setClean(String clean) {
		this.clean = clean;
	}
	
}
