package costabs.beans;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import costabs.structures.CostabsXMLFrontend;


@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement(name = "output")
public class CostabsOutput {
	
	@XmlElement(name = "eicommands")
	private Commands commands;
	
	@XmlElement
	private Interactions interactions;
	
	public Commands getCommands() {
		return commands;
	}

	public void setCommands(Commands commands) {
		this.commands = commands;
	}
	
	public Interactions getInteractions() {
		return interactions;
	}

	public void setInteractions(Interactions interactions) {
		this.interactions = interactions;
	}
	
	@Override
	public String toString() {
		return CostabsXMLFrontend.toString(this);
	}
}
