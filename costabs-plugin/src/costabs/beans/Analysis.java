package costabs.beans;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement(name = "analysis")
public class Analysis {

	@XmlAttribute(name = "id")
	private String analysisId;
	
	private String command;
	
	private String optionsCommand;
	
	@XmlElement(name = "description", type = Description.class)
	private Description description;

	@XmlElement(name = "parameters", type = Options.class)
	private Options options;
	
	public String getCommand() {
		return command;
	}

	public void setCommand(String command) {
		this.command = command;
	}

	public Description getDescription() {
		return description;
	}

	public void setDescription(Description description) {
		this.description = description;
	}

	public Options getOptions() {
		return options;
	}

	public void setOptions(Options options) {
		this.options = options;
	}

	public String getAnalysisId() {
		return analysisId;
	}

	public void setAnalysisId(String analysisId) {
		this.analysisId = analysisId;
	}

	public String getOptionsCommand() {
		return optionsCommand;
	}

	public void setOptionsCommand(String optionsCommand) {
		this.optionsCommand = optionsCommand;
	}
}
