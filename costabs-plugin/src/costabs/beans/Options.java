package costabs.beans;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import costabs.structures.CostabsXMLFrontend;


@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement(name = "parameters")
public class Options {

	@XmlElement(name = "parameter", type = Option.class)
	private List<Option> options;
	
	public List<Option> getOptions() {
		return options;
	}

	public void setOptions(List<Option> options) {
		this.options = options;
	} 
	
	@Override
	public String toString() {
		return CostabsXMLFrontend.toString(this);
	}
	
}
