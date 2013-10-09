package costabs.beans;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement(name = "parameter")
public class Option {
	
	@XmlAttribute
	private String type;
	
	@XmlAttribute
	private String optname;
	
	@XmlElement(name = "default")
	private String defaultValue;

	@XmlElement(name = "description", type = Description.class)
	private Description description;
	
	@XmlElement(name = "values", type = OptionValues.class)
	private OptionValues values;
	
	public String getType() {
		return type;
	}
	public void setType(String type) {
		this.type = type;
	}
	
	public String getOptname() {
		return optname;
	}
	public void setOptname(String optname) {
		this.optname = optname;
	}
	
	public Description getDescription() {
		return description;
	}
	public void setDescription(Description description) {
		this.description = description;
	}
	public OptionValues getValues() {
		return values;
	}
	public void setValues(OptionValues values) {
		this.values = values;
	}
	public String getDefaultValue() {
		return defaultValue;
	}
	public void setDefaultValue(String defaultValue) {
		this.defaultValue = defaultValue;
	}
	
}	
