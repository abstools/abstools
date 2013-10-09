package costabs.beans;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement(name = "values")
public class OptionValues {
	
	@XmlElement(name = "value", type = OptionValue.class)
	private List<OptionValue> values;

	public List<OptionValue> getValues() {
		return values;
	}

	public void setValues(List<OptionValue> values) {
		this.values = values;
	}
	
}
