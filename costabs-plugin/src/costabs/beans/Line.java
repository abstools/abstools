package costabs.beans;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement(name = "line")
public class Line {

	@XmlAttribute
	private String l;

	public String getL() {
		return l;
	}

	public void setL(String l) {
		this.l = l;
	}
	
}
