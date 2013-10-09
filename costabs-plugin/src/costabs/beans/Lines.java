package costabs.beans;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import costabs.structures.CostabsXMLFrontend;

@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement(name = "lines")
public class Lines {
	
	@XmlElement(name = "line", type = Line.class)
	private List<Line> lines;

	public List<Line> getLines() {
		return lines;
	}

	public void setLines(List<Line> lines) {
		this.lines = lines;
	}

	@Override
	public String toString() {
		return CostabsXMLFrontend.toString(this);
	}
}
