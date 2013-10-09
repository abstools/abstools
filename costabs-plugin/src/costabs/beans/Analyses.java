package costabs.beans;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import costabs.structures.CostabsXMLFrontend;

@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement(name = "analyses")
public class Analyses {
	
	@XmlElement(name = "analysis", type = Analysis.class)
	private List<Analysis> analyses;

	public List<Analysis> getAnalyses() {
		return analyses;
	}

	public void setAnalyses(List<Analysis> analyses) {
		this.analyses = analyses;
	}

	@Override
	public String toString() {
		return CostabsXMLFrontend.toString(this);
	}
}
