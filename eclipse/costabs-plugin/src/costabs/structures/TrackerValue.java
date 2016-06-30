package costabs.structures;

public class TrackerValue {
	
	private String callName;
	private String header;
	private String ub;
	private String termin;
	private int line;
	
	public TrackerValue() {
		callName = "";
		header = "";
		ub = "";
		termin = "";
		line = -1;
	}
	
	public TrackerValue(String call, String header, String upperBound, String termin, int lineNumber) {
		setCallName(call);
		setHeader(header);
		setUb(upperBound);
		setTermin(termin);
		setLine(lineNumber);
	}

	public void setCallName(String callName) {
		this.callName = callName;
	}

	public String getCallName() {
		return callName;
	}

	public void setUb(String ub) {
		this.ub = ub;
	}

	public String getUb() {
		return ub;
	}
	
	public void setTermin(String termin) {
		this.termin = termin;
	}

	public String getTermin() {
		return termin;
	}

	public void setLine(int line) {
		this.line = line;
	}

	public int getLine() {
		return line;
	}
	
	public boolean equals(Object obj) {
		TrackerValue o = (TrackerValue) obj;
		return o.callName.equals(callName);
	}

	public void setHeader(String header) {
		this.header = header;
	}

	public String getHeader() {
		return header;
	}
	
	public boolean hasUB() {
		return !ub.equals("");
	}
	
	public boolean hasTermin() {
		return !termin.equals("");
	}
	
}
