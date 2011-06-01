package costabs.structures;

public class TrackerValue {
	
	private String callName;
	private String header;
	private String ub;
	private int line;
	
	public TrackerValue() {
		callName = "";
		ub = "";
		line = -1;
	}
	
	public TrackerValue(String call, String header, String upperBound, int lineNumber) {
		setCallName(call);
		setHeader(header);
		setUb(upperBound);
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
	
}
