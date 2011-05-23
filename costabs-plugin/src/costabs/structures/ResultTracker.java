package costabs.structures;

import java.util.ArrayList;
import java.util.HashMap;

import org.eclipse.core.resources.IFile;

import costabs.markers.UBMarker;

public class ResultTracker {

	private HashMap<String, TrackerValue> results;

	public ResultTracker() {
		results = new HashMap<String, TrackerValue>();
	}

	public void addResult(String callName, String header, String ub, int line) {
		TrackerValue r;

		// If it's the first result with this call, create the container
		if (!results.containsKey(callName))
			results.put(callName, new TrackerValue(callName, header, ub,line));
		else {
			r = results.get(callName);
			r.setCallName(callName);
			r.setUb(ub);
			r.setLine(line);
		}

	}

	public void addResult(String callName, String header, String ub) {
		TrackerValue r;

		// If it's the first result with this call, create the container
		if (!results.containsKey(callName))
			results.put(callName, new TrackerValue(callName,header, ub,-1));
		else {
			r = results.get(callName);
			r.setCallName(callName);
			r.setHeader(header);
			r.setUb(ub);
		}

	}

	public void removeResult(String callName) {
		if (results.containsKey(callName)) 
			results.remove(callName);
	}

	public void mergeUBContent(ResultTracker r) {

		for (TrackerValue p : r.results.values()) {
			// If the package doesn't exist in the actual structure, we add it
			if (!results.containsKey(p.getCallName()))
				results.put(p.getCallName(), p);
			// Else, we merge their classes
			else {
				TrackerValue ourR = results.get(p.getCallName());
				ourR.setUb(p.getUb());
			}	
		}

	}
	
	public void mergeLineContent(ResultTracker r) {

		for (TrackerValue p : r.results.values()) {
			// If the package doesn't exist in the actual structure, we add it
			if (!results.containsKey(p.getCallName()))
				results.put(p.getCallName(), p);
			// Else, we merge their classes
			else {
				TrackerValue ourR = results.get(p.getCallName());
				ourR.setLine(p.getLine());
			}	
		}

	}
	
	public ArrayList<String> getCalls() {
		
		ArrayList<String> retVal = new ArrayList<String>();
		
		for (TrackerValue p : results.values())
				retVal.add(p.getCallName());

		return retVal;
	}

	public void fillMarkers(IFile file) {
		
		for (TrackerValue p : results.values()) {
			UBMarker ub = new UBMarker();
			String message = p.getHeader() + " = " + p.getUb();
			ub.markLine(file, message , p.getLine());
		}
		
	}
	
	public int size() {
		return results.size();
	}
}
