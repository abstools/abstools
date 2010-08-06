package abs.frontend.analyser;

import java.util.Set;
import java.util.List;
import java.util.HashSet;
import java.util.Collections;

// Auxiliary code for aspect DuplicateCheck

public class DuplicateChecks {
	// For lack of standard Java multisets (and unwilling to use Google Collections)
	// we do this the 1970's way: walking over a sorted list.
	public static Set<String> findDuplicates(List<String> names) { 
		Set<String> duplicateNames = new HashSet<String>();
		String previousName = "";
	
		Collections.sort(names);
		for (String currentName : names) {
			if (previousName.equals(currentName)) {
				duplicateNames.add(currentName);
			}
			previousName = currentName;				
		}
		return duplicateNames; 
	} 
}
