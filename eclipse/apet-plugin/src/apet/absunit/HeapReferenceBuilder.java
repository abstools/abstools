package apet.absunit;

import abs.common.StringUtils;

/**
 * 
 * @author woner
 *
 */
final class HeapReferenceBuilder {

	String heapReferenceForTest(String testName, String ref) {
		if (ref.matches("^\\d.*$")) {
			ref = "ref" + ref;
		}
		return ref.toLowerCase() + StringUtils.capitalize(testName);
	}
	
	String testCaseRefNameFromheapReference(String reference) {
		for (int i=0; i<reference.length(); i++) {
			if (Character.isUpperCase(reference.charAt(i))) {
				if (reference.matches("^ref\\d.*$")) {
					reference = reference.substring(2);
				}
				return reference.substring(0, i + 1).toUpperCase();
			}
		}
		return null;
	}
	
}
