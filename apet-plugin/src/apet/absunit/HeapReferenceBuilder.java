package apet.absunit;

import abs.common.StringUtils;

/**
 * 
 * @author woner
 *
 */
final class HeapReferenceBuilder {

	String heapReferenceForTest(String testName, String ref) {
		return ref.toLowerCase() + StringUtils.capitalize(testName);
	}
	
	String testCaseRefNameFromheapReference(String reference) {
		for (int i=0; i<reference.length(); i++) {
			if (Character.isUpperCase(reference.charAt(i))) {
				return reference.substring(0, i + 1).toUpperCase();
			}
		}
		return null;
	}
	
}
