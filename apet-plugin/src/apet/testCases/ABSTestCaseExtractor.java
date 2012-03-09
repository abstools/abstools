package apet.testCases;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import apet.testCases.ABSObject.Field;

public final class ABSTestCaseExtractor {
	
	private ABSTestCaseExtractor() { }
	
	/**
	 * Get method name of the test case
	 * @param c
	 * @return
	 */
	public static String getMethodMame(TestCase c) {
		return c.method_name;
	}
	
	/**
	 * Get input arguments of the test case
	 * @param c
	 * @return
	 */
	public static List<ABSData> getInputArgs(TestCase c) {
		return c.argsIn;
	}
	
	/**
	 * Get the initial state of the test case
	 * @param c
	 * @return
	 */
	public static Map<ABSRef,ABSObject> getInitialState(TestCase c) {
		return c.heapIn;
	}
	
	/**
	 * Get the return value of the test case
	 * @param c
	 * @return
	 */
	public static ABSData getReturnData(TestCase c) {
		return c.returnData;
	}
	
	/**
	 * Get the after state of the test case
	 * @param c
	 * @return
	 */
	public static Map<ABSRef,ABSObject> getAfterState(TestCase c) {
		return c.heapOut;
	}
	
	/**
	 * Get the string value of the test data
	 * @param d
	 * @return
	 */
	public static String getABSData(ABSData d) {
		return d.value;
	}
	
	/**
	 * @param d
	 * @return
	 */
	public static String getABSObjectType(ABSObject d) {
		return d.type;
	}
	
	public static Map<String,ABSData> getABSObjectFields(ABSObject d) {
		Map<String,ABSData> map = new HashMap<String, ABSData>();
		for (Field f : d.fields) {
			map.put(f.fieldName,f.value);
		}
		return map;
	}
	
}
