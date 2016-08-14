package apet.testCases;

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
	 * Get the list of previous calls of the test case
	 * 
	 * @param c
	 * @return
	 */
	public static List<PreviousCall> getPreviousCalls(TestCase c) {
		return c.prevCalls;
	}
	
	/**
	 * Method name of the given previous call
	 * 
	 * @param call
	 * @return
	 */
	public static String getMethodName(PreviousCall call) {
		return call.method;
	}
	
	/**
	 * Arguments of the previous call
	 * 
	 * @param call
	 * @return
	 */
	public static List<ABSData> getCallArgs(PreviousCall call) {
		return call.args;
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
	 * Get the arguments of the ABS term
	 * @param d
	 * @return
	 */
	public static List<ABSData> getABSTermArgs(ABSTerm d) {
		return d.args;
	}
	
	/**
	 * Get the functor of the ABS term
	 * @param d
	 * @return
	 */
	public static String getABSTermFunctor(ABSTerm d) {
		return d.functor;
	}
	
	
	/**
	 * Get the type name of the ABS term 
	 * @param d
	 * @return
	 */
	public static String getABSTermTypeName(ABSTerm d) {
		return d.typeName;
	}
	
	/**
	 * Get the type name of the ABS term 
	 * @param d
	 * @return
	 */
	public static String[] getABSTermTypeParams(ABSTerm d) {
		return d.typeParams;
	}
	
	/**
	 * Get the string value of the test data
	 * @param d
	 * @return
	 */
	public static String getABSDataValue(ABSData d) {
		return d.value;
	}
	
	/**
	 * Get the string type of the test data
	 * @param d
	 * @return
	 */
	public static String getABSDataType(ABSData d) {
		return d.type;
	}
	
	/**
	 * Get the string type of the object.
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
