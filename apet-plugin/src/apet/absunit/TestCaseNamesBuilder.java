package apet.absunit;

import static apet.absunit.ABSUnitTestCaseTranslatorConstants.ASSERT_TEST_PREFIX;
import static apet.absunit.ABSUnitTestCaseTranslatorConstants.CLASS_FUNCTION_PREFIX;
import static apet.absunit.ABSUnitTestCaseTranslatorConstants.CLASS_SUFFIX;
import static apet.absunit.ABSUnitTestCaseTranslatorConstants.DELTA_SUFFIX;
import static apet.absunit.ABSUnitTestCaseTranslatorConstants.GETTER_PREFIX;
import static apet.absunit.ABSUnitTestCaseTranslatorConstants.INITIAL_TEST_PREFIX;
import static apet.absunit.ABSUnitTestCaseTranslatorConstants.INTERFACE_SUFFIX;
import static apet.absunit.ABSUnitTestCaseTranslatorConstants.METHOD_PREFIX;
import static apet.absunit.ABSUnitTestCaseTranslatorConstants.SETTER_PREFIX;
import abs.common.StringUtils;

final class TestCaseNamesBuilder {

	String deltaOnClass(String className) {
		return className + DELTA_SUFFIX;
	}
	
	String interfaceForModifyingFieldOfClass(String className) {
		return "ModifierFieldsOf"+className+"ForTest";
	}
	
	String setterMethodName(String fieldName) {
		return SETTER_PREFIX+StringUtils.capitalize(fieldName);
	}
	
	String getterMethodName(String fieldName) {
		return GETTER_PREFIX + StringUtils.capitalize(fieldName);
	}
	
	String resultOfGetterMethodName(String fieldName) {
		return getterMethodName(fieldName) + "ReturnValue";
	}
	
	String className(String interfaceName) {
		return interfaceName + CLASS_SUFFIX;
	}
	
	String functionClassName(String functionName) {
		return CLASS_FUNCTION_PREFIX + functionName;
	}
	
	String testInterfaceName(String className, String capMethodName) {
		return className + INTERFACE_SUFFIX + "For" + capMethodName;
	}
	
	String testMethodName(String capMethodName, String testCaseName) {
		return METHOD_PREFIX+ capMethodName + testCaseName;
	}
	
	String initialTestMethodName(String testName) {
		return INITIAL_TEST_PREFIX + StringUtils.capitalize(testName);
	}
	
	String assertTestMethodName(String testName) {
		return ASSERT_TEST_PREFIX + StringUtils.capitalize(testName);
	}
	


		
}
