package apet.preferences;

public class ApetOptions {
	
public static final String COVERAGE_CRITERION_TITLE = "Coverage criterion";
	
	public static final String[] COVERAGE_CRITERION_OPTS = {
		"Block-k",
		"Depth-k"
	};
	
	public static final String[] COVERAGE_CRITERION_PROLOG = {
		"bck",
		"dpk"
	};
	
	public static final String NUM_PATH_CONSTRAINTS_TITLE = "Numeric test-cases or path constraints";
	
	public static final String[] NUM_PATH_CONSTRAINTS_OPTS = {
		"Numeric (In this case a range must be especified below)",
		"Path-constraints"
	};
	
	public static final String[] NUM_PATH_CONSTRAINTS_PROLOG = {
		"num",
		"constraint"
	};
	
	public static final String[] RANGE_OPTS = {
		"Specify the range of integers to take data from"
	};
	
	public static final String[] GEN_TEST_OPTS = {
		"Generate test-case generator"
	};
	
	public static final String[] ALIASING_OPTS = {
		"References aliasing"
	};

	public static final String LABELING_TITLE = "Labeling strategy";
	
	public static final String[] LABELING_OPTS = {
		"ff",
		"leftmost",
		"min",
		"max"
	};
	
	public static final String[] LABELING_PROLOG = {
		"ff",
		"leftmost",
		"min",
		"max"
	};
	
	public static final String VERBOSITY_TITLE = "Verbosity";
	
	public static final String[] VERBOSITY = {
		"0",
		"1",
		"2"
	};
	
	public static final String[] SAVE_CLP_OPTS = {
		"Save the intermediate CLP decompiled program"
	};
	
	public static final String TRACING_TITLE = "Tracing";
	
	public static final String[] TRACING_OPTS = {
		"none",
		"statements",
		"blocks"
	};
	
	public static final String[] TRACING_PROLOG = {
		"none",
		"statements",
		"blocks"
	};
	
	public static final String GEN_JUNIT_TITLE = "Generate JUnit test";
	
	public static final String[] GEN_JUNIT_OPTS = {
		"no",
		"minimun",
		"complete"
	};
	
	public static final String[] GEN_JUNIT_PROLOG = {
		"no",
		"minimun",
		"complete"
	};

}
