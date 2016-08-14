/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.util;

import org.eclipse.jface.viewers.StyledString;
import org.eclipse.jface.viewers.StyledString.Styler;
import org.eclipse.swt.graphics.RGB;

import abs.frontend.typechecker.locationtypes.LocationType;
import abs.frontend.typechecker.locationtypes.infer.LocationTypeInferrerExtension.LocationTypingPrecision;

/**
 * Class containing all constants used in the plug-in.
 * @author cseise, fstrauss, mweber, tfischer
 */
public class Constants {
	
	/**
	 * Specifies the path of icons local to the plugin's install directory.
	 * needed for {@link Images#createIcon}.
	 */
	public final static String ICON_PATH = "icons/";
	
	
	//-------------------- GENERAL    -------------------------------------------------------------------------
	public static final boolean  DO_DEBUG       			= true;
	public static final Object[] EMPTY_OBJECT_ARRAY         = new Object[0];
	//-------------------- IDENTIFIER -------------------------------------------------------------------------
	public static final String PLUGIN_PACKAGE               = "org.abs-models.abs.plugin";
	public static final String ABSFRONTEND_PLUGIN_ID        = "org.abs-models.abs.compiler"; //Plugin within absfrontend.jar
	public static final String SDEDIT_PLUGIN_ID             = "org.abs-models.sdedit";
	public static final String PLUGIN_ID                    = PLUGIN_PACKAGE;
	public static final String ABS_FILE_EXTENSION           = "abs";
	/**
	 * ID of this project nature
	 */
	public static final String NATURE_ID               = PLUGIN_PACKAGE + ".ABSNature";
	public static final String ABSDEBUGVIEW_CONTEXT_ID = PLUGIN_PACKAGE + ".debugViewContext";
	public static final String BUILDER_ID              = PLUGIN_PACKAGE + ".ABSBuilder";
	public static final String ACTION_EXEC_ID          = PLUGIN_PACKAGE + ".execMaude";
	public static final String EDITOR_ID               = PLUGIN_PACKAGE + ".abseditor";
	public static final String ABSEDITOR_CONTEXT_ID    = PLUGIN_PACKAGE + ".abseditorscope";
	
	public static final String MODULE_DECORATOR_ID     = PLUGIN_PACKAGE + ".decorator.problemmarker";
	//Perspectives
	public static final String ABSPERSPECTIVE_ID       = PLUGIN_PACKAGE + ".absperspective";
	public static final String ABSDEBUGPERSPECTIVE_ID  = PLUGIN_PACKAGE + ".debug.perspective.debugperspective";
	//Views
	public static final String ABS_DEBUG_VIEW          = PLUGIN_PACKAGE + ".debug.views.debugview.DebugView";
	public static final String ABS_DEBUG_VARIABLE_VIEW = PLUGIN_PACKAGE + ".debug.views.variableview.VariableView";
	public static final String NAVIGATOR_ID            = PLUGIN_PACKAGE + ".navigator.view";
	public static final String ABS_TYPE_HIERARCHY_VIEW = PLUGIN_PACKAGE + ".editor.views.typehierarchy";
	//-------------------- BUILDER MARKER ---------------------------------------------------------------------
	public static final String MARKER_TYPE           = PLUGIN_PACKAGE + ".absproblem";
	public static final String PARSE_MARKER_TYPE     = PLUGIN_PACKAGE + ".absparseproblem";
	public static final String TYPECHECK_MARKER_TYPE = PLUGIN_PACKAGE + ".abstypecheckproblem";
	public static final String LOCATION_TYPE_INFERENCE_MARKER_TYPE = PLUGIN_PACKAGE + ".locationtypeinferenceinfo";
	public static final String START_LINE            = PLUGIN_PACKAGE + ".startline";
	public static final String END_LINE              = PLUGIN_PACKAGE + ".endline";
	public static final String START_COLUMN          = PLUGIN_PACKAGE + ".startcol";
	public static final String END_COLUMN            = PLUGIN_PACKAGE + ".endcol";
//	public static final QualifiedName FILE_AST       = new QualifiedName(PLUGIN_PACKAGE, "fileast");
	
	//-------------------- JAVA COMPILER / DEBUGGER ----------------------------------------------------------------------
	public static final String ACTION_START_SDE 	= PLUGIN_PACKAGE+".startsdedit";
	public static final String ACTION_DEBUG_ID 		= PLUGIN_PACKAGE + ".debugJava";
	public static final String ACTION_COMPILE_ID 	= PLUGIN_PACKAGE + ".compileJava";
	public static final String SDE_MAIN_CLASS   	= "net.sf.sdedit.Main";
	
	//-------------------- RUN CONFIGURATION ----------------------------------------------------------------------
	
	//Attribute Names
	public static final String RUNCONFIG_PROJECT_NAME_ATTRIBUTE 			= "ProjectNames";
	public static final String RUNCONFIG_PRODUCT_NAME_ATTRIBUTE             = "ProductName";
	public static final String RUNCONFIG_RUNTARGET_ATTRIBUTE                = "Runtarget";
	public static final String RUNCONFIG_ECLIPSE_SCHEDULER_ATTRIBUTE        = "EclipseScheduler";
	public static final String RUNCONFIG_DEBUGGER_SCHEDULER_ATTRIBUTE 		= "TotalScheduler";
	public static final String RUNCONFIG_DEBUGGER_OTHER_ARGS_ATTRIBUTE 		= "otherArgs";
	public static final String RUNCONFIG_DEBUGGER_COMPILE_BEFORE 			= "compileBeforeDebugging";
	public static final String RUNCONFIG_DEBUGGER_RANDOMSEED 				= "radomSeed";
	public static final String RUNCONFIG_DEBUGGER_USE_EXTERNAL 				= "externalDebugger";
	public static final String RUNCONFIG_DEBUGGER_DEBUG_MODE				= "debugMode";
	public static final String RUNCONFIG_DEBUGGER_OBSERVER_LIST 			= "observerList";
	public static final String RUNCONFIG_DEBUGGER_CLASSPATH_LIST            = "classpathList"; 
	public static final String RUNCONFIG_MAUDE_EXECUTE						= "executeMaude";
	public static final String RUNCONFIG_MAUDE_PARTIAL_EXEC					= "partialExec";
	public static final String RUNCONFIG_MAUDE_STEPS						= "maudeSteps";
	public static final String RUNCONFIG_MAUDE_REALTIME						= "maudeRealtime";
	public static final String RUNCONFIG_MAUDE_MAINBLOCK					= "maudeMain";
	public static final String RUNCONFIG_TEST_EXECUTION					   = "testExecution";
	public static final String RUNCONFIG_HISTORY_FILE                      = "historyFile";
	public static final String RUNCONFIG_RUN_AUTOMATICALLY                 = "runAutomatically";
	public static final String RUNCONFIG_FLI_IGNORE_MISSING_CLASSES        = "fliIgnoreMissingClasses";
	public static final String RUNCONFIG_USE_FIFO_SEMANTICS        = "useFifoSemantics";
	
	//Default Values (see also RunConfigEnums)			
	public static final String 	DEBUGGER_ARGS_OTHER_DEFAULT					= "";
	public static final boolean DEBUGGER_COMPILE_BEFORE_DEFAULT 			= true;
	public static final boolean RUNCONFIG_DEBUGGER_USE_EXTERNAL_DEFAULT		= false;
	public static final boolean RUNCONFIG_DEBUGGER_DEBUG_MODE_DEFAULT		= true;	
		
	//-------------------- ABSUnit Test Execution -------------------------------------------------------------
	/**
	 * ABSUnit Test Execution terminated successfully (nothing will be printed)
	 */
	public static final int ABSUNIT_TEST_OK = 0;
	/**
	 * Denotes exceptions occurring during execution (status message will be shown in a dialog together with message of the occuring exception)
	 */
	public static final int ABSUNIT_TEST_ERROR = 48;
	/**
	 * Denotes an abort initiated by the user (will print a predefined String as error)
	 */
	public static final int ABSUNIT_TEST_USER_ABORT = 64;
	
	//-------------------- MAUDE JOB --------------------------------------------------------------------------
	/**
	 * Maude Job terminated successfully (nothing will be printed)
	 */
	public static final int MAUDE_OK = 0;
	/**
	 * Can be used to denote additional Information (status message will be printed as info)
	 */
	public static final int MAUDE_INFO = 1;
	/**
	 * Can be used to print warnings (status message will be printed as warning)
	 */
	public static final int MAUDE_WARNING = 2;
	/**
	 * Denotes an error from the Maude executable (status message will be printed as error)
	 */
	public static final int MAUDE_ERROR_MAUDE = 4;
	/**
	 * Denotes exceptions occurring during execution (status message will be shown in a dialog together with message of the occuring exception)
	 */
	public static final int MAUDE_ERROR = 8; 
	/**
	 * Denotes a specific IOException (see MaudeJob for details - will print a predefined String as error)
	 */
	public static final int MAUDE_ERROR_MAUDE_PATH = 16;
	/**
	 * Denotes an abort initiated by the user (will print a predefined String as error)
	 */
	public static final int MAUDE_USER_ABORT = 32;
	
//	public static final String MAUDE_COMMAND = "rew start . \n quit ";
	public static final String MAUDE_COMMAND1     = "rew ";
	public static final String MAUDE_COMMAND2     = " start .";
	public static final String MAUDE_COMMAND_QUIT = " \n quit";
	public static final String MAUDE_COMMAND      = MAUDE_COMMAND1 + MAUDE_COMMAND2 + MAUDE_COMMAND_QUIT;
	public static final String BACKEND_MAUDE_INTERPRETER = "abs/backend/maude/abs-interpreter.maude";
	
	//-------------------- PREFERENCES AND PROPERTIES----------------------------------------------------------
	//Maven Executable path
	public static final String MAVEN_EXEC_PATH                = "MavenPath";
	public static final String DEFAULT_MAVEN_EXEC_PATH        = "";
	public static final String DEFAULT_MAVEN_POM_PATH        = "pom.xml";
	
	//Maude Executable path
	public static final String MAUDE_EXEC_PATH                = "MaudePath";
	public static final String DEFAULT_MAUDE_EXEC_PATH        = "";
	public static final String DEFAULT_MAUDE_EXEC_PATH_WIN32  = "/Program Files/MaudeFW/maude";
    public static final String DEFAULT_MAUDE_EXEC_PATH_LINUX  = "/usr/bin/maude";
    public static final String DEFAULT_MAUDE_EXEC_PATH_MACOSX = "/Applications/maude/maude";
    
    //gen folder Paths
    public static final String JAVA_SOURCE_PATH      = "JavaSourcePath";
	public static final String MAUDE_PATH            = "maudePath";
	public static final String DEFAULT_JAVA_PATH     = "gen/java";
	public static final String DEFAULT_MAUDE_PATH    = "gen/maude";
	public static final String DEFAULT_MAVEN_TARGET_ABS_PATH     = "target/abs/gen/abs";
	public static final String DEFAULT_MAVEN_TARGET_JAVA_PATH     = "target/abs/gen/java";
	public static final String DEFAULT_MAVEN_TARGET_MAUDE_PATH    = "target/abs/gen/maude";
	
	//Java properties
	public static final String SOURCE_ONLY           = "sourceOnly";
	public static final String NO_WARNINGS           = "noWarnings";
	public static final String ALWAYS_COMPILE        = "alwaysCompile";
	
	//Type check properties
	public static final String PRODUCT_TYPECHECK              = "productTypeCheck";
	public static final String LOCATION_TYPECHECK              = "locationTypeCheck";
	public static final String LOCATION_TYPE_OVERLAY           = "locationTypeOverlay";
	public static final String LOCATION_TYPE_HIGHLIGHT         = "locationTypeHighlight";
	public static final String DEFAULT_LOCATION_TYPE           = "defaultLocationType";
	public static final String DEFAULT_DEFAULT_LOCATION_TYPE   = LocationType.INFER.toString();
	public static final String LOCATION_TYPE_PRECISION         = "locationTypePrecision";
    public static final String DEFAULT_LOCATION_TYPE_PRECISION = LocationTypingPrecision.CLASS_LOCAL_FAR.toString();
	
    public static final String LOCATION_TYPE_NEAR_TEXTSTYLE_KEY      = "locationtype.near.textStyle";
    public static final String LOCATION_TYPE_FAR_TEXTSTYLE_KEY       = "locationtype.far.textStyle";
    public static final String LOCATION_TYPE_SOMEWHERE_TEXTSTYLE_KEY = "locationtype.somewhere.textStyle";

    public static final String LOCATION_TYPE_NEAR_TEXTSTYLE_VALUE      = "NEAR_LOCATION";
    public static final String LOCATION_TYPE_FAR_TEXTSTYLE_VALUE       = "FAR_LOCATION";
    public static final String LOCATION_TYPE_SOMEWHERE_TEXTSTYLE_VALUE = "SOMEWHERE_LOCATION";
    
	//Maven properties
    public static final String MAVEN_IGNORE_TARGET_FOLDER = "IGNORE_TARGET_FOLDER";
    
	//project preference store
	public static final String PROJECT_PREFERENCE_STORE_QUALIFIER = "projectProperties";
	
	//-------------------- SYNTAX HIGHLIGHTING ---------------------------------------------------------------
	//Parenthesis Highlighting
	public final static String EDITOR_MATCHING_BRACKETS       = "matchingBrackets";
	public final static String EDITOR_MATCHING_BRACKETS_COLOR = "matchingBracketsColor";

	//Partition Scanner
	public static final String PARTITION_SINLGE_LINE_COMMENT = "singleComment";
	public static final String PARTITION_MULTI_LINE_COMMENT  = "multiComment";
	public static final String PARTITION_STRING              = "string";
	public static final String PARTITION_CHARACTER           = "char";
	// FIXME: This one is not as static as you think (MS_MUTABLE_ARRAY)
	public static final String[] PARTITION_TYPES = {PARTITION_SINLGE_LINE_COMMENT, PARTITION_MULTI_LINE_COMMENT, PARTITION_STRING, PARTITION_CHARACTER};
	
	//PreferenceStore Strings
	public static final String SYNTAXCOLOR_COLOR         = "Color";
	public static final String SYNTAXCOLOR_BOLD          = "Bold";
	public static final String SYNTAXCOLOR_ITALIC        = "Italic";
	public static final String SYNTAXCOLOR_UNDERLINE     = "Underline";
	public static final String SYNTAXCOLOR_STRIKETHROUGH = "Strikethrough";

	public static final String SYNTAXCOLOR_KEYWORD     = "Keyword";
	public static final String SYNTAXCOLOR_STRING      = "String";
	public static final String SYNTAXCOLOR_COMMENT     = "Comment";
	public static final String SYNTAXCOLOR_FUNCTION    = "Function";
	public static final String SYNTAXCOLOR_DATATYPE    = "Datatype";
	public static final String SYNTAXCOLOR_VAR         = "Var";
	public static final String SYNTAXCOLOR_PARAM       = "Param";
	public static final String SYNTAXCOLOR_FIELD       = "Field";
	public static final String SYNTAXCOLOR_INTERFACE   = "Interface";
	public static final String SYNTAXCOLOR_CONSTRUCTOR = "Constructor";
	
	//Colors
	private static final RGB COLOR_BLACK = new RGB(0,0,0);
	
	public static final RGB SYNTAXCOLOR_RGB_KEYWORD      = new RGB(127, 0, 85);
	public static final RGB SYNTAXCOLOR_RGB_STRING       = new RGB(42, 0, 255);
	public static final RGB SYNTAXCOLOR_RGB_COMMENT      = new RGB(63, 127, 95);
	public static final RGB SYNTAXCOLOR_RGB_FUNCTION     = COLOR_BLACK;
	public static final RGB SYNTAXCOLOR_RGB_DATATYPE     = new RGB(64, 64, 64);
	public static final RGB SYNTAXCOLOR_RGB_VAR          = COLOR_BLACK;
	public static final RGB SYNTAXCOLOR_RGB_PARAM        = COLOR_BLACK;
	public static final RGB SYNTAXCOLOR_RGB_FIELD        = new RGB(0, 0, 128); 
	public static final RGB SYNTAXCOLOR_RGB_INTERFACE    = SYNTAXCOLOR_RGB_FIELD;          
	public static final RGB SYNTAXCOLOR_RGB_CONSTRUCTOR  = COLOR_BLACK;
	
	//-------------------- ACTIVATOR JOBS ----------------------------------------------------------------------
	public static final String REBUILD_ABS_MODEL_JOB_NAME      = "Rebuilding ABS Model";
	public static final String DEFAULT_MATCHING_BRACKETS_COLOR = "128,128,128";
	
	//-------------------- DEBUGGER ----------------------------------------------------------------------------
	public static enum  Scheduler {interactive, random, replay}
	public static final Scheduler DEFAULT_SCHEDULER = Scheduler.interactive;

	public static final String CURRENT_IP_MARKER         = PLUGIN_PACKAGE + ".currentIP";
	
	//Buttons
	public static final String DEBUG_BUTTON_STEP_ID             = PLUGIN_PACKAGE + ".debug.perspective.ExecuteSingleStep";
	public static final String DEBUG_BUTTON_NSTEPS_ID           = PLUGIN_PACKAGE + ".debug.perspective.ExecuteNSteps";
	public static final String DEBUG_BUTTON_STEP_OVER_ID        = PLUGIN_PACKAGE + ".debug.perspective.StepOver";
	public static final String DEBUG_BUTTON_RUN_TO_LINE_ID      = PLUGIN_PACKAGE + ".debug.perspective.RunToLine";
	public static final String DEBUG_BUTTON_HISTORY_ID          = PLUGIN_PACKAGE + ".debug.perspective.SaveHistory";
	public static final String DEBUG_BUTTON_RESUME_ID           = PLUGIN_PACKAGE + ".debug.perspective.Resume";
	public static final String DEBUG_BUTTON_SUSPEND_ID          = PLUGIN_PACKAGE + ".debug.perspective.Suspend";
	public static final String DEBUG_BUTTON_TERMINATE_ID        = PLUGIN_PACKAGE + ".debug.perspective.Terminate";
	public static final String DEBUG_BUTTON_SCHEDULER_CHOICE_ID = PLUGIN_PACKAGE + ".debug.perspective.SelectScheduler";
	//-------------------- STYLERS -----------------------------------------------------------------------------
	private static final String STYLER_COLOR_BLACK_STRING = PLUGIN_PACKAGE + ".BLACK";
	private static final String STYLER_COLOR_GREY_STRING  = PLUGIN_PACKAGE + ".GREY";
	private static final String STYLER_COLOR_TYPES_STRING = PLUGIN_PACKAGE + ".TYPES";
	
	public static final String STYLER_COLOR_BLACK = STYLER_COLOR_BLACK_STRING;
	public static final String STYLER_COLOR_GREY  = STYLER_COLOR_GREY_STRING;
	public static final String STYLER_COLOR_TYPES = STYLER_COLOR_TYPES_STRING;
	
	public static final RGB STYLER_COLOR_BLACK_RGB = COLOR_BLACK;
	public static final RGB STYLER_COLOR_TYPES_RGB = new RGB(149, 125, 71);
	public static final RGB STYLER_COLOR_GREY_RGB  = new RGB(150,150,150);
	
	public static final Styler STYLER_BLACK   = StyledString.createColorRegistryStyler(STYLER_COLOR_BLACK, null);
	public static final Styler STYLER_GREY    = StyledString.createColorRegistryStyler(STYLER_COLOR_GREY, null);
	public static final Styler STYLER_TYPES   = StyledString.createColorRegistryStyler(STYLER_COLOR_TYPES, null);


	public static final String CURRENT_INSTRUCTION_POINTER_ID = "org.abs-models.abs.plugin.currentIP";


}