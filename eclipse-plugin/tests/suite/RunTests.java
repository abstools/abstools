/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package suite;



import org.absmodels.abs.plugin.console.ConsoleManagerTest;
import org.absmodels.abs.plugin.editor.outline.ABSContentOutlineUtilsTest;
import org.absmodels.abs.plugin.internal.ModelBuilderTest;
import org.absmodels.abs.plugin.navigator.ModulePathTest;
import org.absmodels.abs.plugin.navigator.NavigatorUtilsTests;
import org.absmodels.abs.plugin.util.InternalASTNodeTests;
import org.absmodels.abs.plugin.util.UtilityFunctionsTest;
import org.absmodels.abs.plugin.wizards.WizardUtilsTests;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;




@RunWith(Suite.class)
@SuiteClasses(
		{
			ModulePathTest.class,
			NavigatorUtilsTests.class,
			UtilityFunctionsTest.class,
			InternalASTNodeTests.class,
			WizardUtilsTests.class,
			ABSContentOutlineUtilsTest.class,
			ModelBuilderTest.class,
			ConsoleManagerTest.class
		}
)
/**
 * IMPORTANT NOTE:
 * 
 * Please execute this test suite as JUnit plug-in test.
 * 
 * debug.scheduling.SchedulingTest is NOT included in this test suite due to incompatibilities with Eclipse and Mockito.
 * Please run debug.scheduling.SchedulingTest separately as a NORMAL JUnit test.
 */
public class RunTests {

}
