/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package suite;



import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import eu.hatsproject.absplugin.console.ConsoleManagerTest;
import eu.hatsproject.absplugin.editor.outline.ABSContentOutlineUtilsTest;
import eu.hatsproject.absplugin.internal.ModelBuilderTest;
import eu.hatsproject.absplugin.navigator.ModulePathTest;
import eu.hatsproject.absplugin.navigator.NavigatorUtilsTests;
import eu.hatsproject.absplugin.util.InternalASTNodeTests;
import eu.hatsproject.absplugin.util.UtilityFunctionsTest;
import eu.hatsproject.absplugin.wizards.WizardUtilsTests;



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
