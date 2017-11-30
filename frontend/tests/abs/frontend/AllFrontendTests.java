/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend;

import abs.frontend.analyser.*;
import abs.frontend.delta.OriginalCallTest;
import abs.frontend.delta.TraitTest;
import abs.frontend.mtvl.SearchSolutionsTest;
import abs.frontend.pardef.PardefTests;
import abs.frontend.parser.*;
import abs.frontend.typesystem.*;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

import deadlock.DeadlockCasestudies;
import deadlock.DeadlockCheckerTests;

@RunWith(Suite.class)
@Suite.SuiteClasses({
        ModuleSystemTests.class, FreeVarTest.class,
            ParserTest.class,
            // RecoverTest.class, // deactivated since switching parser to antlr
            DuplicateCheckTest.class,
            InterfaceDeclarationTest.class,
            ParseSamplesTest.class,
            VarResolutionTest.class,
            TypingTest.class,
            TypeCheckerTest.class,
            NegativeTypeCheckerTests.class,
            LocationTypeTests.class,
            ExamplesTypeChecking.class,
            AnnotationTests.class,
            ClassKindTests.class,
            OtherAnalysisTests.class,
            AtomicityTests.class,
            BackPositionTest.class,
            // IncompleteExpTests.class, // deactivated since switching parser to antlr
            SearchSolutionsTest.class,
            TestABSPackages.class,
            CaseStudyTypeChecking.class,
            DeadlockCheckerTests.class,
            DeadlockCasestudies.class,
            OriginalCallTest.class,
            TraitTest.class,
            PardefTests.class
            })
public class AllFrontendTests {}
