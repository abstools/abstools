package abs.frontend.delta;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

@RunWith(Suite.class)
@SuiteClasses({
    AddImportsTest.class,
    AddRemoveInterfacesTest.class,
    AddRemoveModifyClassesTest.class,
    ApplicationConditionsTest.class,
    DeltaAddFunctionalTest.class,
    DeltaAttributesBooleanTest.class,
    DeltaAttributesIntegerTest.class,
    DeltaAttributesMixedTest.class,
    DeltaOrderingTest.class,
    DeltaSamplesTest.class,
    DeltaSyntax.class,
    DeltaTrieTest.class,
    ModifyInterfacesTest.class,
    OriginalCallTest.class,
    ProductDeclarationTest.class,
    ProductLineTypeAnalysisTest.class,
    ProgramAbstractionTest.class,
    SourceCodePositionTest.class,
    TopologicalSortingTest.class,
    TraitTest.class,
    VarResolutionTest.class
})
public class DeltaTests {
}
