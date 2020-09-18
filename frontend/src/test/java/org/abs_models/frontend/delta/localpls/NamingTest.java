package org.abs_models.frontend.delta.localpls;

import org.abs_models.common.WrongProgramArgumentException;
import org.abs_models.frontend.analyser.SemanticConditionList;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.ast.ModuleDecl;
import org.abs_models.frontend.delta.DeltaTest;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

public class NamingTest  extends DeltaTest {
    String localPLString = "module Service;"
        + "export *;"
        + "interface IService {"
        + "}"
        + "class CService() implements IService {}"
        + "features Insurance, CarRental, Media, Travel;"
        + "delta DIns;"
        + "adds interface I2{}"
        + "modifies class CService {"
        + " adds Unit printS() {"
        + "   println(\"S\");"
        + " }"
        + "}"
        + "delta DCar;"
        + "adds class CService2 (){"
        + " Unit print1() {"
        + "   println(\"Car\");"
        + " }"
        + "}"
        + "delta DMedia;"
        + "modifies class CService{"
        + " adds Unit print1() {"
        + "   println(\"1\");"
        + " }"
        + "}"
        + "delta DTravel;"
        + "modifies class CService{"
        + " adds String s = \"Travel\";"
        + "}"
        + "delta DIns when Insurance;"
        + "delta DCar when CarRental;"
        + "delta DMedia after DIns when Media;"
        ;

    String mainPart = "module M;"
        + "import * from Service;"
        + "{"
        + "  IService s = new CService() with P1; "
        + "}";
    @Test
    public void oneFeatureLongName() throws WrongProgramArgumentException {
        Model model = assertParse(localPLString + mainPart + "product P1 = {Insurance};");
        int before = model.getModuleDecls().size();
        model.flattenforLocalProducts();
        assertEquals(0, model.getTypeErrors().getErrorCount());
        int after = model.getModuleDecls().size();
        assertEquals(before + 1 , after);
        ModuleDecl originalModule = model.lookupModule("Service_Insurance");
        assertNotNull(originalModule);
    }
    @Test
    public void twoFeaturesLongName() throws WrongProgramArgumentException {
        Model model = assertParse(localPLString + mainPart + "product P1 = {Insurance, Media};");
        model.flattenforLocalProducts();
        SemanticConditionList errors = model.getTypeErrors();
        assertEquals(0, model.getTypeErrors().getErrorCount());
        ModuleDecl originalModule = model.lookupModule("Service_Insurance_Media");
        assertNotNull(originalModule);

    }
    /* This test checks that the name is "stable" even though the fearues in a set are stored
    in a java.util.Set */
    @Test
    public void twoFeaturesLongName2() throws WrongProgramArgumentException {
        for (int i =0 ; i< 30; ++i) {
            Model model = assertParse(localPLString + mainPart + "product P1 = {CarRental, Media};");
            model.flattenforLocalProducts();
            SemanticConditionList errors = model.getTypeErrors();
            assertEquals(0, model.getTypeErrors().getErrorCount());
            ModuleDecl originalModule = model.lookupModule("Service_Media_CarRental");
            assertNotNull(" failed at iteration " + i, originalModule);
        }
    }
    @Test
    public void threeFeatures() throws WrongProgramArgumentException {
        for (int i =0 ; i< 30; ++i) {
            Model model = assertParse(localPLString + mainPart + "product P1 = {CarRental, Media, Insurance};");
            model.flattenforLocalProducts();
            SemanticConditionList errors = model.getTypeErrors();
            assertEquals(0, model.getTypeErrors().getErrorCount());
            ModuleDecl originalModule = model.lookupModule("Service_Ins_Med_Car");
            assertNotNull("failed at iteration " + i , originalModule);
        }
    }
    @Test
    public void threeFeatures2() throws WrongProgramArgumentException {
        for (int i =0 ; i< 30; ++i) {
            Model model = assertParse(localPLString + mainPart + "product P1 = {CarRental, Media, Travel};");
            model.flattenforLocalProducts();
            SemanticConditionList errors = model.getTypeErrors();
            assertEquals(0, model.getTypeErrors().getErrorCount());
            ModuleDecl originalModule = model.lookupModule("Service_Tra_Med_Car");
            assertNotNull("failed at iteration " + i, originalModule);
        }
    }

    @Test
    public void fourFeatures() throws WrongProgramArgumentException {
            Model model = assertParse(localPLString
                + "delta DTravel after DIns when Travel;"+ mainPart + "product P1 = {Media, Insurance, CarRental, Travel};");
            model.flattenforLocalProducts();
            SemanticConditionList errors = model.getTypeErrors();
            assertEquals(0, model.getTypeErrors().getErrorCount());
            ModuleDecl originalModule = model.lookupModule("Service_Tra_Ins_Med_Car");
            assertNotNull(originalModule);
    }
}
