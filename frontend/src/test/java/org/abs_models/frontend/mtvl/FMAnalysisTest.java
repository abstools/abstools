package org.abs_models.frontend.mtvl;

import static org.junit.Assert.*;

import org.junit.Test;

import org.abs_models.frontend.FrontendTest;
import org.abs_models.frontend.ast.Model;

public class FMAnalysisTest extends FrontendTest {
	
	  static private String helloprogram =
		        " module Helloworld;" +
		        " product P1 (English);" +
		        " product P2 (French);" +
		        " product P3 (French, Repeat{times=10});" +
		        " product P4 (English, Repeat{times=6});" +
		        " root MultiLingualHelloWorld {" +
		        "   group allof {" +
		        "      Language {" +
		        "        group oneof { English, Dutch, French, German }" +
		        "      }," +
		        "      opt Repeat {" +
		        "        Int times in [0 .. 10];" +
		        "        ifin: times > 0; " +
		        "      } " +
		        "    } " +
		        " }" +
		        " extension English {" +
		        "    group oneof { UK, US }" +
		        "    ifin: Repeat ->" +
		        "          (Repeat.times >= 2 && Repeat.times <= 5);" +
		        " }";
	  
	  static private String emptyprogram = " root FM";

	  @Test
	  public void nsol() {
		  Model model1 = assertParse(emptyprogram);
		  Model model2 = assertParse(helloprogram);
		  
	      ChocoSolver solverEmptyFM = model1.instantiateCSModel();
	      ChocoSolver solver1 = model2.instantiateCSModel(); 
	      model2.dropAttributes();
	      ChocoSolver solver2 = model2.instantiateCSModel(); //model with no attribute

	      //Boundary Test(Lower Bound) - empty FM
	      assertEquals(1, solverEmptyFM.countSolutions());
	      
	      //Boundary Test(Mid Range) - with and without attributes
	      assertEquals(93, solver1.countSolutions());
	      assertEquals(10, solver2.countSolutions());
	  }
	  
	  @Test
	  public void isvoid() {
		  Model model1 = assertParse(emptyprogram);
		  Model model2 = assertParse(helloprogram);
		  
		  ChocoSolver solverEmptyFM = model1.instantiateCSModel();
	      ChocoSolver solver = model2.instantiateCSModel();  
	      
	      //Boundary Test(Lower Bound) - empty FM
	      assertEquals("Feature Model is void.", solverEmptyFM.isVoid());
	      
	      //Boundary Test(Mid Range)
	      assertEquals("Feature Model is not void.", solver.isVoid());
	  }
	  
	  @Test
	  public void core() {
		  String result1 = "FM\n";
		  String result2 = 
				  "MultiLingualHelloWorld\n"+
		          "Language\n";
		  
		  Model model1 = assertParse(emptyprogram);
		  Model model2 = assertParse(helloprogram);
		  
		  ChocoSolver solverEmptyFM = model1.instantiateCSModel();
	      ChocoSolver solver = model2.instantiateCSModel(); 
		  
	      //Boundary Test(Lower Bound) - empty FM
	      assertEquals(result1, solverEmptyFM.coreToStrings());
	      
	      //Boundary Test(Mid Range)
	      assertEquals(result2, solver.coreToStrings());
	  }
	  
	  @Test
	  public void variant() {
		  String result1 = "";
		  String result2 = 
				  "English\n" +
				  "Dutch\n" +
				  "French\n" +
				  "German\n" +
				  "Repeat\n" +
				  "UK\n" +
				  "US\n";
		  
		  Model model1 = assertParse(emptyprogram);
		  Model model2 = assertParse(helloprogram);
		  
		  ChocoSolver solverEmptyFM = model1.instantiateCSModel();
	      ChocoSolver solver = model2.instantiateCSModel(); 
		  
	      //Boundary Test(Lower Bound) - empty FM
	      assertEquals(result1, solverEmptyFM.variantToStrings());
	      
	      //Boundary Test(Mid Range)
	      assertEquals(result2, solver.variantToStrings());
	  }
}
