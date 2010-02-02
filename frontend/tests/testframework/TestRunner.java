package testframework;

public class TestRunner {

   public static void main(String[] args) {

	   String[] testCases = {"abs.frontend.parser.ScannerTest", "abs.frontend.parser.ParserTest"};
	   org.junit.runner.JUnitCore.main(testCases);
	   

   }
}



