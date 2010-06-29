package abs.javabackend;

import abs.frontend.ast.Model;
import abs.frontend.parser.Main;

public class JavaBackend {
	public static void main(String[] args) throws Exception {
	   Main parser = new Main();
	   Model model = parser.parse(args[0]);
   }
}
