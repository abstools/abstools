package abs.javabackend;

import abs.frontend.ast.Model;
import abs.frontend.parser.Main;

public class JavaBackend {
	public static void main(String[] args) throws Exception {
	   Model model = Main.parse(args[0]);
	   
   }
}
