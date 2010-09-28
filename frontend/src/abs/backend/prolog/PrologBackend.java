package abs.backend.prolog;

import java.io.BufferedOutputStream;
import java.io.*;
import java.util.Arrays;

import abs.frontend.ast.*;
import abs.frontend.parser.Main;


public class PrologBackend extends Main {

	private File destDir = new File(".");
	public static int numCaseIds = 0;
	
	public static void main(final String[] args)  {
		try {
			new PrologBackend().absToPrologTerms(args);
		} catch(Exception e) {
			System.err.println("An error occurred during compilation: "+e.getMessage());

			if (Arrays.asList(args).contains("-debug")) {
				e.printStackTrace();
			}
			System.exit(1);
		}
	}
	
    protected void printUsage() {
        super.printUsage();
        System.out.println("Prolog Backend:");
        System.out.println("  -d <dir>     generate files to <dir>");
    }
    
    private void absToPrologTerms(String[] args) throws Exception {
        final Model model = parse(args); // This parses the ABS producing an AST
        if (model.hasErrors() || model.hasTypeErrors())
            return;
                
        if (!destDir.exists()) {
            System.err.println("Destination directory "+destDir.getAbsolutePath()+" does not exist!");
            System.exit(1);
        } 

        if (!destDir.canWrite()) {
            System.err.println("Destination directory "+destDir.getAbsolutePath()+" cannot be written to!");
            System.exit(1);
        } 
        //printAST(model,0);
        PrintStream s = new PrintStream(new BufferedOutputStream(
        		new FileOutputStream(new File(destDir, "abs.pl"))));
        model.generateProlog(s);
    }

    private void printAST(ASTNode<?> ast,int level){
    	if (ast != null){
    		printTab(level);
    		System.out.println(level + ":" + ast.getClass().getName() + "@" + ast.getId());
    		int n = ast.getNumChild();
    		for (int i = 0; i < n; i++)
    			printAST(ast.getChild(i),level+1);
    	}
    }
    
    private void printTab(int n){
    	for (int i = 0;i < n;i++) System.out.print("    ");
    }
}
