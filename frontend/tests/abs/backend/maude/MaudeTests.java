package abs.backend.maude;

import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.io.PrintWriter;

import junit.framework.Assert;

import org.junit.Test;

import abs.ABSTest;
import abs.backend.java.JavaBackendConstants;
import abs.frontend.ast.Model;
import abs.frontend.parser.Main;

public class MaudeTests extends ABSTest {
    private static String STANDARD_HEADER = 
        "in abs-interpreter mod MODEL is " +
        "protecting ABS-SIMULATOR-RL . " +
        "op classes : -> Configuration . " +
        "eq classes = < \".Start\" : Class | Param: noVid, Att: noSubst, Mtds: < \".init\" : Method | Param: noVid, Att: @ \".method\" |-> \"str\"[\".init\"], @ \".staticfuture\" |-> null, Code: noStmt >, Ocnt: 0 > . ";
    
    private static String FINAL_COMMENT = "--- Start the model with 'rew start .' op start : -> State . eq start = main(classes, \".Start\", emp) . endm";


    @Test
    public void classDecl() {
        assertEqualMaude("class C {} {}", "");
    }

    @Test
    public void interfaceDecl() {
        assertEqualMaude("interface I {} {}", "");    
    }
    
    @Test
    public void emptyMainBlock() {
        assertEqualMaude("{}", "");
    }

    @Test
    public void oneVarDeclMainBlock() {
        assertEqualMaude("interface I {} { I i; }", "");
    }

    @Test
    public void oneVarDeclInitBlock() {
        assertEqualMaude("interface I {} class C { { I i; } } {}", "");
    }
    
    void assertEqualMaude(String absCode, String maudeCode) {
        try {
            StringBuffer expectedMaudeCode = new StringBuffer();
            expectedMaudeCode.append(STANDARD_HEADER);
            expectedMaudeCode.append(maudeCode);
            expectedMaudeCode.append(FINAL_COMMENT);
            
            String generatedMaudeCode = getMaudeCode(absCode);
            if (!expectedMaudeCode.toString().equals(generatedMaudeCode))
                System.out.println(generatedMaudeCode);
                
            Assert.assertEquals(expectedMaudeCode.toString(), generatedMaudeCode);
           
            
        } catch (Exception e) {
            e.printStackTrace();
            Assert.fail(e.getMessage());
        }
    }
    
    protected String getMaudeCode(String absCode) {
        try {
            InputStream in = getInputStream(absCode);
            Model model = null;
            try {
                model = Main.parse(in);
            } catch (Exception e) {
                Assert.fail(e.getMessage());
                return null;
            }

            if (model.hasErrors()) {
                Assert.fail(model.getErrors().getFirst().getMsgString());
                return null;
            }
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            model.generateMaude(new PrintStream(out));
            String res = out.toString();
            res = res.replace('\n', ' ');
            res = res.replaceAll("[ ]+", " ");
            res = res.trim();
            return res;
        } catch (NumberFormatException e) {
            Assert.fail(e.getMessage());
            return null;
        }
    }

    protected String getMaudeOutput(String maudeCode) throws IOException {
    	StringBuffer result = new StringBuffer();
    	String s = null;
    	String path = System.getenv("PATH");
    	String[] cmd = {"maude", "-no-banner", "-no-ansi-color", "-no-wrap", "-batch"};
    	// FIXME: find path to interpreter relative to running program
    	String[] env = {"PATH", path, "MAUDE_LIB", "/Users/rudi/Source/hats/Tools/ABS/trunk/interpreter/"}; 
    	Process p = Runtime.getRuntime().exec(cmd, env);
        BufferedReader in = new BufferedReader(new InputStreamReader(p.getInputStream()));
        PrintWriter out = new PrintWriter(p.getOutputStream());
    	out.print(maudeCode);
    	while ((s = in.readLine()) != null) {
    		result.append(s);
    	}
    	return result.toString();
    }
}
