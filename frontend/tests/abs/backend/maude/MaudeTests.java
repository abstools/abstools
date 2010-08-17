package abs.backend.maude;

import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import junit.framework.Assert;

import org.junit.Test;

import abs.ABSTest;
import abs.backend.java.JavaBackendConstants;
import abs.frontend.ast.Model;
import abs.frontend.parser.Main;

public class MaudeTests extends ABSTest {

    @Test
    public void minimalMainBlock() {
    	assertTrueMaude(" data Bool = True | False; { Bool testresult = True; }");
    }

    void assertTrueMaude(String absCode) {
    	try {
        	String generatedMaudeCode = getMaudeCode(absCode);
			String maudeOutput = getMaudeOutput(generatedMaudeCode);
			Pattern pattern = Pattern.compile(".*@ \"testresult\" \\|-> \"(\\w+)\"\\[emp\\].*");
			Matcher matcher = pattern.matcher(maudeOutput);
			if (matcher.find()) {
				String boolValue = matcher.group(1);
				Assert.assertEquals(boolValue, "True");
			} else {
				Assert.fail("Did not find Maude \"testresult\" variable.");
			}
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
            return res;
        } catch (NumberFormatException e) {
            Assert.fail(e.getMessage());
            return null;
        }
    }

    protected String getMaudeOutput(String maudeCode) throws IOException, InterruptedException {
    	StringBuffer result = new StringBuffer();
    	// Assuming `maude' is in $PATH here.
    	String[] cmd = {"maude", "-no-banner", "-no-ansi-color", "-no-wrap", "-batch"};
    	// FIXME: find path to interpreter relative to running program
    	Process p = Runtime.getRuntime().exec(cmd, null, new File("/Users/rudi/Source/hats/Tools/ABS/trunk/interpreter/"));
        BufferedReader in = new BufferedReader(new InputStreamReader(p.getInputStream()));
        PrintWriter out = new PrintWriter(p.getOutputStream());
        while (in.ready()) {
        	result.append(in.readLine());
        }
    	out.println(maudeCode);
    	out.flush();
    	while (in.ready()) {
    		result.append(in.readLine());
    	}
    	out.println("rew start .");
    	out.flush();
    	while (in.ready()) {
    		result.append(in.readLine() + "\n");
    	}
    	out.println("quit");
    	out.flush();
    	p.waitFor();
    	while (in.ready()) {
    		result.append(in.readLine() + "\n");
    	}
    	return result.toString();
    }
}
