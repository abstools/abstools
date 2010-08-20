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
    	assertTrueMaude("{ Bool testresult = True; }");
    }
    
    @Test
    public void listNth() {
    	assertTrueMaude("{ List<Int> list = list[1, 2, 3]; Bool testresult = nth(list, 2) == 3; }");
    }
    
    @Test
    public void setContains1() {
    	assertTrueMaude("{ Set<Int> s = set[1, 2, 3]; Bool testresult = contains(s, 3); }");
    }
    
    @Test
    public void setContains2() {
    	assertFalseMaude("{ Set<Int> s = set[1, 2, 3]; Bool testresult = contains(s, 4); }");
    }
    
    @Test
    public void setRemove() {
    	assertFalseMaude("{ Set<Int> set = set[1, 2, 3]; Bool testresult = contains(remove(set, 3), 3); }");
    }
    
    @Test
    public void mapLookup() {
    	assertTrueMaude("{ Map<Int, Int> map = map[Pair(1, 100), Pair(2, 200), Pair(3, 300)]; Bool testresult = lookup(map, 3) == 300; }");
    }
    
    @Test
    public void mapLookupDefault1() {
    	assertTrueMaude("{ Map<Int, Int> map = map[Pair(1, 100), Pair(2, 200), Pair(3, 300)]; Bool testresult = lookupDefault(map, 3, -1) == 300; }");
    }

    @Test
    public void mapLookupDefault2() {
    	assertTrueMaude("{ Map<Int, Int> map = map[Pair(1, 100), Pair(2, 200), Pair(3, 300)]; Bool testresult = lookupDefault(map, 5, -1) == -1; }");
    }

    @Test
    public void mapPut1() {
    	assertTrueMaude("{ Map<Int, Int> map = map[Pair(1, 100), Pair(2, 200), Pair(3, 300)]; Bool testresult = put(map, 2, -1) == map[Pair(1, 100), Pair(2, -1), Pair(3, 300)]; }");
    }
    
    @Test
    public void mapPut2() {
    	assertTrueMaude("{ Map<Int, Int> map = map[Pair(1, 100), Pair(2, 200), Pair(3, 300)]; Bool testresult = put(map, 4, 400) == map[Pair(1, 100), Pair(2, 200), Pair(3, 300), Pair(4, 400)]; }");
    }
    

    
    
    public void assertTrueMaude(String absCode) {
    	assertMaudeResult(absCode, "True");
    }
    
    public void assertFalseMaude(String absCode) {
    	assertMaudeResult(absCode, "False");
    }
    
    void assertMaudeResult(String absCode, String expectedResult) {
    	try {
        	String generatedMaudeCode = getMaudeCode(absCode);
			String maudeOutput = getMaudeOutput(generatedMaudeCode);
			Pattern pattern = Pattern.compile(".*@ \"testresult\" \\|-> \"(\\w+)\"\\[emp\\].*");
			Matcher matcher = pattern.matcher(maudeOutput);
			if (matcher.find()) {
				String boolValue = matcher.group(1);
				Assert.assertEquals(boolValue, expectedResult);
			} else {
			    System.out.println(maudeOutput);
				Assert.fail("Did not find Maude \"testresult\" variable.");
			}
		} catch (Exception e) {
			e.printStackTrace();
			Assert.fail(e.getMessage());
		}     	
    }
    
   
    protected String getMaudeCode(String absCode) {
        try {
            Model model = null;
            try {
                model = Main.parseString(absCode, true);
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
    	
    	ProcessBuilder pb = new ProcessBuilder();

    	File interpreter = new File(new File(System.getProperty("user.dir")).getParentFile(),"interpreter/abs-interpreter.maude");
        String[] cmd = {"maude", "-no-banner", "-no-ansi-color", "-no-wrap", "-batch", interpreter.getAbsolutePath()};
        pb.command(cmd);
    	
    	if (!interpreter.exists()) {
    	    Assert.fail(interpreter.getAbsolutePath()+" does not exist!");
    	}
    	//pb.directory(workingDir);
    	pb.redirectErrorStream(true);
    	Process p = pb.start();

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
