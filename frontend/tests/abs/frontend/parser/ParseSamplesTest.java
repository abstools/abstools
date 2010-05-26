//$Id$ 

package abs.frontend.parser;

import static org.junit.Assert.fail;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;

import org.junit.Test;

import abs.frontend.ast.Model;


public class ParseSamplesTest {

	@Test
	public void testSamples() {
		String dir="tests/abssamples/"; 	
		String[] files = {"PeerToPeer.abs", "pingpong.abs", "boundedbuffer.abs"};
		for (int i = 0 ; i < files.length; i++){
			System.out.println("parsing " + files[i]);
			assertParseOk(dir + files[i]);
		}	
	}
	
	protected static void assertParseOk(String s) {
		Model m = null;
		try {
			m = Main.parse(s);
		}	catch (Throwable e) {
			fail("Failed to parse: "+ s+"\n"+e.getMessage());
		}
		if (m != null) {
			int numSemErrs = m.errors().size();
			if (numSemErrs > 0){
				StringBuffer errs = new StringBuffer("Semantic errors: " + numSemErrs + "\n");
				for (Object error : m.errors())
					errs = errs.append(s + ":" + error + "\n");	
				fail(errs.toString());
			}		
		}
	}
}
		

  
