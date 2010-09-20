package abs.frontend.typesystem;

import org.junit.Test;

import abs.frontend.FrontendTest;

public class ModuleSystemTests extends FrontendTest {
	
	@Test
	public void simpleModule() {
		assertNoTypeErrors("module A;");
	}

	@Test
	public void simpleModule2() {
		assertNoTypeErrors("module A; module B;");
	}
	
	@Test
	public void simpleModule3() {
		assertNoTypeErrors("module A; data X; module B;");
	}

	@Test
	public void exportedImport() {
		assertNoTypeErrors("module A; export X; data X; module B; import A.X; type Y = A.X;");
	}
	
	
	@Test
	public void notExportedImport() {
		assertTypeErrors("module A; data X; module B; import A.X; ");
	}
	
	@Test
	public void notExportedImport2() {
		assertTypeErrors("module A; data X; module B; import A.X; type Y = X;");
	}
	
	protected void assertNoTypeErrors(String absCode) {
     	 assertTypeErrors(absCode, false, false,false);
      }
	
	protected void assertTypeErrors(String absCode) {
    	 assertTypeErrors(absCode, true, false,false);
     }

}
