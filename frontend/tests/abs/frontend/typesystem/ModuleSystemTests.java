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
	public void starImport() {
		assertNoTypeErrors("module A; export X; data X; module B; import * from A; type Y = X;");
	}

	@Test
	public void exportedImport() {
		assertNoTypeErrors("module A; export X; data X; module B; import A.X; type Y = A.X;");
	}

	@Test
	public void starExportedImport() {
		assertNoTypeErrors("module A; export X; data X; module B; export * from A; import A.X; module C; import * from B; type Y = B.X;");
	}

	@Test
	public void fromExportedImport() {
		assertNoTypeErrors("module A; export X; data X; module B; export X from A; import A.X; module C; import * from B; type Y = B.X;");
	}
	
	@Test
	public void exportQualified() {
		assertNoTypeErrors("module A; export A.X; data X; module B; import X from A; type Y = X;");
	}
	
	@Test
	public void fromImport() {
		assertNoTypeErrors("module A; export X; data X; module B; import X from A; type Y = X;");
	}

	@Test
	public void fromImportQualifiedUse() {
		assertNoTypeErrors("module A; export X; data X; module B; import X from A; type Y = A.X;");
	}
	
	@Test
	public void unqualifiedImport() {
		assertTypeErrors("module A; data X; module B; import X; ");
	}

	@Test
	public void importFromNotExistingModule() {
		assertTypeErrors("module B; import A.X; ");
	}
	
	@Test
	public void notExportedImport() {
		assertTypeErrors("module A; data X; module B; import A.X; ");
	}
	
	@Test
	public void notExportedImport2() {
		assertTypeErrors("module A; data X; module B; import A.X; type Y = X;");
	}

	@Test
	public void notExportedImportFrom() {
		assertTypeErrors("module A; data X; module B; import X from A; ");
	}

	@Test
	public void notExportedImportFrom2() {
		assertTypeErrors("module A; export X; data X; data Y; module B; import X, Y from A; ");
	}
	
	@Test
	public void starImportNotExistingModule() {
		assertTypeErrors("module B; import * from A; ");
	}

	@Test
	public void qualifiedFromImport() {
		assertTypeErrors("module A; export X; data X; module B; import A.X from A; ");
	}

	@Test
	public void invisibleExport() {
		assertTypeErrors("module A; export X; ");
	}
	
	protected void assertNoTypeErrors(String absCode) {
     	 assertTypeErrors(absCode, false, false,false);
      }
	
	protected void assertTypeErrors(String absCode) {
    	 assertTypeErrors(absCode, true, false,false);
     }

}
