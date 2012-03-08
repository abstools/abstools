package eu.hatsproject.absplugin.tests;

import static org.junit.Assert.*;
import static org.eclipse.swtbot.swt.finder.matchers.WidgetOfType.widgetOfType;

import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotPerspective;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotView;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTree;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.ui.navigator.resources.ProjectExplorer;
import org.junit.Test;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleException;

import eu.hatsproject.absplugin.Activator;
import eu.hatsproject.absplugin.navigator.ABSNavigator;
import eu.hatsproject.absplugin.util.Constants;

public class JARTest {

	private static final String DEST_JAR = "leaderelection.jar";
	private static final String PROJECT_NAME = "GUI test";
	public static SWTWorkbenchBot bot;
	public static SWTBotPerspective absPersp;
	public static SWTBotView navigator;
	public static SWTBotTree botTree;
	protected IStatus failed = null;

	@Test
	public void test() throws Exception {
		Bundle bundleM = Platform.getBundle(Constants.PLUGIN_ID);
		if (bundleM.getState() != Bundle.ACTIVE)
			try {
				bundleM.start();
			} catch (BundleException e) {
				throw new InvocationTargetException(e);
			}
		Activator.getDefault().getLog().addLogListener(new ILogListener() {
			@Override public void logging(IStatus status, String plugin) {
				if (Constants.PLUGIN_ID.equals(plugin)) {
					if (status.getSeverity() == IStatus.ERROR)
						failed = status;
				}
			}
		});
		final IProject p = ResourcesPlugin.getWorkspace().getRoot().getProject(PROJECT_NAME);
		new WorkspaceModifyOperation(){

			@Override
			protected void execute(IProgressMonitor monitor)
					throws CoreException, InvocationTargetException,
					InterruptedException {
				p.create(monitor);
				p.open(monitor);
				IProjectDescription description = p.getDescription();
				String[] natures = description.getNatureIds();
				String[] newNatures = new String[natures.length + 1];
				System.arraycopy(natures, 0, newNatures, 0, natures.length);
				newNatures[natures.length] = Constants.NATURE_ID;
				description.setNatureIds(newNatures);
				p.setDescription(description, monitor);
				IFile jarDest = p.getFile(DEST_JAR);
				Bundle bundle = Platform.getBundle("eu.hatsproject.absplugin.tests");
				assert bundle != null;
				try {
					Bundle compilerBundle = Platform.getBundle(Constants.ABSFRONTEND_PLUGIN_ID);
					assert compilerBundle != null;
					InputStream source = FileLocator.openStream(compilerBundle, new Path("tests/abssamples/leaderelection.jar"), false);
					jarDest.create(source, true, monitor);
				} catch (IOException e) {
					throw new InvocationTargetException(e);
				}
			}}.run(new NullProgressMonitor());
			assert p.exists();
			assert p.isOpen();

			bot = new SWTWorkbenchBot();
			absPersp = bot.perspectiveById(Constants.ABSPERSPECTIVE_ID);
			assert(absPersp != null);
			absPersp.activate();

			navigator = bot.viewById("org.eclipse.ui.navigator.ProjectExplorer");
			assertNotNull(navigator);

			botTree = new SWTBotTree((Tree) bot.widget(widgetOfType(Tree.class), navigator.getWidget()));
			assertNotNull(botTree.widget);
			
//			ProjectExplorer peView = (ProjectExplorer) PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().showView("org.eclipse.ui.navigator.ProjectExplorer");
//			ABSNavigator absView = (ABSNavigator) PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().showView("eu.hatsproject.absplugin.navigator.view");
//			IAdaptable i = peView.getSite().getPage().getInput();
//			peView.getCommonViewer().expandAll();

			final SWTBotTreeItem le = botTree.expandNode(PROJECT_NAME,DEST_JAR,"LeaderElection.abs","LeaderElection");
			assertMsg(failed);
			le.select();
			le.doubleClick(); // Assertion
			bot.sleep(2000);
			assertEquals(1, bot.editors().size());
			assertNotNull(bot.editorByTitle("LeaderElection.abs"));
			assertMsg(failed);
	}

	private void assertMsg(IStatus failed) {
		assertNull(failed != null ? failed.getMessage() : "",failed);
	}

}
