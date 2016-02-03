package sdaplugin.actions;

import java.io.PrintStream;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsoleManager;
import org.eclipse.ui.console.MessageConsole;
import org.eclipse.ui.console.MessageConsoleStream;
import org.eclipse.core.resources.IProject;

import sdaplugin.Activator;

import org.absmodels.abs.plugin.actions.ActionUtils;
import org.absmodels.abs.plugin.builder.AbsNature;

import abs.frontend.ast.Model;

/**
 * Our sample action implements workbench action delegate.
 * The action proxy will be created by the workbench and
 * shown in the UI. When the user tries to use the action,
 * this delegate will be created and execution will be 
 * delegated to it.
 * @see IWorkbenchWindowActionDelegate
 */
public class SDAction implements IWorkbenchWindowActionDelegate {
  private IWorkbenchWindow window;
  
 private int unuse;
  
  /**
   * The constructor.
   */
  public SDAction() { }

  /**
   * The action has been activated. The argument of the
   * method represents the 'real' action sitting
   * in the workbench UI.
   * @see IWorkbenchWindowActionDelegate#run
   */
  public void run(IAction action) {
    /* 1. Get the project */
    IProject project = getCurrentProject();
    if(project == null) return;
    /* 2. Get the Console */
    MessageConsoleStream outConsole = findConsole().newMessageStream();
    outConsole.setActivateOnWrite(true);
    PrintStream out = new PrintStream(outConsole);
    /* 3. Get the model in a good shape (typed. For now, there is nothing we can do about deltas) */
    AbsNature nature =  org.absmodels.abs.plugin.util.UtilityFunctions.getAbsNature(project);
    Model model = nature.getCompleteModel();
    model.typeCheck();
    /* 4. Perform the analysis */
    SDARun run = new SDARun(model, false, 3, 2, out);
    run.schedule();
  }

  private IProject getCurrentProject() {
	  try {
		return ActionUtils.getCurrentProject(window, window.getActivePage().getActiveEditor());
	} catch (PartInitException e) {
		Activator.logException(e);
		return null;
	}
  }

	private static final String ID_CONSOLE_VIEW = "SDA Console";

	private MessageConsole findConsole() {
	      ConsolePlugin plugin = ConsolePlugin.getDefault();
	      IConsoleManager conMan = plugin.getConsoleManager();
	      IConsole[] existing = conMan.getConsoles();
	      for (int i = 0; i < existing.length; i++) {
	    	 //System.out.println("Console \"" + existing[i].getName() + "\" found");
	         if (ID_CONSOLE_VIEW.equals(existing[i].getName()))
	            return (MessageConsole) existing[i];
	      }
	      //no console found, so create a new one
	      MessageConsole myConsole = new MessageConsole(ID_CONSOLE_VIEW, null);
	      conMan.addConsoles(new IConsole[]{myConsole});
	      return myConsole;
	   }
  
  
	
	/**
	 * Selection in the workbench has been changed. We 
	 * can change the state of the 'real' action here
	 * if we want, but this can only happen after 
	 * the delegate has been created.
	 * @see IWorkbenchWindowActionDelegate#selectionChanged
	 */
	public void selectionChanged(IAction action, ISelection selection) {
	}

	/**
	 * We can use this method to dispose of any system
	 * resources we previously allocated.
	 * @see IWorkbenchWindowActionDelegate#dispose
	 */
	public void dispose() {
	}

	/**
	 * We will cache window object in order to
	 * be able to provide parent shell for the message dialog.
	 * @see IWorkbenchWindowActionDelegate#init
	 */
	public void init(IWorkbenchWindow window) {
		this.window = window;
	}
}