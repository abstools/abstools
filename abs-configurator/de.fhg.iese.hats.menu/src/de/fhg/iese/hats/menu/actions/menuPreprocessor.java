package de.fhg.iese.hats.menu.actions;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import abs.frontend.parser.Main;

import javax.swing.*;
import java.io.*;
import java.net.URL;
import java.nio.channels.FileChannel;

/**
 * Our sample action implements workbench action delegate.
 * The action proxy will be created by the workbench and
 * shown in the UI. When the user tries to use the action,
 * this delegate will be created and execution will be 
 * delegated to it.
 * @see IWorkbenchWindowActionDelegate
 */
public class menuPreprocessor implements IWorkbenchWindowActionDelegate {
	private IWorkbenchWindow window;
	JFileChooser chooser = new JFileChooser("C:\\");
	/**
	 * The constructor.
	 */
	public menuPreprocessor() {
	}

	/**
	 * The action has been activated. The argument of the
	 * method represents the 'real' action sitting
	 * in the workbench UI.
	 * @see IWorkbenchWindowActionDelegate#run
	 */
	public void run(IAction action) {
		//MessageDialog.openInformation(window.getShell(),"Plugin","Hello, Eclipse world");
		try
		{	
			System.out.print("button clicked by user");
			
			final JFileChooser fc = new JFileChooser("C:\\");
			fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
			int returnVal = fc.showOpenDialog(fc.getParent());
			
			if (returnVal == JFileChooser.APPROVE_OPTION) {
	            File file = fc.getSelectedFile();
	            //This is where a real application would open the file.
	            System.out.print("Opening: " + file.getAbsolutePath() + ".");
	            System.out.print("Call preprocessor here ... ");
	            Main oMain = new Main();
	            oMain.mainMethod("-preprocess",file.getAbsolutePath());
	        } else {
	        	System.out.print("Open command cancelled by user.");
	        }
			
			// To Copy XML file at the appropriate position for the FutureIDE
			File sourceFile = new File("hats\\FMSource.xml");
			File destFile = new File("C:\\app\\FeatureIDE\\runtime-New_configuration\\Test\\model.xml");
			
			if (!sourceFile.exists()) {
				return;
			}
			if (!destFile.exists()) {
				destFile.createNewFile();
			}
			FileChannel source = null;
			FileChannel destination = null;
			source = new FileInputStream(sourceFile).getChannel();
			destination = new FileOutputStream(destFile).getChannel();
			if (destination != null && source != null) {
				destination.transferFrom(source, 0, source.size());
			}
			if (source != null) {
				source.close();
			}
			if (destination != null) {
				destination.close();
			}			
		}
		catch(IOException i)
		{
			i.printStackTrace();
		}
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