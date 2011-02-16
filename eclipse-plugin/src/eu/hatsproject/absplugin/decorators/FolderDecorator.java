package eu.hatsproject.absplugin.decorators;

import static eu.hatsproject.absplugin.util.Images.FOLDER_MARKER;
import static eu.hatsproject.absplugin.util.Constants.MODULE_DECORATOR_ID;

import java.util.ArrayList;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.DecorationOverlayIcon;
import org.eclipse.jface.viewers.IDecoration;
import org.eclipse.jface.viewers.ILightweightLabelDecorator;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.LabelProviderChangedEvent;
import org.eclipse.ui.PlatformUI;

import eu.hatsproject.absplugin.util.Constants;

/**
 * This Decorator is used for decorating folders with the ABS project icon. Only
 * those folders containing ABS files or contain folders with ABS files are
 * decorated
 * 
 * @author cseise
 * 
 */
public class FolderDecorator extends LabelProvider implements ILightweightLabelDecorator {

	private final static ImageDescriptor MARKER_DESCRIPTOR = DecorationOverlayIcon.createFromImage(FOLDER_MARKER);
	
	
	@Override
	public void decorate(Object element, IDecoration decoration) {
		if (element instanceof IFolder){
			if (hasABSFiles((IFolder)element)){
				decoration.addOverlay(MARKER_DESCRIPTOR);
			}
		}/*else if (element instanceof IFile){
			if (checkFile((IResource)element)){
				decoration.addOverlay(MARKER_DESCRIPTOR);				
			}
		}*/
		
	}
	
	private boolean hasABSFiles(IFolder folder) {
		if (folder.isAccessible()) {
			try {
				IResource[] members = folder.members();
				for (IResource member : getNonFolders(members)) {
					if(checkFile(member)){
						return true;
					}
				}

				for (IResource member : getFolders(members)) {

					if (hasABSFiles((IFolder) member)) {
						return true;
					}
				}
			} catch (CoreException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
				return false;
			}
		}

		return false;
	}

	private boolean checkFile(IResource member) {
		if (member instanceof IFile && member.getName().endsWith("." + Constants.ABS_FILE_EXTENSION)) {
			return true;
		}
		return false;
	}

	private static ArrayList<IFolder> getFolders(IResource[] members) {
		ArrayList<IFolder> folders = new ArrayList<IFolder>();
		
		for (IResource member : members){
			if (member instanceof IFolder){
				folders.add((IFolder)member);
			}
		}
		
		return folders;
	}
	
	private static ArrayList<IResource> getNonFolders(IResource[] members) {
		ArrayList<IResource> res = new ArrayList<IResource>();
		
		for (IResource member : members){
			if (!(member instanceof IFolder)){
				res.add((IResource)member);
			}
		}
		
		return res;
	}	

	/**
	 * Fires a LabelProviderChangeEvent to force re-decoration.
	 * This is a workaround for circumventing decorator update problems.
	 */
	public void refresh(){
		boolean isEnabled = PlatformUI.getWorkbench().getDecoratorManager().getEnabled(MODULE_DECORATOR_ID);
		
		if (isEnabled){
			this.fireLabelProviderChanged(new LabelProviderChangedEvent(this));
		}
	}	
}
