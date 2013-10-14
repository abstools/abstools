package costabs.listeners;

import org.eclipse.core.resources.IFile;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IPartListener2;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.internal.EditorReference;
import org.eclipse.ui.part.FileEditorInput;

import costabs.structures.CostabsSVGGraph;
import costabs.trackers.OutputManager;

public class PartListener implements IPartListener2 {
	@Override
	public void partActivated(IWorkbenchPartReference partRef) {
		if (partRef instanceof EditorReference) {
			
			EditorReference editor = (EditorReference)partRef;
			IEditorInput input = editor.getPage().getActiveEditor().getEditorInput();
			IFile file = ((FileEditorInput)input).getFile();
			OutputManager.getInstance().updateView(file);
		}
	}
	@Override
	public void partBroughtToTop(IWorkbenchPartReference partRef) {
//		if (partRef instanceof EditorReference) {
//			EditorReference editor = (EditorReference)partRef;
//			IEditorInput input = editor.getPage().getActiveEditor().getEditorInput();
//			IFile file = ((FileEditorInput)input).getFile();
//			OutputManager.getInstance().updateView(file);
//		}
	}

	@Override
	public void partClosed(IWorkbenchPartReference partRef) {}

	@Override
	public void partDeactivated(IWorkbenchPartReference partRef) {
	}

	@Override
	public void partOpened(IWorkbenchPartReference partRef) {
		if (partRef instanceof EditorReference) {
			EditorReference editor = (EditorReference)partRef;
			IEditorInput input = editor.getPage().getActiveEditor().getEditorInput();
			IFile file = ((FileEditorInput)input).getFile();
			OutputManager.getInstance().updateView(file);
		}
	}
	
	@Override
	public void partHidden(IWorkbenchPartReference partRef) {
	}
	@Override

	public void partVisible(IWorkbenchPartReference partRef) {
	}
	@Override

	public void partInputChanged(IWorkbenchPartReference partRef) {
	}
}
