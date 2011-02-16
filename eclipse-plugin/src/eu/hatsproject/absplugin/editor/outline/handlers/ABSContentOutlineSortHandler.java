/**
 * Handlers for the ABS Content Outline Page
 */
package eu.hatsproject.absplugin.editor.outline.handlers;

import static eu.hatsproject.absplugin.editor.outline.ABSContentOutlineFiltersAndComparators.getAlphabeticalASTNodeComparator;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.handlers.HandlerUtil;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;

import eu.hatsproject.absplugin.editor.outline.ABSContentOutlinePage;
import eu.hatsproject.absplugin.util.UtilityFunctions;

/**
 * Handler for sorting an ABSContentOutlinePage
 * @author cseise
 *
 */
public class ABSContentOutlineSortHandler extends AbstractHandler implements IHandler{

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		// Get the active editor out of the current application context
		try {
			IEditorPart editor = HandlerUtil.getActiveEditorChecked(event);

			// Get the content outline page from the editor
			IContentOutlinePage page = (IContentOutlinePage) editor.getAdapter(IContentOutlinePage.class);

			if (page != null && page instanceof ABSContentOutlinePage) {
				ABSContentOutlinePage absOutlinePage = (ABSContentOutlinePage) page;
				boolean oldState = HandlerUtil.toggleCommandState(event.getCommand());
				if (!oldState) {
					absOutlinePage.getTreeView().setComparator(getAlphabeticalASTNodeComparator());
				} else {
					absOutlinePage.getTreeView().setComparator(null);
				}
			}

			return null;
		} catch (ExecutionException ee) {
			Display.getDefault().asyncExec(new Runnable() {
				
				@Override
				public void run() {
					UtilityFunctions.showErrorMessage("Fatal error in content outline: Could not find editor corresponding to the outline.");			
				}
			});
			return null;
		}
	}
}
