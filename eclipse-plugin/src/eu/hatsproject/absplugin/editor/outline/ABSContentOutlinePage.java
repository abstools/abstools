/**
 * Package for managing the editor content outline
 */
package eu.hatsproject.absplugin.editor.outline;

import static eu.hatsproject.absplugin.editor.outline.ABSContentOutlineConstants.FILTER_COMMANDS;
import static eu.hatsproject.absplugin.editor.outline.ABSContentOutlineConstants.SORT_COMMAND_ID;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.commands.State;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.handlers.RegistryToggleState;
import org.eclipse.ui.services.IServiceScopes;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.views.contentoutline.ContentOutlinePage;

import abs.frontend.ast.CompilationUnit;
import eu.hatsproject.absplugin.builder.AbsNature;
import eu.hatsproject.absplugin.util.CoreControlUnit;
import eu.hatsproject.absplugin.util.CoreControlUnit.ResourceBuildListener;
import eu.hatsproject.absplugin.util.CoreControlUnit.ResourceBuiltEvent;
import eu.hatsproject.absplugin.util.InternalASTNode;
import eu.hatsproject.absplugin.util.UtilityFunctions;

/**
 * Implements the Content Outline for ABS files
 * @author cseise
 *
 */
public class ABSContentOutlinePage extends ContentOutlinePage {

	/**
	 * The text editor associated with this content outline page instance
	 */
	private ITextEditor editor = null;
	
	ResourceBuildListener builtListener = null;
	/**
	 * The ITreeContentProvider delivers the elements that should be shown in the outline
	 */
	private ITreeContentProvider coProv = null;
	/**
	 * Internal Tree viewer of the Content Outline Page
	 */
	private TreeViewer tw = null;
	/**
	 * ICommandService for retrieving the states of the filter buttons
	 */
	private ICommandService commandService;

	public ABSContentOutlinePage(IDocumentProvider docProvider,	ITextEditor editor) {
		this.editor = editor;
		coProv = new ABSContentOutlineProvider();
		// When the project is built this listener is responsible for updating the input
		builtListener = new ABSContentOutlineChangeListener();
	}
	
	/**
	 * @see org.eclipse.ui.views.contentoutline.ContentOutlinePage#createControl(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	public void createControl(Composite parent) {
		super.createControl(parent);
		initTreeViewer();
		addSelectionListener();
		setInput();
		restoreFilters();
		CoreControlUnit.addResourceBuildListener(builtListener);
	}
	
	//suppress Warnings is used here because commandService.refreshElements needs a Map without generics...
	@SuppressWarnings("unchecked")
	private void restoreFilters() {

		commandService = (ICommandService) PlatformUI.getWorkbench().getService(ICommandService.class);
		boolean value = getValueOfCommand(SORT_COMMAND_ID);

		if (tw != null) {
			if (value) {
				tw.setComparator(ABSContentOutlineFiltersAndComparators.getAlphabeticalASTNodeComparator());
			} else {
				tw.setComparator(null);
			}

			List<ViewerFilter> filters = Arrays.asList(tw.getFilters());

			for (String Command : FILTER_COMMANDS) {
				value = getValueOfCommand(Command);

				ViewerFilter filterFromCommand = ABSContentOutlineFiltersAndComparators.getFilterOfCommand(Command);

				if (value) {
					if (!filters.contains(filterFromCommand)) {
						tw.addFilter(filterFromCommand);
					}
				} else {
					tw.removeFilter(filterFromCommand);
				}
			}

			//Trigger update of visible button state (pressed/not pressed)
			@SuppressWarnings("rawtypes")
			Map filter = new HashMap();
			filter.put(IServiceScopes.WINDOW_SCOPE, editor.getSite().getPage().getWorkbenchWindow());
			commandService.refreshElements(SORT_COMMAND_ID, filter);
		}

	}
	
	private boolean getValueOfCommand(String commandID){
		State state = commandService.getCommand(commandID).getState(RegistryToggleState.STATE_ID);
		boolean value = ((Boolean)state.getValue()).booleanValue();
		return value;
	}

	private void initTreeViewer() {
		tw = getTreeViewer();
		tw.setContentProvider(coProv);
		// The label provider is responsible for converting ASTNodes into their String representations
		tw.setLabelProvider(new ABSContentOutlineStyledLabelProvider());
		// TODO: Making tree expansion more "intelligent" than only expanding all tree elements
		tw.setAutoExpandLevel(TreeViewer.ALL_LEVELS);
	}

	private void setInput() {
		IFile file = (IFile)editor.getEditorInput().getAdapter(IFile.class);
		// Tries to get the ABS ProjectNature in order to get the AST
		AbsNature nature = UtilityFunctions.getAbsNature(file);
		if(nature == null){
			return;
		}

		CompilationUnit cu = nature.getCompilationUnit(file);

		// Update the input of the tree viewer to reflect the new outline of the AST
		tw.setInput(new InternalASTNode<CompilationUnit>(cu,nature));
	}

	private void addSelectionListener() {
		// Handle selection changes in the outline
		tw.addSelectionChangedListener(new ISelectionChangedListener() {
			
			@Override
			public void selectionChanged(SelectionChangedEvent event) {
				
				ISelection sel = event.getSelection();
				if (sel.isEmpty()) {
					editor.resetHighlightRange();
				} else {
					// Get only the first element of the selection
					InternalASTNode<?> t = ((InternalASTNode<?>) ((IStructuredSelection) sel)
							.getFirstElement());
					UtilityFunctions.highlightInEditor(editor, t);
				}
				
			}
		});
	}
	
	
	/**
	 * Returns the internal TreeViewer component of the Content Outline Page.
	 * (Useful for setting filters, sorters or other TreeViewer properties)
	 * @return The TreeViewer component associated with this Content Outline Page.
	 */
	public TreeViewer getTreeView(){
		return getTreeViewer();
	}	
	
	@Override
	public void dispose() {
		CoreControlUnit.removeResourceBuildListener(builtListener);
	}
	
	class ABSContentOutlineChangeListener implements ResourceBuildListener {
		/**
		 * {@inheritDoc}
		 * 
		 * @throws SWTException
		 *             - ERROR_DEVICE_DISPOSED if the default {@link Display}
		 *             device has been disposed
		 */
		@Override
		public void resourceBuilt(ResourceBuiltEvent builtevent) {
			final IFile eres = (IFile) editor.getEditorInput()
			.getAdapter(IFile.class);
			if (builtevent.hasChanged(eres)) {
				refreshInput(eres);
			}
		}

		private void refreshInput(final IFile eres) {
		    Display.getDefault().asyncExec(new Runnable() {
		    	@Override
				public void run() {
		    		AbsNature nature = UtilityFunctions.getAbsNature(eres.getProject());
		    		if(nature == null){
		    			return;
		    		}
		    		CompilationUnit cu = nature.getCompilationUnit(eres);
		    		if (cu != null) {
		    			ViewerFilter[] vf = getTreeViewer().getFilters();
		    			ViewerComparator sort = getTreeViewer().getComparator();
		    			getTreeViewer().setInput(new InternalASTNode<CompilationUnit>(cu, nature));
		    			getTreeViewer().setFilters(vf);
		    			getTreeViewer().setComparator(sort);
		    			editor.getSelectionProvider().setSelection(editor.getSelectionProvider().getSelection());
		    		}
		    	}
		    });
		}
	}
}
