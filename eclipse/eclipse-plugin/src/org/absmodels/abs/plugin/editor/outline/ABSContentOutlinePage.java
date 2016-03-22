/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.editor.outline;

import static org.absmodels.abs.plugin.editor.outline.ABSContentOutlineConstants.FILTER_COMMANDS;
import static org.absmodels.abs.plugin.editor.outline.ABSContentOutlineConstants.SORT_COMMAND_ID;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.absmodels.abs.plugin.editor.ABSEditor;
import org.absmodels.abs.plugin.editor.reconciling.CompilationUnitChangeListener;
import org.absmodels.abs.plugin.util.InternalASTNode;
import org.eclipse.core.commands.State;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.handlers.RegistryToggleState;
import org.eclipse.ui.services.IServiceScopes;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.views.contentoutline.ContentOutlinePage;

import abs.frontend.ast.CompilationUnit;

/**
 * Implements the Content Outline for ABS files
 * @author cseise
 *
 */
public class ABSContentOutlinePage extends ContentOutlinePage {

	/**
	 * The text editor associated with this content outline page instance
	 */
	private final ABSEditor editor;
	
	private final CompilationUnitChangeListener modelChangeListener;
	/**
	 * The ITreeContentProvider delivers the elements that should be shown in the outline
	 */
	private final ITreeContentProvider coProv;
	/**
	 * ICommandService for retrieving the states of the filter buttons
	 */
	private final ICommandService commandService;

	/** flag indicating whether a selection should move 
	 * cursor inside the editor
	 */
	private boolean selectionMovesCursor = true;
	
	public ABSContentOutlinePage(ABSEditor editor) {
		this.editor = editor;
		commandService = (ICommandService) PlatformUI.getWorkbench().getService(ICommandService.class);
		assert commandService != null;
		coProv = new ABSContentOutlineProvider();
		// When the project is built this listener is responsible for updating the input
		modelChangeListener = new ABSContentOutlineChangeListener();
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
		editor.addModelChangeListener(modelChangeListener);
	}
	
	private void restoreFilters() {
		boolean value = getValueOfCommand(SORT_COMMAND_ID);
		TreeViewer tw = getTreeViewer();
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
			Map<String, IWorkbenchWindow> filter = new HashMap<String, IWorkbenchWindow>();
			filter.put(IServiceScopes.WINDOW_SCOPE, editor.getSite().getPage().getWorkbenchWindow());
			commandService.refreshElements(SORT_COMMAND_ID, filter);
		}
	}
	
	private boolean getValueOfCommand(String commandID){
		assert commandService != null;
		State state = commandService.getCommand(commandID).getState(RegistryToggleState.STATE_ID);
		boolean value = ((Boolean)state.getValue()).booleanValue();
		return value;
	}

	private void initTreeViewer() {
		TreeViewer tw = getTreeViewer();
		tw.setContentProvider(coProv);
		// The label provider is responsible for converting ASTNodes into their String representations
		tw.setLabelProvider(new ABSContentOutlineStyledLabelProvider());
		// TODO: Making tree expansion more "intelligent" than only expanding all tree elements
		tw.setAutoExpandLevel(TreeViewer.ALL_LEVELS);
	}

	private void setInput() {
		InternalASTNode<CompilationUnit> cu = editor.getCompilationUnit(); 
		
		// Update the input of the tree viewer to reflect the new outline of the AST
		getTreeViewer().setInput(cu);
	}

	/**
	 * Two selection listeners:
	 * - the first propagates a click in the outline to the editor.
	 * - the second updates the position in the outline based on the editor-position.
	 * In principle we could add the first listener in ABSEditor.getAdapter(), but
	 * that wouldn't gain us much in terms of visibility.
	 */
	private void addSelectionListener() {
		// Handle selection changes in the outline
		addSelectionChangedListener(new ISelectionChangedListener() {
			
			@Override
			public void selectionChanged(SelectionChangedEvent event) {
				
				ISelection sel = event.getSelection();
				if (sel.isEmpty()) {
					editor.resetHighlightRange();
				} else {
					
					ABSContentOutlineUtils.insertCostabsItems(sel);
					
					// Get only the first element of the selection
					InternalASTNode<?> t = ((InternalASTNode<?>) ((IStructuredSelection) sel)
							.getFirstElement());
					editor.highlightInEditor(t, selectionMovesCursor);
				}
			}
		});

		// Select the current element under cursor in the outlineView:
		editor.getSelectionProvider().addPostSelectionChangedListener(new ISelectionChangedListener() {
			@Override
			public void selectionChanged(SelectionChangedEvent event) {
				ISelection selection = event.getSelection();
				if (selection instanceof ITextSelection) {
				    // select the current element in the outlineView
					ITextSelection ts = (ITextSelection) selection;
					selectNodeByPos(ts.getStartLine());
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
		editor.removeModelChangeListener(modelChangeListener);
		super.dispose();
	}
	
	/**
	 * updates the outline whenever the compilationUnit of the editor changes
	 */
	class ABSContentOutlineChangeListener implements CompilationUnitChangeListener {
		
		@Override
		public void onCompilationUnitChange(final CompilationUnit newCu) {
			Display.getDefault().asyncExec(new Runnable() {
				@Override
				public void run() {
					ViewerFilter[] vf = getTreeViewer().getFilters();
					ViewerComparator sort = getTreeViewer().getComparator();
					InternalASTNode<CompilationUnit> cu = editor.getCompilationUnit();
					getTreeViewer().setInput(cu);
					getTreeViewer().setFilters(vf);
					getTreeViewer().setComparator(sort);
					editor.getSelectionProvider().setSelection(editor.getSelectionProvider().getSelection());
				}
			});
		}
	}

	/**
	 * selects a node in the outline without moving the cursor
	 * in the editor
	 */
	private void setSelectionWithoutCursorMove(ISelection sel) {
		selectionMovesCursor = false;
		setSelection(sel);
		selectionMovesCursor = true;
	}

	/**
	 * Select the closest node to the given line.
	 * Note that the outline might actually not be visible.
	 * startLine < 0 indicates invalid info, e.g. from upstream, will reset selection.
	 */
	private void selectNodeByPos(int startLine) {
		// Do nothing if viewer not created yet (e.g. invisible on startup)
		if (getTreeViewer() == null || !getValueOfCommand(ABSContentOutlineConstants.LINK_EDITOR_COMMAND_ID)) {
	        // linking with editor not enabled ...
	        return;
	    }
	    Object input = getTreeViewer().getInput();
	    if (input instanceof InternalASTNode<?>) {
	        InternalASTNode<?> internalASTNode = (InternalASTNode<?>) input;
	        final ISelection selection;
	        if (startLine > -1) { // valid line info?
	        	InternalASTNode<?> sel = findNodeInLine(internalASTNode, startLine+1);
	        	if (sel == null)
	        		selection = new TreeSelection();
	        	else
	        		selection = new TreeSelection(new TreePath(new Object[] {sel}));
	        } else
	        	selection = new TreeSelection();
	        setSelectionWithoutCursorMove(selection);
	    }
	}

	private InternalASTNode<?> findNodeInLine(InternalASTNode<?> node, int startLine) {
	    final int line = node.getASTNode().getStartLine();
		if (line > startLine) {
			/* If a module starts with comments, the "module" declaration will be the
			 * root node in line n > 1, yet the editor will be placed in startLine=1.
			 */
	        return null;
	    }
	    InternalASTNode<?> result = node;
	    for (Object child : coProv.getChildren(node)) {
	        if (child instanceof InternalASTNode<?>) {
	            InternalASTNode<?> childNode = (InternalASTNode<?>) child;
	            // FIXME: probably shouldn't be recursive.
	            InternalASTNode<?> r = findNodeInLine(childNode, startLine);
	            if (r != null) {
	                result = r;
	            } else {
	                break;
	            }
	        }
	    }
	    return result;
	}
}
