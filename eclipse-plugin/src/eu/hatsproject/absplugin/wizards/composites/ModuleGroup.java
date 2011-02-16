package eu.hatsproject.absplugin.wizards.composites;

import java.util.ArrayList;
import java.util.List;

import static eu.hatsproject.absplugin.util.Constants.EMPTY_OBJECT_ARRAY;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.ui.part.DrillDownComposite;

import abs.frontend.ast.ModuleDecl;
import eu.hatsproject.absplugin.util.InternalASTNode;
import eu.hatsproject.absplugin.wizards.WizardUtil;

public class ModuleGroup extends CompositeGroup {

	/**
	 * Last selection made by user
	 */
	private InternalASTNode<ModuleDecl> selectedModuleDecl;

	// sizing constants
	private static final int SIZING_SELECTION_PANE_WIDTH = 320;

	private static final int SIZING_SELECTION_PANE_HEIGHT = 300;

	/**
	 * Creates a new instance of the widget.
	 * 
	 * @param parent
	 *            The parent widget of the group.
	 * @param listener
	 *            A listener to forward events to. Can be null if no listener is
	 *            required.
	 */
	public ModuleGroup(Composite parent, Listener listener) {
		this(parent, listener, null);
	}

	/**
	 * Creates a new instance of the widget.
	 * 
	 * @param parent
	 *            The parent widget of the group.
	 * @param listener
	 *            A listener to forward events to. Can be null if no listener is
	 *            required.
	 * @param message
	 *            The text to present to the user.
	 */
	public ModuleGroup(Composite parent, Listener listener,
			String message) {
		this(parent, listener, message,
				SIZING_SELECTION_PANE_HEIGHT,
				SIZING_SELECTION_PANE_WIDTH);
	}

	/**
	 * Creates a new instance of the widget.
	 * 
	 * @param parent
	 *            The parent widget of the group.
	 * @param listener
	 *            A listener to forward events to. Can be null if no listener is
	 *            required.
	 * @param message
	 *            The text to present to the user.
	 * @param heightHint
	 *            height hint for the drill down composite
	 * @param widthHint
	 *            width hint for the drill down composite
	 */
	public ModuleGroup(Composite parent, Listener listener,
			String message, int heightHint, int widthHint) {
		super(parent, listener, message, heightHint, widthHint);
	}

	/**
	 * The resource selection has changed in the tree view. Update the
	 * container name field value and notify all listeners.
	 * 
	 * @param o
	 *            The object that changed
	 */
	@Override
	@SuppressWarnings("unchecked")
	protected void resourceSelectionChanged(Object o) {		
		if (o instanceof InternalASTNode<?>){
			InternalASTNode<?> node = (InternalASTNode<?>)o;
			if (InternalASTNode.hasASTNodeOfType(node, ModuleDecl.class)){
				selectedModuleDecl = (InternalASTNode<ModuleDecl>)o;
			}
			fireSelectionChangedEvent();
		}
	}

	/**
	 * Creates the contents of the composite.
	 * 
	 * @param message
	 * @param heightHint
	 * @param widthHint
	 */
	@Override
	protected void createContents(String message, int heightHint, int widthHint) {
		GridLayout layout = new GridLayout();
		layout.marginWidth = 0;
		setLayout(layout);
		setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		Label label = new Label(this, SWT.WRAP);
		label.setText(message);
		label.setFont(this.getFont());

		createTreeViewer(heightHint);
		Dialog.applyDialogFont(this);
	}

	/**
	 * Returns a new drill down viewer for this dialog.
	 * 
	 * @param heightHint
	 *            height hint for the drill down composite
	 */
	@Override
	protected void createTreeViewer(int heightHint) {
		// Create drill down.
		DrillDownComposite drillDown = createDrillDown(heightHint);

		// Create tree viewer inside drill down.
		treeViewer = new TreeViewer(drillDown, SWT.NONE);
		drillDown.setChildTree(treeViewer);
		ModuleGroupContentProvider cp = new ModuleGroupContentProvider();
		//cp.showClosedProjects(showClosedProjects);
		treeViewer.setContentProvider(cp);
		treeViewer.setLabelProvider(new ABSWizardStyledLabelProvider());
		setUpTreeViewer();
	}

	public InternalASTNode<ModuleDecl> getSelectedModuleDecl(){
		return (selectedModuleDecl instanceof InternalASTNode) ? selectedModuleDecl : null;
	}

	/**
	 * Sets the current selection to container
	 * 
	 * @param container
	 *            the wrapped module declaration that should be selected in this
	 *            ModuleGroup composite.
	 */
	public void setSelectedResource(InternalASTNode<ModuleDecl> container) {
		selectedModuleDecl = container;

		if (container != null) {
			// expand to and select the specified container
			List<Object> itemsToExpand = new ArrayList<Object>();
			itemsToExpand.add(0, container);

			IProject proj = WizardUtil.getProjectOfModuleDecl(container);

			itemsToExpand.add(1, proj);
			treeViewer.setExpandedElements(itemsToExpand.toArray());
			treeViewer.setSelection(new StructuredSelection(container), true);
		} else {
			treeViewer.setExpandedElements(EMPTY_OBJECT_ARRAY);
			treeViewer.setSelection(null, false);
		}
	}
}
