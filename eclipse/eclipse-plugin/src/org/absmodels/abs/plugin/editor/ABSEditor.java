/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.editor;

import static org.absmodels.abs.plugin.util.Constants.*;
import static org.absmodels.abs.plugin.util.UtilityFunctions.standardExceptionHandling;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.absmodels.abs.plugin.Activator;
import org.absmodels.abs.plugin.builder.AbsNature;
import org.absmodels.abs.plugin.editor.decoration.ABSDecorationSupport;
import org.absmodels.abs.plugin.editor.outline.ABSContentOutlinePage;
import org.absmodels.abs.plugin.editor.outline.PackageAbsFileEditorInput;
import org.absmodels.abs.plugin.editor.reconciling.ABSReconcilingStrategy;
import org.absmodels.abs.plugin.editor.reconciling.AbsModelManager;
import org.absmodels.abs.plugin.editor.reconciling.CompilationUnitChangeListener;
import org.absmodels.abs.plugin.util.Constants;
import org.absmodels.abs.plugin.util.CoreControlUnit;
import org.absmodels.abs.plugin.util.InternalASTNode;
import org.absmodels.abs.plugin.util.UtilityFunctions;
import org.absmodels.abs.plugin.util.CoreControlUnit.ResourceBuildListener;
import org.absmodels.abs.plugin.util.CoreControlUnit.ResourceBuiltEvent;
import org.absmodels.abs.plugin.util.UtilityFunctions.EditorPosition;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentExtension3;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.text.source.DefaultCharacterPairMatcher;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.text.source.ICharacterPairMatcher;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.SourceViewer;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.viewers.DecorationOverlayIcon;
import org.eclipse.jface.viewers.IDecoration;
import org.eclipse.jface.viewers.IPostSelectionProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IURIEditorInput;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.contexts.IContextService;
import org.eclipse.ui.editors.text.TextEditor;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.SourceViewerDecorationSupport;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;

import abs.frontend.ast.ASTNode;
import abs.frontend.ast.CompilationUnit;

/**
 * The editor for ABS file. Includes syntax highlighting and content assist for ABS files
 * as well as annotations for ABS errors.
 * 
 * @author tfischer, cseise, fstrauss, mweber
 *
 */
public class ABSEditor extends TextEditor implements CompilationUnitChangeListener {
    
	private final class UpdateEditorIcon implements ResourceBuildListener {
		@Override
		public void resourceBuilt(ResourceBuiltEvent builtevent) {
			final IResource editorres = (IResource)ABSEditor.this.getEditorInput().getAdapter(IResource.class);
			if(builtevent.hasChanged(editorres)){
				
				Display.getDefault().asyncExec(new Runnable() {
					@Override
					public void run() {
						updateEditorIcon(editorres);
						// workaround for squiggly lines not vanishing after rebuild
						//refresh();
					}
				});
			}
		}
	}

	private ABSContentOutlinePage outlinePage = null;
	private ResourceBuildListener builtListener;
	private IPropertyChangeListener propertyChangeListener;
	private List<CompilationUnitChangeListener> modelChangeListeners = new ArrayList<CompilationUnitChangeListener>();
	private CompilationUnit compilationUnit;
	private ABSReconcilingStrategy reconciler;
    private volatile int caretPos;
    private AbsInformationPresenter informationPresenter;
	
	public ABSEditor() {
	    super();
	    setSourceViewerConfiguration(new ABSSourceViewerConfiguration(this));
	    setDocumentProvider(new ABSDocumentProvider());
	    builtListener = new UpdateEditorIcon();
		CoreControlUnit.addResourceBuildListener(builtListener);
		// if preferences of syntax highlighting change: Reinstall the PresentationReconciler to make the changes appear.
		propertyChangeListener = new IPropertyChangeListener() {
			
			@Override
			public void propertyChange(PropertyChangeEvent event) {
				ISourceViewer sourceviewer = ABSEditor.this.getSourceViewer();
				getSourceViewerConfiguration().getPresentationReconciler(sourceviewer).install(sourceviewer);
			}
		};
		Activator.getDefault().getPreferenceStore().addPropertyChangeListener(propertyChangeListener);
		
//		this.getSelectionProvider().addSelectionChangedListener(new ISelectionChangedListener() {
//			
//			@Override
//			public void selectionChanged(SelectionChangedEvent event) {
//				System.out.println("selection = " + event.getSelection());
//				
//			}
//		});
//		this.getSourceViewer().getSelectedRange();
//		StyledText styledText = (StyledText) getAdapter(Control.class);
//		styledText.addCaretListener(new CaretListener() {
//			
//			@Override
//			public void caretMoved(CaretEvent e) {
//				System.out.println(e.caretOffset);
//				
//			}
//		});
		
	}
	
	/**
	 * Reinitializes the editor's source viewer based on the old editor input / document.
	 *
	 */
	private void reinitializeSourceViewer() {
		IEditorInput input = getEditorInput();
		IDocumentProvider documentProvider = getDocumentProvider();
		IAnnotationModel model = documentProvider.getAnnotationModel(input);
		IDocument document = documentProvider.getDocument(input);
		ISourceViewer fSourceViewer = getSourceViewer();
		
		if (document != null) {
			fSourceViewer.setDocument(document, model);
		}
	}
	
	/**
	 * highlights the given line as the current instruction point
	 * @param line the line the debugger is currently running in
	 */
	public void highlightLine(int line){
		IDocument doc = getDocumentProvider().getDocument(getEditorInput());
		int lineOffset;
		try {
			lineOffset = doc.getLineOffset(line);
			
			IResource resource = getResource();
			if (resource != null) { // can be null for files inside jars
			    
    			resource.deleteMarkers(Constants.CURRENT_IP_MARKER, false, IResource.DEPTH_ZERO);
    			
    			getSourceViewer().invalidateTextPresentation();
    			
    			IMarker marker = resource.createMarker(Constants.CURRENT_IP_MARKER);
                marker.setAttribute(IMarker.LINE_NUMBER, line);
                marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_INFO);
                marker.setAttribute(IMarker.MESSAGE, "current instruction pointer");
                
			}
            if(getSourceViewer() instanceof SourceViewer){
                SourceViewer sourceviewer = (SourceViewer)getSourceViewer();
                sourceviewer.setSelection(new TextSelection(lineOffset, 0), true);
                //sourceviewer.refresh();
            }
		} catch (CoreException e) {
			standardExceptionHandling(e);
		} catch (BadLocationException e) {
			standardExceptionHandling(e);
		}
	}

	/**
	 * Highlights the given {@link ASTNode} in the editor.<br/>
	 * Only one ASTNode at a time can be highlighted (This is a restriction of {@link ITextEditor}}).<br/><br/>
     *
	 * @param edit The target editor
	 * @param node The node that should be highlighted.
	 */
	void highlightInEditor(ASTNode<?> node, boolean moveCursor) {
	
		EditorPosition pos = UtilityFunctions.getPosition(node);
	
		if (pos != null) {
			IDocument doc = getDocumentProvider().getDocument(
					getEditorInput());	
			try {
				/* Calculate the position on the editor by retrieving the char offset
				 * of the position line and the target column in this line.
				 */
				int startOff = doc.getLineOffset(pos.getLinestart()) + pos.getColstart();
				int endOff = doc.getLineOffset(pos.getLineend()) + pos.getColend();

				if (startOff < 0) {
					// TODO: This happens sometimes - new parser?
					Activator.logException(new IllegalArgumentException("FIXME"));
					return;
				}
				assert startOff > -1;
				assert endOff >= startOff;
				setHighlightRange(startOff, endOff - startOff, moveCursor);
			} catch (BadLocationException e) {
				/*
				 * Should not be thrown, as an ASTNode in a document must have a
				 * specific location.
				 */
			}
		}
	}

	/**
	 * Highlights the a given {@link InternalASTNode} in the editor.<br/>
	 * @see #highlightInEditor(ITextEditor, ASTNode)
	 */
	public void highlightInEditor(InternalASTNode<?> node, boolean moveCursor) {
		if (node != null){
			highlightInEditor(node.getASTNode(), moveCursor);
		}
	}

	/**
	 * removes the the highlighting set by {@link #highlightLine(int)}.
	 */
	public void removeHighlighting(){
		IResource resource = getResource();
		if (resource == null) {
		    // can be null for files inside jars
			return;
		}
		try {
			resource.deleteMarkers(Constants.CURRENT_IP_MARKER, false, IResource.DEPTH_INFINITE);
			getSourceViewer().invalidateTextPresentation();
		} catch (CoreException e) {
			standardExceptionHandling(e);
		}
	}
	
	@Override
	protected SourceViewerDecorationSupport getSourceViewerDecorationSupport(ISourceViewer viewer) {
		if (fSourceViewerDecorationSupport == null) {
			fSourceViewerDecorationSupport = new ABSDecorationSupport(viewer, getOverviewRuler(), getAnnotationAccess(), getSharedColors());
			configureSourceViewerDecorationSupport(fSourceViewerDecorationSupport);
		}
		return fSourceViewerDecorationSupport;
	}
	
	@Override
	public void init(IEditorSite site, IEditorInput input) throws PartInitException {
		super.init(site, input);
		
		// activate abseditorscope when part is active
		// for example: F3 (JumpToDeclaration) is only active if this editor scope is enabled
	    IContextService cs = (IContextService)getSite().getService(IContextService.class);
	    cs.activateContext(ABSEDITOR_CONTEXT_ID);
	}
	
	@Override
	public void createPartControl(Composite parent) {
	    super.createPartControl(parent);
	    // listen to changes of the caret position:
	    getSelectionProvider().addPostSelectionChangedListener(new ISelectionChangedListener() {
	    	@Override
	    	public void selectionChanged(SelectionChangedEvent event) {
	    		ISelection selection = event.getSelection();
	    		if (selection instanceof ITextSelection) {
	    			ITextSelection ts = (ITextSelection) selection;
	    			caretPos = ts.getOffset();
	    		}
	    	}
		});    
	    initCompilationUnit();
	}

	public IPostSelectionProvider getSelectionProvider() {
		return (IPostSelectionProvider) super.getSelectionProvider();
	}

	private void initCompilationUnit() {
	    AbsNature absNature = UtilityFunctions.getAbsNature(getResource());
	    if (absNature != null) {
	        AbsModelManager modelManager = absNature.getModelManager();
	        compilationUnit = modelManager.getCompilationUnit(getAbsoluteFilePath());
	    } else {
	        // we are looking at abslang.abs or a file inside a jar-package
	        IURIEditorInput uriInput = (IURIEditorInput) getEditorInput().getAdapter(IURIEditorInput.class);

	        if (uriInput != null) {
	            // We're looking e.g. at abslang.abs which only exists in memory.

	            // create an empty model which only contains abslang.abs:
	            absNature = new AbsNature();
	            absNature.emptyModel();
	            File f = new File(uriInput.getURI());
	            String path = f.getAbsolutePath();
	            compilationUnit = absNature.getCompilationUnit(path);
	        }
	    }
	}

	@Override
	@SuppressWarnings("rawtypes")
	public Object getAdapter(Class key){
		if (IContentOutlinePage.class.equals(key)){
			if(outlinePage == null){
				outlinePage = new ABSContentOutlinePage(this);
			}
			return outlinePage;
		}
		return super.getAdapter(key);
	}
	
	@Override
	protected void configureSourceViewerDecorationSupport (SourceViewerDecorationSupport support) {
		IPreferenceStore store = getPreferenceStore();
		store.setValue(LOCATION_TYPE_NEAR_TEXTSTYLE_KEY, LOCATION_TYPE_NEAR_TEXTSTYLE_VALUE);
		store.setValue(LOCATION_TYPE_FAR_TEXTSTYLE_KEY, LOCATION_TYPE_FAR_TEXTSTYLE_VALUE);
		store.setValue(LOCATION_TYPE_SOMEWHERE_TEXTSTYLE_KEY, LOCATION_TYPE_SOMEWHERE_TEXTSTYLE_VALUE);
		super.configureSourceViewerDecorationSupport(support);
	 
		char[] matchChars = {'(', ')', '[', ']', '{', '}'}; //which brackets to match
		ICharacterPairMatcher matcher = new DefaultCharacterPairMatcher(matchChars ,
				IDocumentExtension3.DEFAULT_PARTITIONING);
		support.setCharacterPairMatcher(matcher);
		support.setMatchingCharacterPainterPreferenceKeys(EDITOR_MATCHING_BRACKETS, EDITOR_MATCHING_BRACKETS_COLOR);
		
		//Enable bracket highlighting in the preference store
		store.setDefault(EDITOR_MATCHING_BRACKETS, true);
		store.setDefault(EDITOR_MATCHING_BRACKETS_COLOR, Constants.DEFAULT_MATCHING_BRACKETS_COLOR);
	}

	@Override
	protected void doSetInput(IEditorInput input) throws CoreException {
		super.doSetInput(input);
	}

	public IResource getResource() {
		return (IResource)getEditorInput().getAdapter(IResource.class);
	}

	/**
	 * Throws a {@link SWTException} if the display is disposed
	 * @param editorres the resource of the editor input
	 */
	private void updateEditorIcon(IResource editorres) {
		try {
			int sev = editorres.findMaxProblemSeverity(MARKER_TYPE, true, IResource.DEPTH_INFINITE);
			if(sev == IMarker.SEVERITY_INFO){
				setTitleImage(getEditorInput().getImageDescriptor().createImage());
				return;
			}
			ISharedImages simages = PlatformUI.getWorkbench().getSharedImages();
			ImageDescriptor overlayIcon = null;
			switch(sev){
			case IMarker.SEVERITY_WARNING:
				overlayIcon = simages.getImageDescriptor(ISharedImages.IMG_DEC_FIELD_WARNING);
				break;
			case IMarker.SEVERITY_ERROR:
				overlayIcon = simages.getImageDescriptor(ISharedImages.IMG_DEC_FIELD_ERROR);
				break;
			}
			Image resourceImage = getEditorInput().getImageDescriptor().createImage();
			final DecorationOverlayIcon icon = new DecorationOverlayIcon(resourceImage, overlayIcon, IDecoration.BOTTOM_LEFT);
			setTitleImage(icon.createImage());
		} catch (CoreException e) {
			standardExceptionHandling(e);
		}
	}
	
	@Override
	public void dispose() {
		CoreControlUnit.removeResourceBuildListener(builtListener);
		Activator.getDefault().getPreferenceStore().removePropertyChangeListener(propertyChangeListener);
		super.dispose();
	}

	public void openInformation(String title, String message) {
		MessageDialog.openInformation(getSite().getShell(), title, message);
	}

	public void openError(String title, String message) {
		MessageDialog.openError(getSite().getShell(), title, message);
	}
	
	public IProject getProject() {
		if (getResource() != null) {
			return getResource().getProject();
		}
		PackageAbsFileEditorInput storageInput = (PackageAbsFileEditorInput) getEditorInput().getAdapter(PackageAbsFileEditorInput.class);
		if (storageInput != null) {
			// we are looking at a file inside a jar package
			return storageInput.getFile().getProject();
		}
		return null;
	}

	/**
	 * returns the absolute file path to the file opened by the editor
	 * or "<unknown>" if no such file exists 
	 */
	public String getAbsoluteFilePath() {
		File f = getFile();
		if (f != null) {
			return f.getAbsolutePath();
		}
		PackageAbsFileEditorInput storageInput = (PackageAbsFileEditorInput) getEditorInput().getAdapter(PackageAbsFileEditorInput.class);
		if (storageInput != null) {
			// we are looking at a file inside a jar package
			return storageInput.getFile().getAbsoluteFilePath();
		}
		return "<unknown>";
	}
	
	/**
	 * returns the file shown by the editor
	 * or null if the current resource is not a file
	 */
	public File getFile() {
		if (getResource() instanceof IFile) {
			IFile iFile = (IFile) getResource();
			return iFile.getLocation().toFile();
		}
		return null;
	}
	
	/**
	 * adds a listener which is notified whenever the underlying
	 * compilationUnit of this editor changes
	 */
	public void addModelChangeListener(CompilationUnitChangeListener modelChangeListener) {
		this.modelChangeListeners.add(modelChangeListener);
		if (compilationUnit != null) {
			modelChangeListener.onCompilationUnitChange(compilationUnit);
		}
	}
	
	public void removeModelChangeListener(CompilationUnitChangeListener modelChangeListener) {
		this.modelChangeListeners.remove(modelChangeListener);
	}
	
	@Override
	public void onCompilationUnitChange(CompilationUnit newCu) {
		this.compilationUnit = newCu;
		for (CompilationUnitChangeListener mcl : modelChangeListeners) {
			mcl.onCompilationUnitChange(newCu);
		}
	}

	/**
	 * parses the current contents of the editor
	 * and updates the compilationUnit
	 * @param documentOffset 
	 */
	public void reconcile(boolean withTypechecks) {
	    if (reconciler != null) {
	        reconciler.reconcile(getAbsNature(), withTypechecks);
	    }
	}

	public void setReconciler(ABSReconcilingStrategy absReconcilingStrategy) {
		this.reconciler = absReconcilingStrategy;
	}

	/**
	 * returns the current compilationUnit or null 
	 * when viewing files outside an ABS project
	 */
	public synchronized InternalASTNode<CompilationUnit> getCompilationUnit() {
	    if (getAbsNature() == null || compilationUnit == null) {
	        return null;
	    }
            return new InternalASTNode<CompilationUnit>(compilationUnit, getAbsNature());
	}

    public AbsNature getAbsNature() {
        return UtilityFunctions.getAbsNature(getProject());
    }

    public void setCaretPos(int caretPos) {
        this.caretPos = caretPos;
    }
    
    public int getCaretPos() {
        return caretPos;
    }

    public AbsInformationPresenter getInformationPresenter() {
        if (informationPresenter == null) {
            informationPresenter = new AbsInformationPresenter();
            informationPresenter.install(getSourceViewer());
        }
        return informationPresenter;
    }
	
}