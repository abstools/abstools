/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package eu.hatsproject.absplugin.editor;

import static eu.hatsproject.absplugin.util.Constants.*;
import static eu.hatsproject.absplugin.util.UtilityFunctions.standardExceptionHandling;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentExtension3;
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
import org.eclipse.swt.SWTException;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.IPersistableEditor;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.contexts.IContextService;
import org.eclipse.ui.editors.text.TextEditor;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.texteditor.SourceViewerDecorationSupport;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;

import eu.hatsproject.absplugin.Activator;
import eu.hatsproject.absplugin.editor.decoration.ABSDecorationSupport;
import eu.hatsproject.absplugin.editor.outline.ABSContentOutlinePage;
import eu.hatsproject.absplugin.util.Constants;
import eu.hatsproject.absplugin.util.CoreControlUnit;
import eu.hatsproject.absplugin.util.CoreControlUnit.ResourceBuildListener;
import eu.hatsproject.absplugin.util.CoreControlUnit.ResourceBuiltEvent;

/**
 * The editor for ABS file. Includes syntax highlighting and content assist for ABS files
 * as well as annotations for ABS errors.
 * 
 * @author tfischer, cseise, fstrauss, mweber
 *
 */
public class ABSEditor extends TextEditor implements IPersistableEditor{
    
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

	private IContentOutlinePage outlinePage = null;
	private ResourceBuildListener builtListener;
	private IPropertyChangeListener propertyChangeListener;
	
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
			if (resource == null) {
			    // can be null for files inside jars
				return;
			}
			resource.deleteMarkers(Constants.CURRENT_IP_MARKER, false, IResource.DEPTH_ZERO);
			
			getSourceViewer().invalidateTextPresentation();
			
			IMarker marker = resource.createMarker(Constants.CURRENT_IP_MARKER);
            marker.setAttribute(IMarker.LINE_NUMBER, line);
            marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_INFO);
            marker.setAttribute(IMarker.MESSAGE, "current instruction pointer");
            
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
	@SuppressWarnings("rawtypes")
	public Object getAdapter(Class key){
		if (IContentOutlinePage.class.equals(key)){
			if(outlinePage == null){
				outlinePage = new ABSContentOutlinePage(this.getDocumentProvider(),this);
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
		outlinePage = new ABSContentOutlinePage(this.getDocumentProvider(),this);
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
}