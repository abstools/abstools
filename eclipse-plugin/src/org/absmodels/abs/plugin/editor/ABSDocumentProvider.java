/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.editor;

import static org.absmodels.abs.plugin.util.Constants.PARTITION_TYPES;

import java.io.IOException;
import java.io.InputStream;

import org.absmodels.abs.plugin.util.Constants;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentPartitioner;
import org.eclipse.jface.text.rules.FastPartitioner;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IURIEditorInput;
import org.eclipse.ui.editors.text.FileDocumentProvider;


public class ABSDocumentProvider extends FileDocumentProvider{
	
	@Override
	protected IDocument createDocument(Object element) throws CoreException {
		// Avoid ResourceException if you open a file that has disappeared.
		if (isDeleted(element))
			return super.createEmptyDocument();
		IDocument document = super.createDocument(element);
		if(document == null){
			if(element instanceof IURIEditorInput){
				IURIEditorInput ei = (IURIEditorInput)element;
				document = createEmptyDocument();
				InputStream is = null;
				try{
					is = ei.getURI().toURL().openStream();
					setDocumentContent(document, is, getEncoding(element));
				} catch(IOException ex){
					throw new CoreException(new Status(IStatus.ERROR,Constants.PLUGIN_ID,"ABS Editor",ex));
				} finally{
					if(is != null){
						try{
							is.close();
						} catch(IOException ex){
							throw new CoreException(new Status(IStatus.ERROR,Constants.PLUGIN_ID,"ABS Editor",ex));
						}
					}
				}
			} else
				throw new CoreException(new Status(IStatus.ERROR,Constants.PLUGIN_ID,"Don't know how to open "+element.toString()));
		}
		
		ABSPartitionScanner scanner = new ABSPartitionScanner();                         
		IDocumentPartitioner partitioner = new FastPartitioner(scanner, PARTITION_TYPES);
		document.setDocumentPartitioner(partitioner);                                    
		partitioner.connect(document);          
	
		return document;
	}

	@Override
	protected IAnnotationModel createAnnotationModel(Object element) throws CoreException {
		if (element instanceof IFileEditorInput) {
			IFileEditorInput input = (IFileEditorInput) element;
			return new ABSMarkerAnnotationModel(input.getFile());
		}
		return super.createAnnotationModel(element);
	}
	
}
