/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package eu.hatsproject.absplugin.editor;

import static eu.hatsproject.absplugin.util.Constants.PARTITION_TYPES;

import java.io.IOException;
import java.io.InputStream;

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

import eu.hatsproject.absplugin.util.Constants;

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
			}
		} else {
			ABSPartitionScanner scanner = new ABSPartitionScanner();                         
			IDocumentPartitioner partitioner = new FastPartitioner(scanner, PARTITION_TYPES);
			document.setDocumentPartitioner(partitioner);                                    
			partitioner.connect(document);          
		}
//                                        ,_-=(!7(7/zs_.             
//	                                   .='  ' .`/,/!(=)Zm.           	
//	                     .._,,._..  ,-`- `,\ ` -` -`\\7//WW.         	
//	                ,v=~/.-,-\- -!|V-s.)iT-|s|\-.'   `///mK%.        	
//	              v!`i!-.e]-g`bT/i(/[=.Z/m)K(YNYi..   /-]i44M.       	
//	            v`/,`|v]-DvLcfZ/eV/iDLN\D/ZK@%8W[Z..   `/d!Z8m       	
//	           //,c\(2(X/NYNY8]ZZ/bZd\()/\7WY%WKKW)   -'|(][%4.      	
//	         ,\\i\c(e)WX@WKKZKDKWMZ8(b5/ZK8]Z7%ffVM,   -.Y!bNMi      	
//	         /-iit5N)KWG%%8%%%%W8%ZWM(8YZvD)XN(@.  [   \]!/GXW[      	
//	        / ))G8\NMN%W%%%%%%%%%%8KK@WZKYK*ZG5KMi,-   vi[NZGM[      	
//	       i\!(44Y8K%8%%%**~YZYZ@%%%%%4KWZ/PKN)ZDZ7   c=//WZK%!      	
//	      ,\v\YtMZW8W%%f`,`.t/bNZZK%%W%%ZXb*K(K5DZ   -c\\/KM48       	
//	      -|c5PbM4DDW%f  v./c\[tMY8W%PMW%D@KW)Gbf   -/(=ZZKM8[       	
//	      2(N8YXWK85@K   -'c|K4/KKK%@  V%@@WD8e~  .//ct)8ZK%8`       	
//	      =)b%]Nd)@KM[  !'\cG!iWYK%%|   !M@KZf    -c\))ZDKW%`        	
//	      YYKWZGNM4/Pb  '-VscP4]b@W%     'Mf`   -L\///KM(%W!         	
//	      !KKW4ZK/W7)Z. '/cttbY)DKW%     -`  .',\v)K(5KW%%f          	
//	      'W)KWKZZg)Z2/,!/L(-DYYb54%  ,,`, -\-/v(((KK5WW%f           	
//	       \M4NDDKZZ(e!/\7vNTtZd)8\Mi!\-,-/i-v((tKNGN%W%%            	
//	       'M8M88(Zd))///((|D\tDY\\KK-`/-i(=)KtNNN@W%%%@%[           	
//	        !8%@KW5KKN4///s(\Pd!ROBY8/=2(/4ZdzKD%K%%%M8@%%           	
//	         '%%%W%dGNtPK(c\/2\[Z(ttNYZ2NZW8W8K%%%%YKM%M%%.          	
//	           *%%W%GW5@/%!e]_tZdY()v)ZXMZW%W%%%*5Y]K%ZK%8[          	
//	            '*%%%%8%8WK\)[/ZmZ/Zi]!/M%%%%@f\ \Y/NNMK%%!          	
//	              'VM%%%%W%WN5Z/Gt5/b)((cV@f`  - |cZbMKW%%|          	
//	                 'V*M%%%WZ/ZG\t5((+)L\'-,,/  -)X(NWW%%           	
//	                      `~`MZ/DZGNZG5(((\,    ,t\\Z)KW%@           	
//	                         'M8K%8GN8\5(5///]i!v\K)85W%%f           	
//	                           YWWKKKKWZ8G54X/GGMeK@WM8%@            	
//	                            !M8%8%48WG@KWYbW%WWW%%%@             	
//	                              VM%WKWK%8K%%8WWWW%%%@`             	
//	                                ~*%%%%%%W%%%%%%%@~               	
//	                                   ~*MM%%%%%%@f`                 	
//	                                       '''''                    	
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
