/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package eu.hatsproject.absplugin.editor;

import java.io.PrintStream;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorActionDelegate;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;

import abs.frontend.ast.*;
import abs.frontend.typechecker.KindedName;
import abs.frontend.typechecker.KindedName.Kind;
import abs.frontend.typechecker.ResolvedName;
import abs.frontend.typechecker.Type;
import abs.frontend.typechecker.TypeCheckerException;
import beaver.Symbol;
import eu.hatsproject.absplugin.Activator;
import eu.hatsproject.absplugin.builder.AbsNature;
import eu.hatsproject.absplugin.console.ConsoleManager;
import eu.hatsproject.absplugin.console.ConsoleManager.MessageType;
import eu.hatsproject.absplugin.editor.outline.PackageAbsFile;
import eu.hatsproject.absplugin.editor.outline.PackageAbsFileEditorInput;
import eu.hatsproject.absplugin.util.UtilityFunctions;
import eu.hatsproject.absplugin.util.UtilityFunctions.EditorPosition;

public class JumpToDeclarationDelegate implements IEditorActionDelegate {
	private IEditorPart editor;
	
	private static final boolean doDebug = false;
	
	@Override
	public void run(IAction action) {
		if(!(editor instanceof ABSEditor)){
			return;
		}
		
		try{
			IEditorInput input = editor.getEditorInput();
			ABSEditor abseditor = (ABSEditor)editor;
			AbsNature nature;
			String path;
			IProject project;
			if (input instanceof PackageAbsFileEditorInput) {
				/*
				 * XXX This solution only works if 
				 * dependencies are not cross projects. 
				 */
				PackageAbsFileEditorInput pInput =
					(PackageAbsFileEditorInput) input;
				
				PackageAbsFile packageFile = pInput.getFile();
				project = packageFile.getProject();
				nature = UtilityFunctions.getAbsNature(packageFile.getProject());
				path = packageFile.getAbsoluteFilePath();
			} else {
				IFile file = (IFile)abseditor.getEditorInput().getAdapter(IFile.class);
				if (file == null)
					return; /* TODO: We were looking at a "virtual" file like abslang.abs. #306 */
				nature = UtilityFunctions.getAbsNature(file);
				project = file.getProject();
				path = file.getLocation().toFile().getAbsolutePath();
			}
			
			if(nature==null){
				abseditor.openError("No ABSProject", "The file is not in an ABS project!");
				return;
			}
			
			synchronized (nature.modelLock) {
				IDocument doc = abseditor.getDocumentProvider().getDocument(abseditor.getEditorInput());
				
				CompilationUnit compunit = nature.getCompilationUnit(path);
				if(compunit==null){
					abseditor.openInformation("Error", "AST not set!");
					return;
				}
				
				TextSelection sel = (TextSelection)abseditor.getSelectionProvider().getSelection();
				int carpos = sel.getOffset();
				ASTNode<?> node = UtilityFunctions.getASTNodeOfOffset(doc, compunit, carpos);
				
				EditorPosition pos = getPosition(compunit, node);
				if(doDebug) {
					PrintStream pstream = ConsoleManager.getDefault().getPrintStream(MessageType.MESSAGE_ERROR);
					pstream.println("Node:");
					pstream.println(node.getClass().toString());
					pstream.println("Bumping Tree:");
					node.dumpTree("", pstream);
				}
				if(pos!=null){
					ABSEditor targeteditor = 
						UtilityFunctions.openABSEditorForFile(pos.getPath(),project);
					if(targeteditor==null){
						abseditor.openInformation("File not found!",
								"Could not find file "+pos.getPath().toOSString());
						return;
					}
					doc = targeteditor.getDocumentProvider().getDocument(targeteditor.getEditorInput());
					int startoff = doc.getLineOffset(pos.getLinestart()-1) + pos.getColstart()-1;
					targeteditor.getSelectionProvider().setSelection(new TextSelection(startoff,0));
				}
			}
		} catch(BadLocationException ex){
			Activator.logException(ex);
		}
	}

	EditorPosition getPosition(CompilationUnit cu, ASTNode<?> node) {
		ASTNode<?> decl = null;
		try {
			if(node instanceof Decl){
				decl = (Decl)node;
			} else if(node instanceof FnApp){
				FnApp fnapp = (FnApp)node;
				String name = fnapp.getName();
				decl = fnapp.lookup(new KindedName(Kind.FUN, name));
			} else if(node instanceof VarOrFieldUse){
				VarOrFieldUse vofu = (VarOrFieldUse)node;
				String name = vofu.getName();
				decl = vofu.lookupVarOrFieldName(name,false);
			} else if(node instanceof Call){
				Call call = (Call)node;
				String mname = call.getMethod();
				Type type = call.getCallee().getType();
				decl = type.lookupMethod(mname);
			} else if(node instanceof DataConstructorExp){
				DataConstructorExp exp = (DataConstructorExp)node;
				decl = exp.getDecl();
			} else if(node instanceof VarOrFieldDecl){
				decl = node;
			} else if(node instanceof MethodSig){
				decl = node;
			} else if(node instanceof TypeUse){
				TypeUse tu = (TypeUse)node;
				decl = tu.getDecl();
			} else if(node instanceof NewExp){
				NewExp newexp = (NewExp)node;
				String classname = newexp.getClassName();
				decl = newexp.lookup(new KindedName(Kind.CLASS, classname));
			} else if(node instanceof FromImport){
				FromImport fimport = (FromImport)node;
				String moduleName = fimport.getModuleName();
				decl = fimport.lookupModule(moduleName);
			} else if(node instanceof StarImport){
				StarImport fimport = (StarImport)node;
				String moduleName = fimport.getModuleName();
				decl = fimport.lookupModule(moduleName);
			} else if(node instanceof FromExport){
				FromExport fexport = (FromExport)node;
				decl = fexport.getModuleDecl();
			} else if(node instanceof StarExport){
				StarExport fexport = (StarExport)node;
				decl = fexport.getModuleDecl();
			} else if(node instanceof Name){
				Name name = (Name)node;
				String simpleName = name.getString();
				decl = cu.lookupModule(simpleName);
			} else if (node instanceof DeltaClause) {
				DeltaClause d = (DeltaClause) node;
				String dName = d.getDeltaspec().getName();
				/* XXX: [stolz] Is 'm' ALWAYS the right scope to lookup in? */
				ModuleDecl m = d.getModuleDecl();
				ResolvedName res = m.resolveName(new KindedName(Kind.TYPE_DECL, dName));
				if (res != null)
					decl = res.getDecl();
			} else if (node instanceof ModifyClassModifier) {
				ModifyClassModifier cm = (ModifyClassModifier) node;
				String mName = cm.moduleName();
				ModuleDecl dm = cu.lookupModule(mName);
				decl = dm.lookup(new KindedName(Kind.CLASS, cm.className()));
			} else if (node instanceof List) {
				/* [stolz] Happens e.g. when selecting a delta in a productline */
				List<?> l = (List<?>) node;
				if (l.hasChildren())				
					return getPosition(cu, l.getChild(0));
			}
		} catch (TypeCheckerException e) {
			// Nada - may come from resolveName() on broken models.
			Activator.logException(e);
		}

		if(decl == null || decl instanceof UnknownDecl){
			((ABSEditor)editor).openInformation("Declaration not found!", "Declaration for symbol under cursor not found!");
			return null;
		}

		CompilationUnit declcu = UtilityFunctions.getCompilationUnitOfASTNode(decl);
		
		int start = decl.getStartPos();
		int end = decl.getEndPos();
		
		return new EditorPosition(new Path(declcu.getFileName()), Symbol.getLine(start), Symbol.getColumn(start), Symbol.getLine(end), Symbol.getColumn(end));
	}

	@Override
	public void selectionChanged(IAction action, ISelection selection) {
		//nothing
	}

	@Override
	public void setActiveEditor(IAction action, IEditorPart targetEditor) {
		this.editor = targetEditor;
	}

}
