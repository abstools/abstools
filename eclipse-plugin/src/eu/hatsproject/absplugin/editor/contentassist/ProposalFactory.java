/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package eu.hatsproject.absplugin.editor.contentassist;

import static eu.hatsproject.absplugin.util.Images.NO_IMAGE;
import static eu.hatsproject.absplugin.util.Images.getImageForASTNode;
import static eu.hatsproject.absplugin.util.UtilityFunctions.*;

import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.CompletionProposal;
import org.eclipse.jface.text.contentassist.ICompletionProposal;

import abs.frontend.ast.*;
import abs.frontend.parser.Main;
import abs.frontend.typechecker.KindedName;
import abs.frontend.typechecker.KindedName.Kind;
import abs.frontend.typechecker.ResolvedName;
import abs.frontend.typechecker.Type;
import abs.frontend.typechecker.TypeCheckerException;
import eu.hatsproject.absplugin.builder.AbsNature;
import eu.hatsproject.absplugin.editor.ABSEditor;
import eu.hatsproject.absplugin.editor.outline.ABSContentOutlineUtils;
import eu.hatsproject.absplugin.editor.outline.PackageEntry;
import eu.hatsproject.absplugin.internal.IncrementalModelBuilder;
import eu.hatsproject.absplugin.internal.NoModelException;
import eu.hatsproject.absplugin.util.Constants;
import eu.hatsproject.absplugin.util.UtilityFunctions;

/**
 * Class generating the actual proposals for auto completion
 * @author mweber, tfischer
 */
public class ProposalFactory{
		private static final class ProposalComparator implements
				Comparator<ICompletionProposal> {
			@Override
			public int compare(ICompletionProposal o1, ICompletionProposal o2) {
				return o1.getDisplayString().compareTo(o2.getDisplayString());
			}
		}

		private String qualifier;
		private int documentOffset;
		private IDocument doc;
		private List<ICompletionProposal> proposals;
		private CompilationUnit cu;
		private ABSEditor editor;
		private Main absParser;

		/**
		 * Initializes the {@link ProposalFactory} by parsing all abs files in the current project and
		 * also parsing the document of the currently open abs file with all unsaved changes.
		 * @param qualifier the qualifier typed so far
		 * @param documentOffset the document offset the cursor is located at
		 * @param doc the currently open document (possibly with non-saved changes
		 * @param editor the currently open editor
		 * @param proposals the list of proposals to be filled
		 */
		public ProposalFactory(String qualifier, int documentOffset, IDocument doc, ABSEditor editor,
				List<ICompletionProposal> proposals){
			this.qualifier = qualifier;
			this.documentOffset = documentOffset;
			this.doc = doc;
			this.proposals = proposals;
			this.editor = editor;
			this.absParser = new Main();
			this.absParser.setAllowIncompleteExpr(true);
		    this.absParser.setTypeChecking(false);
			parseProject();
		}
		
		private void parseABSFile(
				final IncrementalModelBuilder builder,
				IResource resource) throws IOException,
				CoreException, NoModelException {
			if (isABSFile(resource)) {
				IFile visitedfile = (IFile) resource;
				if (isABSSourceFile(visitedfile)) {
					CompilationUnit cu = absParser.parseUnit(visitedfile.getLocation().toFile(),
							null, new InputStreamReader(visitedfile.getContents()));
					cu.setName(visitedfile.getLocation().toFile().getAbsolutePath());
					builder.addCompilationUnit(cu);
				} else if (isABSPackage(visitedfile)) {
					builder.addCompilationUnits(
							absParser.parseABSPackageFile(visitedfile.getLocation().toFile()));
				} 
			}
		}
		
		/**
		 * Parse package dependencies in the ABS project containing the input file
		 * @param file
		 * @return a set of {@link CompilationUnit} of package dependencies.  
		 * @throws IOException
		 */
		private Set<CompilationUnit> parseDependencies(IFile file) throws IOException {
			//iterate over package dependencies
			AbsNature nature = UtilityFunctions.getAbsNature(file);
			Set<CompilationUnit> units = new HashSet<CompilationUnit>();  
			for (PackageEntry entry : nature.getPackages().getPackages()) {
				units.addAll(absParser.parseABSPackageFile(new File(entry.getPath())));
			}
			return units;
		}

		/**
		 * parse the abs files of the current project and the currently open document
		 */
		private void parseProject() {
			final IncrementalModelBuilder builder = new IncrementalModelBuilder();
			final IFile file = (IFile)editor.getEditorInput().getAdapter(IFile.class);
			try {
				file.getProject().accept(new IResourceVisitor() {
					
					@Override
					public boolean visit(IResource resource) throws CoreException {
						try{
							if(isABSFile(resource)){
								parseABSFile(builder, resource);
							}
						} catch(IOException ex){
							throw new CoreException(new Status(IStatus.ERROR, Constants.PLUGIN_ID, ex.getLocalizedMessage(), ex));
						} catch (NoModelException e) {
							throw new CoreException(new Status(IStatus.ERROR, Constants.PLUGIN_ID, e.getLocalizedMessage(), e));
						}
						return true;
					}
				});
				
				builder.addCompilationUnits(parseDependencies(file));
				
				// compile the current document with non-saved changes
				cu = absParser.parseUnit(file.getLocation().toFile(), null,
						new StringReader(prepareDocContent()));
				cu.setName(file.getLocation().toFile().getAbsolutePath());
				builder.addCompilationUnit(cu);
			} catch (CoreException e) {
				standardExceptionHandling(e);
			} catch (NoModelException e) {
				standardExceptionHandling(e);
			} catch (IOException e) {
				standardExceptionHandling(e);
			} catch (BadLocationException e) {
				standardExceptionHandling(e);
			}
			if(cu==null){
				AbsNature nature = getAbsNature(file.getProject());
				if(nature!=null){
					cu = nature.getCompilationUnit(file);
				}
			}
		}

		/**
		 * The document content has to be enhanced by inserting a semicolon in the right position.
		 * @return the adapted document content
		 * @throws BadLocationException if the location of the cursor is not valid (should not happen)
		 */
		private String prepareDocContent() throws BadLocationException {
			return doc.get(0,documentOffset+qualifier.length())+";"+doc.get(documentOffset+qualifier.length(),
					doc.getLength() - (documentOffset+qualifier.length()));
		}
		
		/**
		 * Creates the list of completion proposals for the current qualifier. The list contains all top level elements 
		 * as well as methods and fields for incomplete access (dot or exclamation mark).
		 * The list of keywords has to be provided by the frontend.
		 * @see abs.frontend.parser.Keywords
		 */
		public void computeStructureProposals() { 
			
			if(cu==null) {
			    addKeywordProposals();
				return;
			}
			
			try {
				ASTNode<?> node = getASTNodeOfOffset(doc, cu, documentOffset);
				
				int tmpOffset = documentOffset;
				tmpOffset--;
				while(doc.getChar(tmpOffset)==' '){
					tmpOffset--;
				}
				ASTNode<?> accessNode = getASTNodeOfOffset(doc, cu, tmpOffset);
				addMainblockProposals(node);
				if(accessNode instanceof IncompleteAccess){
					addIncompleteAccessProposals(accessNode); 
				} else if(accessNode instanceof IncompleteNewExp){
					addClassProposals(node);
				} else if (accessNode instanceof FieldUse) {
				    addFieldUseProposals((FieldUse)accessNode);
				} else {
				    addKeywordProposals();
					addToplevelProposals(node);
					addClassFieldProposals(node);
				}
			} catch (BadLocationException e) {
				standardExceptionHandling(e);
			}
		}

		/**
		 * adds the abs keywords to the list of proposals
		 */
        private void addKeywordProposals() {
            //Loop through keywords
			for(String s : abs.frontend.parser.Keywords.getKeywords()){
				if(qualifierIsPrefixOf(s)){
					CompletionProposal proposal = new CompletionProposal(s, documentOffset, qualifier.length(),
							s.length(), NO_IMAGE, s, null, "");
					proposals.add(proposal);
				}
			}
			
			Collections.sort(proposals, new ProposalComparator());
        }

		private void addFieldUseProposals(FieldUse accessNode) {
		    if(accessNode == null){
                throw new IllegalArgumentException("AccessNode may not be null!");
            }
            
            Type type = accessNode.getContextDecl().getType();
            addMethodProposal(type);
        }

        /**
		 * add the variables of the main block
		 * @param node the node under the cursor
		 */
		private void addMainblockProposals(ASTNode<?> node) {
			ProposalComparator comp = new ProposalComparator();
			ArrayList<ICompletionProposal> temp = new ArrayList<ICompletionProposal>();
			MainBlock mainblock = (MainBlock)node.calcContextNode(MainBlock.class);
			
			if(mainblock!=null){
				for(VarDecl vardecl : mainblock.getVars()){
					String name = vardecl.getName();
					if(qualifierIsPrefixOf(name)){
						CompletionProposal proposal = new CompletionProposal(name, documentOffset, qualifier.length(),
								name.length(), getImageForASTNode(vardecl), name, null, getAdditionalProposalInfo(vardecl));
						temp.add(proposal);
					}
				}

				Collections.sort(temp, comp);
				proposals.addAll(0, temp);
			}
		}

		/**
		 * add the classes in the current module
		 * @param node the node under the cursor
		 */
		private void addClassProposals(ASTNode<?> node) {
			ProposalComparator comp = new ProposalComparator();
			ArrayList<ICompletionProposal> tempNonqual = new ArrayList<ICompletionProposal>();
			ArrayList<ICompletionProposal> tempQual = new ArrayList<ICompletionProposal>();
			ModuleDecl moddecl = node.getModuleDecl();
			if(moddecl == null){
				throw new IllegalArgumentException("Node is not in a Module!");
			}
			Map<KindedName, ResolvedName> visibleNames = moddecl.getVisibleNames();
			
			for(Entry<KindedName, ResolvedName> kentry : visibleNames.entrySet()){
			        KindedName kname = kentry.getKey();
				if(qualifierIsPrefixOf(kname.getName()) && kname.getKind()==Kind.CLASS){
					CompletionProposal proposal = makeVisibleNameProposal(kentry.getValue(), kname);
					if(isQualified(kname)){
						tempQual.add(proposal);
					} else {
						tempNonqual.add(proposal);
					}
				}
			}
			
			Collections.sort(tempNonqual, comp);
			proposals.addAll(0, tempNonqual);
			
			Collections.sort(tempQual, comp);
			proposals.addAll(0, tempQual);
		}

		/**
		 * add proposals for the incomplete access under the cursor. An incomplete access exists if
		 * the user enters a dot or an exclamation mark after an identifier. An incomplete access
		 * proposal can be a method of an interface or a field
		 * @param accessNode the node under the cursor
		 */
		private void addIncompleteAccessProposals(ASTNode<?> accessNode) {
			if(accessNode == null){
				throw new IllegalArgumentException("AccessNode may not be null!");
			}
			
			IncompleteAccess ia = (IncompleteAccess)accessNode;
			PureExp target = ia.getTarget();
			Type type = target.getType();
			addMethodProposal(type);
		}

        private void addMethodProposal(Type type) {
            if (type.isFutureType()) {
                proposals.clear();
                String name = "get";
                proposals.add( new CompletionProposal(name, documentOffset, qualifier.length(),
                        name.length(), null, name, null, null));
            }
            ArrayList<ICompletionProposal> temp = new ArrayList<ICompletionProposal>();
			for(MethodSig methodSig : type.getAllMethodSigs()){
				String name = methodSig.getName();
				if(qualifierIsPrefixOf(name)){
					CompletionProposal proposal = makeMethodSigProposal(methodSig, name);
					temp.add(proposal);
				}
			}
			Collections.sort(temp, new ProposalComparator());
			proposals.addAll(0, temp);
			
			temp.clear();
			for(FieldDecl fdecl : type.getAllFieldDecls()){
				String name = fdecl.getName();
				if(qualifierIsPrefixOf(name)){
					CompletionProposal proposal = new CompletionProposal(name, documentOffset, qualifier.length(),
							name.length(), getImageForASTNode(fdecl), name, null, getAdditionalProposalInfo(fdecl));
					temp.add(proposal);
				}
			}
			Collections.sort(temp, new ProposalComparator());
			proposals.addAll(0, temp);
		}

		private CompletionProposal makeMethodSigProposal(MethodSig methodSig, String name) {
			String visibleName = ABSContentOutlineUtils.formatMethodSig(methodSig).toString();
			String replacement = name+"()";
			int cursorposition = name.length()+1;
			
			Decl classorinterfacedecl = methodSig.getContextDecl();
			Type type = classorinterfacedecl.getType();
			visibleName += " -- " + type.getSimpleName();
			
			CompletionProposal proposal = new CompletionProposal(replacement, documentOffset, qualifier.length(),
					cursorposition, getImageForASTNode(methodSig), visibleName, null, getAdditionalProposalInfo(methodSig));
			return proposal;
		}

		/**
		 * Checks, if the given String starts with the current qualifier
		 * @param name String to compare to qualifier
		 * @return true if String begins with qualifier (not case sensitive), false otherwise.
		 */
		public boolean qualifierIsPrefixOf(String name) {
			if(name == null){
				throw new IllegalArgumentException("Name may not be null!");
			}
			return name.toLowerCase().startsWith(qualifier.toLowerCase());
		}

		/**
		 * add the fields and variables of the class the cursor is in.
		 * @param node the node under the cursor
		 */
		private void addClassFieldProposals(ASTNode<?> node) {
			ArrayList<ICompletionProposal> temp = new ArrayList<ICompletionProposal>();
			MethodImpl methodimpl = node.getContextMethod();
			
			if(methodimpl!=null){
				for(VarDecl varDecl : methodimpl.getBlock().getVars()){
					String name = varDecl.getName();
					if(qualifierIsPrefixOf(name)){
						CompletionProposal proposal = new CompletionProposal(name, documentOffset, qualifier.length(),
								name.length(), getImageForASTNode(varDecl), name, null, getAdditionalProposalInfo(varDecl));
						temp.add(proposal);
					}
				}
			}
			
			ClassDecl classdecl = (ClassDecl)node.calcContextNode(ClassDecl.class);
			if(classdecl!=null){
				for(FieldDecl fieldDecl : classdecl.getType().getAllFieldDecls()){
					String name = fieldDecl.getName();
					if(qualifierIsPrefixOf(name)){
						CompletionProposal proposal = new CompletionProposal(name, documentOffset, qualifier.length(),
								name.length(), getImageForASTNode(fieldDecl), name, null, getAdditionalProposalInfo(fieldDecl));
						temp.add(proposal);
					}
				}
			}
			Collections.sort(temp, new ProposalComparator());
			proposals.addAll(0, temp);
		}

		/**
		 * add proposals for all visible names.
		 * @param node the node under the cursor
		 */
		private void addToplevelProposals(ASTNode<?> node) {
			ProposalComparator comp = new ProposalComparator();
			ArrayList<ICompletionProposal> tempNonqual = new ArrayList<ICompletionProposal>();
			ArrayList<ICompletionProposal> tempQual = new ArrayList<ICompletionProposal>();
			ModuleDecl moddecl = node.getModuleDecl();
			if(moddecl == null){
				return;
			}
			
			try {
				Map<KindedName, ResolvedName> visibleNames = moddecl.getVisibleNames();
				
				for(Entry<KindedName, ResolvedName> kentry : visibleNames.entrySet()){
				        KindedName kname = kentry.getKey();
					if(qualifierIsPrefixOf(kname.getName())){
						CompletionProposal proposal = makeVisibleNameProposal(kentry.getValue(), kname);
						if(isQualified(kname)){
							tempQual.add(proposal);
						} else {
							tempNonqual.add(proposal);
						}
					}
				}
				
				Collections.sort(tempNonqual, comp);
				proposals.addAll(tempNonqual);
				
				Collections.sort(tempQual, comp);
				proposals.addAll(tempQual);
			} catch (TypeCheckerException e ) {
			    // ignore all type check exceptions
			}
		}

		private CompletionProposal makeVisibleNameProposal(ResolvedName resolvedName, KindedName kname) {
			Decl decl = resolvedName.getDecl();
			String name = kname.getName();
			
			String visibleName = name;
			String replacement = name;
			int cursorposition = name.length();
			switch (kname.getKind()) {
			     case DATA_CONSTRUCTOR:
			     case FUN:
			         replacement = name+"()";
		             cursorposition = name.length()+1;
		             break;
			     case TYPE_DECL:
			         if (decl instanceof ParametricDataTypeDecl) {
			             ParametricDataTypeDecl parametricDataTypeDecl = (ParametricDataTypeDecl) decl;
			             if (parametricDataTypeDecl.getTypeParameterList().getNumChild() > 0) {
    			             replacement = name+"<>";
    	                     cursorposition = name.length()+1;
			             }
			         }
			         break;
			}
            CompletionProposal proposal = new CompletionProposal(replacement, documentOffset, qualifier.length(),
					cursorposition, getImageForASTNode(decl), visibleName, null, getAdditionalProposalInfo(decl));
			return proposal;
		}

		private String getAdditionalProposalInfo(Decl decl){
			return decl.getModuleDecl().getName()+"."+decl.getName();
		}

		private String getAdditionalProposalInfo(MethodSig methodsig){
			return ABSContentOutlineUtils.formatMethodSig(methodsig).toString();
		}

		private String getAdditionalProposalInfo(TypedVarOrFieldDecl vofdecl){
			return ABSContentOutlineUtils.formatTypedVarOrFieldDecl(vofdecl).toString();
		}

		private boolean isQualified(KindedName kname){
			return kname.getName().indexOf('.')>=0;
		}	
		
	}