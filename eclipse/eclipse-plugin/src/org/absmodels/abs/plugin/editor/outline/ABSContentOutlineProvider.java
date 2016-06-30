/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.editor.outline;

import static org.absmodels.abs.plugin.editor.outline.ABSContentOutlineUtils.*;
import static org.absmodels.abs.plugin.util.Constants.ABS_FILE_EXTENSION;
import static org.absmodels.abs.plugin.util.Constants.EMPTY_OBJECT_ARRAY;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.jar.JarEntry;

import org.absmodels.abs.plugin.Activator;
import org.absmodels.abs.plugin.builder.AbsNature;
import org.absmodels.abs.plugin.util.InternalASTNode;
import org.absmodels.abs.plugin.util.UtilityFunctions;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.ui.model.BaseWorkbenchContentProvider;
import org.eclipse.ui.progress.UIJob;

import abs.frontend.ast.*;
import abs.frontend.parser.ABSPackageFile;

/**
 * The ABS content outline provider is responsible for dissecting the AST of an
 * ABS file (or more specifically a compilation unit) into a tree structure.
 * <br/><br/>
 * The ABS content provider is set using the TreeViewer's method
 * setContentProvider(ITreeContentProvider) during the setup of the TreeViewer.
 *
 * @author cseise
 *
 */
public class ABSContentOutlineProvider implements ITreeContentProvider, IResourceChangeListener, IResourceDeltaVisitor {

	private static final ASTNode<?>[] EMPTY_NODES = new ASTNode<?>[0];

	private final BaseWorkbenchContentProvider baseProvider = new BaseWorkbenchContentProvider();

	private StructuredViewer viewer;

	public ABSContentOutlineProvider() {
		ResourcesPlugin.getWorkspace().addResourceChangeListener(this, IResourceChangeEvent.POST_CHANGE);
	}

	/**
	 * @see
	 * org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.Object)
	 */
	@Override
	public Object[] getChildren(Object parentElement) {

		if (parentElement instanceof IFile) {
			return getChildrenOf((IFile) parentElement);
		} else if (parentElement instanceof InternalASTNode<?>) {
			InternalASTNode<?> node = (InternalASTNode<?>) parentElement;

			synchronized (node.getNature().modelLock) {
				ASTNode<?>[] children = getChildrenOfASTNode(node.getASTNode());
				return InternalASTNode.wrapASTNodes(children, node.getNature()).toArray();
			}
		} else if (parentElement instanceof IProject) {
			return getChildrenOf((IProject) parentElement);
		} else if (parentElement instanceof PackageContainer) {
			return ((PackageContainer) parentElement).getPackages().toArray();
		} else if (parentElement instanceof PackageEntry) {
			return getChildrenOf((PackageEntry) parentElement);
		} else if (parentElement instanceof PackageAbsFile) {
			return getChildrenOf((PackageAbsFile) parentElement);
		}

		return EMPTY_OBJECT_ARRAY;
	}

	private Object[] getChildrenOf(PackageEntry element) {
		try {
			ABSPackageFile pf = new ABSPackageFile(new File(element.getPath()));
			if (pf.isABSPackage()) {
				java.util.ArrayList<PackageAbsFile> results = new ArrayList<PackageAbsFile>();
				for (Enumeration<JarEntry> e = pf.entries(); e.hasMoreElements();) {
					JarEntry jarEntry = e.nextElement();
					if (!jarEntry.isDirectory() && jarEntry.getName().endsWith(".abs")) {
						results.add(new PackageAbsFile(element, jarEntry.getName()));
		            }
				}
				return results.toArray();
			}
		} catch (IOException e) {
			Activator.logException(e);
		}
		return EMPTY_OBJECT_ARRAY;
	}

	private Object[] getChildrenOf(IProject project) {
		if (project.isOpen()) {
			AbsNature nature = UtilityFunctions.getAbsNature(project);
			java.util.List<Object> children = new ArrayList<Object>();
			children.add(nature.getPackages());
			children.addAll(Arrays.asList(baseProvider.getChildren(project)));
			return children.toArray();
		}
		return EMPTY_OBJECT_ARRAY;
	}

	private ASTNode<?>[] getChildrenOfASTNode(ASTNode<?> parentElement){
		if (parentElement instanceof ModuleDecl) {
			//no use of static imports here due to name clash with local methods...
			return ABSContentOutlineUtils.getChildrenOf((ModuleDecl) parentElement).toArray(EMPTY_NODES);
		} else if (parentElement instanceof CompilationUnit) {
			return getChildrenOf((CompilationUnit) parentElement);
		} else if (parentElement instanceof ClassDecl) {
			return getChildrenOf((ClassDecl) parentElement);
		} else if (parentElement instanceof InterfaceDecl) {
			return getChildrenOf((InterfaceDecl) parentElement);
		} else if (parentElement instanceof DataTypeDecl) {
			return getChildrenOf((DataTypeDecl) parentElement);
		} else if (parentElement instanceof MainBlock) {
			return getChildrenOf((MainBlock) parentElement);
		} else if (parentElement instanceof List<?>) {
			return getChildrenOf((List<?>) parentElement);
		} else if (parentElement instanceof DeltaDecl) {
		    return getChildrenOf((DeltaDecl) parentElement);
		}
		return EMPTY_NODES;
	}

	private Object[] getChildrenOf(IFile file) {
		if (UtilityFunctions.isABSPackage(file)) {
			return getChildren(makeABSPackage(file));
		}
		return getChildrenOf(new AbsFileImpl(file));
	}

	private Object[] getChildrenOf(AbsFile file) {
		if(!ABS_FILE_EXTENSION.equalsIgnoreCase(file.getFileExtension()))
			return null;
		AbsNature nature = UtilityFunctions.getAbsNature(file.getProject());
		if(nature != null){
			CompilationUnit cu = nature.getCompilationUnit(file.getAbsoluteFilePath());
			return getChildrenOf(cu,nature);
		} else {
			return null;
		}
	}

	@SuppressWarnings("unchecked")
	private ASTNode<?>[] getChildrenOf(List<?> parentElement) {
	    List<?> nodeList = (parentElement);
	    ArrayList<ASTNode<?>> impExp = new ArrayList<ASTNode<?>>();

	    //Check whether we have an import or an export list
	    if (isImportList(nodeList)){
	    	for (Import i : (List<Import>)nodeList){
	    		if (isStandardLibImport(i)){
	    			continue;
	    		}
	    		impExp.add(i);
	    	}

	    	return impExp.toArray(EMPTY_NODES);
	    }else if (isExportList(nodeList)){
	    	for (Export e : (List<Export>)nodeList) {
	    		impExp.add(e);
	    	}
	    	return impExp.toArray(EMPTY_NODES);
	    }

	    return EMPTY_NODES;
	}

	/**
	 * Retrieve the Children of a CompilationUnit
	 * @param d The target CompilationUnit
	 * @return The children of a CompilationUnit, which are relevant for the Content Outline
	 */
	private InternalASTNode<?>[] getChildrenOf (CompilationUnit cu,AbsNature nature){
	    if (cu != null){
	        ArrayList<InternalASTNode<ModuleDecl>> modules = new ArrayList<InternalASTNode<ModuleDecl>>();

	        for (ModuleDecl d : cu.getModuleDecls()) {
	    	modules.add(new InternalASTNode<ModuleDecl>(d,nature));
	        }
		return modules.toArray(new InternalASTNode<?>[0]);
	    } else{
	        return new InternalASTNode<?>[0];
	    }
	}

	/**
	 * Retrieve the Children of a CompilationUnit
	 * @param d The target CompilationUnit
	 * @return The children of a CompilationUnit, which are relevant for the Content Outline
	 */
	private ASTNode<?>[] getChildrenOf (CompilationUnit cu){
	    if (cu != null){
	        ArrayList<ASTNode<?>> children = new ArrayList<ASTNode<?>>();

	        for (ModuleDecl d : cu.getModuleDecls())
	            children.add(d);
	        for (DeltaDecl d : cu.getDeltaDecls())
	            children.add(d);
	        if (cu.hasProductLine())
	            children.add(cu.getProductLine());
	        for (ProductDecl d : cu.getProductDecls())
	            children.add(d);
	        for (FeatureDecl d : cu.getFeatureDecls())
	            children.add(d);
	        for (FExt d : cu.getFExts())
	            children.add(d);

	        return children.toArray(EMPTY_NODES);
	    } else{
	        return EMPTY_NODES;
	    }
	}

	/**
	 * Retrieve the Children of a ClassDecl
	 * @param d The target ClassDecl
	 * @return The children of a ClassDecl, which are relevant for the Content Outline
	 */
	private ASTNode<?>[] getChildrenOf(ClassDecl d) {
		ArrayList<ASTNode<?>> pDecl = new ArrayList<ASTNode<?>>();
		for (FieldDecl fD : d.getFieldList()) {
			pDecl.add(fD);
		}
		for (MethodImpl mD : d.getMethodList()) {
			pDecl.add(mD);
		}
		return pDecl.toArray(EMPTY_NODES);
	}

	/**
	 * Retrieve the Children of a DeltaDecl
	 * @param d The target DeltaDecl
	 * @return The children of a DeltaDecl, which are relevant for the Content Outline
	 */
	private ASTNode<?>[] getChildrenOf(DeltaDecl d) {
	    ArrayList<ASTNode<?>> children = new ArrayList<ASTNode<?>>();
	    for (DeltaAccess a : d.getDeltaAccessList())
	        children.add(a);
	    for (ModuleModifier m : d.getModuleModifiers())
	        children.add(m);
	    return children.toArray(EMPTY_NODES);
	}

	/**
	 * Retrieve the Children of an InterfaceDecl
	 * @param d The target InterfaceDecl
	 * @return The children of an InterfaceDecl, which are relevant for the Content Outline
	 */
	private ASTNode<?>[] getChildrenOf(InterfaceDecl d) {
		ArrayList<ASTNode<?>> pDecl = new ArrayList<ASTNode<?>>();
		for (MethodSig mD : d.getBodys()) {
			pDecl.add(mD);
		}
		return pDecl.toArray(EMPTY_NODES);
	}

	/**
	 * Retrieve the Children of a MainBlock
	 * @param d The target MainBlock
	 * @return The children of a MainBlock, which are relevant for the Content Outline
	 */
	private ASTNode<?>[] getChildrenOf(MainBlock mb){
		ArrayList<ASTNode<?>> pDecl = new ArrayList<ASTNode<?>>();
		for (VarDecl mD : mb.getVars()) {
			pDecl.add(mD);
		}
		return pDecl.toArray(EMPTY_NODES);
	}

	/**
	 * Retrieve the Children of a DataTypeDecl
	 * @param d The target DataTypeDecl
	 * @return The children of a DataTypeDecl, which are relevant for the Content Outline
	 */
	private ASTNode<?>[] getChildrenOf(DataTypeDecl d){
		ArrayList<ASTNode<?>> pDecl = new ArrayList<ASTNode<?>>();
		for (DataConstructor dc : d.getDataConstructors()){
			pDecl.add(dc);
		}
		return pDecl.toArray(EMPTY_NODES);
	}

	private boolean hasChildren(ModuleDecl m){
		return (m.getNumDecl() > 0) || (m.getNumImport() > 0) || (m.getNumExport() > 0);
	}

	private boolean hasChildren(ClassDecl c){
		return (c.getNumField() > 0) || (c.getNumMethod() > 0);
	}

	private boolean hasChildren(DeltaDecl c){
	    return c.getNumModuleModifier() > 0;
	}

	@Override
	public Object[] getElements(Object inputElement) {
		if (inputElement instanceof InternalASTNode){
			return getChildren(inputElement);
		}else if (inputElement instanceof Model
				|| inputElement instanceof CompilationUnit
				|| inputElement instanceof ModuleDecl
				|| inputElement instanceof ClassDecl
				|| inputElement instanceof InterfaceDecl
				|| inputElement instanceof DataTypeDecl
				|| inputElement instanceof MainBlock
				|| inputElement instanceof List<?>
				|| inputElement instanceof IFile) {
			throw new IllegalArgumentException("No unwrapped ASTNodes should be used. Use InternalASTnodes instead.");
		} else if (inputElement instanceof Object[]) {
			return (Object[]) inputElement;
		}
		return EMPTY_OBJECT_ARRAY;
	}

	@Override
	public boolean hasChildren(Object element) {
		if (element instanceof InternalASTNode<?>){
			InternalASTNode<?> node = ((InternalASTNode<?>) element);
			synchronized(node.getNature().modelLock){
				return hasChildren(node.getASTNode());
			}
		} else if (element instanceof IFile) {
			return hasChildren((IFile) element);
		} else if (element instanceof PackageContainer) {
			return ! ((PackageContainer) element).getPackages().isEmpty();
		} else if (element instanceof IProject) {
			return getChildrenOf((IProject) element).length > 0;
		} else if (element instanceof PackageEntry) {
			return getChildrenOf((PackageEntry) element).length > 0;
		} else if (element instanceof PackageAbsFile) {
			Object[] children = getChildrenOf((PackageAbsFile) element);
			return children != null && children.length > 0;
		}
		return false;

	}

	public boolean hasChildren(ASTNode<?> element) {
	    if (element instanceof ModuleDecl) {
	        return hasChildren((ModuleDecl) element);
	    } else if (element instanceof ClassDecl) {
	        return hasChildren((ClassDecl) element);
	    } else if (element instanceof InterfaceDecl) {
	        return ((InterfaceDecl) element).getNumBody() > 0;
	    } else if (element instanceof DataTypeDecl) {
	        return ((DataTypeDecl) element).getNumDataConstructor() > 0;
	    } else if (element instanceof MainBlock) {
	        return ((MainBlock) element).getNumVar() > 0;
	    } else if (element instanceof List<?>) {
	        return hasChilden((List<?>) element);
	    } else if (element instanceof DeltaDecl) {
	        return hasChildren((DeltaDecl) element);
	    }
	    return false;
	}

	/**
	 * Create a {@link PackageEntry} that represents an {@link PackageAbsFile}
	 * from an ABS package (as {@link IFile}) in the project.
	 *
	 * @param file
	 * @return
	 */
	private PackageEntry makeABSPackage(IFile file) {
		PackageContainer container = new PackageContainer();
		container.setProject(file.getProject());
		return new PackageEntry(container,file.getName(),file.getLocation().toString(),false);
	}

	private boolean hasChildren(IFile file){
		if(!UtilityFunctions.isABSFile(file))
			return false;

		if(UtilityFunctions.isABSPackage(file)) {
			return hasChildren(makeABSPackage(file));
		}

		AbsNature nature = UtilityFunctions.getAbsNature(file);
		if(nature != null){
			CompilationUnit cu = nature.getCompilationUnit(file);
			if(cu != null)
				return true;
		}
		return false;
	}

	private boolean hasChilden(List<?> element) {
	    List<?> nodeList = (element);
	    if (ABSContentOutlineUtils.isImportList(nodeList) || ABSContentOutlineUtils.isExportList(nodeList)){
	    	return true;
	    } else{
		return false;
	    }
	}

	@Override
	public void dispose() {
		baseProvider.dispose();
		ResourcesPlugin.getWorkspace().removeResourceChangeListener(this);
	}

	@Override
	public Object getParent(Object element) {
		if (element instanceof InternalASTNode<?>) {
			InternalASTNode<?> node = (InternalASTNode<?>) element;

			AbsNature nature = node.getNature();
			ASTNode<?> parent = node.getASTNode().getParent();
			assert nature != null;
			/* Root nodes don't have parents, so there's nothing to wrap (ABSTools #301) */
			if (parent == null) {
				assert node.getASTNode() instanceof Model : element;
				return null;
			}
			return new InternalASTNode<ASTNode<?>>(parent,nature);
		}
		return null;
	}

	@Override
	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		this.viewer = (StructuredViewer) viewer;
	}

	/** Notify viewer that content has changed.
	 * @author stolz
	 */
	@Override
	public boolean visit(IResourceDelta delta) throws CoreException {
		IResource source = delta.getResource();
		switch (source.getType()) {
		case IResource.ROOT:
		case IResource.PROJECT:
		case IResource.FOLDER:
			return true;
		case IResource.FILE:
			final IFile file = (IFile) source;
			new UIJob("Update CommonViewer") {
				@Override
                public IStatus runInUIThread(IProgressMonitor monitor) {
					if (viewer != null && !viewer.getControl().isDisposed())
						viewer.refresh(file);
					return Status.OK_STATUS;
				}
			}.schedule();
		}
		return false;
	}

	@Override
	public void resourceChanged(IResourceChangeEvent event) {
		IResourceDelta delta = event.getDelta();
		try {
			delta.accept(this);
		} catch (CoreException e) {
			Activator.logException(e);
		}
	}
}