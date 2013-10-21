/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.util;

import java.net.URL;

import org.absmodels.abs.plugin.Activator;
import org.absmodels.abs.plugin.editor.outline.ABSContentOutlineUtils;
import org.absmodels.abs.plugin.editor.outline.PackageAbsFile;
import org.absmodels.abs.plugin.editor.outline.PackageContainer;
import org.absmodels.abs.plugin.editor.outline.PackageEntry;
import org.absmodels.abs.plugin.editor.outline.ABSContentOutlineConstants.AnnotationType;
import org.absmodels.abs.plugin.navigator.ModulePath;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.osgi.framework.Bundle;

import abs.frontend.ast.*;

public abstract class Images {
	/**
	 * ISharedImages for retrieving ImageDescriptors for Eclipse IDE default icons.
	 */
	private final static ISharedImages sharedImages = PlatformUI.getWorkbench().getSharedImages();
	//-------------------- ICONS ----------------------------------------------------------------------------
	/**
	 * Empty icon
	 */
	public final static Image NO_IMAGE = createIcon("transparent.gif");
	/**
	 * The standard icon for a function declaration
	 */
	public final static Image FUNCTION_IMAGE = createIcon("abs_function.gif");
	/**
	 * The standard icon for a datatype declaration
	 */
	public final static Image TYPE_IMAGE = createIcon("abs_datatype.gif");
	/**
	 * The standard icon for a class declaration
	 */
	public final static Image CLASS_IMAGE = createIcon("abs_class.gif");
	/**
	 * The standard icon for a COG class declaration
	 */
	public final static Image COG_CLASS_IMAGE = createIcon("abs_cog_class.gif");
	/**
	 * The standard icon for a Plain class declaration
	 */
	public final static Image PLAIN_CLASS_IMAGE = createIcon("abs_plain_class.gif");	
	/**
	 * The standard icon for a field declaration
	 */
	public final static Image FIELD_IMAGE = createIcon("abs_field.gif");
	/**
	 * The standard icon for a method declaration
	 */
	public final static Image METHOD_IMAGE = createIcon("abs_method.gif");
	/**
	 * The standard icon for an interface declaration
	 */
	public final static Image INTERFACE_IMAGE = createIcon("abs_interface.gif");
	/**
	 * The standard icon for a module declaration
	 */
	public final static Image MODULE_IMAGE = createIcon("abs_module.gif");
	/**
	 * The standard icon for an empty module declaration
	 */
	public final static Image MODULE_EMPTY_IMAGE = createIcon("abs_module_empty.gif");	
	/**
	 * The standard icon for an import declaration
	 */
	public final static Image IMPORT_IMAGE = createIcon("abs_import.gif");
	/**
	 * The standard icon for an export declaration
	 */
	public final static Image EXPORT_IMAGE = createIcon("abs_export.gif");
	/**
	 * The standard icon for a main block
	 */
	public final static Image MAIN_BLOCK_IMAGE = createIcon("abs_mainblock.gif");
	/**
	 * The standard icons for the Import list
	 */
	public final static Image IMPORTS_IMAGE = createIcon("abs_imports.gif");
	/**
	 * The standard icon for the Exports list
	 */
	public final static Image EXPORTS_IMAGE = createIcon("abs_exports.gif");
	/**
	 * The standard icon for a Data constructor
	 */
	public final static Image DATACONSTRUCTOR_IMAGE = createIcon("abs_constructor.gif");
	/**
	 * The standard icon for package dependencies library
	 */	
	public final static Image PACKAGE_CONTAINER_IMAGE = createIcon("abs_packages.gif");
	/**
	 * The standard icon for an ABS package
	 */	
	public final static Image PACKAGE_IMAGE = createIcon("abs_package.gif");
	/**
	 * The standard icon for an ABS file in an ABS package
	 */	
	public final static Image PACKAGE_ABS_FILE_IMAGE = createIcon("absicon.gif");
	/**
	 * The standard icon for an open ABS Project
	 */	
	public final static Image PROJECT_IMAGE = createStandardIcon(org.eclipse.ui.ide.IDE.SharedImages.IMG_OBJ_PROJECT);
	/**
	 * The standard icon for a closed ABS Project
	 */
	public final static Image CLOSED_PROJECT_IMAGE = createStandardIcon(org.eclipse.ui.ide.IDE.SharedImages.IMG_OBJ_PROJECT_CLOSED);
	/**
	 * The standard icon for error markers.
	 * [stolz] Workaround for https://bugs.eclipse.org/bugs/show_bug.cgi?id=383810 on Juno/4.2.0
	 */
	public final static Image ERROR_MARKER = createIcon("error_co.gif");
	/**
	 * The standard icon for folder markers
	 */
	public final static Image FOLDER_MARKER = createIcon("abs_project_logo.gif");	
        /**
         * The standard icon for a delta declaration
         */
        public final static Image DELTA_IMAGE = createIcon("abs_delta.gif");
        /**
         * The standard icon for a product line declaration
         */
        public final static Image PRODUCTLINE_IMAGE = createIcon("abs_productline.gif");
        /**
         * The standard icon for a product declaration
         */
        public final static Image PRODUCT_IMAGE = createIcon("abs_product.gif");

	//Icons for the debug perspective
	public final static Image DEBUGGER_INTERACTIVE           = createIcon("debug/debug_mode_interactive.gif");
	public final static Image DEBUGGER_RANDOM                = createIcon("debug/debug_mode_random.gif");
	public final static Image DEBUGGER_HISTORY                = createIcon("debug/debug_mode_history.gif");
	
	public final static Image DEBUGGER_RESUME                = createIcon("debug/resume.gif");
	
	public final static Image DEBUGGER_PROGRAM               = createIcon("abs_logo.png");
	
	public final static Image DEBUGGER_COG                   = createIcon("debug/cog.gif");
	public final static Image DEBUGGER_COG_SUSPENDED         = createIcon("debug/cog_suspend.gif");
	public final static Image DEBUGGER_COG_INACTIVE          = createIcon("debug/cog_inactive.gif");
	
	public final static Image DEBUGGER_OBJECTS               = createIcon("debug/objects.gif");
	public final static Image DEBUGGER_OBJECT                = createIcon("debug/object.gif");
	
	public final static Image DEBUGGER_TASKS                 = createIcon("debug/tasks.gif");
	public final static Image DEBUGGER_TASKS_INACTIVE        = createIcon("debug/tasks_inactive.gif");
	
	public final static Image DEBUGGER_TASK_READY            = createIcon("debug/task.gif");
	public final static Image DEBUGGER_TASK_SUSPENDED        = createIcon("debug/task_suspended.gif");
	public final static Image DEBUGGER_TASK_RUNNING          = createIcon("debug/task_running.gif");
	public final static Image DEBUGGER_TASK_FINISHED         = createIcon("debug/task_inactive.gif");
	public final static Image DEBUGGER_TASK_DEADLOCKED       = createIcon("debug/task_deadlock.gif");
	public final static Image DEBUGGER_TASK_ASSERTION_FAILED = createIcon("debug/task_warning.gif");
	public final static Image DEBUGGER_TASK_EXCEPTION        = createIcon("debug/task_exception.gif");
	public final static Image DEBUGGER_TASK_BLOCKED          = createIcon("debug/task_blocked.gif");
	
	public final static Image DEBUGGER_STACK_FRAME           = createIcon("debug/stckframe_obj.gif");
	/**
	 * Retrieves an image specified by a path <b>LOCAL</b> to the plugin  
	 * @param path The Path of the image
	 * @return The desired Image or a default image, if the desired image was not found;
	 */
	public static Image createImage(String path){
		Bundle bundle = Activator.getDefault().getBundle();
		assert bundle != null;
		IPath imagepath = new Path(path);
		URL imageUrl = FileLocator.find(bundle, imagepath,null);
		ImageDescriptor id = ImageDescriptor.createFromURL(imageUrl);
		Image im = id.createImage();
		return im;
	}

	/**
	 * Retrieves an Eclipse IDE default image, specified by its
	 * {@link ISharedImages} constant.
	 * 
	 * @param sharedImage
	 *            The ISharedImages constant specifying the desired Image
	 * @return The desired image, if the image could be loaded, or a default
	 *         Image on the occurrence of errors.
	 */
	public static Image createStandardIcon(String sharedImage) {
		ImageDescriptor id = sharedImages.getImageDescriptor(sharedImage);
		return id.createImage(true);
	}
	
	/**
	 * Retrieves an icon specified by its file name <b>LOCAL</b> to the <code>ICON_PATH</code>
	 * @see #createIcon(String)  
	 * @param path Path of the Icon relative to <code>ICON_PATH</code>
	 * @return The desired Image or a default image, if the desired image was not found;
	 */
	public static Image createIcon(String path){
		return createImage(Constants.ICON_PATH+path);
	}
	
	/**
	 * Gives an icon for the corresponding <code>{@link ASTNode}</code>
	 * @param node The target node
	 * @return The icon of the target node. If the icon for the target node cannot be determined,
	 * 	       an empty image will be returned
	 */
	public static Image getImageForASTNode(ASTNode<?> node){
		if (node instanceof FunctionDecl) {
			return FUNCTION_IMAGE;
		} else if (node instanceof ClassDecl) {
			if (UtilityFunctions.hasClassAnnotation((ClassDecl) node, AnnotationType.COG_ANNOTATION)){
				return COG_CLASS_IMAGE;
			}else if (UtilityFunctions.hasClassAnnotation((ClassDecl) node,AnnotationType.PLAIN_ANNOTATION)){
				return PLAIN_CLASS_IMAGE;
			} else{
				return CLASS_IMAGE;
			}
		} else if (node instanceof TypedVarOrFieldDecl) {
			return FIELD_IMAGE;
		} else if (node instanceof MethodImpl
				|| node instanceof MethodSig) {
			return METHOD_IMAGE;
		} else if (node instanceof InterfaceDecl) {
			return INTERFACE_IMAGE;
		} else if (node instanceof TypeDecl) {
			return TYPE_IMAGE;
		} else if (node instanceof ModuleDecl) {		
			return UtilityFunctions.hasDecls(((ModuleDecl) node)) ? MODULE_IMAGE : MODULE_EMPTY_IMAGE;
		} else if (node instanceof Import) {
			return IMPORT_IMAGE;
		} else if (node instanceof Export) {
			return EXPORT_IMAGE;
		} else if (node instanceof MainBlock){
			return MAIN_BLOCK_IMAGE;
		} else if (node instanceof List<?>){
			if (ABSContentOutlineUtils.isImportList((List<?>)node)){
				return IMPORTS_IMAGE;
			}else if (ABSContentOutlineUtils.isExportList((List<?>)node)){
				return EXPORTS_IMAGE;
			}
		} else if (node instanceof DataConstructor){
			return DATACONSTRUCTOR_IMAGE;
		} else if (node instanceof ProductLine) {
		    return PRODUCTLINE_IMAGE;
		} else if (node instanceof Product) {
		    return PRODUCT_IMAGE;
		} else if (node instanceof DeltaDecl) {
		    return DELTA_IMAGE;
		} else if (node instanceof ClassModifier) {
		    return CLASS_IMAGE;
		} else if (node instanceof InterfaceModifier) {
		    return INTERFACE_IMAGE;
		} else if (node instanceof TypeSynModifier) {
		    return TYPE_IMAGE;
		} else if (node instanceof DataTypeModifier) {
		    return TYPE_IMAGE;
		} else if (node instanceof FunctionModifier) {
		    return FUNCTION_IMAGE;
		}
		return NO_IMAGE;
	}
	
	/**
	 * Gives an icon for the corresponding Object. If the Object is an
	 * <code>{@link ASTNode}</code> or an
	 * <code>{@link InternalASTNode}, {@link #getImageForASTNode} will be used to
	 * get the icon.
	 * 
	 * If the Object is a ModulePath a respective icons is returned. 
	 * This icon is a different for ModulePaths with and without module declarations.  
	 * 
	 * If the Object is not an <code>{@link ASTNode}</code>, an empty icon is
	 * returned.
	 * 
	 * @param obj
	 *            The target object
	 * @return The icon of the object. If the icon for the target node cannot be
	 *         determined, an empty image will be returned
	 */
	public static Image getImageForObject(Object obj) {
		if (obj instanceof ASTNode<?>) {
			return getImageForASTNode((ASTNode<?>) obj);
		}else if (obj instanceof InternalASTNode<?>){
			return getImageForASTNode(((InternalASTNode<?>) obj).getASTNode()); 
		}else if (obj instanceof ModulePath) {
			ModulePath mp = (ModulePath)obj;
			if (mp.hasModuleWithDecls()){
				return MODULE_IMAGE;
			}else{
				return MODULE_EMPTY_IMAGE;
			}
		} else if (obj instanceof PackageContainer) {
			return PACKAGE_CONTAINER_IMAGE;
		} else if (obj instanceof PackageEntry) {
			return PACKAGE_IMAGE;
		} else if (obj instanceof PackageAbsFile) {
			return PACKAGE_ABS_FILE_IMAGE;
		} else {
			return WorkbenchLabelProvider.getDecoratingWorkbenchLabelProvider().getImage(obj);
		}
	}
	
}
