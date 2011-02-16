package eu.hatsproject.absplugin.decorators;

import static eu.hatsproject.absplugin.util.Constants.MARKER_TYPE;
import static eu.hatsproject.absplugin.util.Constants.MODULE_DECORATOR_ID;
import static eu.hatsproject.absplugin.util.Images.ERROR_MARKER;

import java.util.List;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.DecorationOverlayIcon;
import org.eclipse.jface.viewers.IDecoration;
import org.eclipse.jface.viewers.ILightweightLabelDecorator;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.LabelProviderChangedEvent;
import org.eclipse.ui.PlatformUI;

import abs.frontend.analyser.SemanticError;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.CompilationUnit;
import abs.frontend.ast.Model;
import abs.frontend.ast.ModuleDecl;
import abs.frontend.parser.ParserError;
import eu.hatsproject.absplugin.builder.AbsNature;
import eu.hatsproject.absplugin.editor.outline.ABSContentOutlineUtils;
import eu.hatsproject.absplugin.navigator.ModulePath;
import eu.hatsproject.absplugin.util.InternalASTNode;
import eu.hatsproject.absplugin.util.UtilityFunctions;
import eu.hatsproject.absplugin.util.UtilityFunctions.EditorPosition;

/**
 * Class for decorating {@link ModuleDecl} and {@link ModulePath} instances with
 * error markers
 * 
 * @author cseise
 * 
 */
public class ModuleDecorator extends LabelProvider implements ILightweightLabelDecorator {

	private final static ImageDescriptor ERROR_MARKER_DESCRIPTOR = DecorationOverlayIcon.createFromImage(ERROR_MARKER);
	

	/**
	 * {@inheritDoc}
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void decorate(Object element, IDecoration decoration) {
		if (element instanceof InternalASTNode<?>) {
			InternalASTNode<?> node = (InternalASTNode<?>) element;
			if (InternalASTNode.hasASTNodeOfType(node, ModuleDecl.class)){
				checkModuleDecl(decoration, (InternalASTNode<ModuleDecl>) node);	
			}
		} else if (element instanceof ModulePath) {
			ModulePath m = (ModulePath) element;
			checkModulePath(decoration, m);
		} else if (element instanceof IProject) {
			IProject project = (IProject) element;
			checkProject(project, decoration);
		}
	}

	private void checkProject(IProject project, IDecoration decoration) {
		try {

			if (project.isAccessible()) {
				IMarker[] markers = getABSErrorMarkers(project);

				if (markers.length > 0) {
					addErrorOverlay(decoration);
				}
			}
		} catch (CoreException e) {
			e.printStackTrace();
		}
	}

	private void checkModulePath(IDecoration decoration, ModulePath m) {
		AbsNature nature = m.getNature();
		Model model = nature.getCompleteModel();
		if (model != null) {
			for (ModuleDecl mod : model.getModuleDecls()) {
				if (mod.getName().startsWith(m.getModulePath()+".")){
						if (hasModuleDeclErrors(mod, nature)) {
							addErrorOverlay(decoration);
							return;
						}
					
				}
			}
		}
	}

	private void checkModuleDecl(IDecoration decoration, InternalASTNode<ModuleDecl> modDecl) {
		AbsNature nature = ABSContentOutlineUtils.getNatureForObject(modDecl);
		if (nature != null && hasModuleDeclErrors(modDecl.getASTNode(), modDecl.getNature())) {
			addErrorOverlay(decoration);
		}
	}

	private IMarker[] getABSErrorMarkers(Object element) throws CoreException {
		if (element instanceof IProject) {
			IProject project = (IProject) element;
			IMarker[] markers = project.findMarkers(MARKER_TYPE, true, IResource.DEPTH_INFINITE);
			return markers;
		}else{
			return new IMarker[0];
		}
	}

	private void addErrorOverlay(IDecoration decoration) {
		decoration.addOverlay(ERROR_MARKER_DESCRIPTOR, IDecoration.BOTTOM_LEFT);
	}

	/**
	 * Determines if a module declaration has any errors
	 * 
	 * @param m
	 *            the module declaration
	 * @param nature
	 *            the ABS nature
	 * @return TRUE if the module declaration has errors, FALSE if not or m or
	 *         nature is null
	 */
	public boolean hasModuleDeclErrors(ModuleDecl m, AbsNature nature) {
		synchronized (nature.modelLock) {
			if (m != null) {
				CompilationUnit cu = UtilityFunctions.getCompilationUnitOfASTNode((ModuleDecl) m);
				EditorPosition pos = UtilityFunctions.getPosition(m);
				int startLine = pos.getLinestart();
				int endLine = pos.getLineend();

				SemanticErrorList list = new SemanticErrorList();
				List<ParserError> parserErrors = cu.getParserErrors();

				list = cu.getModel().getTypeErrors();
				
				if (checkParserErrorRange(startLine, endLine, parserErrors)){
					return true;
				}else{
					return checkSemanticErrorRange(list,cu,startLine,endLine,nature);
				}
			}
			return false;
		}
	}

	/**
	 * Fires a LabelProviderChangeEvent to force re-decoration.
	 * This is a workaround for circumventing decorator update problems.
	 */
	public void refresh(){
		boolean isEnabled = PlatformUI.getWorkbench().getDecoratorManager().getEnabled(MODULE_DECORATOR_ID);
		
		if (isEnabled){
			this.fireLabelProviderChanged(new LabelProviderChangedEvent(this));
		}
	}
	
	private boolean checkSemanticErrorRange(SemanticErrorList list, CompilationUnit c, int startLine, int endLine, AbsNature nature) {
		synchronized (nature.modelLock) {
			if (list != null) {
				if (list.size() > 0) {
					for (SemanticError err : list) {
						ASTNode<?> node = err.getNode();
						int line = err.getLine();
						CompilationUnit cu = UtilityFunctions.getCompilationUnitOfASTNode(node);
						if (c == cu && checkLine(line, startLine, endLine)) {
							return true;
						}
					}
				}
			}
		}
		return false;
	}

	private boolean checkParserErrorRange(int startLine, int endLine, List<ParserError> parserErrors) {
		if (parserErrors.size() > 0) {
			for (ParserError err : parserErrors) {
				int line = err.getLine();
				return checkLine(line, startLine, endLine);
			}

		}
		return false;
	}

	private boolean checkLine(int line, int startLine, int endLine) {
		if (startLine <= line && endLine >= line) {
			return true;
		}
		
		return false;
	}

}
