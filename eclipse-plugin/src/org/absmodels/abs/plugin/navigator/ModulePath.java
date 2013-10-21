/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.navigator;

import static abs.common.Constants.STDLIB_NAME;
import static org.absmodels.abs.plugin.util.UtilityFunctions.hasDecls;

import java.util.*;

import org.absmodels.abs.plugin.builder.AbsNature;
import org.absmodels.abs.plugin.util.InternalASTNode;

import abs.frontend.ast.Model;
import abs.frontend.ast.ModuleDecl;


/**
 * A ModulePath represents the ABS module hierarchy with a specified modulePath as prefix
 * @author cseise
 *
 */
public class ModulePath {

	/**
	 * The current prefix of this ModulePath instance
	 */
	private String modulePath;
	/**
	 * AbsNature to retrieve the current {@link Model}. Additionally used for synchronization.
	 */
	final private AbsNature absNature;
	/**
	 * Cached, synchronized list of all children of a ModulPath
	 */
	private Collection<Object> internalChildren;

	/**
	 * Caching mechanism, mapping a root ModulePath to an ABSNature
	 */
	private static Map<AbsNature,ModulePath> rootPath = new HashMap<AbsNature,ModulePath>();
	
	/**
	 * Creates a new ModulePath
	 * 
	 * @param nature
	 *            The AbsNature this ModulePath is bound to. If the given
	 *            AbsNature is null, an IllegalArgumentException will be thrown.
	 * @param initialModulePath
	 *            The prefix of this modulePath. Used for calculating its
	 *            children. The modulePath can also be an empty String
	 *            indicating that is ModulePath instance represents the module
	 *            root
	 * @throws IllegalArgumentException if nature is null
	 */
	public ModulePath(AbsNature nature, String initialModulePath){
		if (nature == null) {
			throw new IllegalArgumentException("A Module Path can only be instantiated with an ABSNature != null!");
		}
		absNature = nature;
		modulePath = initialModulePath;
		//To avoid race conditions, internalChildren is synchronized
		internalChildren = Collections.synchronizedCollection((List<Object>)new ArrayList<Object>());
	}

	/**
	 * @return The current module prefix of the ModulePath instance
	 */
	public String getModulePath() {
		return modulePath;
	}
	
	/**
	 * 
	 * @return The {@link AbsNature} stored in this ModulePath instance. The
	 *         return value should never be null.
	 */
	public AbsNature getNature() {
		return absNature;
	}

	/**
	 * Calculates the Children of the ModulePath.
	 * 
	 * @return The children of this ModulePaths, which are both subordinate to
	 *         ModulePaths and ModuleDecls. The list will be empty, if the
	 *         ModulePath's nature is null.
	 */
	public List<Object> getChildModulePathsAndModuleDecls() {
		Model model = absNature.getCompleteModel();
		
		if (model != null) {

			ArrayList<Object> paths = new ArrayList<Object>();
			Set<ModulePath> names;

			synchronized (absNature.modelLock) {
				names = getModuleHierarchyForPrefix(modulePath);
			}
			paths.addAll(names);

			// There was a change in the modulePath structure
			if (internalChildren.isEmpty() || !internalChildren.containsAll(paths) || !paths.containsAll(internalChildren)) {			
				//Adapt internalChildren to model
				addNewModulePaths(paths);			
				removeDeletedModulePaths(paths);
			}

			return addModulesWithoutHierarchy(internalChildren);

		}

		return Collections.emptyList();

	}

	/**
	 * Removes ModulePaths from the internalChildren Collection that are specified by paths
	 * @param paths
	 */
	private void removeDeletedModulePaths(ArrayList<Object> paths) {
		ArrayList<ModulePath> removeModulePaths= new ArrayList<ModulePath>();
		for (Object mp : internalChildren) {
			if (!paths.contains(mp) && mp instanceof ModulePath) {
				removeModulePaths.add((ModulePath) mp);
			}
		}
		
		internalChildren.removeAll(removeModulePaths);
	}

	/**
	 * Removes ModulePaths from the internalChildren Collection that are specified by paths
	 * @param paths
	 */
	private void addNewModulePaths(ArrayList<Object> paths) {
		ArrayList<Object> addModulePaths= new ArrayList<Object>();
		for (Object path : paths) {
			if (!internalChildren.contains(path)) {
				addModulePaths.add(path);
			}
		}
		
		internalChildren.addAll(addModulePaths);
	}
	
	/**
	 * Helper method for adding modules without any module hierarchy, that means
	 * their name does not contain '.'
	 * 
	 * @param moduleHierarchy
	 * @return union of moduleHierarchy and the list of modules that do not contain '.'
	 */
	private List<Object> addModulesWithoutHierarchy(Collection<Object> moduleHierarchy) {
		ArrayList<Object> ret = new ArrayList<Object>();

		ret.addAll(moduleHierarchy);
		synchronized (absNature.modelLock) {
			ret.addAll(getModulesForPrefix());
		}

		return ret;
	}
	
	/**
	 * Calculates the <b>first</b> layer (prefix := "") of the module hierarchy
	 * and caches its results
	 * 
	 * @param nature The ABSNature whose root hierarchy should be calculated
	 * @return The children of the modulePath with prefix = ""
	 * @throws IllegalArgumentException when the nature is null
	 */
	public static List<Object> getRootHierarchy(AbsNature nature){
		ModulePath root;

		if (rootPath.containsKey(nature)) {
			root = rootPath.get(nature);
		} else {
			root = new ModulePath(nature, "");
			rootPath.put(nature, root);
		}

		return root.getChildModulePathsAndModuleDecls();
	}
	
	/**
	 * Clears the cache maintained by getRootHierarchy
	 */
	public static void clearRootHierarchyCache() {
		rootPath = new HashMap<AbsNature, ModulePath>();
	}

	/**
	 * Collects ModuleDecls with a given prefix. Only ModuleDecls with a name
	 * prefix.Name are returned, ModuleDecls with a name prefix.subPrefix.Name
	 * are ignored. 
	 * 
	 * prefix is the current module path of this instance.
	 * 
	 * @return Only ModuleDecls with a name prefix.Name are returned,
	 *         ModuleDecls with a name prefix.subPrefix.Name are ignored
	 */	
	public Set<InternalASTNode<ModuleDecl>> getModulesForPrefix() {
		LinkedHashSet<InternalASTNode<ModuleDecl>> names = new LinkedHashSet<InternalASTNode<ModuleDecl>>();
		Model model = absNature.getCompleteModel();

		if ("".equals(modulePath)) {
			// Get the first category of module elements

			for (ModuleDecl m : model.getModuleDecls()) {
				String name = m.getName();
				if (!name.equals(STDLIB_NAME)) {

					if (name.indexOf('.') < 0 &&
					 !hasSubModule(name) &&
					 !name.equals(modulePath) &&
					 !names.contains(m)) { // FIXME: Review, can a ModuleDecl really be inside Set<nternalASTNode<..>>? GC_UNRELATED_TYPES
							names.add(new InternalASTNode<ModuleDecl>(m, absNature));
					}
				}
			}
			return names;
		} else {
			for (ModuleDecl m : model.getModuleDecls()) {
				String name = m.getName();

				if (!name.equals(STDLIB_NAME)) {

					String regex = NavigatorUtils.buildRegex(modulePath);

					if ((name.matches(regex) && !existsSubLayer(name)) && !hasSubModule(name)) {
						names.add(new InternalASTNode<ModuleDecl>(m, absNature));
					}

				}
			}
			return names;
		}
	}

	private Set<ModulePath> getModuleHierarchyForPrefix(String prefix) {
		HashSet<String> names = new HashSet<String>();
		Model model = absNature.getCompleteModel();

		if ("".equals(prefix)) {
			// Get the first category of module elements

			for (String name : getModuleNames(model)) {
				if (!name.equals(STDLIB_NAME)){
				
				
				if (name.indexOf('.') > -1) {
					String subName = name.substring(0, name.indexOf('.'));
					if (!names.contains(subName)){
						names.add(subName);
					}
				}
				
				}
			}
			
			HashSet<ModulePath> paths = new HashSet<ModulePath>();
			for (String name : names){
				paths.add(new ModulePath(absNature, name));
			}
			return paths;
		} else {

			for (String name : getModuleNames(model)) {
				if (name.startsWith(prefix + '.')) {
					String modWithoutPrefix = name.substring(prefix.length() + 1, name.length());

					if (modWithoutPrefix.indexOf('.') > -1) {
						String afterPrefix = modWithoutPrefix.substring(0, modWithoutPrefix.indexOf('.'));
						names.add(prefix + "." + afterPrefix);
					}
				}
			}
			
			HashSet<ModulePath> paths = new HashSet<ModulePath>();
			for (String name : names){
				paths.add(new ModulePath(absNature, name));
			}
			return paths;
		}
	}

	private boolean hasSubModule(String name){
		for (String moduleName : getModuleNames(absNature.getCompleteModel())){
			if (moduleName.startsWith(name+".")){
				return true;
			}
		}
		return false;
	}
	private boolean existsSubLayer(String name) {
		return (getModuleHierarchyForPrefix(name).size() > 0);
	}
		
	/**
	 * Determines, whether the AbsNature's Model contains a ModuleDecl with
	 * exactly the ModulePath's name that has any kind of declarations
	 * 
	 * @return True, if the Model contains a ModuleDecl with exactly the
	 *         ModulePath's name having declarations <br/>
	 *         False, else
	 */
	public boolean hasModuleWithDecls() {
		synchronized (absNature.modelLock) {
			for (ModuleDecl m : absNature.getCompleteModel().getModuleDecls()) {

				if (m.getName().equals(modulePath) && hasDecls(m)) {
					return true;
				}
			}
		}
		return false;
	}
	
	/**
	 * Determines, whether the ModulePaths AbsNature (respectively its
	 * associated Model) contains a ModuleDecl with exactly the ModulePath's
	 * name
	 * 
	 * @return True, if the Model contains a ModuleDecl with exactly the
	 *         ModulePath's <br/>
	 *         False, else
	 */
	public boolean hasModule() {
		synchronized (absNature.modelLock) {
			for (ModuleDecl m : absNature.getCompleteModel().getModuleDecls()) {

				if (m.getName().equals(modulePath)) {
					return true;
				}
			}
		}
		return false;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((absNature == null) ? 0 : absNature.hashCode());
		result = prime * result + ((modulePath == null) ? 0 : modulePath.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof ModulePath))
			return false;
		ModulePath other = (ModulePath) obj;
		if (absNature == null) {
			if (other.absNature != null)
				return false;
		} else if (!absNature.equals(other.absNature))
			return false;
		if (modulePath == null) {
			if (other.modulePath != null)
				return false;
		} else if (!modulePath.equals(other.modulePath))
			return false;
		return true;
	}
	
	
	
	/**
	 * @return The ModuleDecl with exactly the name of the ModulePath, or null
	 *         if no such ModuleDecl exists
	 */
	public InternalASTNode<ModuleDecl> getModuleDecl() {
		synchronized (absNature.modelLock) {
			Model model = absNature.getCompleteModel();

			for (ModuleDecl m : model.getModuleDecls()) {
				if (m.getName().equals(modulePath)) {
					return new InternalASTNode<ModuleDecl>(m,absNature);
				}
			}
		}

		return null;
	}

	@Override
	public String toString(){
		return this.modulePath;
	}

	/**
	 * @param Model model
	 * @return A String array with the names of all the model's ModuleDecls
	 */
	private String[] getModuleNames(Model model) {
		ArrayList<String> list = new ArrayList<String>();
		
		synchronized (absNature.modelLock) {
			for (ModuleDecl m : model.getModuleDecls()) {
				list.add(m.getName());
			}
		}
		return list.toArray(new String[0]);
	}
}
