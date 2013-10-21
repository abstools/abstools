/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.editor.outline;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.resources.IProject;

/**
 * A container for entries of ABS package dependencies. Each ABS project
 * has one or more package containers.
 * 
 * @author woner
 *
 */
public class PackageContainer {

	private final Set<PackageEntry> packages = new HashSet<PackageEntry>();
	private IProject project; 

	public void setProject(IProject project) {
		this.project = project;
	}
	
	public void addPackages(Set<PackageEntry> packages) {
		this.packages.addAll(packages);
	}
	
	public void setPackages(Set<PackageEntry> packages) {
		this.packages.clear();
		this.packages.addAll(packages);
	}
	
	public Set<PackageEntry> getPackages() {
		return Collections.unmodifiableSet(this.packages);
	}

	public IProject getProject() {
		return project;
	}
	
	public void clear() {
		packages.clear();
	}

}
