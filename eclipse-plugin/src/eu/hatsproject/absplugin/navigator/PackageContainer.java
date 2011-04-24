/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package eu.hatsproject.absplugin.navigator;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/**
 * A container for entries of ABS package dependencies. Each ABS project
 * has one or more package containers.
 * 
 * @author woner
 *
 */
public class PackageContainer {

	private final Set<PackageEntry> packages = new HashSet<PackageEntry>(); 

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

}
