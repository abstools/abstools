/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.editor.outline;

/**
 * Represents an entry in the ABS package dependencies. Specifically an entry
 * refers to an ABS package that may live externally to the current workspace
 * but is in the local file system.
 * 
 * @author woner
 * 
 */
public class PackageEntry {

	private final PackageContainer parent;
	private final String packageName;
	private final String absolutePath;
	private final boolean isDependency;
	
	public PackageEntry(
			PackageContainer parent,
			String packageName, 
			String absolutePath, 
			boolean isDependency) {
		
		this.parent = parent;
		this.packageName = packageName;
		this.absolutePath = absolutePath;
		this.isDependency = isDependency;
	}
	
	public PackageContainer getPackageContainer() {
		return parent;
	}
	
	public String getName() { 
		return packageName; 
	}
	
	public String getPath() { 
		return absolutePath; 
	}

	/**
	 * True if and only if this is a package managed by an external dependency
	 * management system (read-only)
	 * 
	 * @return
	 */
	public boolean isDependency() {
		return isDependency;
	}

}