/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.editor;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;

import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.core.runtime.content.IContentDescription;
import org.eclipse.core.runtime.content.ITextContentDescriber;

public class ABSContentTypeDescriptor implements ITextContentDescriber {

	public ABSContentTypeDescriptor() {
	}

	@Override
	public int describe(InputStream contents, IContentDescription description)
			throws IOException {
		return INDETERMINATE;
	}

	@Override
	public QualifiedName[] getSupportedOptions() {
		return new QualifiedName[0];
	}

	@Override
	public int describe(Reader contents, IContentDescription description)
			throws IOException {
		return INDETERMINATE;
	}

}
