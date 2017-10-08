/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.parser;

import java.io.File;
import java.io.IOException;
import java.util.jar.JarFile;

/**
 * This class represents ABS packages
 * 
 * ABS packages are JAR files that contain ABS files or other ABS packages.
 * <p>
 * ABS packages must have a special header the 
 * MANIFEST file, defined as follows:
 * 
 * <pre>
 * ABS-Package-Version: 1.0
 * </pre>
 * 
 * where the Version attribute defines the version of the ABS package format and might
 * be increased in future versions. 
 * 
 * @see JarFile
 * 
 * @author Jan Schäfer
 *
 */
public class ABSPackageFile extends JarFile {

    public static final String VERSION_ATTRIBUTE = "ABS-Package-Version";

    /**
     * Creates a new ABSPackageFile from the given File object 
     * @param file the ABS package file to be read
     * @throws IOException if the file could not be read
     */
    public ABSPackageFile(File file) throws IOException {
        super(file);
    }

    /**
     * Returns whether this JAR file is an ABS package as defined above.
     * JARs without a manifest can never be packages.
     * @return whether this JAR file is an ABS package as defined above
     * @throws IOException if this JAR file cannot be read at all
     */
    public boolean isABSPackage() throws IOException {
        return getManifest() != null && getJarVersion() != null;
    }

    /**
     * Returns the version of this ABS package as defined in the MANIFEST file.
     * @return the version of this ABS package as defined in the MANIFEST file.
     * @throws IOException if this JAR file cannot be read at all
     * @throws NullPointerException if there is no ABS section in the MANIFEST file
     */
    public String getJarVersion() throws IOException {
        assert getManifest() != null;
        return getManifest().getMainAttributes().getValue(VERSION_ATTRIBUTE);
    }
    
}
