package org.absmodels.abs.plugin.actions.runconfig.java;

import org.eclipse.jdt.debug.ui.launchConfigurations.JavaClasspathTab;

/**
 * Tab for setting the classpath from where foreign classes are loaded via FLI 
 */
public class JavaTabForeignLanguageInterface extends JavaClasspathTab {

    @Override
    public String getName() {
        return "FLI classpath";
    }

}