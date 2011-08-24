package eu.hats_project.build.maven.plugin;

import org.apache.maven.plugin.logging.Log;

class DebugArgOutput {

    void debug(String message, String[] argArray, Log log) {
        StringBuilder builder = new StringBuilder();
        for (String a : argArray) { 
            builder.append(a+" ");
        }
        log.debug(message+" -->\n"+builder.toString());
    }
    
}
