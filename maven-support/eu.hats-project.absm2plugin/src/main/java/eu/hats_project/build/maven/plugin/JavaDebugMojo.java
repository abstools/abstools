package eu.hats_project.build.maven.plugin;


/**
 * 
 * A Maven 2 plugin to debug ABS test code in Java
 * 
 * @goal debugjava
 * @requiresDependencyResolution
 * @phase test
 * 
 */
public class JavaDebugMojo extends JavaTestMojo {

    private static final String[] DEBUG_PROPERTIES =
        {   
            "-Dabs.debug=true",
            "-Dabs.loglevel=finest",
            "-Dabs.systemobserver=abs.backend.java.debugging.GraphicalDebugger",
            "-Dabs.totalscheduler=abs.backend.java.scheduling.InteractiveScheduler"
        };
    
    protected void makeTest() throws Exception {
        setJVMOptions(DEBUG_PROPERTIES);
        setAppendable(System.out);
        super.makeTest();
    }

}
