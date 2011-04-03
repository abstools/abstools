package eu.hats_project.build.maven.plugin;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.apache.maven.artifact.DefaultArtifact;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.project.MavenProject;

abstract class AbstractABSMojo extends AbstractMojo {
    /**
     * The ABS source folder.
     * 
     * @parameter expression="${abs.srcFolder}"
     *            default-value="${project.basedir}/src/main/abs"
     * @required
     */
    protected File absSrcFolder;
    
    /**
     * The Maven Project.
     *
     * @parameter expression="${project}"
     * @required
     * @readonly
     */
    protected MavenProject project = null;
    
    protected File getABSFrontEnd() {
        for (Object o : project.getArtifacts()) {
            if (o instanceof DefaultArtifact) {
                DefaultArtifact a = (DefaultArtifact) o;
                return a.getFile();
            }
        }
        return null;
    }

    protected List<String> getFileNames(List<File> files) {
        List<String> res = new ArrayList<String>(files.size());
        for (File f : files) {
            res.add(f.getAbsolutePath());
        }

        return res;
    }

    protected List<File> getAbsFiles(File dir) {
        List<File> absFiles = new ArrayList<File>();
        for (File f : dir.listFiles()) {
            if (!f.isHidden() && f.canRead()) {
                if (f.isDirectory()) {
                    absFiles.addAll(getAbsFiles(f));
                } else {
                    if (f.getName().endsWith(".abs")) {
                        absFiles.add(f);
                    }
                }
            }
        }
        return absFiles;
    }


}
