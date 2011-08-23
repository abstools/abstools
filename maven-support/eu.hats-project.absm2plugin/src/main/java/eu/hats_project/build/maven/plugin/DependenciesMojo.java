package eu.hats_project.build.maven.plugin;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.HashSet;
import java.util.Properties;
import java.util.Set;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.plugin.MojoFailureException;

/**
 * This mojo provides the goal configs that adds all ABS dependencies of the
 * this Maven project to a .dependencies file that is to be used in the Eclipse
 * environment.
 * 
 * @author woner
 * @goal configs
 * 
 */
public class DependenciesMojo extends AbstractABSMojo {

	/**
	 * @parameter default-value="${basedir}"
	 */
	private File workingDir;

	@Override
	protected void doExecute() throws Exception {

		if (!workingDir.exists() && !workingDir.mkdirs()) {
			throw new MojoFailureException("Cannot create working directory "
					+ workingDir);
		}

		File dep = new File(workingDir, ".dependencies");

		try {
			if (!dep.exists() && !dep.createNewFile()) {
				throw new MojoFailureException(
						"Cannot create .dependencies configuration file");
			}
		} catch (IOException e) {
			throw new MojoFailureException(
					"Cannot create .dependencies configuration file", e);
		}

		Properties prop = new Properties();
		getLog().debug("Generating ABS Dependencies -->");
		setPackages(new HashSet<Artifact>(),prop,resolveDependencyArtifacts(project));

		FileOutputStream out = new FileOutputStream(dep);
		try {
			prop.storeToXML(out, null);
		} finally {
			out.close();
		}
	}
	
	private void setPackages(Set<Artifact> added, 
	        Properties prop, Set<Artifact> as) throws Exception {
		if (as.isEmpty()) 
			return;
		
		for (Artifact a : as) {
			if (a.getType().equals("jar") && isABSPackage(a.getFile())) {
			    String path = a.getFile().getAbsolutePath();
			    getLog().debug(path);
			    prop.setProperty(path, "true");
			    added.add(a);
			    Set<Artifact> ra = resolveArtifactDependencies(a);
			    if (added.containsAll(ra)) {
			        continue;
			    }
			    setPackages(added,prop,ra);
			}
		}
	}

}
