package eu.hats_project.build.maven.plugin;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Properties;
import java.util.jar.JarFile;

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

	private static final String VERSION_ATTRIBUTE = "ABS-Package-Version";

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
		for (Artifact af : resolveDependencyArtifacts(project)) {
			if (af.getType().equals("jar") && isABSPackage(af.getFile())) {
				prop.setProperty(af.getFile().getAbsolutePath(), "true");
			}
		}

		FileOutputStream out = new FileOutputStream(dep);
		try {
			prop.storeToXML(out, null);
		} finally {
			out.close();
		}
	}

	private boolean isABSPackage(File file) throws IOException {
		return new JarFile(file).getManifest().getMainAttributes()
				.getValue(VERSION_ATTRIBUTE) != null;
	}

}
