package eu.hats_project.build.maven.plugin;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.jar.JarFile;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.DefaultArtifact;
import org.apache.maven.artifact.factory.ArtifactFactory;
import org.apache.maven.artifact.metadata.ArtifactMetadataSource;
import org.apache.maven.artifact.repository.ArtifactRepository;
import org.apache.maven.artifact.resolver.ArtifactCollector;
import org.apache.maven.artifact.resolver.ArtifactNotFoundException;
import org.apache.maven.artifact.resolver.ArtifactResolutionException;
import org.apache.maven.artifact.resolver.ArtifactResolver;
import org.apache.maven.artifact.resolver.filter.AndArtifactFilter;
import org.apache.maven.artifact.resolver.filter.ArtifactFilter;
import org.apache.maven.artifact.resolver.filter.ScopeArtifactFilter;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.MavenProjectBuilder;
import org.apache.maven.project.ProjectBuildingException;
import org.apache.maven.project.artifact.InvalidDependencyVersionException;
import org.apache.maven.shared.dependency.tree.DependencyTreeBuilder;

abstract class AbstractABSMojo extends AbstractMojo {

    public static final String[] ABS_EXTENSIONS = {".abs", ".mtvl"};
    public static final String[] LEGAL_ABS = {".abs"};
    public static final String ABS_GROUPID = "eu.hats-project";
    public static final String ABS_FRONTEND_ARTIFACTID = "absfrontend";
    public static final String MTVL_ARTIFACTID = "mTVL";
    public static final String VERSION_ATTRIBUTE = "ABS-Package-Version";

    /**
     * @parameter expression="${abs.check.selection}" default-value=true
     */
    protected boolean checkProductSelection;
    
    /**
     * @parameter expression="${abs.stdlib}" default-value=true
     */
    protected boolean stdlib;
    
    /**
     * @parameter expression="${abs.verbose}" default-value=false
     */
    protected boolean verbose;
    
    /**
     * @parameter expression="${abs.loctype}" default-value=false
     */
    protected boolean loctype;
    
    /**
     * Product selection
     * @parameter expression="${abs.product}"
     */
    protected String productName;
    
    /**
     * The ABS source folder.
     * 
     * @parameter expression="${abs.srcFolder}"
     *            default-value="${project.basedir}/src/main/abs"
     * @required
     */
    protected File absSrcFolder;
    
    /**
     * The ABS test source folder.
     * 
     * @parameter expression="${abs.test.srcFolder}"
     *            default-value="${project.basedir}/src/test/abs/"
     */
    protected File absTestSrcFolder;
    
    /**
     * @parameter expression="${project}"
     * @required
     * @readonly
     */
    protected MavenProject project;

    /**
     * Used to look up Artifacts in the remote repository.
     * 
     * @component
     * @required
     * @readonly
     */
    protected ArtifactFactory factory;

    /**
     * Used to look up Artifacts in the remote repository.
     * 
     * @component
     * @required
     * @readonly
     */
    protected ArtifactResolver resolver;
    /**
     * Location of the local repository.
     * 
     * @parameter expression="${localRepository}"
     * @readonly
     * @required
     */
    protected ArtifactRepository localRepo;

    /**
     * List of Remote Repositories used by the resolver
     * 
     * @parameter expression="${project.remoteArtifactRepositories}"
     * @readonly
     * @required
     */
    protected List<?> remoteRepos;

    /**
     * Jvm Arguments.
     * 
     * @parameter
     */
    protected String[] jvmArgs;

    /**
     * compiler additionnals arguments
     * 
     * @parameter
     */
    protected String[] args;

    /**
     * Display the command line called ?
     * 
     * @required
     * @parameter expression="${displayCmd}" default-value="false"
     */
    public boolean displayCmd;

    /**
     * Forks the execution into a separate process.
     * 
     * @parameter default-value="true"
     */
    protected boolean fork = true;

    /**
     * Force the use of an external ArgFile to run any forked process.
     * 
     * @parameter default-value="false"
     */
    protected boolean forceUseArgFile = false;

    /**
     * Artifact factory, needed to download source jars.
     * 
     * @component
     * @required
     * @readonly
     */
    protected MavenProjectBuilder mavenProjectBuilder;

    /**
     * The artifact metadata source to use.
     * 
     * @component
     * @required
     * @readonly
     */
    private ArtifactMetadataSource artifactMetadataSource;

    /**
     * The artifact collector to use.
     * 
     * @component
     * @required
     * @readonly
     */
    private ArtifactCollector artifactCollector;

    /**
     * The dependency tree builder to use.
     * 
     * @component
     * @required
     * @readonly
     */
    private DependencyTreeBuilder dependencyTreeBuilder;

    /**
     * ABS frontend jar
     */
    protected File absfrontEnd;
    
    /**
     * mTVL jar
     */
    protected File mTVL;
    
    public void execute() throws MojoExecutionException, MojoFailureException {
        try {
            absfrontEnd = new File(getToolClassPath(ABS_FRONTEND_ARTIFACTID));
            mTVL = new File(getToolClassPath(MTVL_ARTIFACTID));
            doExecute();
        } catch (MojoExecutionException exc) {
            throw exc;
        } catch (MojoFailureException exc) {
            throw exc;
        } catch (RuntimeException exc) {
            throw exc;
        } catch (Exception exc) {
            throw new MojoExecutionException("wrap: " + exc, exc);
        }
    }

    protected abstract void doExecute() throws Exception;

    void setABSSrcFolder(File absSrcFolder) {
        this.absSrcFolder = absSrcFolder;
    }
    
    /**
     * This method resolves the dependency artifacts from the project.
     * 
     * @param theProject
     *            The POM.
     * @return resolved set of dependency artifacts.
     * 
     * @throws ArtifactResolutionException
     * @throws ArtifactNotFoundException
     * @throws InvalidDependencyVersionException
     */
    @SuppressWarnings("unchecked")
    protected Set<Artifact> resolveDependencyArtifacts(MavenProject theProject) throws Exception {
        AndArtifactFilter filter = new AndArtifactFilter();
        filter.add(new ScopeArtifactFilter(Artifact.SCOPE_TEST));
        filter.add(new ArtifactFilter() {
            public boolean include(Artifact artifact) {
                return !artifact.isOptional();
            }
        });
        // TODO follow the dependenciesManagement and override rules
        Set<Artifact> artifacts = theProject.createArtifacts(factory, Artifact.SCOPE_RUNTIME, filter);
        for (Artifact artifact : artifacts) {
            resolver.resolve(artifact, remoteRepos, localRepo);
        }
        return artifacts;
    }

    /**
     * This method resolves all transitive dependencies of an artifact.
     * 
     * @param artifact
     *            the artifact used to retrieve dependencies
     * 
     * @return resolved set of dependencies
     * 
     * @throws ArtifactResolutionException
     * @throws ArtifactNotFoundException
     * @throws ProjectBuildingException
     * @throws InvalidDependencyVersionException
     */
    protected Set<Artifact> resolveArtifactDependencies(Artifact artifact) throws Exception {
        Artifact pomArtifact = factory.createArtifact(artifact.getGroupId(), artifact.getArtifactId(),
                artifact.getVersion(), "", "pom");
        MavenProject pomProject = mavenProjectBuilder.buildFromRepository(pomArtifact, remoteRepos, localRepo);
        return resolveDependencyArtifacts(pomProject);
    }

    protected String getToolClassPath(String artifactId) throws Exception {
        return getClasspath(ABS_GROUPID, artifactId, "1.0-SNAPSHOT");
    }

    protected String getClasspath(String groupId, String artifactId, String version) throws Exception {
        return getClasspath(factory.createArtifact(groupId, artifactId, version, Artifact.SCOPE_RUNTIME, "jar"));
    }
    
    protected String getClasspath(Artifact artifact) throws Exception {
        resolver.resolve(artifact, remoteRepos, localRepo);
        return artifact.getFile().getCanonicalPath();
    }

    @SuppressWarnings("unchecked")
    protected List<Dependency> getDependencies() {
        return project.getCompileDependencies();
    }

    protected List<String> getAbsDependencies() {
        Set<File> absJars = new HashSet<File>();
        for (Object a : project.getArtifacts()) {
            if (a instanceof Artifact) {
                absJars.add(((Artifact) a).getFile());
            }
        }
        return getFileNames(absJars);
    }

    protected File getArtifact(String artifactId) {
        for (Object o : project.getArtifacts()) {
            if (o instanceof DefaultArtifact) {
                DefaultArtifact a = (DefaultArtifact) o;
                if (a.getArtifactId().equals(artifactId)) {
                    return a.getFile();
                }
            }
        }
        return null;
    }

    protected List<String> getFileNames(Collection<File> files) {
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
                    String name = f.getName();
                    for (String ext : ABS_EXTENSIONS) {
                        if (name.endsWith(ext)) {
                            absFiles.add(f);
                            break;
                        }   
                    }
                }
            }
        }
        return absFiles;
    }
    
    protected List<String> getABSArguments() throws Exception {
        List<String> args = new ArrayList<String>();
        args.addAll(getFileNames(getAbsFiles(absSrcFolder)));
        for (String dep : getAbsDependencies()) {
            if (isABSPackage(new File(dep))) {
                args.add(dep);
            }
        }
        return args;
    }
    
    protected boolean isABSPackage(File file) throws IOException {
        return new JarFile(file).getManifest().getMainAttributes()
                        .getValue(VERSION_ATTRIBUTE) != null;
    }
    
}
