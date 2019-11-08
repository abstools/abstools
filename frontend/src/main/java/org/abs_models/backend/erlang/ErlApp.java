/**
 *
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.erlang;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.net.JarURLConnection;
import java.net.URLConnection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

import org.abs_models.frontend.ast.*;
import org.abs_models.frontend.parser.Main;
import org.apache.commons.io.FileUtils;

import org.abs_models.backend.common.CodeStream;
import org.abs_models.backend.common.InternalBackendException;
import org.abs_models.common.CompilerUtils;
import org.abs_models.frontend.analyser.AnnotationHelper;

import com.google.common.collect.ImmutableSet;
import com.google.common.io.ByteStreams;
import com.google.common.io.Files;

/**
 * Represents a to be generate Erlang application.
 *
 * Provides facilities to create a module and creates a copy of the runtime
 * there.
 *
 * @author Georg Göri
 *
 */
public class ErlApp {

    public File destDir;
    public File destCodeDir;
    public File destIncludeDir;
    public File index_file;
    public File static_dir;

    public Map<String, CodeStream> funMod = new HashMap<>();

    public ErlApp(File destDir, File http_index_file, File http_static_dir) throws IOException, InternalBackendException {
        // FIXME this should probably be a method; strange to have a
        // class which does all its work in the constructor
        super();
        this.destDir = destDir;
        this.destCodeDir = new File(destDir, "absmodel/src/");
        this.destIncludeDir = new File(destDir, "absmodel/include/");
        destDir.mkdirs();
        FileUtils.cleanDirectory(destDir);
        destDir.mkdirs();
        new File(destDir, "absmodel/ebin").mkdir();
        index_file = http_index_file;
        static_dir = http_static_dir;
        if (static_dir != null && !static_dir.isDirectory()) {
            throw new InternalBackendException("Please provide a directory with static files");
        }
        copyRuntime();
    }

    public CodeStream createSourceFile(String moduleName) throws FileNotFoundException, UnsupportedEncodingException {
        return new CodeStream(new File(destCodeDir, moduleName + ".erl"));
    }

    public CodeStream createIncludeFile(String filename) throws FileNotFoundException, UnsupportedEncodingException {
        return new CodeStream(new File(destIncludeDir, filename + ".hrl"));
    }


    /**
     * All functions for an ABS module are stored in one Erlang module.
     *
     * This method creates the necessary stream.
     */
    public CodeStream getFunStream(String moduleName) throws FileNotFoundException, UnsupportedEncodingException {
        if (!funMod.containsKey(moduleName)) {
            CodeStream ecs = new CodeStream(new File(destCodeDir, ErlUtil.getModuleName(moduleName)
                    + "_funs.erl"));
            funMod.put(moduleName, ecs);
            ecs.pf("-module(%s).", ErlUtil.getModuleName(moduleName) + "_funs");
            ecs.println("-compile(export_all).");
            ecs.println("-include_lib(\"../include/abs_types.hrl\").");
            ecs.println();
        }

        return funMod.get(moduleName);
    }

    public void close() {
        for (Entry<String, CodeStream> e : funMod.entrySet())
            e.getValue().close();
        funMod.clear();
    }

    private static final Set<String> RUNTIME_FILES = ImmutableSet.of(
            "absmodel/src/*",
            "absmodel/include/*",
            "absmodel/deps/*",
            "absmodel/priv/*",
            "absmodel/priv/static/*",
            // do not copy this since absmodulename.hrl is generated later --
            // runtime.erl and main_app.erl use the wrong constant
            // "absmodel/ebin/*",
            "absmodel/ebin/absmodel.app",
            "absmodel/Emakefile",
            "Dockerfile",
            "start_console",
            "run",
            "run.bat",
            "run.escript",
            "absmodel/rebar.config",
            "bin/*",
            "link_sources"
            );
    private static final Set<String> EXEC_FILES = ImmutableSet.of(
            "bin/rebar",
            "run",
            "start_console",
            "link_sources"
            );

    private static final String JAR_PATH = "erlang/";

    private void copyRuntime() throws IOException {
        InputStream is = null;
        // TODO: this only works when the erlang compiler is invoked
        // from a jar file.  See http://stackoverflow.com/a/2993908 on
        // how to handle the other case.
        URLConnection resource = getClass().getResource("").openConnection();
        try {
            new File(destDir + "/absmodel/ebin").mkdirs();
            if (resource instanceof JarURLConnection) {
                for (String f : RUNTIME_FILES) {
                    if (f.endsWith("/*")) {
                        String dirname = f.substring(0, f.length() - 2);
                        String inname = JAR_PATH + dirname;
                        String outname = destDir + "/" + dirname;
                        new File(outname).mkdirs();
                        copyJarDirectory(((JarURLConnection) resource).getJarFile(),
                                inname, outname);
                    } else {
                        is = ClassLoader.getSystemResourceAsStream(JAR_PATH + f);
                        if (is == null)
                            throw new RuntimeException("Could not locate Runtime file:" + f);
                        String outputFile = f.replace('/', File.separatorChar);
                        File file = new File(destDir, outputFile);
                        file.getParentFile().mkdirs();
                        Files.asByteSink(file).writeFrom(is);
                    }
                }
            }
            else if (resource.getURL().getProtocol().equals("file")) {
                /* stolz: This at least works for the unit tests from within Eclipse */
                File file = new File("src/main/resources/erlang/");
                assert file.exists();
                FileUtils.copyDirectory(file, destDir);
            } else {
                throw new UnsupportedOperationException("File type: "+resource);
            }
            if (index_file != null) {
                File http_out_file = new File(destDir + "/absmodel/priv/index.html");
                http_out_file.getParentFile().mkdirs();
                FileUtils.copyFile(index_file, http_out_file);
            }
            if (static_dir != null) {
                File static_out_dir = new File(destDir + "/absmodel/priv/static");
                static_out_dir.mkdirs();
                FileUtils.copyDirectory(static_dir, static_out_dir);
            }
        } finally {
            if (is != null)
                is.close();
        }
        for (String f : EXEC_FILES) {
            new File(destDir, f).setExecutable(true, false);
        }
    }

    private void copyJarDirectory(JarFile jarFile, String inname, String outname)
            throws IOException {
        InputStream is = null;
        for (JarEntry entry : Collections.list(jarFile.entries())) {
            if (entry.getName().startsWith(inname)) {
                String relFilename = entry.getName().substring(inname.length());
                if (!entry.isDirectory()) {
                    is = jarFile.getInputStream(entry);
                    ByteStreams.copy(is, Files.asByteSink(new File(outname, relFilename)).openStream());
                } else {
                    new File(outname, relFilename).mkdirs();
                }
            }
        }
        is.close();
    }

    public void generateModuleDefinitions(String absModulename, String erlModulename) throws FileNotFoundException, UnsupportedEncodingException {
        CodeStream hcs = createIncludeFile("absmodulename");
        hcs.println("%%This file is licensed under the terms of the Modified BSD License.");
        hcs.println("-undef(ABSMAINMODULE).");
        hcs.println("-define(ABSMAINMODULE," + erlModulename + ").");
        hcs.println("-undef(ABSCOMPILERVERSION).");
        hcs.println("-define(ABSCOMPILERVERSION,\"" + Main.getGitVersion() + "\").");
        hcs.close();
    }

    public void generateDataConstructorInfo(Model model) throws IOException {
        CodeStream s = createSourceFile("abs_constructor_info");
        s.println("%%This file is licensed under the terms of the Modified BSD License.");
        s.println("-module(abs_constructor_info).");
        s.println("-compile(export_all).");
        s.println("-include_lib(\"../include/abs_types.hrl\").");
        s.println();
        String separator = "";

        for (ModuleDecl m : model.getModuleDecls()) {
            for (Decl d : m.getDecls()) {
                if (d instanceof DataTypeDecl) {
                    DataTypeDecl dd = (DataTypeDecl) d;
                    for (DataConstructor c : dd.getDataConstructors()) {
                        boolean useToString = true;
                        for (ConstructorArg ca : c.getConstructorArgs()) {
                            List<Annotation> ann = ca.getTypeUse().getAnnotations();
                            PureExp key = AnnotationHelper.getAnnotationValueFromName(ann, "ABS.StdLib.HTTPName");
                            if (ca.hasSelectorName() || key != null) {
                                useToString = false;
                            }
                        }
                        if (!useToString) {
                            s.println(separator);
                            separator = ";";
                            s.format("to_json(Abs=[data%s | _]) -> ", c.getName());
                            String mapSeparator = "";
                            s.print("#{");
                            s.incIndent();
                            for (int elem = 0; elem < c.getNumConstructorArg(); elem++) {
                                ConstructorArg ca = c.getConstructorArg(elem);
                                List<Annotation> ann = ca.getTypeUse().getAnnotations();
                                String key = null;
                                PureExp keyann = AnnotationHelper.getAnnotationValueFromName(ann, "ABS.StdLib.HTTPName");
                                if (keyann != null && keyann instanceof StringLiteral) {
                                    key = ((StringLiteral)keyann).getContent();
                                } else if (ca.hasSelectorName()) {
                                    key = ca.getSelectorName().toString();
                                }
                                if (key != null) {
                                    s.println(mapSeparator);
                                    mapSeparator = ",";
                                    s.format("<<\"%s\"/utf8>> => modelapi_v2:abs_to_json(lists:nth(%s, Abs))",
                                             key,
                                             // nth() indexes 1-based and
                                             // we need to skip over the first
                                             // element:
                                             elem + 2);
                                }
                            }
                            s.println();
                            s.decIndent();
                            s.print("}");
                        }
                    }
                }
            }
        }
        s.println(separator);
        s.pf("to_json(Abs) -> builtin:toString(null, list_to_tuple(Abs)).");
        s.close();
    }

}
