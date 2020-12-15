package org.abs_models.backend.c.codegen;

import com.google.common.io.ByteStreams;
import com.google.common.io.CharSink;
import com.google.common.io.FileWriteMode;
import com.google.common.io.Files;
import org.apache.commons.io.FileUtils;

import java.io.*;
import java.net.JarURLConnection;
import java.net.URLConnection;
import java.nio.charset.StandardCharsets;
import java.util.Enumeration;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

/**
 * A C project which is stored inside a directory.
 */
public class CProject {
    private final File target;

    public CProject(File target) {
        this.target = target;
    }

    /**
     * Copies files from the resource directory. The pathname should have a leading slash.
     *
     * @param pathname Path
     * @throws IOException Whenever
     */
    public void copyFromResources(String pathname) throws IOException {
        URLConnection resource = getClass().getResource(pathname).openConnection();

        if (resource instanceof JarURLConnection) {
            JarURLConnection jarResource = (JarURLConnection) resource;
            String entryName = jarResource.getEntryName() + "/";

            JarFile jarFile = jarResource.getJarFile();
            Enumeration<JarEntry> entries = jarFile.entries();
            while (entries.hasMoreElements()) {
                JarEntry entry = entries.nextElement();
                String path = entry.getName();
                if (path.startsWith(entryName)) {
                    String localName = path.substring(entryName.length());
                    File finalFile = new File(target.getAbsolutePath(), localName);
                    if (!entry.isDirectory()) {
                        InputStream is = jarFile.getInputStream(entry);
                        ByteStreams.copy(is, Files.asByteSink(finalFile).openStream());
                    } else {
                        finalFile.mkdirs();
                    }
                }
            }
        } else if (resource.getURL().getProtocol().equals("file")) {
            File file = new File(resource.getURL().getFile());
            assert file.exists();
            FileUtils.copyDirectory(file, target);
        } else {
            throw new UnsupportedOperationException("File type: " + resource);
        }
    }

    /**
     * Creates a new CFile.
     */
    public CFile openFile(String name) throws IOException {
        BufferedWriter writer = new BufferedWriter(new FileWriter(new File(target.getAbsolutePath(), name)));
        return new CFile(this, writer);
    }

    /**
     * Runs the main program in release mode.
     *
     * @return true if the program executed successfully.
     * @throws IOException
     * @throws InterruptedException if the program
     */
    public boolean run() throws IOException, InterruptedException {
        ProcessBuilder pb = new ProcessBuilder()
            .command("make", "run")
            .inheritIO()
            .directory(target);

        Process p = pb.start();
        int returnValue = p.waitFor();
        return returnValue == 0;
    }
}
