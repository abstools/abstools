package org.abs_models.backend.c.codegen;

import com.google.common.io.ByteStreams;
import com.google.common.io.CharSink;
import com.google.common.io.Files;
import org.apache.commons.io.FileUtils;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.Writer;
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
     * Writes a dummy main file. Only used for testing.
     *
     * @throws IOException
     */
    public void writeMain() throws IOException {
        CharSink sink = Files.asCharSink(new File(target.getAbsolutePath(), "main.c"), StandardCharsets.UTF_8);
        Writer writer = sink.openBufferedStream();
        writer.write("int main(int argc, char *argv[]) { return 0; }");
        writer.close();
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
            .directory(target);

        Process p = pb.start();
        int returnValue = p.waitFor();
        return returnValue == 0;
    }
}
