package org.abs_models.backend.c.codegen;

import java.io.IOException;
import java.io.Writer;

public class CFile {
    public final CProject cProject;
    public final Writer writer;

    public CFile(CProject cProject, Writer writer) {
        this.cProject = cProject;
        this.writer = writer;
    }

    public void writeLine(String line) throws IOException {
        this.writer.write(line + "\n");
    }

    public void close() throws IOException {
        this.writer.close();
    }

    public void startFunction(CFunctionDecl decl) throws IOException {
        writeLine(decl.cString());
        writeLine("{");
    }

    public void stopFunction() throws IOException {
        writeLine("}");
    }
}
