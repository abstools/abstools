package abs.backend.java.debugging;

import java.io.File;

public class DebugPosition {
    private final int line;
    private final String fileName;

    public DebugPosition(String fileName, int line) {
        this.fileName = fileName;
        this.line = line;
    }

    public String getFileName() {
        return fileName;
    }

    public int getLine() {
        return line;
    }
}
