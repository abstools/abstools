/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.debugging;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import org.abs_models.backend.java.observing.TaskView;

public abstract class AbstractDebugger implements Debugger {
    protected int step;

    protected final Map<String, FileContent> fileContent = new HashMap<>();

    @Override
    public synchronized void nextStep(TaskView taskView, String fileName, int line) {
        step++;
    }

    static class FileContent {
        File file;
        ArrayList<String> lines = new ArrayList<>();

        FileContent(File file) {
            this.file = file;
            try {
                BufferedReader reader = new BufferedReader(new FileReader(file));
                while (reader.ready()) {
                    String line = reader.readLine();
                    if (line == null)
                        break;
                    lines.add(line);
                }
                reader.close();
            } catch (FileNotFoundException e) {
                e.printStackTrace();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }

        String getLine(int i) {
            return lines.get(i - 1);
        }

        File getFile() {
            return file;
        }
    }

}
