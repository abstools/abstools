/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.debugging;

import java.io.File;
import java.io.IOException;

import org.abs_models.backend.java.observing.TaskView;

public class OutputDebugger extends AbstractDebugger {

    @Override
    public void nextStep(TaskView taskView, String fileName, int line) {
        System.out.println(getLine(fileName, line));
        try {
            System.in.read();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    int step = 0;

    private String getLine(String fileName, int line) {
        FileContent c = fileContent.get(fileName);
        if (c == null) {
            c = new FileContent(new File(fileName));
            fileContent.put(fileName, c);
        }
        String lineString = c.getLine(line);
        step++;
        return "Step " + step + ": " + c.getFile().getName() + ":" + line + ": " + lineString;
    }

}
