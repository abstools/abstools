// Copyright (c) 2006 - 2008, Markus Strauch.
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// 
// * Redistributions of source code must retain the above copyright notice, 
// this list of conditions and the following disclaimer.
// * Redistributions in binary form must reproduce the above copyright notice, 
// this list of conditions and the following disclaimer in the documentation 
// and/or other materials provided with the distribution.
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE 
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF 
// THE POSSIBILITY OF SUCH DAMAGE.// Copyright (c) 2006 - 2008, Markus Strauch.

package net.sf.sdedit.util;

import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.Window;
import java.io.File;

public class UIUtilities {
	
    public static void centerWindow (Window window) {
        Dimension screen = Toolkit.getDefaultToolkit().getScreenSize();
        int width = window.getWidth();
        int height = window.getHeight();
        int left = Math.max(0, screen.width / 2 - width / 2);
        int top = Math.max(0, screen.height / 2 - height / 2);
        window.setLocation(left, top);
    }
    
    public static void centerWindow (Window window, Window parent) {
        int width = window.getWidth();
        int height = window.getHeight();
        int left = Math.max(0, parent.getLocationOnScreen().x + parent.getSize().width / 2 - width / 2);
        int top = Math.max(0, parent.getLocationOnScreen().y + parent.getSize().height / 2 - height / 2);
        window.setLocation(left, top);    	
    }
    
    public static File affixType (File file, String type) {
    	String fileName = file.getAbsolutePath();
    	int dot = fileName.lastIndexOf('.');
    	if (dot == -1) {
    		return new File(fileName + "." + type);
    	}
    	String baseName = fileName.substring(0,dot);
    	return new File(baseName + "." + type);
    }

}
