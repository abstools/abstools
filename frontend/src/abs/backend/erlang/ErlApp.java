/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.erlang;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.commons.io.FileUtils;

public class ErlApp {

    public File destDir;

    public Map<String, ErlangCodeStream> funMod = new HashMap<String, ErlangCodeStream>();

    public ErlApp(File destDir) throws IOException {
        super();
        this.destDir = destDir;
        destDir.mkdirs();
        FileUtils.cleanDirectory(destDir);
        destDir.mkdirs();
    }

    public ErlangCodeStream createFile(String moduleName) throws FileNotFoundException, UnsupportedEncodingException {
        return new ErlangCodeStream(new File(destDir, moduleName + ".erl"));
    }

    public ErlangCodeStream getFunStream(String moduleName) throws FileNotFoundException, UnsupportedEncodingException {
        if (!funMod.containsKey(moduleName)) {
            ErlangCodeStream ecs = new ErlangCodeStream(new File(destDir, ErlUtil.getModuleName(moduleName)
                    + "_funs.erl"));
            funMod.put(moduleName, ecs);
            ecs.pf("-module(%s).", ErlUtil.getModuleName(moduleName) + "_funs");
            ecs.println("-compile(export_all).");
            ecs.println();
        }

        return funMod.get(moduleName);
    }

    public void close() {
        for (Entry<String, ErlangCodeStream> e : funMod.entrySet())
            e.getValue().close();
        funMod.clear();
    }
}
