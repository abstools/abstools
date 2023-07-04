/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime;

import java.io.IOException;
import java.io.PrintStream;
import java.lang.ref.WeakReference;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Scanner;
import java.util.Set;

import org.abs_models.backend.java.JavaBackend;
import org.abs_models.backend.java.JavaBackendConstants;

public class ABSDynamicRuntime extends ABSRuntime {

    private ABSDynamicRuntime() {
        super();
    }

    public static ABSDynamicRuntime getRuntime() {
        if (runtimeSingleton == null) {
            synchronized(ABSRuntime.class) {
                if (runtimeSingleton == null) {
                    runtimeSingleton = new ABSDynamicRuntime();
                }
            }
        }
        return (ABSDynamicRuntime)runtimeSingleton;
    }

    /*
     * Object roster maintains a list of objects in the system
     *
     */
    private Map<ABSDynamicClass, Set<WeakReference<ABSDynamicObject>>> objectRoster
        = new HashMap<>();

    public void registerObject(ABSDynamicObject obj) {
        Set<WeakReference<ABSDynamicObject>> objectSet;
        if (objectRoster.get(obj.getClazz()) == null) {
            objectSet = new HashSet<>();
            objectRoster.put(obj.getClazz(), objectSet);
        } else {
            objectSet = objectRoster.get(obj.getClazz());
        }
        objectSet.add(new WeakReference<>(obj));
        System.out.println("*** Runtime registered instance of " + obj.getClazz().getName());
    }

    public Set<ABSDynamicObject> getAllObjects(ABSDynamicClass cls) {
        Set<ABSDynamicObject> allObjects = new HashSet<>();
        for (WeakReference<ABSDynamicObject> weakObject : objectRoster.get(cls)) {
            if (weakObject.get() != null)
               allObjects.add(weakObject.get());
        }
        return allObjects;
    }


    /*
     * The Dynamic SPL
     */
    private ABSDynamicProductLine dspl = null;
    public void initDSPL(ABSDynamicProduct initP) {
        dspl = new ABSDynamicProductLine();

        dspl.addProduct(initP);
        dspl.setCurrentProduct(initP);
        // TODO add all products and reconfigurations that are reachable from initP

        // A rudimentary but practical management interface
        Thread listener = new NetworkListenerThread(this, "ABS Runtime Network Interface");
        listener.start();

    }

    public ABSDynamicProductLine getDSPL() {
        return dspl;
    }

    /*
     * The Open Adaptivity Interface
     */
    private static class NetworkListenerThread extends Thread {
        private ABSDynamicRuntime runtime;
        public NetworkListenerThread(ABSDynamicRuntime r, String name) {
            super(name);
            this.runtime = r;
        }

        public void run() {
            final int myPort = 8810;
            ServerSocket ssock;
            try {
                ssock = new ServerSocket(myPort);
                System.out.println("*** Socket created at port " + myPort);
                Socket sock = ssock.accept();
                Scanner scanner = new Scanner(sock.getInputStream());
                PrintStream out = new PrintStream(sock.getOutputStream(), true, "UTF-8");

                while(true) {
                    out.print("[ABSRuntime]>> ");
                    String line = scanner.nextLine();
                    String className;
                    if (line.startsWith("q")) {
                        out.println("[ABSRuntime] Exiting.");
                        break;
                    } else if (line.startsWith("delta ")) {
                        String name = (line.split("\\s+"))[1];
                        className = JavaBackend.getDeltaPackageName(name) + "." + JavaBackend.getDeltaName(name);
                    } else if (line.startsWith("update ")) {
                        String name = (line.split("\\s+"))[1];
                        className = JavaBackendConstants.LIB_UPDATES_PACKAGE + "." + JavaBackend.getUpdateName(name);
                    } else {
                        out.println("[ABSRuntime] Unrecognised command.");
                        continue;
                    }
                    out.print("[ABSRuntime] Applying " + className + " ");
                    loadAndApply(className, out);
                    out.println(".");
                }

                out.close();
                scanner.close();
                ssock.close();

            } catch (IOException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }

        private void loadAndApply(String className, PrintStream out) {

            try {
                Class<?> clazz = Class.forName(className);
                Method method;
                try {
                    method = clazz.getDeclaredMethod("apply", ABSDynamicRuntime.class);
                    try {
                        method.invoke(null, runtime);
                    } catch (IllegalArgumentException e) {
                        // TODO Auto-generated catch block
                        e.printStackTrace();
                    } catch (IllegalAccessException e) {
                        // TODO Auto-generated catch block
                        e.printStackTrace();
                    } catch (InvocationTargetException e) {
                        // TODO Auto-generated catch block
                        e.printStackTrace();
                    }
                } catch (SecurityException e1) {
                    // TODO Auto-generated catch block
                    e1.printStackTrace();
                } catch (NoSuchMethodException e1) {
                    // TODO Auto-generated catch block
                    e1.printStackTrace();
                }

            } catch (ClassNotFoundException e) {
                out.print("error loading class: " + className);
            }
        }
    }

}
