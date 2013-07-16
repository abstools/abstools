/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

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

import abs.backend.java.JavaBackend;
import abs.backend.java.codegeneration.dynamic.DynamicException;

public class ABSDynamicRuntime extends ABSRuntime {

    /* 
     * Object roster maintains a list of objects in the system
     * 
     */
    private Map<ABSDynamicClass, Set<WeakReference<ABSDynamicObject>>> objectRoster 
        = new HashMap<ABSDynamicClass, Set<WeakReference<ABSDynamicObject>>>();
    
    public void registerObject(ABSDynamicObject obj) {
        Set<WeakReference<ABSDynamicObject>> objectSet;
        if (objectRoster.get(obj.getClazz()) == null) {
            objectSet = new HashSet<WeakReference<ABSDynamicObject>>();
            objectRoster.put(obj.getClazz(), objectSet);
        } else {
            objectSet = objectRoster.get(obj.getClazz());
        }
        objectSet.add(new WeakReference<ABSDynamicObject>(obj));
    }
    
    
    /*
     * The Dynamic SPL
     */
    private ABSDynamicProductLine dspl = null;
    public void initDSPL(ABSDynamicProduct initP) {
        dspl = new ABSDynamicProductLine();
        System.out.println("*** DSPL init");
        
        dspl.addProduct(initP);
        dspl.setCurrentProduct(initP);
        // TODO add all products and reconfigurations that are reachable from initP
        
        // FIXME this hangs the program for some reason
        //new NetworkListenerThread().start();

    }
    
    public ABSDynamicProductLine getDSPL() {
        return dspl;
    }

    
    
    
    /*
     * The Open Adaptivity Interface
     */
    static class NetworkListenerThread extends Thread {

        public void run() {
            final int myPort = 8810;
            ServerSocket ssock;
            try {
                ssock = new ServerSocket(myPort);
                Socket sock = ssock.accept();
                Scanner scanner = new Scanner(sock.getInputStream());
                PrintStream out = new PrintStream(sock.getOutputStream(), true, "UTF-8");

                while(true) {
                    out.print("[ABSRuntime]>> ");
                    String line = scanner.nextLine();

                    if (line.startsWith("apply ")) {
                        String delta = (line.substring(6)).trim();
                        out.print("[ABSRuntime] Applying delta " + delta + " ... ");
                        applyDelta(delta);
                        out.println("done.");
                    } else if (line.startsWith("q")) {
                        out.println("[ABSRuntime] Exiting.");
                        break;
                    } else {
                        out.println("[ABSRuntime] Unrecognised command.");
                    }
                }

                out.close();
                scanner.close();
                ssock.close();

            } catch (IOException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
    }

    // FIXME this method is only needed for the network listener interface
    // Otherwise deltas are now applied via ABSDynamicDelta.apply()
    private static void applyDelta(String deltaName) {
        //System.err.println("*** Applying delta." + deltaName);
        String className = JavaBackend.getDeltaPackageName(deltaName) + "." + deltaName;
        
        try {
            Class<?> clazz = Class.forName(className);
            Method method;
            try {
                method = clazz.getDeclaredMethod("apply", new Class[0]);
                try {
                    method.invoke(null, new Object[0]);
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
            throw new DynamicException("Error loading class: " + className);
        }
    }


}
