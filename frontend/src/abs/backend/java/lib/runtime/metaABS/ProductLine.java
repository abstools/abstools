/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime.metaABS;

import java.io.IOException;
import java.io.PrintStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.List;
import java.util.Scanner;

import abs.backend.java.JavaBackend;
import abs.backend.java.JavaBackendConstants;
import abs.backend.java.codegeneration.dynamic.DynamicException;
import abs.backend.java.lib.runtime.ABSClosure;
import abs.backend.java.lib.runtime.ABSDynamicClass;
import abs.backend.java.lib.runtime.ABSDynamicDelta;
import abs.backend.java.lib.runtime.ABSDynamicObject;
import abs.backend.java.lib.runtime.ABSDynamicProduct;
import abs.backend.java.lib.runtime.ABSRuntime;
import abs.backend.java.lib.types.*;
import abs.common.ListUtils;

public class ProductLine {
    private static ABSDynamicClass thisClass;

    /* 
     * Create the singleton "ProductLine" class object
     */
    public static ABSDynamicClass singleton() {
        if (thisClass == null) {
            thisClass = new ABSDynamicClass();
            
            setupAPI();
            new NetworkListenerThread().start();
        }
        return thisClass;
    }

    private static void setupAPI() {
        thisClass.setName("ProductLine");
        
       
        thisClass.addMethod(/*ABSUnit*/ "configureProduct", new ABSClosure() {
            @Override
            public ABSUnit exec(ABSDynamicObject t, ABSValue... params) {
                ABSDynamicProduct currentProd = getCurrentProduct(t.__ABS_getRuntime());
                ABSDynamicProduct targetProd = (ABSDynamicProduct)params[0];

                List<ABSDynamicDelta> deltas = currentProd.getDeltas(targetProd.getName());
                for (ABSDynamicDelta delta : deltas) {
                    delta.apply();
                }
                t.__ABS_getRuntime().setCurrentProduct(targetProd);
                return ABSUnit.UNIT;
            }
        });

        thisClass.addMethod(/*ABSDynamicProduct*/ "getCurrentProduct", new ABSClosure() {
            @Override
            public ABSDynamicProduct exec(ABSDynamicObject t, ABSValue... params) {
                ABSDynamicProduct currentProd = getCurrentProduct(t.__ABS_getRuntime());
                return currentProd;
            }
        });

        thisClass.addMethod(/*List<ABSDynamicProduct>*/ "getConfigurableProducts", new ABSClosure() {
            @Override
            public ABSValue exec(ABSDynamicObject t, ABSValue... params) {
                ABSDynamicProduct currentProd = getCurrentProduct(t.__ABS_getRuntime());
                return ListUtils.toABSList(currentProd.getConfigurableProducts());
            }
        });

        thisClass.addMethod(/*ABSDynamicProduct*/ "getConfigurableProduct", new ABSClosure() {
            @Override
            public ABSValue exec(ABSDynamicObject t, ABSValue... params) {
                ABSDynamicProduct currentProd = getCurrentProduct(t.__ABS_getRuntime());
                ABSString name = (ABSString)params[0];
                
                for (ABSDynamicProduct p : currentProd.getConfigurableProducts()) {
                    if (p.getName().equals(name.getString()))
                        return p;
                }
                throw new DynamicException("Product " + name + " is not configurable from the current product " 
                        + currentProd.getName() + ".");
            }
        });
    }
    
    private static ABSDynamicProduct getCurrentProduct(ABSRuntime runtime) {
        ABSDynamicProduct currentProd = runtime.getCurrentProduct();
        if (currentProd == null)
            throw new DynamicException("The current system does not represent a product of the SPL. Please specify the initial product when compiling.");
        else
            return currentProd;
    }
    
    // FIXME this method is only needed for the network listener interface below
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
}
