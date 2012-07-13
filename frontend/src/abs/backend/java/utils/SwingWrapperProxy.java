/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.utils;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;

import javax.swing.SwingUtilities;

public class SwingWrapperProxy implements InvocationHandler {
    final Object target;

    public SwingWrapperProxy(Object target) {
        this.target = target;
    }

    @Override
    public synchronized Object invoke(final Object proxy, final Method method, final Object[] args) throws Throwable {
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                try {
                    method.invoke(target, args);
                } catch (IllegalArgumentException e) {
                    e.printStackTrace();
                } catch (IllegalAccessException e) {
                    e.printStackTrace();
                } catch (InvocationTargetException e) {
                    e.printStackTrace();
                }
            }
        });

        return null;
    }

    public static <V> V newInstance(V target, Class<?> interfce) {
        return (V) Proxy.newProxyInstance(SwingWrapperProxy.class.getClassLoader(), new Class[] { interfce },
                new SwingWrapperProxy(target));
    }
}
