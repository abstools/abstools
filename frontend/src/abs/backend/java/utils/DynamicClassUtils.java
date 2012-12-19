/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.utils;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.logging.Logger;

import abs.backend.java.codegeneration.dynamic.DynamicException;
import abs.backend.java.lib.runtime.ABSException;
import abs.backend.java.lib.runtime.Logging;

public class DynamicClassUtils {

    private static final Logger logger = Logging.getLogger(DynamicClassUtils.class.getName());

    public static Class<?> getClass(String name) {
        Class<?> cls;
        try {
            cls = Class.forName(name);
        } catch (ClassNotFoundException e) {
            throw new DynamicException("Failed to dynamically load class " + name);
            
        }
        return cls;
    }
    
    
    public static Object instance(String name, Object... args) {
        Class<?> cls = getClass(name);
        if (cls.getDeclaredConstructors().length != 1)
            logger.warning("Class " + name + " has either more than one, or zero constructors.");

        Constructor<?> ctor = cls.getDeclaredConstructors()[0];
        Object result;
        try {
            result = ctor.newInstance(args);
        } catch (IllegalArgumentException e) {
            throw new GeneratedClassLoadingException("Failed to instantiate class " + name + "\n" + e.toString());
        } catch (InstantiationException e) {
            throw new GeneratedClassLoadingException("Failed to instantiate class " + name + "\n" + e.toString());
        } catch (IllegalAccessException e) {
            throw new GeneratedClassLoadingException("Failed to instantiate class " + name + "\n" + e.toString());
        } catch (InvocationTargetException e) {
            throw new GeneratedClassLoadingException("Failed to instantiate class " + name + "\n" + e.toString());
        }
        
        return result;
    }
    
    public static class GeneratedClassLoadingException extends ABSException {

        public GeneratedClassLoadingException(String string) {
            super(string);
        }

        private static final long serialVersionUID = 1261395931117319034L;

        @Override
        public String getName() {
            return "Loading generated class failed";
        }

    }
}
