/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.treecopy;

import java.lang.reflect.Field;
import java.lang.reflect.Method;

import abs.frontend.ast.ASTNode;


public class ParseTreeCopyHelper {
 
    /**
     * creates a copy of the given node
     * 
     *  in contrast to fullCopy this method only copies the parse tree as
     *  it is produced by the parser. It does not try to copy attributes
     *  or other information in the AST (results from typechecking).
     *  
     */
    public static <T extends ASTNode<S>, S extends ASTNode<?>> T parseTreeCopy(T node) {
        try {
            Class<? extends T> nodeClass =  (Class<? extends T>) node.getClass();
            T copy = nodeClass.newInstance();
            // set children
            for (int i=0; i<node.getNumChildNoTransform(); i++) {
                S child = node.getChildNoTransform(i);
                S childCopy = (S) child.parseTreeCopy();
                copy.setChild(childCopy, i);
            }
            // set other fields (no ASTNodes; Strings, etc.)
            setOtherFields(node, copy, nodeClass, nodeClass);
            
            // set position
            copy.setPositionFromNode(node);
            
            return copy;
        } catch (Exception e) {
            throw new Error(e);
        }
    }

    /**
     * searches for fields named "token<Type>_<Name>" in searchClass
     * and uses set and get methods from nodeClass to copy the values from node to copy
     */
    private static <T> void setOtherFields(T node, T copy, Class<? extends T> nodeClass, Class<?> searchClass) throws Exception {
        Field[] fields = searchClass.getDeclaredFields();
        for (Field field : fields) {
            String fieldName = field.getName();
            if (fieldName.startsWith("token") && fieldName.contains("_")) {
                String name = fieldName.substring(fieldName.indexOf('_')+1);
                Class<?> fieldType = field.getType();
                Method getMethod = nodeClass.getMethod("get" + name);
                Method setMethod = nodeClass.getMethod("set" + name, fieldType);
                
                // value = node.getName();
                Object value = getMethod.invoke(node);
                // copy.setName(value);
                setMethod.invoke(copy, value);
            }
        }
        if (searchClass.getSuperclass() != null) {
            setOtherFields(node, copy, nodeClass, searchClass.getSuperclass());
        }
    }
    
}
