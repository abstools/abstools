/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta.exceptions;

/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 1/19/11
 * Time: 2:00 PM
 */
public class VertexNotFoundException extends Exception {

    private String msg;

    public VertexNotFoundException(String vertex) {
        super(vertex);
        msg = vertex;
    }

    public String toString(){
        return "VertexNotFoundException["+msg+"]";
    }

}
