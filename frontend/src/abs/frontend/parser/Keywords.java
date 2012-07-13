/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.parser;



/**

 * This class is currently used by the Eclipse-Plugin as we have not found yet a

 * better way to get a list of all keywords. The list must be hold consistent

 * with the list defined in the ABS.flex file manually

 * 

 */

public class Keywords {



    // This list has to be consistent with keywords in ABS.flex

    private static String[] keywords = { 
        "module", 
        "import", 
        "export", 
        "from", 
        "class", 
        "interface", 
        "extends", 
        "data",

        "def", 
        "implements", 
        "delta", 
        "adds", 
        "modifies", 
        "removes", 
        "hasField", 
        "hasMethod",
        "hasInterface",
        "productline", 
        "features", 
        "core", 
        "after",

        "when", 
        "product", 
        "while", 
        "return", 
        "skip", 
        "get", 
        "null", 
        "await", 
        "if", 
        "then", 
        "else", 
        "suspend", 
        "duration",
        "new",

        "this", 
        "case", 
        "let", 
        "in", 
        "cog", 
        "type", 
        "assert", 
        "builtin",
        
        "root",
        "extension",
        "group",
        "opt",
        "oneof",
        "allof",
        "ifin",
        "ifout",
        "exclude",
        "require" };

    

    public static String[] getKeywords() {

        return keywords;

    }

}

