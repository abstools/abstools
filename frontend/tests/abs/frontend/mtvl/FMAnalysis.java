/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.mtvl;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import abs.common.WrongProgramArgumentException;
import abs.frontend.FrontendTest;
import abs.frontend.ast.Model;
import abs.frontend.ast.ProductDecl;

public class FMAnalysis extends FrontendTest {
    
    static private String chatPL =
            "product HighStatic(Text, Voice, Video, Files);\n" +
            "root Chat {" +
            "   group allof {" +
            "      Mode {" +
            "        group [1..*] { Text, Voice, Video }" +
            "      }," +
            "      opt Files " +
            "    } " +
            " }" +
            " extension Voice {" +
            "    require: Text;" +
            " }" +
            " extension Video {" +
            "    require: Voice;" +
            " }";
    
    private String product = "HighStatic";
    private String variable = "Video";
    
    static private String solve =
            "Chat -> 1\n" +
            "Mode -> 1\n" +
            "Text -> 1\n" +
            "Voice -> 1\n" +
            "Video -> 0\n" +
            "Files -> 0\n" ;
    
    static private String solveall =
            "---Solution 1---\n" +
            "Chat -> 1\n" +
            "Mode -> 1\n" +
            "Text -> 1\n" +
            "Voice -> 1\n" +
            "Video -> 0\n" +
            "Files -> 0\n" +
            "---Solution 2---\n" +
            "Chat -> 1\n" +
            "Mode -> 1\n" +
            "Text -> 1\n" +
            "Voice -> 1\n" +
            "Video -> 0\n" +
            "Files -> 1\n" +
            "---Solution 3---\n" +
            "Chat -> 1\n" +
            "Mode -> 1\n" +
            "Text -> 1\n" +
            "Voice -> 1\n" +
            "Video -> 1\n" +
            "Files -> 1\n" +
            "---Solution 4---\n" +
            "Chat -> 1\n" +
            "Mode -> 1\n" +
            "Text -> 1\n" +
            "Voice -> 1\n" +
            "Video -> 1\n" +
            "Files -> 0\n" +
            "---Solution 5---\n" +
            "Chat -> 1\n" +
            "Mode -> 1\n" +
            "Text -> 1\n" +
            "Voice -> 0\n" +
            "Video -> 0\n" +
            "Files -> 1\n" +
            "---Solution 6---\n" +
            "Chat -> 1\n" +
            "Mode -> 1\n" +
            "Text -> 1\n" +
            "Voice -> 0\n" +
            "Video -> 0\n" +
            "Files -> 0\n" ;
    
    static private String solvewith =
            "---Solution 1---\n" +
            "Chat -> 1\n" +
            "Mode -> 1\n" +
            "Text -> 1\n" +
            "Voice -> 1\n" +
            "Video -> 1\n" +
            "Files -> 1\n" ;
    
    static private String minimise =
            "---Solution 1---\n" +
            "Chat -> 1\n" +
            "Mode -> 1\n" +
            "Text -> 1\n" +
            "Voice -> 0\n" +
            "Video -> 0\n" +
            "Files -> 1\n" +
            "---Solution 2---\n" +
            "Chat -> 1\n" +
            "Mode -> 1\n" +
            "Text -> 1\n" +
            "Voice -> 1\n" +
            "Video -> 0\n" +
            "Files -> 1\n" +
            "---Solution 3---\n" +
            "Chat -> 1\n" +
            "Mode -> 1\n" +
            "Text -> 1\n" +
            "Voice -> 0\n" +
            "Video -> 0\n" +
            "Files -> 0\n" +
            "---Solution 4---\n" +
            "Chat -> 1\n" +
            "Mode -> 1\n" +
            "Text -> 1\n" +
            "Voice -> 1\n" +
            "Video -> 0\n" +
            "Files -> 0\n" ;
    
    static private String maximise =
            "---Solution 1---\n" +
            "Chat -> 1\n" +
            "Mode -> 1\n" +
            "Text -> 1\n" +
            "Voice -> 1\n" +
            "Video -> 1\n" +
            "Files -> 0\n" +
            "---Solution 2---\n" +
            "Chat -> 1\n" +
            "Mode -> 1\n" +
            "Text -> 1\n" +
            "Voice -> 1\n" +
            "Video -> 1\n" +
            "Files -> 1\n" ;
    
    static private String minwith =
            "Chat -> 1\n" +
            "Mode -> 1\n" +
            "Text -> 1\n" +
            "Voice -> 1\n" +
            "Video -> 1\n" +
            "Files -> 1\n" ;
    
    static private String maxproduct =
            "Chat -> 1\n" +
            "Mode -> 1\n" +
            "Text -> 1\n" +
            "Voice -> 1\n" +
            "Video -> 1\n" +
            "Files -> 1\n" ;
    
    @Test
    public void solve() {
        Model model = assertParseOk(chatPL);
        ChocoSolver s = model.instantiateCSModel();
        
        assertEquals(solve, s.resultToString());
    }
    
    @Test
    public void solveall() {
        Model model = assertParseOk(chatPL);
        ChocoSolver s = model.instantiateCSModel();
        
        assertEquals(solveall, s.resultsToString());
    }
    
    @Test
    public void minimise() {
        Model model = assertParseOk(chatPL);
        ChocoSolver s = model.instantiateCSModel();
        
        assertEquals(minimise, s.minimiseToString(variable));
    }
    
    @Test
    public void maximise() {
        Model model = assertParseOk(chatPL);
        ChocoSolver s = model.instantiateCSModel();
        
        assertEquals(maximise, s.maximiseToString(variable));
    }
    
    @Test
    public void solvewith() {
        Model model = assertParseOk(chatPL);
        ProductDecl p_product = null;
        try { p_product = product == null ? null : model.findProduct(product); } 
        catch (WrongProgramArgumentException e) { }
        ChocoSolver s = model.instantiateCSModel();
        p_product.getProduct().getProdConstraints(s);
        
        assertEquals(solvewith, s.resultsToString());
    }
    
    @Test
    public void minwith() {
        Model model = assertParseOk(chatPL);
        ProductDecl p_product = null;
        try { p_product = product == null ? null : model.findProduct(product); } 
        catch (WrongProgramArgumentException e) { }
        ChocoSolver s = model.instantiateCSModel();
        p_product.getProduct().getProdConstraints(s);
        
        assertEquals(minwith, s.resultsToString());
    }
    
    @Test
    public void maxproduct() {
        Model model = assertParseOk(chatPL);
        ChocoSolver s = model.instantiateCSModel();
        
        assertEquals(maxproduct, s.maxProductToString());
    }
    
    @Test
    public void check() {
        Model model = assertParseOk(chatPL);
        ProductDecl p_product = null;
        try { p_product = product == null ? null : model.findProduct(product); } 
        catch (WrongProgramArgumentException e) { }
        ChocoSolver s = model.instantiateCSModel();
        p_product.getProduct().getProdConstraints(s);
        
        assertEquals("checking solution: true", s.checkToString());
    }
    
    @Test
    public void nsol() {
        Model model = assertParseOk(chatPL);
        ChocoSolver s = model.instantiateCSModel();

        assertEquals(6, s.countSolutions());
    }
}
