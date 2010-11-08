package abs.frontend.parser;

import abs.frontend.ast.ASTNode;
import beaver.Symbol;

public class SourcePosition {
    private int pos;
    private ASTNode<?> contextNode;
    
    SourcePosition(ASTNode<?> node, int pos) {
        this.pos = pos;
        contextNode = node;
    }
    
    public ASTNode<?> getContextNode() {
        return contextNode;
    }
    
    public int getColumn() {
        return Symbol.getColumn(pos);
    }
    
    public int getLine() {
        return Symbol.getLine(pos);
    }
    
    public static SourcePosition findPosition(ASTNode<?> node, int line, int col) {
        return findPosition(node, Symbol.makePosition(line, col));
    }

    private static SourcePosition findPosition(ASTNode<?> node, int searchPos) {

        if (inNode(node,searchPos)) {
            ASTNode<?> foundNode = node;
            for (int i = 0; i < node.getNumChild(); i++) {
                SourcePosition pos = findPosition(node.getChild(i),searchPos);
                if (pos != null)
                    return pos;
            }
            return new SourcePosition(node,searchPos);
        }
        
        return null;
    }
    
    private static boolean inNode(ASTNode<?> node, int pos) {
        int col = Symbol.getColumn(node.getStartPos());
        int line = Symbol.getLine(node.getStartPos());
        int endcol = Symbol.getColumn(node.getEndPos());
        int endline = Symbol.getLine(node.getEndPos());
        int posCol = Symbol.getColumn(pos);
        int posLine = Symbol.getLine(pos);
        if (smaller(pos,node.getStartPos()) || 
            larger(pos,node.getEndPos()))
            return false;
        return true;
    }
    
    private static boolean larger(int pos, int pos2) {
        return smaller(pos2, pos);
    }
    
    private static boolean smaller(int pos, int pos2) {
        int col = Symbol.getColumn(pos);
        int line = Symbol.getLine(pos);
        int col2 = Symbol.getColumn(pos2);
        int line2 = Symbol.getLine(pos2);

        if (line < line2)
            return true;
        
        if (line > line2)
            return false;
        
        return col < col2;
    }
}
