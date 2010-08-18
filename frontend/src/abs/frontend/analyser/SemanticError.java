package abs.frontend.analyser;

import beaver.Symbol;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.CompilationUnit;

public class SemanticError {
    public final ErrorMessage msg;
    public final String[] args;
    public final ASTNode<?> node;
    
    public SemanticError(ASTNode<?> node, ErrorMessage msg, String... args) {
        this.node = node;
        this.msg = msg;
        this.args = args;
    }
    
    public String fileName() {
        ASTNode<?> parent = node;
        while (! (parent instanceof CompilationUnit) ) {
            parent = parent.getParent();
        }
        CompilationUnit u = (CompilationUnit) parent; 
        return "<unkown>";
    }
    
    public int getLine() {
        return Symbol.getLine(node.getStart());
    }
    
    public int getColumn() {
        return Symbol.getColumn(node.getStart());
    }
    
    public String getMsg() {
    	return msg.withArgs(args);
    }
    
    public String getMsgString() {
        return getLine() + ":" + 
            getColumn() + ": " + 
            msg.withArgs(args);
    }
    
    public String getMsgWithHint(String absCode) {
        return getMsgString()+"\n"+getHint(absCode);
    }

    public String getHint(String absCode) {
        int prevIndex = 0;
        int endIndex = -1;
        endIndex = absCode.indexOf('\n', prevIndex);
        for (int l = 1; l < getLine() && endIndex != -1; l++) {
            prevIndex = endIndex;
            endIndex = absCode.indexOf('\n', prevIndex);
        }
        String line = "";
        if (endIndex == -1)
            line = absCode.substring(prevIndex);
        else
            line = absCode.substring(prevIndex, endIndex);
        StringBuffer lineHint = new StringBuffer();
        for (int c = 0; c < getColumn()-1; c++) {
            lineHint.append('-');
        }
        lineHint.append('^');
        return line+"\n"+lineHint;
    }
}
