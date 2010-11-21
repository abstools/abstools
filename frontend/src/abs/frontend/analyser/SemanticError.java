package abs.frontend.analyser;

import java.io.File;

import beaver.Symbol;
import abs.common.CompilerError;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.CompilationUnit;
import abs.frontend.ast.Name;

public class SemanticError extends CompilerError {
    public final ErrorMessage msg;
    public final String[] args;
    public final ASTNode<?> node;

    public SemanticError(ASTNode<?> node, ErrorMessage msg, String... args) {
        this.node = node;
        this.msg = msg;
        this.args = args;
    }

    public SemanticError(ASTNode<?> node, ErrorMessage msg, Name... args) {
        this.node = node;
        this.msg = msg;

        this.args = new String[args.length];
        for (int i = 0; i < args.length; i++) {
            this.args[i] = args[i].getString();
        }
    }

    @Override
    public String getFileName() {
        if (file == null) {
            ASTNode<?> parent = node;
            while (!(parent instanceof CompilationUnit)) {
                parent = parent.getParent();
                if (parent == null)
                    return "<could not find filename>";
            }
            CompilationUnit u = (CompilationUnit) parent;
            String name = u.getName();
            if (name == null)
                return "<unkown>";
            file = new File(name);
        }
        return super.getFileName();
    }

    @Override
    public int getLine() {
        return Symbol.getLine(node.getStart());
    }

    @Override
    public int getColumn() {
        return Symbol.getColumn(node.getStart());
    }

    public ASTNode<?> getNode() {
        return node;
    }

    @Override
    public String getMessage() {
        return getMsg();
    }

    public String getMsg() {
        return msg.withArgs(args);
    }

    public String getMsgWithHint(String absCode) {
        return getHelpMessage() + "\n" + getHint(absCode);
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
        for (int c = 0; c < getColumn() - 1; c++) {
            lineHint.append('-');
        }
        lineHint.append('^');
        return line + "\n" + lineHint;
    }

    /**
     * Deprecated. Use getHelpMessage() instead
     * 
     * @return
     */
    @Deprecated
    public String getMsgString() {
        return getHelpMessage();
    }
}
