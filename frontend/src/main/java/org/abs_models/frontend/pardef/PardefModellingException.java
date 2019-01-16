package org.abs_models.frontend.pardef;

import org.abs_models.frontend.analyser.ErrorMessage;
import org.abs_models.frontend.analyser.SemanticError;
import org.abs_models.frontend.ast.ASTNode;

public class PardefModellingException extends RuntimeException {

    public PardefModellingException() {
        super();
    }

    public PardefModellingException(String message) {
        super(message);
    }

    public PardefModellingException(String message, Throwable cause) {
        super(message, cause);
    }

    public PardefModellingException(ASTNode<?> node, ErrorMessage msg, Object... args) {
        super(createMessage(node, msg, args));
    }

    public PardefModellingException(Throwable cause, ASTNode<?> node, ErrorMessage message, Object... args) {
        super(createMessage(node, message, args), cause);
    }

    private static String createMessage(ASTNode<?> node, ErrorMessage message, Object... args) {
        return new SemanticError(node, message, toString(args)).getHelpMessage();
        /* StringWriter stringWriter = new StringWriter();
        try (PrintWriter printWriter = new PrintWriter(stringWriter)) {
            node.doPrettyPrint(printWriter, new DefaultABSFormatter(printWriter));
        }
        return error.getMsgWithHint(stringWriter.toString());*/
    }

    private static String[] toString(Object[] args) {
        String[] result = new String[args.length];
        for (int index = 0; index < result.length; ++index) {
            result[index] = String.valueOf(args[index]);
        }
        return result;
    }
}
