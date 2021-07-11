package org.abs_models.backend.c.codegen;

import java.util.List;

/**
 * CFunctionDecl represents a function declaration.
 */
public class CFunctionDecl {
    public final String name;
    public final String retType;
    public final List<CFunctionParam> params;

    public CFunctionDecl(String name, String retType, List<CFunctionParam> params) {
        this.name = name;
        this.retType = retType;
        this.params = params;
    }

    public String cString() {
        StringBuilder sb = new StringBuilder();
        sb.append(retType).append(" ").append(name).append("(");
        if (params.isEmpty()) {
            sb.append("void");
        } else {
            for (int i = 0; i < params.size(); i++) {
                if (i > 0) {
                    sb.append(", ");
                }
                CFunctionParam param = params.get(i);
                sb.append(param.type).append(" ").append(param.name);
            }
        }
        sb.append(")");
        return sb.toString();
    }
}
