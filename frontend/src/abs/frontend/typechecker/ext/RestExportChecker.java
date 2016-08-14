/** 
 * Copyright (c) 2016, The Envisage Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.typechecker.ext;

import java.util.List;

import abs.common.CompilerUtils;

import abs.frontend.analyser.AnnotationHelper;
import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.TypeError;
import abs.frontend.ast.*;

public class RestExportChecker extends DefaultTypeSystemExtension {

    public RestExportChecker(Model m) {
        super(m);
    }

    private boolean isParameterUsableFromREST(ParamDecl param) {
        if (param.getType().isBoolType()) return true;
        if (param.getType().isStringType()) return true;
        if (param.getType().isIntType()) return true;
        return false;
    }

    @Override
    public void checkInterfaceDecl(InterfaceDecl i) {
        for (MethodSig ms : i.getBodyList()) {
            if (ms.isRESTCallable()) {
                for (ParamDecl p : ms.getParamList()) {
                    if (!isParameterUsableFromREST(p)) {
                        errors.add(new TypeError(p, ErrorMessage.WRONG_RESTCALLABLE, p.getName(), p.getType().getQualifiedName()));
                    }
                }
            }
        }
    }
}
