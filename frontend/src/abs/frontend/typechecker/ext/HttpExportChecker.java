/**
 * Copyright (c) 2016, The Envisage Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.typechecker.ext;

import java.util.List;
import java.util.Set;
import java.util.HashSet;

import abs.common.CompilerUtils;

import abs.frontend.analyser.AnnotationHelper;
import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.SemanticWarning;
import abs.frontend.analyser.TypeError;
import abs.frontend.ast.*;

public class HttpExportChecker extends DefaultTypeSystemExtension {

    public HttpExportChecker(Model m) {
        super(m);
    }

    private boolean isParameterUsableFromHTTP(ParamDecl param) {
        if (param.getType().isBoolType()) return true;
        if (param.getType().isStringType()) return true;
        if (param.getType().isIntType()) return true;
        return false;
    }

    @Override
    public void checkInterfaceDecl(InterfaceDecl i) {
        for (MethodSig ms : i.getBodyList()) {
            if (ms.isHTTPCallable()) {
                for (ParamDecl p : ms.getParamList()) {
                    if (!isParameterUsableFromHTTP(p)) {
                        errors.add(new TypeError(p, ErrorMessage.WRONG_HTTPCALLABLE, p.getName(), p.getType().getQualifiedName()));
                    }
                }
            }
        }
    }

    @Override
    public void checkDataTypeDecl(DataTypeDecl decl) {
        for (DataConstructor c : decl.getDataConstructors()) {
            Set<String> names = new HashSet<>();
            for (ConstructorArg ca : c.getConstructorArgs()) {
                ASTNode arg = null;
                String key = null;
                abs.frontend.ast.List<Annotation> ann = ca.getTypeUse().getAnnotations();
                PureExp keyann = CompilerUtils.getAnnotationValueFromName(ann, "ABS.StdLib.HTTPName");
                if (keyann != null) {
                    if (!(keyann instanceof StringLiteral)) {
                        errors.add(new TypeError(keyann, ErrorMessage.WRONG_HTTPNAME, keyann.getType()));
                    } else {
                        key = ((StringLiteral)keyann).getContent();
                        arg = keyann;
                    }
                }
                if (ca.hasSelectorName() && key == null) {
                    key = ca.getSelectorName().toString();
                    arg = ca.getSelectorName();
                }
                if (key != null) {
                    if (names.contains(key)) {
                        errors.add(new SemanticWarning(arg, ErrorMessage.DUPLICATE_HTTPNAME, key));
                    } else {
                        names.add(key);
                    }
                }
            }
        }
    }
}
