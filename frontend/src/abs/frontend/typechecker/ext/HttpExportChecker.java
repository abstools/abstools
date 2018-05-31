/**
 * Copyright (c) 2016, The Envisage Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.typechecker.ext;

import java.util.Set;
import java.util.HashSet;

import abs.common.CompilerUtils;

import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.SemanticWarning;
import abs.frontend.analyser.TypeError;
import abs.frontend.ast.*;
import abs.frontend.typechecker.Type;
import abs.frontend.typechecker.DataTypeType;

public class HttpExportChecker extends DefaultTypeSystemExtension {

    public HttpExportChecker(Model m) {
        super(m);
    }

    private boolean isTypeUsableFromHTTP(Type paramtype) {
        if (paramtype.isBoolType()) return true;
        if (paramtype.isStringType()) return true;
        if (paramtype.isIntType()) return true;
        if (paramtype.isFloatType()) return true;
        if (paramtype.isDataType()) {
            DataTypeType ptype = (DataTypeType) paramtype;
            if (ptype.getDecl().qualifiedName().equals("ABS.StdLib.List")) {
                return isTypeUsableFromHTTP(ptype.getTypeArg(0));
            }
            return false;
        }
        return false;
    }

    @Override
    public void checkInterfaceDecl(InterfaceDecl i) {
        for (MethodSig ms : i.getBodyList()) {
            if (ms.isHTTPCallable()) {
                for (ParamDecl p : ms.getParamList()) {
                    if (!isTypeUsableFromHTTP(p.getType())) {
                        errors.add(new TypeError(p, ErrorMessage.WRONG_HTTPCALLABLE, p.getName(), p.getType().toString()));
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
