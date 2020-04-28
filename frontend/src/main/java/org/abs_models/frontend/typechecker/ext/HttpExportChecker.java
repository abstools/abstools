/**
 * Copyright (c) 2016, The Envisage Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.typechecker.ext;

import java.util.Set;
import java.util.HashSet;

import org.abs_models.frontend.analyser.ErrorMessage;
import org.abs_models.frontend.analyser.SemanticWarning;
import org.abs_models.frontend.analyser.AnnotationHelper;
import org.abs_models.frontend.analyser.TypeError;
import org.abs_models.frontend.ast.*;
import org.abs_models.frontend.typechecker.Type;
import org.abs_models.frontend.typechecker.DataTypeType;

/**
 * @author rudi
 *
 * Checks for type correctness of `HTTPName' annotations.
 *
 * - Parameters of methods annotated with `HTTPCallable' must have supported
 *   types
 *
 * - `HTTPName' annotation must be of type String
 *
 * - Variables declared as accessibel via `HTTPName' must be of some interface
 *   type
 */
public class HttpExportChecker extends DefaultTypeSystemExtension {

    public HttpExportChecker(Model m) {
        super(m);
    }

    private void checkHTTPNameCorrect(ASTNode<?> n, Type t, PureExp restname) {
        if (restname == null) return;
        if (!restname.getType().isStringType()) {
            errors.add(new TypeError(n, ErrorMessage.WRONG_HTTPNAME, restname.getType().getQualifiedName()));
        }
        if (!t.isInterfaceType()) {
            errors.add(new TypeError(n, ErrorMessage.WRONG_HTTP_OBJECT, t.getQualifiedName()));
        }
    }

    private boolean isTypeUsableFromHTTP(Type paramtype) {
        if (paramtype.isBoolType()) return true;
        if (paramtype.isStringType()) return true;
        if (paramtype.isIntType()) return true;
        if (paramtype.isFloatType()) return true;
        if (paramtype.isDataType()) {
            DataTypeType ptype = (DataTypeType) paramtype;
            if (ptype.getDecl().getQualifiedName().equals("ABS.StdLib.List")) {
                return isTypeUsableFromHTTP(ptype.getTypeArg(0));
            }
            if (ptype.getDecl().getQualifiedName().equals("ABS.StdLib.Map")) {
                return ptype.getTypeArg(0).isStringType()
                    && isTypeUsableFromHTTP(ptype.getTypeArg(1));
            }
            return false;
        }
        return false;
    }

    @Override
    public void checkExpressionStmt(ExpressionStmt expressionStmt) {
        checkHTTPNameCorrect(expressionStmt, expressionStmt.getExp().getType(),
                             AnnotationHelper.getAnnotationValueFromName(expressionStmt.getAnnotations(), "ABS.StdLib.HTTPName"));
    }

    @Override
    public void checkAssignStmt(AssignStmt s) {
        checkHTTPNameCorrect(s, s.getVar().getType(),
                             AnnotationHelper.getAnnotationValueFromName(s.getAnnotations(), "ABS.StdLib.HTTPName"));
    }

    @Override
    public void checkVarDeclStmt(VarDeclStmt varDeclStmt) {
        checkHTTPNameCorrect(varDeclStmt, varDeclStmt.getVarDecl().getType(),
                             AnnotationHelper.getAnnotationValueFromName(varDeclStmt.getAnnotations(), "ABS.StdLib.HTTPName"));
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
                List<Annotation> ann = ca.getTypeUse().getAnnotations();
                PureExp keyann = AnnotationHelper.getAnnotationValueFromName(ann, "ABS.StdLib.HTTPName");
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
