/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.analyser;

import java.util.ArrayList;

import abs.frontend.ast.Annotation;
import abs.frontend.ast.DataConstructorExp;
import abs.frontend.ast.List;


public final class AnnotationHelper {
    private AnnotationHelper() {};

    public static java.util.List<Annotation> getAnnotationsOfType(List<Annotation> annos, String qualifiedName) {
        ArrayList<Annotation> res = new ArrayList<>();
        for (Annotation a : annos) {
            if (a.getType().getQualifiedName().equals(qualifiedName)) {
                DataConstructorExp de = (DataConstructorExp) a.getValue();
                res.add(a);
            }
        }
        return res;
    }
}
