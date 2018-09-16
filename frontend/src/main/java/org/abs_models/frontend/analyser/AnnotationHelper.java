/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.analyser;

import java.util.ArrayList;

import org.abs_models.frontend.ast.Annotation;
import org.abs_models.frontend.ast.DataConstructorExp;
import org.abs_models.frontend.ast.List;


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
