/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.analyser;

import java.util.ArrayList;

import org.abs_models.frontend.ast.Annotation;
import org.abs_models.frontend.ast.DataConstructorExp;
import org.abs_models.frontend.ast.List;
import org.abs_models.frontend.ast.PureExp;
import org.abs_models.frontend.ast.TypedAnnotation;


public final class AnnotationHelper {
    private AnnotationHelper() {};

    /**
     * Get all annotations of the specified type.
     *
     * @param annotations The list of annotations.  Can be {@code null}
     * @param annotationName The qualified name of the annotation
     *  type.
     * @return a {@code List} value, empty if nothing found
     */
    public static java.util.List<Annotation> getAnnotationsOfType(List<Annotation> annos, String qualifiedName) {
        ArrayList<Annotation> res = new ArrayList<>();
        if (annos == null) return res;
        for (Annotation a : annos) {
            if (a.getType().getQualifiedName().equals(qualifiedName)) {
                DataConstructorExp de = (DataConstructorExp) a.getValue();
                res.add(a);
            }
        }
        return res;
    }

    /**
     * Get the value of an annotation.  Will return the value of the first
     * annotation with the given simple name.
     *
     * @param annotations The list of annotations.  Can be {@code null}
     * @param annotationName The simple name (without module prefix) of the
     * annotation
     * @return a <code>PureExp</code> value or null
     */
    public static PureExp getAnnotationValueFromSimpleName(List<Annotation> annotations, String annotationName) {
        if (annotations == null) return null;
        for (Annotation a : annotations) {
            if (a instanceof TypedAnnotation) {
                TypedAnnotation ta = (TypedAnnotation)a;
                if (ta.getTypeIdUse().getName().equals(annotationName))
                    return ta.getValue();
            }
        }
        return null;
    }

    /**
     * Get the value of an annotation.  Returns the value of the first
     * annotation with the given qualified (module-prefixed) name or
     * {@code null} if none found.
     *
     * @param annotations The list of annotations.  Can be {@code null}
     * @param qualifiedAnnotationName The qualified name of the annotation
     * @return a <code>PureExp</code> value or {@code null} if none found
     */
    public static PureExp getAnnotationValueFromName(List<Annotation> annotations, String qualifiedAnnotationName) {
        if (annotations == null) return null;
        for (Annotation a : annotations) {
            if (a instanceof TypedAnnotation) {
                TypedAnnotation ta = (TypedAnnotation)a;
                if (ta.getTypeIdUse().getDecl().getQualifiedName().equals(qualifiedAnnotationName))
                    return ta.getValue();
            }
        }
        return null;
    }


}
