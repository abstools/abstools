/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.typechecker;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import abs.frontend.ast.Annotation;
import abs.frontend.ast.Decl;
import abs.frontend.ast.FieldDecl;
import abs.frontend.ast.MethodSig;
import abs.frontend.ast.TypeUse;

public abstract class Type {
    private static final Object ANNOTATION_KEY = "ANNOTATION_KEY";
    
    protected Map<Object,Object> metaData = new HashMap<Object,Object>();
    {
        metaData.put(ANNOTATION_KEY, Collections.EMPTY_LIST);
    }
    
    public Type withAnnotations(abs.frontend.ast.List<Annotation> anns) {
        Type copy = this.fullCopy();
        List<TypeAnnotation> newAnnos = new ArrayList<TypeAnnotation>();
        @SuppressWarnings("unchecked")
        List<TypeAnnotation> annos = (List<TypeAnnotation>) copy.getMetaData(ANNOTATION_KEY);
        if (annos != null) {
            newAnnos.addAll(annos);
        }
        newAnnos.addAll(convertToTypeAnnotations(anns));
        copy.addMetaData(ANNOTATION_KEY, newAnnos);
        return copy;
    }
    
    public void addMetaData(Object key, Object value) {
        metaData.put(key, value);
    }
    
    public Object getMetaData(Object key) {
        return metaData.get(key);
    }
    
    public Type fullCopy() {
        Type copy = copy();
        copy.metaData.putAll(metaData);
        return copy;
    }
    
    protected abstract Type copy();
    
    private List<TypeAnnotation> convertToTypeAnnotations(abs.frontend.ast.List<Annotation> anns) {
        ArrayList<TypeAnnotation> res = new ArrayList<TypeAnnotation>();
        for (Annotation a : anns) {
            Type t = a.getType();
            if (t.isAnnotationType()) {
                res.add(new TypeAnnotation(a));
            }
        }
        return res;
    }

    
    /**
     * A string representation of this type
     */
    public String toString() {
        StringBuilder res = new StringBuilder();
        if (!getTypeAnnotations().isEmpty()) {
            res.append("[");
            boolean first = true;
            for (TypeAnnotation a : getTypeAnnotations()) {
                if (first) first = false;
                else res.append(",");
                res.append(a.toString());
            }
            res.append("] ");
        }
        
        res.append(getQualifiedName());
        return res.toString();
    }

    /**
     * Returns the full qualified name of this type.
     * Returns in general getModuleName()+"."+getSimpleName(),
     * if however getModuleName() == null it returns getSimpleName();
     * @return the full qualified name of this type
     */
    public String getQualifiedName() {
        String modulePart = getModuleName();
        modulePart = modulePart == null ? "" : modulePart+".";
        return modulePart+getSimpleName();
    }
    
    /**
     * The module name of this type.
     * e.g., for type ABS.StdLib.List<Bool> returns ABS.StdLib
     * This may return null for special built-in types that
     * are not declared in a module
     * @return the module name of this type
     */
    public String getModuleName() {
        return null;
    }

    /**
     * The simple name of this type without the module name.
     * Does not include type arguments. 
     * E.g. for type ABS.StdLib.List<Bool> returns List
     * @return the simple name of this type without the module name
     */
    public abstract String getSimpleName();

    /**
     * Whether a type may contain references to objects.
     * Reference types are the simplest cases, but
     * algebraic types may also contain references.
     */
    public boolean hasReferences() {
        return isReferenceType();
    }

    /**
     * A type is an annotation type if and only if it is a data type declaration
     * and it has an annotation [TypeAnnotation]
     * @return
     */
    public boolean isAnnotationType() {
        return false;
    }
    
    public boolean isReferenceType() {
        return false;
    }

    public boolean isInterfaceType() {
        return false;
    }

    public boolean isDataType() {
        return false;
    }

    public boolean isNullType() {
        return false;
    }

    public boolean isTypeParameter() {
        return false;
    }

    public boolean isUnknownType() {
        return false;
    }

    public boolean isFutureType() {
        return false;
    }

    public boolean isBoolType() {
        return false;
    }

    public boolean isUnitType() {
        return false;
    }

    public boolean isStringType() {
        return false;
    }

    public boolean isIntType() {
        return false;
    }

    public boolean isRatType() {
        return false;
    }

    public boolean isNumericType() {
        return isIntType() || isRatType();
    }

    public boolean isExceptionType() {
        return false;
    }

    public boolean isAnyType() {
        return false;
    }

    public boolean isDeploymentComponentType() {
        return false;
    }

    public MethodSig lookupMethod(String name) {
        return null;
    }

    public boolean equals(Object o) {
        if (o == null)
            return false;
        if (!(o instanceof Type))
            return false;
        System.err.println(this+":"+o);
        return true;
    }

    public int hashCode() {
        assert false : "hashCode not designed:"+this;
        return 42; // any arbitrary constant will do
        }

    public boolean isBoundedType() {
        return false;
    }

    public boolean isAssignable(Type t, boolean considerSubtyping) {
        return isAssignable(t);
    }

    public boolean isAssignable(Type t) {
        if (t == null)
            throw new IllegalArgumentException("t is null");

        if (t.isAnyType())
            return true;

        if (this.equals(t))
            return true;

        if (t.isBoundedType()) {
            BoundedType bt = (BoundedType) t;
            if (bt.hasBoundType())
                return this.isAssignable(bt.getBoundType());
            bt.bindTo(this);
            return true;
        }

        return false;
    }

    public boolean isUnionType() {
        return false;
    }

    public boolean canBeBoundTo(Type t) {
        return false;
    }

    @SuppressWarnings("unchecked")
    public List<TypeAnnotation> getTypeAnnotations() {
        return Collections.unmodifiableList((List<TypeAnnotation>) getMetaData(ANNOTATION_KEY));
    }
    
    public Collection<MethodSig> getAllMethodSigs() {
        return Collections.emptyList();
    }
    
    public Collection<FieldDecl> getAllFieldDecls() {
        return Collections.emptyList();
    }
    
    /**
     * returns the declaration of this type or null if there is no 
     * declaration for this type
     */
    public Decl getDecl() {
        return null;
    }
    
    public Type applyBinding(Map<TypeParameter, Type> binding) { return this; }
    
    /**
     * Use by the rewriting logic for AwaitAsyncCalls in GenerateCoreAbs.
     */
    public TypeUse toUse() {
        assert false : "Should never happen for a well-typed model:" + getClass().getSimpleName()+"/"+getSimpleName();
        return null;
    }
}
