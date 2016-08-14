package org.absmodels.abs.plugin.editor.views.typehierarchy;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

import abs.frontend.ast.DataConstructor;
import abs.frontend.ast.DataTypeDecl;
import abs.frontend.ast.Decl;
import abs.frontend.ast.HasTypeHierarchy;
import abs.frontend.ast.InterfaceDecl;
import abs.frontend.ast.TypeSynDecl;
import abs.frontend.typechecker.InterfaceType;
import abs.frontend.typechecker.Type;
import abs.frontend.typechecker.UnionType;

public class TypeHierarchyContentProvider implements ITreeContentProvider {

    private static final Object[] nothing = new Object[] {};

    @Override
    public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
    }
    
    @Override
    public void dispose() {
    }


    @Override
    public Object[] getElements(Object inputElement) {
        if (inputElement instanceof Type) {
            Type type = (Type) inputElement;
            if (type.getDecl() != null) {
                return new Object[] { type.getDecl() };
            }
            if (type instanceof UnionType) {
                UnionType unionType = (UnionType) type;
                java.util.List<Object> result = new ArrayList<Object>();
                for (InterfaceType t : unionType.getTypes()) {
                    for (Object e : getElements(t)) {
                        result.add(e);
                    }
                }
                return result.toArray();
            }
        }
        
        return nothing;
    }

    @Override
    public Object[] getChildren(Object node) {
        if (node instanceof InterfaceDecl) {
            InterfaceDecl interfaceDecl = (InterfaceDecl) node;
            Collection<HasTypeHierarchy> subTypes = interfaceDecl.getDirectSubTypes();
            return subTypes.toArray();
        } else if (node instanceof DataTypeDecl) {
            DataTypeDecl dataTypeDecl = (DataTypeDecl) node;
            Object[] result = new Object[dataTypeDecl.getNumDataConstructor()];
            int i = 0;
            for (DataConstructor d : dataTypeDecl.getDataConstructors()) {
                result[i] = d;
                i++;
            }
            return result;
        } else if (node instanceof TypeSynDecl) {
            TypeSynDecl typeSynDecl = (TypeSynDecl) node;
            return new Object[] { typeSynDecl.getType().getDecl() };
        }
        return nothing;
    }

    @Override
    public Object getParent(Object element) {
        return null;
    }

    @Override
    public boolean hasChildren(Object element) {
        return element instanceof InterfaceDecl 
                || element instanceof DataTypeDecl 
                || element instanceof TypeSynDecl;
    }


   

}
