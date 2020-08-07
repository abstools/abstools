package org.abs_models.frontend.typechecker.locationtypes;

import org.abs_models.frontend.ast.ClassDecl;
import org.abs_models.frontend.ast.MethodImpl;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.ast.ModuleDecl;

public class LocationTypeInferrer {
    private Scope scope = Scope.METHOD_LOCAL_FAR;
    private UnificationTable table = new UnificationTable();

    public void InferMethod(MethodImpl m) {}

    public void InferClass(ClassDecl d) {}

    public void InferModule(ModuleDecl m) {}

    public void InferModel(Model m) {}
}
