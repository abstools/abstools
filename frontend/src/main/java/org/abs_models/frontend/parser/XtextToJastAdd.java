package org.abs_models.frontend.parser;

import org.abs_models.common.NotImplementedYetException;
import org.abs_models.frontend.ast.*;
import org.abs_models.xtext.abs.AbsPackage;
import org.abs_models.xtext.abs.AndExp;
import org.abs_models.xtext.abs.AwaitExp;
import org.abs_models.xtext.abs.CaseExpBranch;
import org.abs_models.xtext.abs.CaseStmtBranch;
import org.abs_models.xtext.abs.ConstructorAppExp;
import org.abs_models.xtext.abs.DataConstructorParamDecl;
import org.abs_models.xtext.abs.ExpGuard;
import org.abs_models.xtext.abs.FloatLiteralPattern;
import org.abs_models.xtext.abs.FunctionAppExp;
import org.abs_models.xtext.abs.IntLiteralPattern;
import org.abs_models.xtext.abs.MethodCallExp;
import org.abs_models.xtext.abs.MethodDecl;
import org.abs_models.xtext.abs.MethodSignature;
import org.abs_models.xtext.abs.OrExp;
import org.abs_models.xtext.abs.OriginalCallExp;
import org.abs_models.xtext.abs.PartialFunctionAppExp;
import org.abs_models.xtext.abs.StringLiteralPattern;
import org.abs_models.xtext.abs.TemplateStringExp;
import org.abs_models.xtext.abs.VarOrFieldExp;
import org.abs_models.xtext.abs.VariablePattern;
import org.abs_models.xtext.abs.VariadicFunctionAppExp;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.nodemodel.INode;
import org.eclipse.xtext.nodemodel.util.NodeModelUtils;
import org.eclipse.xtext.resource.ILocationInFileProvider;
import org.eclipse.xtext.resource.XtextResourceSet;
import org.eclipse.xtext.util.ITextRegionWithLineInformation;
import org.eclipse.xtext.util.LineAndColumn;

// The methods in this class are roughly in the same order as the grammar rule
// / class definitions in Abs.xtext
public class XtextToJastAdd {

    private static ILocationInFileProvider location_provider = Main.absinjector.getInstance(ILocationInFileProvider.class);
    private static <T extends ASTNode<?>> T nodeWithLocation(T node, EObject obj) {
        INode n = NodeModelUtils.findActualNodeFor(obj); // do we want .getNode() instead?
        if (n != null) {
            ITextRegionWithLineInformation location = (ITextRegionWithLineInformation)location_provider.getSignificantTextRegion(obj);
            // End location is untested since we only print beginning
            // locations in error output
            LineAndColumn beg = NodeModelUtils.getLineAndColumn(n, location.getOffset());
            LineAndColumn end = NodeModelUtils.getLineAndColumn(n, location.getOffset() + location.getLength());
            // Xtext column values are 1-based, we want 0-based
            node.setPosition(beg.getLine(), beg.getColumn() - 1,
                             end.getLine(), end.getColumn() - 1);
            String filename = obj.eResource().getURI().toFileString();
            if (filename == null) filename = obj.eResource().getURI().toPlatformString(false);
            node.setFileName(filename);
        }
        return node;
    }

    private static <T extends ASTNode<?>> T nodeWithLocation(T node, EObject obj, EStructuralFeature feature) {
        // FIXME: there’s a constant defined somewhere deep in xtext for this
        // magic 0 value -- INSIGNIFICANT_INDEX or similar name
        return nodeWithLocation(node, obj, feature, 0);
    }


    private static <T extends ASTNode<?>> T nodeWithLocation(T node, EObject obj, EStructuralFeature feature, int indexInList) {
        INode n = NodeModelUtils.findActualNodeFor(obj); // do we want .getNode() instead?
        if (n != null) {
            ITextRegionWithLineInformation location = (ITextRegionWithLineInformation)location_provider.getSignificantTextRegion(obj, feature, indexInList);
            // End location is untested since we only print beginning
            // locations in error output
            LineAndColumn beg = NodeModelUtils.getLineAndColumn(n, location.getOffset());
            LineAndColumn end = NodeModelUtils.getLineAndColumn(n, location.getOffset() + location.getLength());
            // Xtext column values are 1-based, we want 0-based
            node.setPosition(beg.getLine(), beg.getColumn() - 1,
                             end.getLine(), end.getColumn() - 1);
            String filename = obj.eResource().getURI().toFileString();
            if (filename == null) filename = "(standard library)";
            node.setFileName(filename);
        }
        return node;
    }

    /**
     * The main entry point for Xtext-to-JastAdd conversion.  Takes a resource
     * set containing ABS files, parsed and validated by Xtext.  Does not
     * check for parse / validation errors.
     *
     * @param resourceSet The parsed ABS files, including one resource for the standard library
     * @return a fresh JastAdd Model object
     */
    public static Model fromResourceSet(XtextResourceSet resourceSet) {
        Model result = new Model();
        for (Resource r : resourceSet.getResources()) {
            for (EObject unit : r.getContents()) {
                result.addCompilationUnitNoTransform(fromXtext((org.abs_models.xtext.abs.CompilationUnit) unit));
            }
        }
        return result;
    }

    static CompilationUnit fromXtext(org.abs_models.xtext.abs.CompilationUnit xtext_unit) {
        CompilationUnit result = new CompilationUnit();
        result.setName(xtext_unit.eResource().getURI().toFileString());
        for (org.abs_models.xtext.abs.ModuleDecl module : xtext_unit.getModules()) {
            result.addModuleDeclNoTransform(fromXtext(module));
        }
        return nodeWithLocation(result, xtext_unit);
    }

    static ModuleDecl fromXtext(org.abs_models.xtext.abs.ModuleDecl xtext_module) {
        ModuleDecl result = new ModuleDecl();
        result.setName(xtext_module.getName());
        for (org.abs_models.xtext.abs.ModuleExport export : xtext_module.getExports()) {
            Export e;
            if (export.isStar()) {
                // "export *;"
                // "export * from OtherModule;"
                StarExport se = new StarExport();
                e = se;
                if (export.getModulename() != null)
                    se.setModuleName(nodeWithLocation(new Name(export.getModulename()), export, AbsPackage.eINSTANCE.getModuleExport_Modulename()));
            } else if (export.getModulename() != null) {
                // "export a, b from OtherModule;"
                FromExport fe = new FromExport();
                e = fe;
                fe.setModuleName(export.getModulename());
                for (int i = 0; i < export.getIdentifiers().size(); i++) {
                    String id = export.getIdentifiers().get(i);
                    fe.addNameNoTransform(nodeWithLocation(new Name(id), export, AbsPackage.eINSTANCE.getModuleExport_Identifiers(), i));
                }
            } else {
                // "export a, b;"
                NamedExport ne = new NamedExport();
                e = ne;
                for (int i = 0; i < export.getIdentifiers().size(); i++) {
                    String id = export.getIdentifiers().get(i);
                    ne.addNameNoTransform(nodeWithLocation(new Name(id), export, AbsPackage.eINSTANCE.getModuleExport_Identifiers(), i));
                }
            }
            result.addExportNoTransform(nodeWithLocation(e, export));
        }
        for (org.abs_models.xtext.abs.ModuleImport imp : xtext_module.getImports()) {
            Import ji;
            if (imp.isStar()) {
                // "import * from OtherModule;"
                StarImport si = new StarImport(imp.getModulename());
                ji = si;
            } else if (imp.getModulename() != null) {
                // "import a, b from OtherModule;"
                FromImport fi = new FromImport();
                ji = fi;
                fi.setModuleName(imp.getModulename());
                for (int i = 0; i < imp.getIdentifiers().size(); i++) {
                    String id = imp.getIdentifiers().get(i);
                    fi.addNameNoTransform(nodeWithLocation(new Name(id), imp, AbsPackage.eINSTANCE.getModuleImport_Identifiers(), i));
                }
            } else {
                // "import OtherModule.a, OtherModule.b;"
                NamedImport ni = new NamedImport();
                ji = ni;
                for (int i = 0; i < imp.getIdentifiers().size(); i++) {
                    String id = imp.getIdentifiers().get(i);
                    ni.addNameNoTransform(nodeWithLocation(new Name(id), imp, AbsPackage.eINSTANCE.getModuleImport_Identifiers(), i));
                }
            }
            result.addImportNoTransform(nodeWithLocation(ji, imp));
        }
        for (org.abs_models.xtext.abs.Declaration decl : xtext_module.getDeclarations()) {
            result.addDeclNoTransform(fromXtext(decl));
        }

        if(xtext_module.getMainblockstmts() != null) {
            List<Stmt> statements = new List<>();
            for(org.abs_models.xtext.abs.Stmt stmt : xtext_module.getMainblockstmts()) {
                statements.add(fromXtext(stmt));
            }

            final MainBlock mainBlock = new MainBlock();
            mainBlock.setStmtList(statements);
            result.setBlock(mainBlock);
        }
        return nodeWithLocation(result, xtext_module);
    }

    private static List<Annotation> annotationsfromXtext(org.abs_models.xtext.abs.Annotations annotations) {
        List<Annotation> annotationList = new List<>();
        for(org.abs_models.xtext.abs.Annotation annotation : annotations.getAnnotations()) {
            Annotation astAnnotation;
            PureExp exp = pureExpFromXtext(annotation.getValue());
            if (annotation.getId() != null) {
                astAnnotation = new TypedAnnotation(exp, nodeWithLocation(new UnresolvedTypeUse(annotation.getId(), new List<>()), annotation, AbsPackage.eINSTANCE.getAnnotation_Id()));
            } else {
                astAnnotation = new Annotation(exp);
            }
            annotationList.add(nodeWithLocation(astAnnotation, annotation));
        }
        return annotationList;
    }

    static Decl fromXtext(org.abs_models.xtext.abs.Declaration xtext_decl) {
        Decl result = null;
        // This manual, explicit dispatch will complain when someone adds a
        // new type of declaration; this is something we want (if it doesn’t
        // cost too much)
        if (xtext_decl instanceof org.abs_models.xtext.abs.DataTypeDecl) {
            result = fromXtext((org.abs_models.xtext.abs.DataTypeDecl) xtext_decl);
        } else if (xtext_decl instanceof org.abs_models.xtext.abs.TypeSynonymDecl) {
            result = fromXtext((org.abs_models.xtext.abs.TypeSynonymDecl) xtext_decl);
        } else if (xtext_decl instanceof org.abs_models.xtext.abs.ExceptionDecl) {
            result = fromXtext((org.abs_models.xtext.abs.ExceptionDecl) xtext_decl);
        } else if (xtext_decl instanceof org.abs_models.xtext.abs.FunctionDecl) {
            result = fromXtext((org.abs_models.xtext.abs.FunctionDecl) xtext_decl);
        } else if (xtext_decl instanceof org.abs_models.xtext.abs.PartialFunctionDecl) {
            result = fromXtext((org.abs_models.xtext.abs.PartialFunctionDecl) xtext_decl);
        } else if (xtext_decl instanceof org.abs_models.xtext.abs.InterfaceDecl) {
            result = fromXtext((org.abs_models.xtext.abs.InterfaceDecl) xtext_decl);
        } else if (xtext_decl instanceof org.abs_models.xtext.abs.ClassDecl) {
            result = fromXtext((org.abs_models.xtext.abs.ClassDecl) xtext_decl);
        } else {
            throw new NotImplementedYetException(new ASTNode(),
                                                 "No conversion to JastAdd implemented for Xtext node "
                                                 + xtext_decl.getClass().toString());
        }
        return result;
    }

    static DataTypeDecl fromXtext(org.abs_models.xtext.abs.DataTypeDecl xtext_decl) {
        DataTypeDecl result;
        if (xtext_decl.getTypeparams().isEmpty()) {
            result = new DataTypeDecl();
        } else {
            result = new ParametricDataTypeDecl();
            for (int i = 0; i < xtext_decl.getTypeparams().size(); i++) {
                String tp = xtext_decl.getTypeparams().get(i);
                ((ParametricDataTypeDecl)result).addTypeParameterNoTransform(nodeWithLocation(new TypeParameterDecl(tp), xtext_decl, AbsPackage.eINSTANCE.getDataTypeDecl_Typeparams(), i));
            }
        }
        result.setName(xtext_decl.getName());

        result.setAnnotationList(annotationsfromXtext(xtext_decl.getAnnotations()));

        for (org.abs_models.xtext.abs.DataConstructorDecl xtext_d : xtext_decl.getConstructors()) {
             result.addDataConstructor(fromXtext(xtext_d));
        }
        return nodeWithLocation(result, xtext_decl);
    }

    private static DataConstructor fromXtext(org.abs_models.xtext.abs.DataConstructorDecl xtext_d) {
        DataConstructor constructor = new DataConstructor();
        constructor.setName(xtext_d.getName());

        for (DataConstructorParamDecl arg : xtext_d.getArgs()) {
            constructor.addConstructorArgNoTransform(fromXtext(arg));
        }

        return nodeWithLocation(constructor, xtext_d);
    }

    private static ConstructorArg fromXtext(DataConstructorParamDecl xtext_arg) {
        ConstructorArg constructorArg = new ConstructorArg();
        if(xtext_arg.getName() != null) {
            constructorArg.setSelectorName(nodeWithLocation(new Name(xtext_arg.getName()), xtext_arg, AbsPackage.eINSTANCE.getDataConstructorParamDecl_Name()));
        }
        constructorArg.setTypeUse(fromXtext(xtext_arg.getType()));
        return nodeWithLocation(constructorArg, xtext_arg);
    }

    static TypeSynDecl fromXtext(org.abs_models.xtext.abs.TypeSynonymDecl xtext_decl) {
        TypeSynDecl result = new TypeSynDecl();
        result.setName(xtext_decl.getName());
        result.setAnnotationList(annotationsfromXtext(xtext_decl.getAnnotations()));
        result.setValue(fromXtext(xtext_decl.getType()));
        return nodeWithLocation(result, xtext_decl);
    }

    static ExceptionDecl fromXtext(org.abs_models.xtext.abs.ExceptionDecl xtext_decl) {
        ExceptionDecl result = new  ExceptionDecl();
        result.setName(xtext_decl.getName());

        result.setAnnotationList(annotationsfromXtext(xtext_decl.getAnnotations()));

        DataConstructor constructor = new DataConstructor();
        constructor.setName(xtext_decl.getName());

        for (DataConstructorParamDecl arg : xtext_decl.getArgs()) {
            constructor.addConstructorArgNoTransform(fromXtext(arg));
        }

        result.setDataConstructorList(new List<>(constructor));

        return nodeWithLocation(result, xtext_decl);
    }

    static FunctionDecl fromXtext(org.abs_models.xtext.abs.FunctionDecl xtext_decl) {
        FunctionDecl result;
        if (!xtext_decl.getTypeparams().isEmpty()) {
            ParametricFunctionDecl presult = new ParametricFunctionDecl();
            result = presult;
            for (int i = 0; i < xtext_decl.getTypeparams().size(); i++) {
                String tp = xtext_decl.getTypeparams().get(i);
                presult.addTypeParameterNoTransform(nodeWithLocation(new TypeParameterDecl(tp), xtext_decl, AbsPackage.eINSTANCE.getFunctionDecl_Typeparams(), i));
            }
        } else {
            result = new  FunctionDecl();
        }
        result.setName(xtext_decl.getName());
        result.setAnnotationList(annotationsfromXtext(xtext_decl.getAnnotations()));
        result.setTypeUse(fromXtext(xtext_decl.getResulttype()));

        for(org.abs_models.xtext.abs.ParamDecl arg : xtext_decl.getArgs()) {
            result.addParamNoTransform(fromXtext(arg));
        }

        if (xtext_decl.isBuiltin()) {
            result.setFunctionDef(nodeWithLocation(new BuiltinFunctionDef(), xtext_decl, AbsPackage.eINSTANCE.getFunctionDecl_Builtin()));
            ;
        } else {
            PureExp exp = pureExpFromXtext(xtext_decl.getBody());
            result.setFunctionDef(nodeWithLocation(new ExpFunctionDef(exp), xtext_decl.getBody()));
        }
        return nodeWithLocation(result, xtext_decl);
    }

    static PartialFunctionDecl fromXtext(org.abs_models.xtext.abs.PartialFunctionDecl xtext_decl) {
        PartialFunctionDecl result;
        if (!xtext_decl.getTypeparams().isEmpty()) {
            ParametricPartialFunctionDecl presult = new ParametricPartialFunctionDecl();
            result = presult;
            for (int i = 0; i < xtext_decl.getTypeparams().size(); i++) {
                String tp = xtext_decl.getTypeparams().get(i);
                presult.addTypeParameterNoTransform(nodeWithLocation(new TypeParameterDecl(tp), xtext_decl, AbsPackage.eINSTANCE.getPartialFunctionDecl_Typeparams(), i));
            }
        } else {
            result = new  PartialFunctionDecl();
        }
        result.setName(xtext_decl.getName());
        result.setAnnotationList(annotationsfromXtext(xtext_decl.getAnnotations()));
        result.setTypeUse(fromXtext(xtext_decl.getResulttype()));

        for(org.abs_models.xtext.abs.ParamDecl arg : xtext_decl.getArgs()) {
            result.addParamNoTransform(fromXtext(arg));
        }

        for (int i = 0; i < xtext_decl.getFunction_args().size(); i++) {
            String fArg = xtext_decl.getFunction_args().get(i);
            result.addFuncParamNoTransform(nodeWithLocation(new FunctionParamDecl(fArg), xtext_decl, AbsPackage.eINSTANCE.getPartialFunctionDecl_Function_args(), i));
        }

        PartialFunctionDef functionDef = new PartialFunctionDef(pureExpFromXtext(xtext_decl.getBody()));
        result.setPartialFunctionDef(nodeWithLocation(functionDef, xtext_decl.getBody()));

        return nodeWithLocation(result, xtext_decl);
    }

    static InterfaceDecl fromXtext(org.abs_models.xtext.abs.InterfaceDecl xtext_decl) {
        InterfaceDecl result = new  InterfaceDecl();
        result.setName(xtext_decl.getName());

        result.setAnnotationList(annotationsfromXtext(xtext_decl.getAnnotations()));

        for (int i = 0; i < xtext_decl.getSuperinterfaces().size(); i++) {
            String iname = xtext_decl.getSuperinterfaces().get(i);
            result.addExtendedInterfaceUseNoTransform(nodeWithLocation(new InterfaceTypeUse(iname, new List<>()), xtext_decl, AbsPackage.eINSTANCE.getInterfaceDecl_Superinterfaces(), i));
        }

        for (MethodSignature ms : xtext_decl.getMethods()) {
            result.addBodyNoTransform(fromXtext(ms));
        }

        return nodeWithLocation(result, xtext_decl);
    }

    private static MethodSig fromXtext(MethodSignature xtext_decl) {
        MethodSig result = new MethodSig();
        result.setName(result.getName());
        result.setAnnotationList(annotationsfromXtext(xtext_decl.getAnnotations()));
        for(org.abs_models.xtext.abs.ParamDecl arg : xtext_decl.getArgs()) {
            result.addParamNoTransform(fromXtext(arg));
        }
        result.setReturnType(fromXtext(xtext_decl.getResulttype()));
        return nodeWithLocation(result, xtext_decl);
    }

    static ClassDecl fromXtext(org.abs_models.xtext.abs.ClassDecl xtext_decl) {
        ClassDecl result = new  ClassDecl();
        result.setName(xtext_decl.getName());
        result.setAnnotationList(annotationsfromXtext(xtext_decl.getAnnotations()));
        for(org.abs_models.xtext.abs.ParamDecl arg : xtext_decl.getArgs()) {
            result.addParamNoTransform(fromXtext(arg));
        }

        for (int i = 0; i < xtext_decl.getInterfaces().size(); i++) {
            String iname = xtext_decl.getInterfaces().get(i);
            result.addImplementedInterfaceUseNoTransform(nodeWithLocation(new InterfaceTypeUse(iname, new List<>()), xtext_decl, AbsPackage.eINSTANCE.getInterfaceDecl_Superinterfaces(), i));
        }

        for(org.abs_models.xtext.abs.FieldDecl fieldDecl : xtext_decl.getFields()) {
            result.addFieldNoTransform(fromXtext(fieldDecl));
        }

        InitBlock astInitBlock = new InitBlock();
        for(org.abs_models.xtext.abs.Stmt statement : xtext_decl.getInitblockstmts()) {
            astInitBlock.addStmt(fromXtext(statement));
        }
        result.setInitBlock(astInitBlock);

        for(MethodDecl methodDecl : xtext_decl.getMethods()) {
            result.addMethodNoTransform(fromXtext(methodDecl));
        }

        for (CaseStmtBranch recover_branch : xtext_decl.getRecoverbranches()) {
            result.addRecoverBranchNoTransform(fromXtext(recover_branch));
        }

        return nodeWithLocation(result, xtext_decl);
    }

    private static FieldDecl fromXtext(org.abs_models.xtext.abs.FieldDecl xtext_decl) {
        FieldDecl result = new FieldDecl();
        result.setName(xtext_decl.getName());
        result.setAnnotationList(annotationsfromXtext(xtext_decl.getAnnotations()));
        if (xtext_decl.getInit() != null) {
            result.setInitExp(pureExpFromXtext(xtext_decl.getInit()));
        }
        result.setAccess(fromXtext(xtext_decl.getType()));
        return nodeWithLocation(result, xtext_decl);
    }

    private static MethodImpl fromXtext(org.abs_models.xtext.abs.MethodDecl xtext_decl) {
        MethodImpl result = new MethodImpl();

        MethodSig sig = new MethodSig();
        sig.setName(xtext_decl.getName());
        sig.setAnnotationList(annotationsfromXtext(xtext_decl.getAnnotations()));
        for(org.abs_models.xtext.abs.ParamDecl arg : xtext_decl.getArgs()) {
            sig.addParamNoTransform(fromXtext(arg));
        }
        sig.setReturnType(fromXtext(xtext_decl.getResulttype()));
        result.setMethodSig(sig);

        Block block = new Block();
        for(org.abs_models.xtext.abs.Stmt stmt : xtext_decl.getStatements()) {
            block.addStmtNoTransform(fromXtext(stmt));
        }
        result.setBlock(block);

        return nodeWithLocation(result, xtext_decl);
    }

    private static ParamDecl fromXtext(org.abs_models.xtext.abs.ParamDecl xtext_decl) {
        ParamDecl result = new ParamDecl();
        result.setName(xtext_decl.getName());
        result.setAnnotationList(annotationsfromXtext(xtext_decl.getAnnotations()));
        result.setAccess(fromXtext(xtext_decl.getType()));
        return nodeWithLocation(result, xtext_decl);
    }

    /**
     * Convert a statement from Xtext to JastAdd.  If the statement is not a
     * block, wrap it in a JastAdd Block.
     *
     * @param stmt The statement to be converted
     * @return a JastAdd Block containing the statement.
     */
    private static Block blockFromXtext(org.abs_models.xtext.abs.Stmt stmt) {
        Stmt result = fromXtext(stmt);
        if (result instanceof Block) {
            return (Block)result;
        } else {
            Block block = new Block();
            block.addStmtNoTransform(result);
            return nodeWithLocation(block, stmt);
        }
    }

    private static Stmt fromXtext(org.abs_models.xtext.abs.Stmt stmt) {
        Stmt result = null;

        if(stmt instanceof org.abs_models.xtext.abs.VarDeclStmt) {
            org.abs_models.xtext.abs.VarDeclStmt value = (org.abs_models.xtext.abs.VarDeclStmt) stmt;
            List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());
            VarDecl varDecl = new VarDecl();
            varDecl.setName(value.getName());
            varDecl.setAccess(fromXtext(value.getType()));
            if (value.getInit() != null) {
                varDecl.setInitExp(fromXtext(value.getInit()));
            }
            result = new VarDeclStmt(annotations, varDecl);
        }
        else if(stmt instanceof org.abs_models.xtext.abs.AssignStmt) {
            org.abs_models.xtext.abs.AssignStmt value = (org.abs_models.xtext.abs.AssignStmt) stmt;
            List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());
            org.abs_models.xtext.abs.Exp lhsExp = value.getLhs();
            VarOrFieldUse varOrFieldUse;
            if(lhsExp instanceof VarOrFieldExp) {
                VarOrFieldExp lhs = (VarOrFieldExp) lhsExp;
                if (lhs.isField()) {
                    varOrFieldUse = new FieldUse(lhs.getName());
                } else {
                    // might still get rewritten to FieldUse by JastAdd
                    // TODO: make this more precise once scoping / linking is
                    // implemented
                    varOrFieldUse = new VarUse(lhs.getName());
                }
            } else {
                assert false : "Invalid left-hand side expression in Xtext AST reached XtextToJastAdd -- check validation rules";
                varOrFieldUse = null;
            }
            Exp expresssion = fromXtext(value.getExp());
            result = new AssignStmt(annotations, varOrFieldUse, expresssion);
        }
        else if(stmt instanceof org.abs_models.xtext.abs.SkipStmt) {
            org.abs_models.xtext.abs.SkipStmt value = (org.abs_models.xtext.abs.SkipStmt) stmt;
            List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());
            result = new SkipStmt(annotations);
        }
        else if(stmt instanceof org.abs_models.xtext.abs.ReturnStmt) {
            org.abs_models.xtext.abs.ReturnStmt value = (org.abs_models.xtext.abs.ReturnStmt) stmt;
            List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());

            result = new ReturnStmt(annotations, fromXtext(value.getExp()));
        }
        else if(stmt instanceof org.abs_models.xtext.abs.AssertStmt) {
            org.abs_models.xtext.abs.AssertStmt value = (org.abs_models.xtext.abs.AssertStmt) stmt;
            List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());

            result = new AssertStmt(annotations, pureExpFromXtext(value.getExp()));
        }
        else if(stmt instanceof org.abs_models.xtext.abs.Block) {
            org.abs_models.xtext.abs.Block value = (org.abs_models.xtext.abs.Block) stmt;
            Block block = new Block();
            List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());

            for(org.abs_models.xtext.abs.Stmt subStmt : value.getStmts()) {
                block.addStmtNoTransform(fromXtext(subStmt));
            }
            block.setAnnotationList(annotations);
            result = block;
        }
        else if(stmt instanceof org.abs_models.xtext.abs.IfStmt) {
            org.abs_models.xtext.abs.IfStmt value = (org.abs_models.xtext.abs.IfStmt) stmt;
            IfStmt ifstmt = new IfStmt();
            ifstmt.setAnnotationList(annotationsfromXtext(value.getAnnotations()));
            ifstmt.setCondition(pureExpFromXtext(value.getCondition()));
            ifstmt.setThen(blockFromXtext(value.getConsequence()));
            if (value.getAlternate() != null) {
                ifstmt.setElse(blockFromXtext(value.getAlternate()));
            }
            result = ifstmt;
        }
        else if(stmt instanceof org.abs_models.xtext.abs.WhileStmt) {
            org.abs_models.xtext.abs.WhileStmt value = (org.abs_models.xtext.abs.WhileStmt) stmt;
            List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());
            PureExp condition = pureExpFromXtext(value.getCondition());
            Block body = blockFromXtext(value.getBody());
            result = new WhileStmt(annotations, condition, body);
        }
        else if(stmt instanceof org.abs_models.xtext.abs.ForeachStmt) {
            org.abs_models.xtext.abs.ForeachStmt value = (org.abs_models.xtext.abs.ForeachStmt) stmt;
            List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());
            LoopVarDecl var = new LoopVarDecl(value.getLoopvar());
            PureExp list = pureExpFromXtext(value.getList());
            Block body = blockFromXtext(value.getBody());
            result = new ForeachStmt(annotations, var, list, body);
        }
        else if(stmt instanceof org.abs_models.xtext.abs.TryCatchFinallyStmt) {
            org.abs_models.xtext.abs.TryCatchFinallyStmt value = (org.abs_models.xtext.abs.TryCatchFinallyStmt) stmt;
            List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());
            Block body = blockFromXtext(value.getBody());
            List<CaseBranchStmt> branches = caseBranchStmtsFromXtext(value.getBranches());
            Block finallyBlock = blockFromXtext(value.getFinally());
            Opt<Block> finallyOpt = new Opt<>(finallyBlock);
            result = new TryCatchFinallyStmt(annotations, body, branches, finallyOpt);
        }
        else if(stmt instanceof org.abs_models.xtext.abs.SuspendStmt) {
            org.abs_models.xtext.abs.SuspendStmt value = (org.abs_models.xtext.abs.SuspendStmt) stmt;
            List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());
            result = new SuspendStmt(annotations);
        }
        else if(stmt instanceof org.abs_models.xtext.abs.DurationStmt) {
            org.abs_models.xtext.abs.DurationStmt value = (org.abs_models.xtext.abs.DurationStmt) stmt;
            List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());
            PureExp min = pureExpFromXtext(value.getMin());
            PureExp max = pureExpFromXtext(value.getMax());
            result = new DurationStmt(annotations, min, max);
        }
        else if(stmt instanceof org.abs_models.xtext.abs.ThrowStmt) {
            org.abs_models.xtext.abs.ThrowStmt value = (org.abs_models.xtext.abs.ThrowStmt) stmt;
            List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());
            PureExp exception = pureExpFromXtext(value.getException());
            result = new ThrowStmt(annotations, exception);
        }
        else if(stmt instanceof org.abs_models.xtext.abs.DieStmt) {
            org.abs_models.xtext.abs.DieStmt value = (org.abs_models.xtext.abs.DieStmt) stmt;
            List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());
            PureExp exception = pureExpFromXtext(value.getException());
            result = new DieStmt(annotations, exception);
        }
        else if(stmt instanceof org.abs_models.xtext.abs.MoveCogToStmt) {
            org.abs_models.xtext.abs.MoveCogToStmt value = (org.abs_models.xtext.abs.MoveCogToStmt) stmt;
            List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());
            PureExp target = pureExpFromXtext(value.getTarget());
            result = new MoveCogToStmt(annotations, target);
        }
        else if(stmt instanceof org.abs_models.xtext.abs.ExpStmt) {
            org.abs_models.xtext.abs.ExpStmt value = (org.abs_models.xtext.abs.ExpStmt) stmt;
            org.abs_models.xtext.abs.Exp xtextExp = value.getExp();
            List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());
            if (xtextExp instanceof AwaitExp
                && (!(((AwaitExp)xtextExp).getGuard() instanceof ExpGuard)
                    || !(((org.abs_models.xtext.abs.ExpGuard)(((AwaitExp)xtextExp).getGuard())).getExp() instanceof MethodCallExp))) {
                // Are we an expression that is NOT await + single expression
                // "guard" with an asynchronous method call expression?  Then
                // generate a JastAdd AwaitStmt.  (The "await o!m()" case is
                // handled as part of fromXtext(Exp) below.)
                Guard guard = fromXtext(((AwaitExp)xtextExp).getGuard());
                result = new AwaitStmt(annotations, guard);
            } else {
                Exp exp = fromXtext(xtextExp);
                result = new ExpressionStmt(annotations, exp);
            }
        }
        else if(stmt instanceof org.abs_models.xtext.abs.CaseStmt) {
            org.abs_models.xtext.abs.CaseStmt value = (org.abs_models.xtext.abs.CaseStmt) stmt;
            List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());
            PureExp condition = pureExpFromXtext(value.getCondition());
            List<CaseBranchStmt> branches = caseBranchStmtsFromXtext(value.getBranches());
            result = new CaseStmt(annotations, condition, branches);
        }
        else {
            throw new NotImplementedYetException(new ASTNode(),
                "No conversion to JastAdd implemented for Xtext node "
                    + stmt.getClass().toString());
        }
        return nodeWithLocation(result, stmt);
    }

    private static Guard fromXtext(org.abs_models.xtext.abs.Guard guard) {
        Guard result = null;
        if (guard instanceof org.abs_models.xtext.abs.ClaimGuard) {
            org.abs_models.xtext.abs.ClaimGuard cguard = (org.abs_models.xtext.abs.ClaimGuard)guard;
            result = nodeWithLocation(new org.abs_models.frontend.ast.ClaimGuard(pureExpFromXtext(cguard.getFuture())), guard);
        } else if (guard instanceof org.abs_models.xtext.abs.DurationGuard) {
            org.abs_models.xtext.abs.DurationGuard dguard = (org.abs_models.xtext.abs.DurationGuard) guard;
            result = nodeWithLocation(new org.abs_models.frontend.ast.DurationGuard(pureExpFromXtext(dguard.getMin()), pureExpFromXtext(dguard.getMax())), guard);
        } else if (guard instanceof org.abs_models.xtext.abs.ExpGuard) {
            org.abs_models.xtext.abs.ExpGuard eguard = (org.abs_models.xtext.abs.ExpGuard) guard;
            result = nodeWithLocation(new org.abs_models.frontend.ast.ExpGuard(pureExpFromXtext(eguard.getExp())), guard);
        } else if (guard instanceof org.abs_models.xtext.abs.AndGuard) {
            org.abs_models.xtext.abs.AndGuard aguard = (org.abs_models.xtext.abs.AndGuard)guard;
            result = nodeWithLocation(new org.abs_models.frontend.ast.AndGuard(fromXtext(aguard.getLeft()), fromXtext(aguard.getRight())), guard);
        } else {
            throw new NotImplementedYetException(new ASTNode(),
               "No conversion to JastAdd implemented for Xtext node "
                   + guard.getClass().toString());
        }
        return result;
    }

    private static List<CaseBranchStmt> caseBranchStmtsFromXtext(EList<CaseStmtBranch> statements) {
        List<CaseBranchStmt> branchStmts = new List<>();
        for(CaseStmtBranch branch : statements) {
            branchStmts.add(fromXtext(branch));
        }
        return branchStmts;
    }

    private static CaseBranchStmt fromXtext(CaseStmtBranch xtext_branch) {
        CaseBranchStmt result = new CaseBranchStmt();
        result.setLeft(fromXtext(xtext_branch.getPattern()));

        Block block = blockFromXtext(xtext_branch.getBody());
        result.setRight(block);

        return nodeWithLocation(result, xtext_branch);
    }

    private static Pattern fromXtext(org.abs_models.xtext.abs.Pattern pattern) {
        Pattern result;
        if (pattern instanceof org.abs_models.xtext.abs.WildcardPattern) {
            result = nodeWithLocation(new UnderscorePattern(), pattern);
        } else if (pattern instanceof org.abs_models.xtext.abs.IntLiteralPattern) {
            org.abs_models.xtext.abs.IntLiteralPattern value = (IntLiteralPattern) pattern;
            LiteralExp exp = nodeWithLocation(new org.abs_models.frontend.ast.IntLiteral(value.getValue().toString()), value, AbsPackage.eINSTANCE.getIntLiteralPattern_Value());
            result = nodeWithLocation(new LiteralPattern(exp), pattern);
        } else if (pattern instanceof org.abs_models.xtext.abs.StringLiteralPattern) {
            org.abs_models.xtext.abs.StringLiteralPattern value = (StringLiteralPattern) pattern;
            LiteralExp exp = nodeWithLocation(new org.abs_models.frontend.ast.StringLiteral(value.getValue()), value, AbsPackage.eINSTANCE.getStringLiteralPattern_Value());
            result = nodeWithLocation(new LiteralPattern(exp), pattern);
        }
        else if(pattern instanceof org.abs_models.xtext.abs.FloatLiteralPattern) {
            org.abs_models.xtext.abs.FloatLiteralPattern value = (FloatLiteralPattern) pattern;
            LiteralExp exp = nodeWithLocation(new org.abs_models.frontend.ast.FloatLiteral(Double.toString(value.getValue())), value, AbsPackage.eINSTANCE.getFloatLiteralPattern_Value());
            result = nodeWithLocation(new LiteralPattern(exp), pattern);
        } else if (pattern instanceof org.abs_models.xtext.abs.VariablePattern) {
            org.abs_models.xtext.abs.VariablePattern value = (VariablePattern) pattern;
            result = nodeWithLocation(new PatternVarUse(value.getValue()), pattern);
        } else if (pattern instanceof org.abs_models.xtext.abs.ConstructorPattern) {
            // TODO: once Xtext linking is in place, create ExceptionPattern
            // here?
            org.abs_models.xtext.abs.ConstructorPattern value = (org.abs_models.xtext.abs.ConstructorPattern) pattern;
            ConstructorPattern presult = new ConstructorPattern();
            presult.setConstructor(value.getName());
            for(org.abs_models.xtext.abs.Pattern p : value.getArgs()) {
                presult.addParamNoTransform(fromXtext(p));
            }
            result = nodeWithLocation(presult, pattern);
        } else {
            throw new NotImplementedYetException(new ASTNode(),
                "No conversion to JastAdd implemented for Xtext node "
                    + pattern.getClass().toString());
        }
        return nodeWithLocation(result, pattern);
    }

    private static Exp fromXtext(org.abs_models.xtext.abs.Exp value) {
        Exp result;

        if (value instanceof org.abs_models.xtext.abs.GetExp) {

            org.abs_models.xtext.abs.GetExp xtextExp = (org.abs_models.xtext.abs.GetExp) value;
            GetExp exp = new GetExp(pureExpFromXtext(xtextExp.getFutureExp()));
            result = nodeWithLocation(exp, value);

        } else if (value instanceof org.abs_models.xtext.abs.OriginalCallExp) {

            org.abs_models.xtext.abs.OriginalCallExp xtextExp = (OriginalCallExp) value;
            List<PureExp> paramList = new List<>();
            for(org.abs_models.xtext.abs.Exp e : xtextExp.getArgs()) {
                paramList.add(pureExpFromXtext(e));
            }
            OriginalCall exp;
            if (xtextExp.isCore()) {
                // ‘core.original()’
                exp = new TargetedOriginalCall(nodeWithLocation(new DeltaID("core"), xtextExp, AbsPackage.eINSTANCE.getOriginalCallExp_Core()), paramList);
            } else if (xtextExp.getDelta() != null) {
                // ‘Delta.original()’
                exp = new TargetedOriginalCall(nodeWithLocation(new DeltaID(xtextExp.getDelta()), xtextExp, AbsPackage.eINSTANCE.getOriginalCallExp_Delta()), paramList);
            } else {
                // ‘original()’
                exp = new OriginalCall(paramList);
            }
            result = nodeWithLocation(exp, value);

        } else if (value instanceof org.abs_models.xtext.abs.MethodCallExp) {

            org.abs_models.xtext.abs.MethodCallExp xtextExp = (MethodCallExp) value;
            Call exp;

            if (xtextExp.getOperator().equals("!")) {
                // o!m()
                exp = new AsyncCall();
                // see below for AwaitAsyncCall
            } else if (".".equals(xtextExp.getOperator())) {
                // o.m()
                exp = new SyncCall();
            } else {
                throw new NotImplementedYetException(new ASTNode(),
                                                     "Unknown operator " + xtextExp.getOperator() + " in expression"
                                                     + value.getClass().toString());
            }
            exp.setMethod(xtextExp.getMethodname());
            exp.setCallee(pureExpFromXtext(xtextExp.getTarget()));
            for (org.abs_models.xtext.abs.Exp e : xtextExp.getArgs()) {
                exp.addParamNoTransform(pureExpFromXtext(e));
            }
            result = exp;

        } else if (value instanceof org.abs_models.xtext.abs.AwaitExp) {

            org.abs_models.xtext.abs.AwaitExp awaitExp = (org.abs_models.xtext.abs.AwaitExp) value;
            // Are we an await + single expression "guard" with an
            // asynchronous method call expression?  Then generate a JastAdd
            // AwaitAsyncCallExp.  We rely on validation having blocked all
            // invalid constructs; fix the validator if there’s a class cast
            // exception in the code below.
            ExpGuard guard = (ExpGuard)awaitExp.getGuard();
            org.abs_models.xtext.abs.MethodCallExp xtextExp = (MethodCallExp) guard.getExp();
            AwaitAsyncCall exp = new AwaitAsyncCall();
            exp.setMethod(xtextExp.getMethodname());
            exp.setCallee(pureExpFromXtext(xtextExp.getTarget()));
            for (org.abs_models.xtext.abs.Exp e : xtextExp.getArgs()) {
                exp.addParamNoTransform(pureExpFromXtext(e));
            }
            result = exp;
        } else if (value instanceof org.abs_models.xtext.abs.NewExp) {

            org.abs_models.xtext.abs.NewExp xtextExp = (org.abs_models.xtext.abs.NewExp)value;
            NewExp exp = new NewExp();
            exp.setClassName(xtextExp.getClassname());
            for(org.abs_models.xtext.abs.Exp e : xtextExp.getArgs()) {
                exp.addParamNoTransform(pureExpFromXtext(e));
            }
            if (xtextExp.isLocal()) {
                exp.setLocal(nodeWithLocation(new Local(), xtextExp, AbsPackage.eINSTANCE.getNewExp_Local()));
            }
            result = nodeWithLocation(exp, xtextExp);

        } else {
            result = pureExpFromXtext(value);
        }
        return nodeWithLocation(result, value);
    }

    private static PureExp pureExpFromXtext(org.abs_models.xtext.abs.Exp value) {
        PureExp result = null;

        if(value instanceof org.abs_models.xtext.abs.OrExp) {
            org.abs_models.xtext.abs.OrExp xtextExp = (OrExp) value;
            result = new OrBoolExp(pureExpFromXtext(xtextExp.getLeft()),
                                          pureExpFromXtext(xtextExp.getRight()));
        } else if(value instanceof org.abs_models.xtext.abs.AndExp) {
            org.abs_models.xtext.abs.AndExp xtextExp = (AndExp) value;
            result = new AndBoolExp(pureExpFromXtext(xtextExp.getLeft()),
                                    pureExpFromXtext(xtextExp.getRight()));
        } else if(value instanceof org.abs_models.xtext.abs.EqExp) {
            org.abs_models.xtext.abs.EqExp xtextExp = (org.abs_models.xtext.abs.EqExp) value;
            result = new EqExp(pureExpFromXtext(xtextExp.getLeft()),
                               pureExpFromXtext(xtextExp.getRight()));
        } else if(value instanceof org.abs_models.xtext.abs.NotEqExp) {
            org.abs_models.xtext.abs.NotEqExp xtextExp = (org.abs_models.xtext.abs.NotEqExp) value;
            result = new NotEqExp(pureExpFromXtext(xtextExp.getLeft()),
                                  pureExpFromXtext(xtextExp.getRight()));
        } else if(value instanceof org.abs_models.xtext.abs.LTExp) {
            org.abs_models.xtext.abs.LTExp xtextExp = (org.abs_models.xtext.abs.LTExp) value;
            result = new LTExp(pureExpFromXtext(xtextExp.getLeft()),
                               pureExpFromXtext(xtextExp.getRight()));
        } else if(value instanceof org.abs_models.xtext.abs.GTExp) {
            org.abs_models.xtext.abs.GTExp xtextExp = (org.abs_models.xtext.abs.GTExp) value;
            result = new GTExp(pureExpFromXtext(xtextExp.getLeft()),
                               pureExpFromXtext(xtextExp.getRight()));
        } else if(value instanceof org.abs_models.xtext.abs.LTEQExp) {
            org.abs_models.xtext.abs.LTEQExp xtextExp = (org.abs_models.xtext.abs.LTEQExp) value;
            result = new LTEQExp(pureExpFromXtext(xtextExp.getLeft()),
                                 pureExpFromXtext(xtextExp.getRight()));
        } else if(value instanceof org.abs_models.xtext.abs.GTEQExp) {
            org.abs_models.xtext.abs.GTEQExp xtextExp = (org.abs_models.xtext.abs.GTEQExp) value;
            result = new GTEQExp(pureExpFromXtext(xtextExp.getLeft()),
                                 pureExpFromXtext(xtextExp.getRight()));
        } else if(value instanceof org.abs_models.xtext.abs.PlusExp) {
            org.abs_models.xtext.abs.PlusExp xtextExp = (org.abs_models.xtext.abs.PlusExp) value;
            result = new AddAddExp(pureExpFromXtext(xtextExp.getLeft()),
                                   pureExpFromXtext(xtextExp.getRight()));
        } else if(value instanceof org.abs_models.xtext.abs.MinusExp) {
            org.abs_models.xtext.abs.MinusExp xtextExp = (org.abs_models.xtext.abs.MinusExp) value;
            result = new SubAddExp(pureExpFromXtext(xtextExp.getLeft()),
                                   pureExpFromXtext(xtextExp.getRight()));
        } else if(value instanceof org.abs_models.xtext.abs.MulExp) {
            org.abs_models.xtext.abs.MulExp xtextExp = (org.abs_models.xtext.abs.MulExp) value;
            result = new MultMultExp(pureExpFromXtext(xtextExp.getLeft()),
                                     pureExpFromXtext(xtextExp.getRight()));
        } else if(value instanceof org.abs_models.xtext.abs.DivExp) {
            org.abs_models.xtext.abs.DivExp xtextExp = (org.abs_models.xtext.abs.DivExp) value;
            result = new DivMultExp(pureExpFromXtext(xtextExp.getLeft()),
                                    pureExpFromXtext(xtextExp.getRight()));
        } else if(value instanceof org.abs_models.xtext.abs.ModExp) {
            org.abs_models.xtext.abs.ModExp xtextExp = (org.abs_models.xtext.abs.ModExp) value;
            result = new ModMultExp(pureExpFromXtext(xtextExp.getLeft()),
                                    pureExpFromXtext(xtextExp.getRight()));
        }
        // MethodCallExp, GetExp are handled in fromXtext(Exp)
        else if(value instanceof org.abs_models.xtext.abs.ImplementsExp) {
            org.abs_models.xtext.abs.ImplementsExp xtextExp = (org.abs_models.xtext.abs.ImplementsExp) value;
            result = new ImplementsExp(pureExpFromXtext(xtextExp.getBody()),
                                       new InterfaceTypeUse(xtextExp.getInterface(), new List<>()));
        } else if(value instanceof org.abs_models.xtext.abs.AsExp) {
            org.abs_models.xtext.abs.AsExp xtextExp = (org.abs_models.xtext.abs.AsExp) value;
            result = new AsExp(pureExpFromXtext(xtextExp.getBody()),
                               new InterfaceTypeUse(xtextExp.getInterface(), new List<>()));
        } else if(value instanceof org.abs_models.xtext.abs.NotExp) {
            org.abs_models.xtext.abs.NotExp xtextExp = (org.abs_models.xtext.abs.NotExp) value;
            result = new NegExp(pureExpFromXtext(xtextExp.getBody()));
        } else if(value instanceof org.abs_models.xtext.abs.NegExp) {
            org.abs_models.xtext.abs.NegExp xtextExp = (org.abs_models.xtext.abs.NegExp) value;
            result = new MinusExp(pureExpFromXtext(xtextExp.getBody()));
        }
        // AwaitExp is handled in fromXtext(ExpStmt), fromXtext(Exp)
        // NewExp, OriginalCallExp are handled in fromXtext(Exp)
        else  if(value instanceof org.abs_models.xtext.abs.CaseExp) {
            org.abs_models.xtext.abs.CaseExp xtextExp = (org.abs_models.xtext.abs.CaseExp) value;
            List<CaseBranch> branches = new List<>();
            for(CaseExpBranch cb : xtextExp.getCasebranches()) {
                CaseBranch branch = new CaseBranch(fromXtext(cb.getPattern()),
                                                   pureExpFromXtext(cb.getExpression()));
                branches.add(branch);
            }
            result = new CaseExp(pureExpFromXtext(xtextExp.getCondition()),
                                 branches);
        } else if(value instanceof org.abs_models.xtext.abs.LetExp) {
            org.abs_models.xtext.abs.LetExp xtextExp = (org.abs_models.xtext.abs.LetExp) value;
            result = pureExpFromXtext(xtextExp.getBody());
            // Construct the wrapping LetExp’s from the inside out -- later
            // expressions have access to the identifiers of earlier ones so
            // are on the inside
            for (int i = xtextExp.getVariables().size() - 1; i >= 0; i--) {
                // getVariables(), getExps() have the same size
                result = new LetExp(fromXtext(xtextExp.getVariables().get(i)),
                                    pureExpFromXtext(xtextExp.getExps().get(i)),
                                    result);
            }
        } else if(value instanceof org.abs_models.xtext.abs.IfExp) {
            org.abs_models.xtext.abs.IfExp xtextExp = (org.abs_models.xtext.abs.IfExp) value;
            result = new IfExp(pureExpFromXtext(xtextExp.getCondition()),
                               pureExpFromXtext(xtextExp.getConsequence()),
                               pureExpFromXtext(xtextExp.getAlternate()));
        } else if(value instanceof org.abs_models.xtext.abs.FunctionAppExp) {
            org.abs_models.xtext.abs.FunctionAppExp xtextExp = (FunctionAppExp) value;
            FnApp exp = new FnApp();
            exp.setName(xtextExp.getName());
            for (org.abs_models.xtext.abs.Exp arg : xtextExp.getArgs()) {
                exp.addParamNoTransform(pureExpFromXtext(arg));
            }
            result = exp;
        } else if(value instanceof org.abs_models.xtext.abs.VariadicFunctionAppExp) {
            org.abs_models.xtext.abs.VariadicFunctionAppExp xtextExp = (VariadicFunctionAppExp) value;
            List<PureExp> arglist = new List<PureExp>();
            for (org.abs_models.xtext.abs.Exp arg : xtextExp.getArgs()) {
                arglist.add(pureExpFromXtext(arg));
            }
            PureExp args;
            if (xtextExp.getArgs().isEmpty()) {
                args = new DataConstructorExp("Nil", new List<>());
            } else {
                args = new ListLiteral(arglist);
            }
            result = new FnApp(xtextExp.getName(), new List<PureExp>(args));
        } else if(value instanceof org.abs_models.xtext.abs.PartialFunctionAppExp) {
            org.abs_models.xtext.abs.PartialFunctionAppExp xtextExp = (PartialFunctionAppExp) value;
            List<PureExp> args = new List<PureExp>();
            List<ParFnAppParam> fnargs = new List<ParFnAppParam>();
            for (org.abs_models.xtext.abs.Exp arg : xtextExp.getArgs()) {
                args.add(pureExpFromXtext(arg));
            }
            for (org.abs_models.xtext.abs.PartialFunctionParam p : xtextExp.getFunctionArgs()) {
                fnargs.add(fromXtext(p));
            }
            result = new ParFnApp(xtextExp.getName(), args, fnargs);
        } else if(value instanceof org.abs_models.xtext.abs.ConstructorAppExp) {
            org.abs_models.xtext.abs.ConstructorAppExp xtextExp = (ConstructorAppExp) value;
            List<PureExp> params = new List<>();
            for(org.abs_models.xtext.abs.Exp param : xtextExp.getArgs()) {
                params.add(pureExpFromXtext(param));
            }
            result = new DataConstructorExp(xtextExp.getName(), params);
        } else if(value instanceof org.abs_models.xtext.abs.TemplateStringSimpleExp){
            org.abs_models.xtext.abs.TemplateStringSimpleExp xtextExp = (org.abs_models.xtext.abs.TemplateStringSimpleExp) value;
            result = new StringLiteral(xtextExp.getString());
        } else if(value instanceof org.abs_models.xtext.abs.TemplateStringExp) {
            org.abs_models.xtext.abs.TemplateStringExp xtextExp = (org.abs_models.xtext.abs.TemplateStringExp) value;
            PureExp exp = new AddAddExp(new StringLiteral(xtextExp.getStartString()),
                                        new FnApp("toString", new List<>(pureExpFromXtext(xtextExp.getFirstExp()))));
            for (int i = 0; i < xtextExp.getExps().size(); i++) {
                PureExp part = new AddAddExp(new StringLiteral(xtextExp.getBetweenStrings().get(i)),
                                             new FnApp("toString", new List<>(pureExpFromXtext(xtextExp.getExps().get(i)))));
                exp = new AddAddExp(exp, part);
            }
            exp = new AddAddExp(exp, new StringLiteral(xtextExp.getEndString()));
            result = exp;
        } else if (value instanceof org.abs_models.xtext.abs.IntLiteral) {
            org.abs_models.xtext.abs.IntLiteral xtextExp = (org.abs_models.xtext.abs.IntLiteral) value;
            result = new IntLiteral(xtextExp.getValue().toString());
        } else if (value instanceof org.abs_models.xtext.abs.FloatLiteral) {
            org.abs_models.xtext.abs.FloatLiteral xtextExp = (org.abs_models.xtext.abs.FloatLiteral) value;
            result = new FloatLiteral(Double.toString(xtextExp.getValue()));
        } else if (value instanceof org.abs_models.xtext.abs.StringLiteral) {
            org.abs_models.xtext.abs.StringLiteral xtextExp = (org.abs_models.xtext.abs.StringLiteral) value;
            result = new StringLiteral(xtextExp.getValue());
        } else if(value instanceof org.abs_models.xtext.abs.VarOrFieldExp) {
            org.abs_models.xtext.abs.VarOrFieldExp xtextExp = (org.abs_models.xtext.abs.VarOrFieldExp) value;
            if (xtextExp.isField()) {
                result = new FieldUse(xtextExp.getName());
            } else {
                result = new VarUse(xtextExp.getName());
            }
        } else if(value instanceof org.abs_models.xtext.abs.ThisExp) {
            result = new ThisExp();
        } else if(value instanceof org.abs_models.xtext.abs.NullLiteral) {
            result = new NullExp();
        } else {
            throw new NotImplementedYetException(new ASTNode(),
                "No conversion to JastAdd implemented for Xtext node "
                    + value.getClass().toString());
        }
        return nodeWithLocation(result, value);
    }

    private static ParFnAppParam fromXtext(org.abs_models.xtext.abs.PartialFunctionParam p) {
        if (p.getName() != null) {
            return nodeWithLocation(new NamedParFnAppParam(p.getName()),
                                    p, AbsPackage.eINSTANCE.getPartialFunctionParam_Name());
        } else {
            List<ParamDecl> params = new List<ParamDecl>();
            for(org.abs_models.xtext.abs.ParamDecl arg : p.getParams()) {
                params.add(fromXtext(arg));
            }

            return nodeWithLocation(new AnonymousFunctionDecl(params, pureExpFromXtext(p.getBody())),
                                    p);
        }
    }

    private static TypeUse fromXtext(org.abs_models.xtext.abs.TypeUse type) {
        TypeUse result = fromXtext(type.getType());
        result.setAnnotationList(annotationsfromXtext(type.getAnnotations()));
        return result;
    }

    private static TypeUse fromXtext(org.abs_models.xtext.abs.TypeUseNoAnnotations type) {
        TypeUse result;
        if (!type.getParams().isEmpty()) {
            ParametricDataTypeUse presult = new ParametricDataTypeUse();
            result = (ParametricDataTypeUse) presult;
            for (org.abs_models.xtext.abs.TypeUse param : type.getParams()) {
                presult.addParamNoTransform(fromXtext(param));
            }
        } else {
            // will be rewritten by JastAdd -- also once we implement scoping
            // and linking, we can use the exact class here
            result = new UnresolvedTypeUse();
        }
        result.setName(type.getName());
        return nodeWithLocation(result, type);
    }

}
