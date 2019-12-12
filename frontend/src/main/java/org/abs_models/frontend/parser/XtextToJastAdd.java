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
            ITextRegionWithLineInformation location = (ITextRegionWithLineInformation)location_provider.getFullTextRegion(obj);
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
            ITextRegionWithLineInformation location = (ITextRegionWithLineInformation)location_provider.getFullTextRegion(obj, feature, indexInList);
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
     * Convert a resource set produced by Xtext into a JastAdd AST.
     *
     * This is the main entry point for Xtext-to-JastAdd conversion.  Does not
     * check for parse / validation errors, so  should only be
     * called if the argument contains no Xtext-detected errors.
     *
     * The result will be pre-processed by the ASTPreProcessor class.
     *
     * @param resourceSet The parsed ABS files, including one resource for the standard library
     * @return a fresh JastAdd Model object, processed by ASTPreProcessor
     */
    public static Model fromResourceSet(XtextResourceSet resourceSet) {
        Model result = new Model();
        for (Resource r : resourceSet.getResources()) {
            for (EObject unit : r.getContents()) {
                CompilationUnit jastadd_unit = fromXtext((org.abs_models.xtext.abs.CompilationUnit) unit);
                new ASTPreProcessor().preprocess(jastadd_unit);
                result.addCompilationUnitNoTransform(jastadd_unit);
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
        for (org.abs_models.xtext.abs.DeltaDecl delta : xtext_unit.getDeltas()) {
            result.addDeltaDeclNoTransform(fromXtext(delta));
        }
        if (xtext_unit.getProductline() != null) {
            result.setProductLine(fromXtext(xtext_unit.getProductline()));
        }
        for (org.abs_models.xtext.abs.ProductDecl product : xtext_unit.getProducts()) {
            result.addProductDeclNoTransform(fromXtext(product));
        }
        for (org.abs_models.xtext.abs.MTVLFeatureRoot f : xtext_unit.getRoot_features()) {
            result.addFeatureDeclNoTransform(fromXtext(f));
        }
        for (org.abs_models.xtext.abs.MTVLFeatureExtension e : xtext_unit.getFeature_extensions()) {
            result.addFExtNoTransform(fromXtext(e));
        }
        return nodeWithLocation(result, xtext_unit);
    }

    static ModuleDecl fromXtext(org.abs_models.xtext.abs.ModuleDecl xtext_module) {
        ModuleDecl result = new ModuleDecl();
        result.setName(xtext_module.getName());
        for (org.abs_models.xtext.abs.ModuleExport export : xtext_module.getExports()) {
            result.addExportNoTransform(fromXtext(export));
        }
        for (org.abs_models.xtext.abs.ModuleImport imp : xtext_module.getImports()) {
            result.addImportNoTransform(fromXtext(imp));
        }
        for (org.abs_models.xtext.abs.Declaration decl : xtext_module.getDeclarations()) {
            result.addDeclNoTransform(fromXtext(decl));
        }

        if(xtext_module.isMain()) {
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

    private static Export fromXtext(org.abs_models.xtext.abs.ModuleExport xtext_export) {
        Export result;
        if (xtext_export.isStar()) {
            // "export *;"
            // "export * from OtherModule;"
            StarExport se = new StarExport();
            result = se;
            if (xtext_export.getModulename() != null)
                se.setModuleName(nodeWithLocation(new Name(xtext_export.getModulename()), xtext_export, AbsPackage.eINSTANCE.getModuleExport_Modulename()));
        } else if (xtext_export.getModulename() != null) {
            // "export a, b from OtherModule;"
            FromExport fe = new FromExport();
            result = fe;
            fe.setModuleName(xtext_export.getModulename());
            for (int i = 0; i < xtext_export.getIdentifiers().size(); i++) {
                String id = xtext_export.getIdentifiers().get(i);
                fe.addNameNoTransform(nodeWithLocation(new Name(id), xtext_export, AbsPackage.eINSTANCE.getModuleExport_Identifiers(), i));
            }
        } else {
            // "export a, b;"
            NamedExport ne = new NamedExport();
            result = ne;
            for (int i = 0; i < xtext_export.getIdentifiers().size(); i++) {
                String id = xtext_export.getIdentifiers().get(i);
                ne.addNameNoTransform(nodeWithLocation(new Name(id), xtext_export, AbsPackage.eINSTANCE.getModuleExport_Identifiers(), i));
            }
        }
        return nodeWithLocation(result, xtext_export);
    }

    private static Import fromXtext(org.abs_models.xtext.abs.ModuleImport xtext_import) {
        Import result;
        if (xtext_import.isStar()) {
            // "import * from OtherModule;"
            StarImport si = new StarImport(xtext_import.getModulename());
            result = si;
        } else if (xtext_import.getModulename() != null) {
            // "import a, b from OtherModule;"
            FromImport fi = new FromImport();
            result = fi;
            fi.setModuleName(xtext_import.getModulename());
            for (int i = 0; i < xtext_import.getIdentifiers().size(); i++) {
                String id = xtext_import.getIdentifiers().get(i);
                fi.addNameNoTransform(nodeWithLocation(new Name(id), xtext_import, AbsPackage.eINSTANCE.getModuleImport_Identifiers(), i));
            }
        } else {
            // "import OtherModule.a, OtherModule.b;"
            NamedImport ni = new NamedImport();
            result = ni;
            for (int i = 0; i < xtext_import.getIdentifiers().size(); i++) {
                String id = xtext_import.getIdentifiers().get(i);
                ni.addNameNoTransform(nodeWithLocation(new Name(id), xtext_import, AbsPackage.eINSTANCE.getModuleImport_Identifiers(), i));
            }
        }
        return nodeWithLocation(result, xtext_import);
    }

    private static List<Annotation> annotationsfromXtext(org.abs_models.xtext.abs.Annotations annotations) {
        List<Annotation> annotationList = new List<>();
        for(org.abs_models.xtext.abs.Annotation annotation : annotations.getAnnotations()) {
            Annotation astAnnotation;
            PureExp exp = pureExpFromXtext(annotation.getValue());
            if (annotation.getId() != null) {
                astAnnotation = new TypedAnnotation(exp, nodeWithLocation(new UnresolvedTypeUse(annotation.getId(), new List<>()), AbsPackage.eINSTANCE.getAnnotation_Id()));
            } else {
                astAnnotation = new Annotation(exp);
            }
            annotationList.add(nodeWithLocation(astAnnotation, annotation));
        }
        return annotationList;
    }

    static Decl fromXtext(org.abs_models.xtext.abs.Declaration xtext_decl) {
        Decl result = null;
        if (xtext_decl.getDatatypedecl() != null) {
            result = fromXtext(xtext_decl.getDatatypedecl());
            ((DataTypeDecl)result).setAnnotationList(annotationsfromXtext(xtext_decl.getAnnotations()));
        } else if (xtext_decl.getTypesynonymdecl() != null) {
            result = fromXtext(xtext_decl.getTypesynonymdecl());
            ((TypeSynDecl)result).setAnnotationList(annotationsfromXtext(xtext_decl.getAnnotations()));
        } else if (xtext_decl.getExceptiondecl() != null) {
            result = fromXtext(xtext_decl.getExceptiondecl());
            ((ExceptionDecl)result).setAnnotationList(annotationsfromXtext(xtext_decl.getAnnotations()));
        } else if (xtext_decl.getFunctiondecl() != null) {
            result = fromXtext(xtext_decl.getFunctiondecl());
            if (result instanceof PartialFunctionDecl) {
                ((PartialFunctionDecl)result).setAnnotationList(annotationsfromXtext(xtext_decl.getAnnotations()));
            } else {
                ((FunctionDecl)result).setAnnotationList(annotationsfromXtext(xtext_decl.getAnnotations()));
            }
        } else if (xtext_decl.getTraitdecl() != null) {
            result = fromXtext(xtext_decl.getTraitdecl());
            ((TraitDecl)result).setAnnotationList(annotationsfromXtext(xtext_decl.getAnnotations()));
        } else if (xtext_decl.getInterfacedecl() != null) {
            result = fromXtext(xtext_decl.getInterfacedecl());
            ((InterfaceDecl)result).setAnnotationList(annotationsfromXtext(xtext_decl.getAnnotations()));
        } else if (xtext_decl.getClassdecl() != null) {
            result = fromXtext(xtext_decl.getClassdecl());
            ((ClassDecl)result).setAnnotationList(annotationsfromXtext(xtext_decl.getAnnotations()));
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

        for (org.abs_models.xtext.abs.DataConstructorDecl xtext_d : xtext_decl.getConstructors()) {
            DataConstructor constructor = new DataConstructor();
            constructor.setName(xtext_d.getName());

            for (DataConstructorParamDecl arg : xtext_d.getArgs()) {
                constructor.addConstructorArgNoTransform(fromXtext(arg));
            }

            result.addDataConstructor(nodeWithLocation(constructor, xtext_d));
        }
        return nodeWithLocation(result, xtext_decl);
    }

    private static ConstructorArg fromXtext(DataConstructorParamDecl xtext_arg) {
        ConstructorArg constructorArg = new ConstructorArg();
        if(xtext_arg.getName() != null) {
            constructorArg.setSelectorName(nodeWithLocation(new Name(xtext_arg.getName()), xtext_arg, AbsPackage.eINSTANCE.getDataConstructorParamDecl_Name()));
        }
        // This code section transferred from the old
        // CreateJast.exitData_constructor(), which had a comment saying "see
        // below, we may be facing an UnresolvedTypeUse".  In
        // CreateJastAdd.exitType_use, the commend said: "As we could be
        // looking at an interface type, first keep symbol unresolved and have
        // rewrite-rules patch it up.  However, this means that in the parser
        // the DataConstructor could be seeing [sic]. But there we know what
        // it must be and "rewrite" it ourselves."

        // The upshot: this code is cargo-culted and can be changed /
        // simplified if subsequent stages do not complain.
        TypeUse tu = fromXtext(xtext_arg.getType());
        DataTypeUse turesolved;
        if (tu instanceof DataTypeUse) {
            turesolved = (DataTypeUse)tu;
        } else {
            assert tu instanceof UnresolvedTypeUse : tu.getClass().getName();
            turesolved = new DataTypeUse(tu.getName(), tu.getAnnotations());
            nodeWithLocation(turesolved, xtext_arg.getType());
        }
        constructorArg.setTypeUse(turesolved);
        return nodeWithLocation(constructorArg, xtext_arg);
    }

    static TypeSynDecl fromXtext(org.abs_models.xtext.abs.TypeSynonymDecl xtext_decl) {
        TypeSynDecl result = new TypeSynDecl();
        result.setName(xtext_decl.getName());
        result.setValue(fromXtext(xtext_decl.getType()));
        return nodeWithLocation(result, xtext_decl);
    }

    static ExceptionDecl fromXtext(org.abs_models.xtext.abs.ExceptionDecl xtext_decl) {
        ExceptionConstructor constructor = new ExceptionConstructor();
        constructor.setName(xtext_decl.getName());
        for (DataConstructorParamDecl arg : xtext_decl.getArgs()) {
            constructor.addConstructorArgNoTransform(fromXtext(arg));
        }

        ExceptionDecl result = new ExceptionDecl();
        result.setName(xtext_decl.getName());
        result.setDataConstructorList(new List<DataConstructor>(constructor));
        return nodeWithLocation(result, xtext_decl);
    }

    static Decl fromXtext(org.abs_models.xtext.abs.FunctionDecl xtext_decl) {
        // This parses FunctionDecl and PartialFunctionDecl; these are
        // unfortunately distinct classes in JastAdd, so we return type Decl
        // instead.
        if (xtext_decl.getFunction_args().size() > 0) {
            PartialFunctionDecl result;
            if (!xtext_decl.getTypeparams().isEmpty()) {
                ParametricPartialFunctionDecl presult = new ParametricPartialFunctionDecl();
                result = presult;
                for (int i = 0; i < xtext_decl.getTypeparams().size(); i++) {
                    String tp = xtext_decl.getTypeparams().get(i);
                    presult.addTypeParameterNoTransform(nodeWithLocation(new TypeParameterDecl(tp), xtext_decl, AbsPackage.eINSTANCE.getFunctionDecl_Typeparams(), i));
                }
            } else {
                result = new  PartialFunctionDecl();
            }
            result.setName(xtext_decl.getName());
            result.setTypeUse(fromXtext(xtext_decl.getResulttype()));

            for(org.abs_models.xtext.abs.ParamDecl arg : xtext_decl.getArgs()) {
                result.addParamNoTransform(fromXtext(arg));
            }

            for (int i = 0; i < xtext_decl.getFunction_args().size(); i++) {
                String fArg = xtext_decl.getFunction_args().get(i);
                result.addFuncParamNoTransform(nodeWithLocation(new FunctionParamDecl(fArg), xtext_decl, AbsPackage.eINSTANCE.getFunctionDecl_Function_args(), i));
            }

            PartialFunctionDef functionDef = new PartialFunctionDef(pureExpFromXtext(xtext_decl.getBody()));
            result.setPartialFunctionDef(nodeWithLocation(functionDef, xtext_decl.getBody()));

            return nodeWithLocation(result, xtext_decl);
        } else {
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
            result.setTypeUse(fromXtext(xtext_decl.getResulttype()));

            for(org.abs_models.xtext.abs.ParamDecl arg : xtext_decl.getArgs()) {
                result.addParamNoTransform(fromXtext(arg));
            }

            if (xtext_decl.isBuiltin()) {
                result.setFunctionDef(nodeWithLocation(new BuiltinFunctionDef(), xtext_decl, AbsPackage.eINSTANCE.getFunctionDecl_Builtin()));
            } else {
                PureExp exp = pureExpFromXtext(xtext_decl.getBody());
                result.setFunctionDef(nodeWithLocation(new ExpFunctionDef(exp), xtext_decl.getBody()));
            }
            return nodeWithLocation(result, xtext_decl);
        }
    }

    static TraitDecl fromXtext(org.abs_models.xtext.abs.TraitDecl xtext_decl) {
        TraitDecl result = new TraitDecl();
        result.setName(xtext_decl.getName());
        result.setTraitExpr(fromXtext(xtext_decl.getTrait_exp()));
        return nodeWithLocation(result, xtext_decl);
    }

    static TraitExpr fromXtext(org.abs_models.xtext.abs.TraitExp xtext_exp) {
        TraitExpr result = fromXtext(xtext_exp.getBasic_exp());
        for (org.abs_models.xtext.abs.TraitOp op : xtext_exp.getTrait_ops()) {
            result = new TraitModifyExpr(result, fromXtext(op));
        }
        return nodeWithLocation(result, xtext_exp);
    }

    static TraitExpr fromXtext(org.abs_models.xtext.abs.BasicTraitExp xtext_exp) {
        TraitExpr result = null;
        if (xtext_exp.isMethodSet() || xtext_exp.getMethods().size() > 0) {
            TraitSetExpr fresult = new TraitSetExpr();
            for (org.abs_models.xtext.abs.MethodDecl m : xtext_exp.getMethods()) {
                fresult.addMethodImplNoTransform(fromXtext(m));
            }
            result = fresult;
        } else {
            result = new TraitNameExpr(xtext_exp.getRef());
        }
        return nodeWithLocation(result, xtext_exp);
    }

    static MethodModifier fromXtext(org.abs_models.xtext.abs.TraitOp xtext_exp) {
        MethodModifier result = null;
        if (xtext_exp.isRemoveMethodModifier() || xtext_exp.getRemoved_sigs().size() > 0) {
            List<MethodSig> l = new List<>();
            for (org.abs_models.xtext.abs.MethodSignature s : xtext_exp.getRemoved_sigs()) {
                l.add(fromXtext(s));
            }
            result = new RemoveMethodModifier(l);
        } else if (xtext_exp.getAdded_exp() != null) {
            result = new AddMethodModifier(fromXtext(xtext_exp.getAdded_exp()));
        } else {
            result = new ModifyMethodModifier(fromXtext(xtext_exp.getModified_exp()));
        }
        return nodeWithLocation(result, xtext_exp);
    }

    static InterfaceDecl fromXtext(org.abs_models.xtext.abs.InterfaceDecl xtext_decl) {
        InterfaceDecl result = new  InterfaceDecl();
        result.setName(xtext_decl.getName());

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
        result.setName(xtext_decl.getName());
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

        // TODO treat the case of an empty init block
        if (xtext_decl.getInitblockstmts().size() > 0) {
            InitBlock astInitBlock = new InitBlock();
            for(org.abs_models.xtext.abs.Stmt statement : xtext_decl.getInitblockstmts()) {
                astInitBlock.addStmt(fromXtext(statement));
            }
            result.setInitBlock(astInitBlock);
        }

        for(MethodDecl methodDecl : xtext_decl.getMethods()) {
            result.addMethodNoTransform(fromXtext(methodDecl));
        }

        for (CaseStmtBranch recover_branch : xtext_decl.getRecoverbranches()) {
            result.addRecoverBranchNoTransform(fromXtext(recover_branch));
        }
        for (org.abs_models.xtext.abs.TraitExp trait_exp : xtext_decl.getUsed_traits()) {
            result.addTraitUseNoTransform(new TraitUse(fromXtext(trait_exp)));
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
        // Initialize position here already so we can use it for the embedded
        // MethodSig
        MethodImpl result = nodeWithLocation(new MethodImpl(), xtext_decl);

        MethodSig sig = new MethodSig();
        sig.setName(xtext_decl.getName());
        sig.setAnnotationList(annotationsfromXtext(xtext_decl.getAnnotations()));
        for(org.abs_models.xtext.abs.ParamDecl arg : xtext_decl.getArgs()) {
            sig.addParamNoTransform(fromXtext(arg));
        }
        sig.setReturnType(fromXtext(xtext_decl.getResulttype()));
        // Handle position of embedded MethodSig: first set position to
        // closing ")", then adjust start position to start of MethodDecl
        nodeWithLocation(sig, xtext_decl, AbsPackage.eINSTANCE.getMethodDecl_Sig_end_position());
        sig.setPosition(result.getStartLine(), result.getStartColumn(),
                        sig.getEndLine(), sig.getEndColumn());
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
            Opt<Block> finallyOpt = new Opt<>();
            if (value.getFinally() != null) {
                Block finallyBlock = blockFromXtext(value.getFinally());
                finallyOpt.setChild(finallyBlock, 0);
            }
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
        if (guard instanceof org.abs_models.xtext.abs.DurationGuard) {
            org.abs_models.xtext.abs.DurationGuard dguard = (org.abs_models.xtext.abs.DurationGuard) guard;
            result = nodeWithLocation(new org.abs_models.frontend.ast.DurationGuard(pureExpFromXtext(dguard.getMin()), pureExpFromXtext(dguard.getMax())), guard);
        } else if (guard instanceof org.abs_models.xtext.abs.ExpGuard) {
            org.abs_models.xtext.abs.ExpGuard eguard = (org.abs_models.xtext.abs.ExpGuard) guard;
            if (eguard.isClaim()) {
                result = nodeWithLocation(new org.abs_models.frontend.ast.ClaimGuard(pureExpFromXtext(eguard.getExp())), guard);
            } else {
                result = nodeWithLocation(new org.abs_models.frontend.ast.ExpGuard(pureExpFromXtext(eguard.getExp())), guard);
            }
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
            LiteralExp exp = nodeWithLocation(new org.abs_models.frontend.ast.StringLiteral(ASTPreProcessor.preprocessStringLiteral(value.getValue())), value, AbsPackage.eINSTANCE.getStringLiteralPattern_Value());
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
                exp = new TargetedOriginalCall(paramList, nodeWithLocation(new DeltaID("core"), xtextExp, AbsPackage.eINSTANCE.getOriginalCallExp_Core()));
            } else if (xtextExp.getDelta() != null) {
                // ‘Delta.original()’
                exp = new TargetedOriginalCall(paramList, nodeWithLocation(new DeltaID(xtextExp.getDelta()), xtextExp, AbsPackage.eINSTANCE.getOriginalCallExp_Delta()));
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
            if (xtextExp.getFunctionArgs().size() > 0) {
                ParFnApp exp = new ParFnApp();
                exp.setName(xtextExp.getName());
                for (org.abs_models.xtext.abs.Exp arg : xtextExp.getArgs()) {
                    exp.addParamNoTransform(pureExpFromXtext(arg));
                }
                for (org.abs_models.xtext.abs.PartialFunctionParam p : xtextExp.getFunctionArgs()) {
                    exp.addFuncParamNoTransform(fromXtext(p));
                }
                result = exp;
            } else {
                FnApp exp = new FnApp();
                exp.setName(xtextExp.getName());
                for (org.abs_models.xtext.abs.Exp arg : xtextExp.getArgs()) {
                    exp.addParamNoTransform(pureExpFromXtext(arg));
                }
                result = exp;
            }
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
        } else if(value instanceof org.abs_models.xtext.abs.ConstructorAppExp) {
            org.abs_models.xtext.abs.ConstructorAppExp xtextExp = (ConstructorAppExp) value;
            List<PureExp> params = new List<>();
            for(org.abs_models.xtext.abs.Exp param : xtextExp.getArgs()) {
                params.add(pureExpFromXtext(param));
            }
            result = new DataConstructorExp(xtextExp.getName(), params);
        } else if(value instanceof org.abs_models.xtext.abs.TemplateStringSimpleExp){
            org.abs_models.xtext.abs.TemplateStringSimpleExp xtextExp = (org.abs_models.xtext.abs.TemplateStringSimpleExp) value;
            result = new StringLiteral(ASTPreProcessor.preprocessTemplateStringLiteral(xtextExp.getString()));
        } else if(value instanceof org.abs_models.xtext.abs.TemplateStringExp) {
            org.abs_models.xtext.abs.TemplateStringExp xtextExp = (org.abs_models.xtext.abs.TemplateStringExp) value;
            PureExp exp = new AddAddExp(new StringLiteral(ASTPreProcessor.preprocessTemplateStringLiteral(xtextExp.getStartString())),
                                        new FnApp("toString", new List<>(pureExpFromXtext(xtextExp.getFirstExp()))));
            for (int i = 0; i < xtextExp.getExps().size(); i++) {
                PureExp part = new AddAddExp(new StringLiteral(ASTPreProcessor.preprocessTemplateStringLiteral(xtextExp.getBetweenStrings().get(i))),
                                             new FnApp("toString", new List<>(pureExpFromXtext(xtextExp.getExps().get(i)))));
                exp = new AddAddExp(exp, part);
            }
            exp = new AddAddExp(exp, new StringLiteral(ASTPreProcessor.preprocessTemplateStringLiteral(xtextExp.getEndString())));
            result = exp;
        } else if (value instanceof org.abs_models.xtext.abs.IntLiteral) {
            org.abs_models.xtext.abs.IntLiteral xtextExp = (org.abs_models.xtext.abs.IntLiteral) value;
            result = new IntLiteral(xtextExp.getValue().toString());
        } else if (value instanceof org.abs_models.xtext.abs.FloatLiteral) {
            org.abs_models.xtext.abs.FloatLiteral xtextExp = (org.abs_models.xtext.abs.FloatLiteral) value;
            result = new FloatLiteral(Double.toString(xtextExp.getValue()));
        } else if (value instanceof org.abs_models.xtext.abs.StringLiteral) {
            org.abs_models.xtext.abs.StringLiteral xtextExp = (org.abs_models.xtext.abs.StringLiteral) value;
            result = new StringLiteral(ASTPreProcessor.preprocessStringLiteral(xtextExp.getValue()));
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

    // ========== end of Core ABS ==========

    private static DeltaDecl fromXtext(org.abs_models.xtext.abs.DeltaDecl xtext_delta) {
        DeltaDecl result = new DeltaDecl();
        result.setName(xtext_delta.getName());
        for (org.abs_models.xtext.abs.DeltaParamDecl arg : xtext_delta.getArgs()) {
            if (arg.getNormalParam() != null) {
                result.addParam(nodeWithLocation(new DeltaFieldParam(fromXtext(arg.getNormalParam())), arg));
            } else {
                result.addParam(nodeWithLocation(new DeltaClassParam(arg.getClassModifier(), fromXtext(arg.getCondition())), arg));
            }
        }
        for (int i = 0; i < xtext_delta.getUsedModulenames().size(); i++) {
            String iname = xtext_delta.getUsedModulenames().get(i);
            result.addDeltaAccessNoTransform(nodeWithLocation(new DeltaAccess(iname), xtext_delta, AbsPackage.eINSTANCE.getDeltaDecl_UsedModulenames(), i));
        }
        for (org.abs_models.xtext.abs.DeltaModuleModifier m : xtext_delta.getModifiers()) {
            result.addModuleModifier(fromXtext(m));
        }
        return nodeWithLocation(result, xtext_delta);
    }

    private static HasCondition fromXtext(org.abs_models.xtext.abs.DeltaCondition xtext_cond) {
        HasCondition result = null;
        if (xtext_cond.getDeltaFieldCondition() != null) {
            result = new HasField(fromXtext(xtext_cond.getDeltaFieldCondition()));
        } else if (xtext_cond.getDeltaMethodCondition() != null) {
            result = new HasMethod(fromXtext(xtext_cond.getDeltaMethodCondition()));
        } else {
            result = new HasInterface(new InterfaceTypeUse(xtext_cond.getDeltaInterfaceCondition(), new List<>()));
        }
        return nodeWithLocation(result, xtext_cond);
    }

    private static ModuleModifier fromXtext(org.abs_models.xtext.abs.DeltaModuleModifier xtext_mod) {
        ModuleModifier result = null;
        if (xtext_mod.getAdded_decl() != null) {
            Decl d = fromXtext(xtext_mod.getAdded_decl());
            if (d instanceof ClassDecl) {
                result = new AddClassModifier((ClassDecl) d);
            } else if (d instanceof InterfaceDecl) {
                result = new AddInterfaceModifier((InterfaceDecl) d);
            } else if (d instanceof TypeSynDecl) {
                result = new AddTypeSynModifier((TypeSynDecl) d);
            } else if (d instanceof DataTypeDecl) {
                result = new AddDataTypeModifier((DataTypeDecl) d);
            } else if (d instanceof FunctionDecl) {
                result = new AddFunctionModifier((FunctionDecl) d);
            }
        } else if (xtext_mod.getAdded_import() != null) {
            result = new AddImportModifier(fromXtext(xtext_mod.getAdded_import()));
        } else if (xtext_mod.getAdded_export() != null) {
            result = new AddExportModifier(fromXtext(xtext_mod.getAdded_export()));
        } else if (xtext_mod.getRemoved_class_name() != null) {
            result = new RemoveClassModifier(xtext_mod.getRemoved_class_name());
        } else if (xtext_mod.getRemoved_interface_name() != null) {
            result = new RemoveInterfaceModifier(xtext_mod.getRemoved_interface_name());
        } else if (xtext_mod.getModified_class_name() != null) {
            ModifyClassModifier mresult = new ModifyClassModifier();
            mresult.setName(xtext_mod.getModified_class_name());
            for (int i = 0; i < xtext_mod.getAdded_interfaces().size(); i++) {
                String iname = xtext_mod.getAdded_interfaces().get(i);
                mresult.addAddedInterfaceNoTransform(nodeWithLocation(new InterfaceTypeUse(iname, new List<>()), xtext_mod, AbsPackage.eINSTANCE.getDeltaModuleModifier_Added_interfaces(), i));
            }
            for (int i = 0; i < xtext_mod.getRemoved_interfaces().size(); i++) {
                String iname = xtext_mod.getRemoved_interfaces().get(i);
                mresult.addRemovedInterfaceNoTransform(nodeWithLocation(new InterfaceTypeUse(iname, new List<>()), xtext_mod, AbsPackage.eINSTANCE.getDeltaModuleModifier_Removed_interfaces(), i));
            }
            for (org.abs_models.xtext.abs.ClassModifier mod : xtext_mod.getClass_modifiers()) {
                if (mod.getAdded_field() != null) {
                    mresult.addModifierNoTransform(new AddFieldModifier(fromXtext(mod.getAdded_field())));
                } else if (mod.getAdded_methods().size() > 0) {
                    TraitSetExpr tse = new TraitSetExpr();
                    for (org.abs_models.xtext.abs.MethodDecl m : mod.getAdded_methods()) {
                        tse.addMethodImplNoTransform(fromXtext(m));
                    }
                    mresult.addModifierNoTransform(new DeltaTraitModifier(new AddMethodModifier(tse)));
                } else if (mod.getAdded_trait() != null) {
                    mresult.addModifierNoTransform(new DeltaTraitModifier(new AddMethodModifier(new TraitNameExpr(mod.getAdded_trait()))));
                } else if (mod.getRemoved_field() != null) {
                    mresult.addModifierNoTransform(new RemoveFieldModifier(fromXtext(mod.getRemoved_field())));
                } else if (mod.getRemoved_methods().size() > 0) {
                    List<MethodSig> methodlist = new List<MethodSig>();
                    for (org.abs_models.xtext.abs.MethodSignature s : mod.getRemoved_methods()) {
                        methodlist.add(fromXtext(s));
                    }
                    mresult.addModifierNoTransform(new DeltaTraitModifier(new RemoveMethodModifier(methodlist)));
                } else if (mod.getModified_methods().size() > 0) {
                    TraitSetExpr tse = new TraitSetExpr();
                    for (org.abs_models.xtext.abs.MethodDecl m : mod.getModified_methods()) {
                        tse.addMethodImplNoTransform(fromXtext(m));
                    }
                    mresult.addModifierNoTransform(new DeltaTraitModifier(new ModifyMethodModifier(tse)));
                } else if (mod.getModified_trait() != null) {
                    mresult.addModifierNoTransform(new DeltaTraitModifier(new ModifyMethodModifier(new TraitNameExpr(mod.getModified_trait()))));
                }
            }
            result = mresult;
        } else if (xtext_mod.getModified_interface_name() != null) {
            ModifyInterfaceModifier mresult = new ModifyInterfaceModifier();
            mresult.setName(xtext_mod.getModified_interface_name());
            for (org.abs_models.xtext.abs.InterfaceModifier mod : xtext_mod.getInterface_modifiers()) {
                if (mod.getAddedMethod() != null) {
                    mresult.addMethodSigModifier(new AddMethodSigModifier(fromXtext(mod.getAddedMethod())));
                } else {
                    mresult.addMethodSigModifier(new RemoveMethodSigModifier(fromXtext(mod.getRemovedMethod())));
                }
            }
            result = mresult;
        } else if (xtext_mod.getModified_typesyn() != null) {
            result = new ModifyTypeSynModifier(fromXtext(xtext_mod.getModified_typesyn()));
        } else if (xtext_mod.getModified_datatype() != null) {
            result = new ModifyDataTypeModifier(fromXtext(xtext_mod.getModified_datatype()));
        }
        return nodeWithLocation(result, xtext_mod);
    }

    // Product lines

    private static ProductLine fromXtext(org.abs_models.xtext.abs.ProductlineDecl xtext_decl) {
        ProductLine result = new ProductLine();
        result.setName(xtext_decl.getName());
        for (org.abs_models.xtext.abs.ProductFeature feature : xtext_decl.getFeatures()) {
            result.addFeatureNoTransform(fromXtext(feature));
        }
        for (org.abs_models.xtext.abs.ProductlineDeltaClause xtext_clause : xtext_decl.getDelta_clauses()) {
            result.addDeltaClause(fromXtext(xtext_clause));
        }
        return nodeWithLocation(result, xtext_decl);
    }

    private static Feature fromXtext(org.abs_models.xtext.abs.ProductFeature xtext_feature) {
        Feature result = new Feature();
        result.setName(xtext_feature.getName());
        for (org.abs_models.xtext.abs.AttributeAssignment xtext_attr : xtext_feature.getAttribute_assignments()) {
            AttrAssignment attr = new AttrAssignment();
            attr.setName(xtext_attr.getName());
            org.abs_models.xtext.abs.AttributeAssignmentValue xtext_value = xtext_attr.getValue();
            if (xtext_value instanceof org.abs_models.xtext.abs.AttributeAssignmentValue_Int) {
                int value = ((org.abs_models.xtext.abs.AttributeAssignmentValue_Int)xtext_value).getValue().intValue();
                attr.setValue(nodeWithLocation(new IntVal(value), xtext_value));
            } else if (xtext_value instanceof org.abs_models.xtext.abs.AttributeAssignmentValue_Bool) {
                boolean value = ((org.abs_models.xtext.abs.AttributeAssignmentValue_Bool)xtext_value).getValue().equals("True");
                attr.setValue(nodeWithLocation(new BoolVal(value), xtext_value));
            } else if (xtext_value instanceof org.abs_models.xtext.abs.AttributeAssignmentValue_String) {
                String value = ((org.abs_models.xtext.abs.AttributeAssignmentValue_String)xtext_value).getValue();
                attr.setValue(nodeWithLocation(new StringVal(value), xtext_value));
            } else {
                // The antlr code created an UnknownValue here and rewrote it
                // into BoolVal later -- we catch invalid constructor names
                // during xtext validation instead.
                throw new NotImplementedYetException(new ASTNode(),
                                                     "No conversion to JastAdd implemented for Xtext node "
                                                     + xtext_value.getClass().toString());
            }
            result.addAttrAssignment(nodeWithLocation(attr, xtext_attr));
        }
        return nodeWithLocation(result, xtext_feature);
    }

    private static DeltaClause fromXtext(org.abs_models.xtext.abs.ProductlineDeltaClause xtext_clause) {
        DeltaClause result = new DeltaClause();
        Deltaspec result_name = new Deltaspec();
        result_name.setDeltaID(xtext_clause.getName());
        for (org.abs_models.xtext.abs.DeltaClauseParam param : xtext_clause.getParams()) {
            result_name.addDeltaparam(fromXtext(param));
        }
        result.setDeltaspec(result_name);
        for (String after_id : xtext_clause.getAfter_ids()) {
            result.addAfterDeltaID(new DeltaID(after_id));
        }
        if (xtext_clause.getFrom_condition() != null) {
            result.setFromAppCond(fromXtext(xtext_clause.getFrom_condition()));
        }
        if (xtext_clause.getWhen_condition() != null) {
            result.setAppCond(fromXtext(xtext_clause.getWhen_condition()));
        }
        return nodeWithLocation(result, xtext_clause);
    }

    private static Deltaparam fromXtext(org.abs_models.xtext.abs.DeltaClauseParam xtext_param) {
        Deltaparam result = null;
        if (xtext_param instanceof org.abs_models.xtext.abs.DeltaClauseParam_Int) {
            result = new Const(new IntVal(((org.abs_models.xtext.abs.DeltaClauseParam_Int)xtext_param).getInt_param().intValue()));
        } else if (xtext_param instanceof org.abs_models.xtext.abs.DeltaClauseParam_Id) {
            org.abs_models.xtext.abs.DeltaClauseParam_Id id_param = (org.abs_models.xtext.abs.DeltaClauseParam_Id) xtext_param;
            if (id_param.getFidaid_param() == null) {
                if (id_param.getId_param().equals("True")) {
                    result = new Const(new BoolVal(true));
                } else if (id_param.getId_param().equals("False")) {
                    result = new Const(new BoolVal(false));
                } else {
                    result = new FID(id_param.getId_param());
                }
            } else {
                result = new FIDAID(id_param.getId_param(), id_param.getFidaid_param());
            }
        } else {
            throw new NotImplementedYetException(new ASTNode(),
                                                 "No conversion to JastAdd implemented for Xtext node "
                                                 + xtext_param.getClass().toString());
        }
        return nodeWithLocation(result, xtext_param);
    }

    private static AppCond fromXtext(org.abs_models.xtext.abs.DeltaClauseApplicationClause xtext_clause) {
        AppCond result = null;
        if (xtext_clause instanceof org.abs_models.xtext.abs.DeltaClauseApplicationClauseOr) {
            result = new AppCondOr(fromXtext(xtext_clause.getLeft()),
                                   fromXtext(xtext_clause.getRight()));
        } else if (xtext_clause instanceof org.abs_models.xtext.abs.DeltaClauseApplicationClauseAnd) {
            result = new AppCondAnd(fromXtext(xtext_clause.getLeft()),
                                    fromXtext(xtext_clause.getRight()));
        } else {
            // primary
            if (xtext_clause.getNot() != null) {
                result = new AppCondNot(fromXtext(xtext_clause.getNot()));
            } else if (xtext_clause.getParen() != null) {
                return fromXtext(xtext_clause.getParen());
            } else {
                result = new AppCondFeature(xtext_clause.getFeature_name());
            }
        }
        return nodeWithLocation(result, xtext_clause);
    }

    // Products

    private static ProductDecl fromXtext(org.abs_models.xtext.abs.ProductDecl xtext_decl) {
        ProductDecl result = new ProductDecl();
        result.setName(xtext_decl.getName());
        if (xtext_decl.getExpr() != null) {
            // new syntax
            result.setProductExpr(fromXtext(xtext_decl.getExpr()));
        } else {
            // old syntax
            ProductFeatureSet fs = new ProductFeatureSet();
            for (org.abs_models.xtext.abs.ProductFeature feat : xtext_decl.getFeatures()) {
                fs.addFeature(fromXtext(feat));
            }
            result.setProductExpr(fs);
        }
        return nodeWithLocation(result, xtext_decl);
    }

    private static ProductExpr fromXtext(org.abs_models.xtext.abs.ProductExpr xtext_exp) {
        ProductExpr result = null;
        if (xtext_exp instanceof org.abs_models.xtext.abs.ProductExprDifference) {
            result = new ProductDifference(fromXtext(xtext_exp.getLeft()),
                                           fromXtext(xtext_exp.getRight()));
        } else if (xtext_exp instanceof org.abs_models.xtext.abs.ProductExprUnion) {
            result = new ProductUnion(fromXtext(xtext_exp.getLeft()),
                                      fromXtext(xtext_exp.getRight()));
        } else if (xtext_exp instanceof org.abs_models.xtext.abs.ProductExprIntersect) {
            result = new ProductIntersect(fromXtext(xtext_exp.getLeft()),
                                          fromXtext(xtext_exp.getRight()));
        } else {
            if (xtext_exp.getProdname() != null) {
                result = new ProductName(xtext_exp.getProdname());
            } else if (xtext_exp.getParen() != null) {
                return fromXtext(xtext_exp.getParen());
            } else {
                result = new ProductFeatureSet();
                for (org.abs_models.xtext.abs.ProductFeature feat : xtext_exp.getFeatures()) {
                    ((ProductFeatureSet)result).addFeature(fromXtext(feat));
                }
            }
        }
        return nodeWithLocation(result, xtext_exp);
    }

    // mTVL

    private static FeatureDecl fromXtext(org.abs_models.xtext.abs.MTVLFeatureRoot xtext_root) {
        FeatureDecl result = new FeatureDecl();
        result.setName(xtext_root.getName());
        if (xtext_root.getGroup() != null) {
            result.setGroup(fromXtext(xtext_root.getGroup()));
        }
        AttrConstraints constraints = new AttrConstraints();
        for (org.abs_models.xtext.abs.MTVLAttributeDecl attr : xtext_root.getAttributes()) {
            constraints.addAttributeNoTransform(fromXtext(attr));
        }
        for (org.abs_models.xtext.abs.MTVLConstraint constraint : xtext_root.getConstraints()) {
            constraints.addConstrNoTransform(fromXtext(constraint));
        }
        result.setAttrConstraints(constraints);
        return nodeWithLocation(result, xtext_root);
    }

    private static FeatureDecl fromXtext(org.abs_models.xtext.abs.MTVLFeatureDecl xtext_feature) {
        FeatureDecl result = new FeatureDecl();
        result.setName(xtext_feature.getName());
        if (xtext_feature.getGroup() != null) {
            result.setGroup(fromXtext(xtext_feature.getGroup()));
        }
        AttrConstraints constraints = new AttrConstraints();
        for (org.abs_models.xtext.abs.MTVLAttributeDecl attr : xtext_feature.getAttributes()) {
            constraints.addAttributeNoTransform(fromXtext(attr));
        }
        for (org.abs_models.xtext.abs.MTVLConstraint constraint : xtext_feature.getConstraints()) {
            constraints.addConstrNoTransform(fromXtext(constraint));
        }
        result.setAttrConstraints(constraints);
        return nodeWithLocation(result, xtext_feature);
    }

    private static FExt fromXtext(org.abs_models.xtext.abs.MTVLFeatureExtension xtext_ext) {
        FExt result = new FExt();
        result.setName(xtext_ext.getName());
        if (xtext_ext.getGroup() != null) {
            result.setGroup(fromXtext(xtext_ext.getGroup()));
        }
        AttrConstraints constraints = new AttrConstraints();
        for (org.abs_models.xtext.abs.MTVLAttributeDecl attr : xtext_ext.getAttributes()) {
            constraints.addAttributeNoTransform(fromXtext(attr));
        }
        for (org.abs_models.xtext.abs.MTVLConstraint constraint : xtext_ext.getConstraints()) {
            constraints.addConstrNoTransform(fromXtext(constraint));
        }
        result.setAttrConstraints(constraints);
        return nodeWithLocation(result, xtext_ext);
    }

    private static Group fromXtext(org.abs_models.xtext.abs.MTVLFeatureGroup xtext_group) {
        Group result = new Group();
        if (xtext_group.isAllof()) {
            result.setCard(new AllOf());
        } else if(xtext_group.isOneof()) {
            result.setCard(new CRange(1, 1));
        } else if(xtext_group.getUpper().isStar()) {
            result.setCard(new Minim(xtext_group.getLower().intValue()));
        } else {
            result.setCard(new CRange(xtext_group.getLower().intValue(),
                                      xtext_group.getUpper().getValue().intValue()));
        }
        for (org.abs_models.xtext.abs.MTVLChildFeature f : xtext_group.getChildren()) {
            result.addFNodeNoTransform(fromXtext(f));
        }
        return nodeWithLocation(result, xtext_group);
    }

    private static Attribute fromXtext(org.abs_models.xtext.abs.MTVLAttributeDecl xtext_attr) {
        Attribute result = new Attribute();
        result.setName(xtext_attr.getName());
        if (xtext_attr.getType().equals("Int")) {
            if (xtext_attr.isInterval()) {
                result.setAType(new IntMType("Int",
                                             fromXtext(xtext_attr.getLower()),
                                             fromXtext(xtext_attr.getUpper())));
            } else if (xtext_attr.isSet()) {
                IntListMType mt = new IntListMType("Int", new List<>());
                for (org.abs_models.xtext.abs.MTVLIntValue v : xtext_attr.getContent()) {
                    mt.addBoundaryValNoTransform(new BoundaryVal((v.isMinus() ? -1 : +1)
                                                                 * v.getValue().intValue()));
                }
                result.setAType(mt);
            } else {
                result.setAType(new IntMType("Int", new Limit(), new Limit()));
            }
        } else if (xtext_attr.getType().equals("String")) {
            result.setAType(new StringMType("String"));
        } else if (xtext_attr.getType().equals("Bool")) {
            result.setAType(new BoolMType("Bool"));
        } else {
            // should not happen - caught during validation
            result.setAType(new UnresolvedMType(xtext_attr.getType()));
        }
        return nodeWithLocation(result, xtext_attr);
    }

    private static BoundaryInt fromXtext(org.abs_models.xtext.abs.MTVLIntLimit xtext_lim) {
        BoundaryInt result = null;
        if (xtext_lim.isStar()) {
            result = new Limit();
        } else {
            result = new BoundaryVal((xtext_lim.isMinus() ? -1 : +1)
                                     * xtext_lim.getValue().intValue());
        }
        return nodeWithLocation(result, xtext_lim);
    }

    private static Constr fromXtext(org.abs_models.xtext.abs.MTVLConstraint xtext_constr) {
        Constr result = null;
        if (xtext_constr instanceof org.abs_models.xtext.abs.MTVLIfInConstraint) {
            result = new IfIn(fromXtext(((org.abs_models.xtext.abs.MTVLIfInConstraint)xtext_constr).getExpr()));
        } else if (xtext_constr instanceof org.abs_models.xtext.abs.MTVLIfOutConstraint) {
            result = new IfOut(fromXtext(((org.abs_models.xtext.abs.MTVLIfOutConstraint)xtext_constr).getExpr()));
        } else if (xtext_constr instanceof org.abs_models.xtext.abs.MTVLRequireConstraint) {
            result = new Require(new FeatVar(((org.abs_models.xtext.abs.MTVLRequireConstraint)xtext_constr).getRequire()));
        } else if (xtext_constr instanceof org.abs_models.xtext.abs.MTVLExcludeConstraint) {
            result = new Exclude(new FeatVar(((org.abs_models.xtext.abs.MTVLExcludeConstraint)xtext_constr).getExclude()));
        } else {
            throw new NotImplementedYetException(new ASTNode(),
                                                 "No conversion to JastAdd implemented for Xtext node "
                                                 + xtext_constr.getClass().toString());
        }
        return nodeWithLocation(result, xtext_constr);
    }

    private static FNode fromXtext(org.abs_models.xtext.abs.MTVLChildFeature xtext_feat) {
        FNode result = null;
        if (xtext_feat.isOpt()) {
            result = new OptFeat(fromXtext(xtext_feat.getFeature()));
        } else {
            result = new MandFeat(fromXtext(xtext_feat.getFeature()));
        }
        return nodeWithLocation(result, xtext_feat);
    }

    private static MExp fromXtext(org.abs_models.xtext.abs.MTVLConstraintExpr xtext_exp) {
        MExp result = null;

        if (xtext_exp instanceof org.abs_models.xtext.abs.MTVLConstraintExprOr) {
            result = new MOrBoolExp(fromXtext(xtext_exp.getLeft()),
                                    fromXtext(xtext_exp.getRight()));
        } else if (xtext_exp instanceof org.abs_models.xtext.abs.MTVLConstraintExprAnd) {
            result = new MAndBoolExp(fromXtext(xtext_exp.getLeft()),
                                     fromXtext(xtext_exp.getRight()));
        } else if (xtext_exp instanceof org.abs_models.xtext.abs.MTVLConstraintExprImpl) {
            result = new MImpliesExp(fromXtext(xtext_exp.getLeft()),
                                     fromXtext(xtext_exp.getRight()));
        } else if (xtext_exp instanceof org.abs_models.xtext.abs.MTVLConstraintExprEqv) {
            result = new MEquivExp(fromXtext(xtext_exp.getLeft()),
                                   fromXtext(xtext_exp.getRight()));
        } else if (xtext_exp instanceof org.abs_models.xtext.abs.MTVLConstraintExprEq) {
            result = new MEqExp(fromXtext(xtext_exp.getLeft()),
                                fromXtext(xtext_exp.getRight()));
        } else if (xtext_exp instanceof org.abs_models.xtext.abs.MTVLConstraintExprNeq) {
            result = new MNotEqExp(fromXtext(xtext_exp.getLeft()),
                                   fromXtext(xtext_exp.getRight()));
        } else if (xtext_exp instanceof org.abs_models.xtext.abs.MTVLConstraintExprLT) {
            result = new MLTExp(fromXtext(xtext_exp.getLeft()),
                                fromXtext(xtext_exp.getRight()));
        } else if (xtext_exp instanceof org.abs_models.xtext.abs.MTVLConstraintExprGT) {
            result = new MGTExp(fromXtext(xtext_exp.getLeft()),
                                fromXtext(xtext_exp.getRight()));
        } else if (xtext_exp instanceof org.abs_models.xtext.abs.MTVLConstraintExprLTEQ) {
            result = new MLTEQExp(fromXtext(xtext_exp.getLeft()),
                                  fromXtext(xtext_exp.getRight()));
        } else if (xtext_exp instanceof org.abs_models.xtext.abs.MTVLConstraintExprGTEQ) {
            result = new MGTEQExp(fromXtext(xtext_exp.getLeft()),
                                  fromXtext(xtext_exp.getRight()));
        } else if (xtext_exp instanceof org.abs_models.xtext.abs.MTVLConstraintExprPlus) {
            result = new MAddAddExp(fromXtext(xtext_exp.getLeft()),
                                    fromXtext(xtext_exp.getRight()));
        } else if (xtext_exp instanceof org.abs_models.xtext.abs.MTVLConstraintExprMinus) {
            result = new MSubAddExp(fromXtext(xtext_exp.getLeft()),
                                    fromXtext(xtext_exp.getRight()));
        } else if (xtext_exp instanceof org.abs_models.xtext.abs.MTVLConstraintExprMul) {
            result = new MMultMultExp(fromXtext(xtext_exp.getLeft()),
                                      fromXtext(xtext_exp.getRight()));
        } else if (xtext_exp instanceof org.abs_models.xtext.abs.MTVLConstraintExprDiv) {
            result = new MDivMultExp(fromXtext(xtext_exp.getLeft()),
                                     fromXtext(xtext_exp.getRight()));
        } else if (xtext_exp instanceof org.abs_models.xtext.abs.MTVLConstraintExprMod) {
            result = new MModMultExp(fromXtext(xtext_exp.getLeft()),
                                     fromXtext(xtext_exp.getRight()));
        }
        // MTVLConstraintExprPrimary
        else if (xtext_exp.getParen_exp() != null) {
            return fromXtext(xtext_exp.getParen_exp());
        } else if (xtext_exp.getMinus_exp() != null) {
            result = new MMinusExp(fromXtext(xtext_exp.getMinus_exp()));
        } else if (xtext_exp.getNeg_exp() != null) {
            result = new MNegExp(fromXtext(xtext_exp.getNeg_exp()));
        } else if (xtext_exp.getInt_exp() != null) {
            result = new MValue(new IntVal(xtext_exp.getInt_exp().intValue()));
        } else if (xtext_exp.getId_exp() != null) {
            result = new AttVar(xtext_exp.getId_exp());
        } else {
            if (xtext_exp.getDot_id() != null) {
                result = new FAVar(xtext_exp.getType_exp(),
                                   xtext_exp.getDot_id());
            } else {
                if (xtext_exp.getType_exp().equals("True")) {
                    result = new MValue(new BoolVal(true));
                } else if (xtext_exp.getType_exp().equals("False")) {
                    result = new MValue(new BoolVal(false));
                } else {
                    result = new FeatVar(xtext_exp.getType_exp());
                }
            }
        }
        return nodeWithLocation(result, xtext_exp);
    }

}
