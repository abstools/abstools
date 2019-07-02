package org.abs_models.frontend.parser;

import org.abs_models.common.NotImplementedYetException;
import org.abs_models.frontend.ast.*;
import org.abs_models.frontend.ast.Annotation;
import org.abs_models.frontend.ast.AssertStmt;
import org.abs_models.frontend.ast.AssignStmt;
import org.abs_models.frontend.ast.AwaitStmt;
import org.abs_models.frontend.ast.Block;
import org.abs_models.frontend.ast.CaseExp;
import org.abs_models.frontend.ast.CaseStmt;
import org.abs_models.frontend.ast.ClassDecl;
import org.abs_models.frontend.ast.CompilationUnit;
import org.abs_models.frontend.ast.ConstructorPattern;
import org.abs_models.frontend.ast.DataTypeDecl;
import org.abs_models.frontend.ast.DieStmt;
import org.abs_models.frontend.ast.DurationStmt;
import org.abs_models.frontend.ast.EqExp;
import org.abs_models.frontend.ast.ExceptionDecl;
import org.abs_models.frontend.ast.Exp;
import org.abs_models.frontend.ast.FieldDecl;
import org.abs_models.frontend.ast.ForeachStmt;
import org.abs_models.frontend.ast.FunctionDecl;
import org.abs_models.frontend.ast.GetExp;
import org.abs_models.frontend.ast.Guard;
import org.abs_models.frontend.ast.IfExp;
import org.abs_models.frontend.ast.IfStmt;
import org.abs_models.frontend.ast.InterfaceDecl;
import org.abs_models.frontend.ast.LetExp;
import org.abs_models.frontend.ast.ModuleDecl;
import org.abs_models.frontend.ast.MoveCogToStmt;
import org.abs_models.frontend.ast.NewExp;
import org.abs_models.frontend.ast.ParamDecl;
import org.abs_models.frontend.ast.PartialFunctionDecl;
import org.abs_models.frontend.ast.Pattern;
import org.abs_models.frontend.ast.ReturnStmt;
import org.abs_models.frontend.ast.SkipStmt;
import org.abs_models.frontend.ast.Stmt;
import org.abs_models.frontend.ast.SuspendStmt;
import org.abs_models.frontend.ast.ThrowStmt;
import org.abs_models.frontend.ast.TryCatchFinallyStmt;
import org.abs_models.frontend.ast.TypeUse;
import org.abs_models.frontend.ast.VarDeclStmt;
import org.abs_models.frontend.ast.WhileStmt;
import org.abs_models.xtext.abs.*;
import org.abs_models.xtext.abs.AbsPackage;
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
            if (filename == null) filename = "(standard library)";
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
                result.addCompilationUnitNoTransform((CompilationUnit) fromXtext((org.abs_models.xtext.abs.CompilationUnit) unit));
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
            // TODO set locations for Name arguments
            if (export.isStar()) {
                // "export *;"
                // "export * from OtherModule;"
                StarExport se = new StarExport();
                e = se;
                if (export.getModulename() != null)
                    se.setModuleName(new Name(export.getModulename()));
            } else if (export.getModulename() != null) {
                // "export a, b from OtherModule;"
                FromExport fe = new FromExport();
                e = fe;
                fe.setModuleName(export.getModulename());
                for (String id : export.getIdentifiers()) {
                    fe.addNameNoTransform(new Name(id));
                }
            } else {
                // "export a, b;"
                NamedExport ne = new NamedExport();
                e = ne;
                for (String id : export.getIdentifiers()) {
                    ne.addNameNoTransform(new Name(id));
                }
            }
            result.addExportNoTransform(nodeWithLocation(e, export));
        }
        for (org.abs_models.xtext.abs.ModuleImport imp : xtext_module.getImports()) {
            Import i;
            if (imp.isStar()) {
                // "import * from OtherModule;"
                StarImport si = new StarImport(imp.getModulename());
                i = si;
            } else if (imp.getModulename() != null) {
                // "import a, b from OtherModule;"
                FromImport fi = new FromImport();
                i = fi;
                fi.setModuleName(imp.getModulename());
                for (String id : imp.getIdentifiers()) {
                    fi.addNameNoTransform(new Name(id));
                }
            } else {
                // "import OtherModule.a, OtherModule.b;"
                NamedImport ni = new NamedImport();
                i = ni;
                for (String id : imp.getIdentifiers()) {
                    ni.addNameNoTransform(new Name(id));
                }
            }
            result.addImportNoTransform(nodeWithLocation(i, imp));
        }
        for (org.abs_models.xtext.abs.Declaration decl : xtext_module.getDeclarations()) {
            result.addDeclNoTransform(fromXtext(decl));
        }

        if(xtext_module.getMainblockstmts() != null) {
            List<Stmt> statements = new List<>();
            for(org.abs_models.xtext.abs.Stmt stmt : xtext_module.getMainblockstmts()) {
                statements.add(statementFromXtext(stmt));
            }

            final MainBlock mainBlock = new MainBlock();
            mainBlock.setStmtList(statements);
            result.setBlock(nodeWithLocation(mainBlock, xtext_module));
        }
        return nodeWithLocation(result, xtext_module);
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
            for (String tp : xtext_decl.getTypeparams()) {
                ((ParametricDataTypeDecl)result).addTypeParameter(new TypeParameterDecl(tp));
            }
        }
        result.setName(xtext_decl.getName());

        result.setAnnotationList(annotationsfromXtext(xtext_decl.getAnnotations()));

        for (org.abs_models.xtext.abs.DataConstructorDecl xtext_d : xtext_decl.getConstructors()) {
             result.addDataConstructor(fromXtext(xtext_d));
        }
        return result;
    }

    private static List<Annotation> annotationsfromXtext(org.abs_models.xtext.abs.Annotations annotations) {
        List<Annotation> annotationList = new List<>();
        for(org.abs_models.xtext.abs.Annotation annotation : annotations.getAnnotations()) {
            Annotation astAnnotation;
            PureExp exp = pureExpFromXtext(annotation.getValue());
            if (annotation.getId() != null) {
                // FIXME: check that DataTypeUse is the right Access subclass to use
                astAnnotation = new TypedAnnotation(exp, nodeWithLocation(new DataTypeUse(annotation.getId(), new List<>()), annotation, AbsPackage.eINSTANCE.getAnnotation_Id()));
            } else {
                astAnnotation = new Annotation(exp);
            }
            annotationList.add(nodeWithLocation(astAnnotation, annotation));
        }
        return annotationList;
    }

    private static Exp expFromXtext(org.abs_models.xtext.abs.Exp value) {
        if(value instanceof GetExp || value instanceof OriginalCallExp || value instanceof MethodCallExp || value instanceof NewExp) {
            return effExpFromXtext(value);
        }
        else {
            return pureExpFromXtext(value);
        }
    }

    private static EffExp effExpFromXtext(org.abs_models.xtext.abs.Exp value) {
        EffExp result = null;
        if(value instanceof org.abs_models.xtext.abs.GetExp) {
            org.abs_models.xtext.abs.GetExp xtextExp = (org.abs_models.xtext.abs.GetExp) value;
            GetExp exp = new GetExp();
            exp.setPureExp(pureExpFromXtext(xtextExp.getFuture()));

            result = exp;
        }
        else if(value instanceof org.abs_models.xtext.abs.OriginalCallExp) {
            org.abs_models.xtext.abs.OriginalCallExp xtextExp = (OriginalCallExp) value;
            OriginalCall exp = new OriginalCall();

            List<PureExp> paramList = new List<>();
            for(org.abs_models.xtext.abs.Exp e : xtextExp.getArgs()) {
                paramList.add(pureExpFromXtext(e));
            }
            exp.setParamList(paramList);

            result = exp;
        }
        else if(value instanceof org.abs_models.xtext.abs.MethodCallExp) {
            org.abs_models.xtext.abs.MethodCallExp xtextExp = (MethodCallExp) value;

            if("!".equals(xtextExp.getOperator())) {
                // FIXME implementation missing --> target class?
            }
            else if(".".equals(xtextExp.getOperator())) {
                // FIXME implementation missing --> target class?
            }
            else {
                throw new NotImplementedYetException(new ASTNode(),
                    "Unknown operator " + xtextExp.getOperator() + " in expression" + value.getClass().toString());
            }
        }
        else if(value instanceof org.abs_models.xtext.abs.NewExp) {
            org.abs_models.xtext.abs.NewExp xtextExp = (org.abs_models.xtext.abs.NewExp) value;
            NewExp exp = new NewExp();
            exp.setClassName(xtextExp.getClassname());

            List<PureExp> paramList = new List<>();
            for(org.abs_models.xtext.abs.Exp e : xtextExp.getArgs()) {
                paramList.add(pureExpFromXtext(e));
            }
            exp.setParamList(paramList);

            // FIXME where to get exp.setLocal()

            result = exp;
        }
        else {
            throw new NotImplementedYetException(new ASTNode(),
                "No conversion to JastAdd implemented for Xtext node "
                    + value.getClass().toString());
        }

        return nodeWithLocation(result, value);
    }

    private static PureExp pureExpFromXtext(org.abs_models.xtext.abs.Exp value) {
        PureExp result = null;
        if(value instanceof org.abs_models.xtext.abs.CaseExp) {
            org.abs_models.xtext.abs.CaseExp xtextExp = (org.abs_models.xtext.abs.CaseExp) value;
            CaseExp exp = new CaseExp();
            exp.setExpr(pureExpFromXtext(xtextExp.getCondition()));

            List<CaseBranch> branches = new List<>();
            for(CaseExpBranch cb : xtextExp.getCasebranches()) {
                Pattern pattern = patternFromXtext(cb.getPattern());
                PureExp pureExp = pureExpFromXtext(cb.getExpression());
                CaseBranch branch = new CaseBranch(pattern, pureExp);
                branches.add(branch);
            }
            exp.setBranchList(branches);

            result = exp;
        }
        else if(value instanceof org.abs_models.xtext.abs.LetExp) {
            org.abs_models.xtext.abs.LetExp xtextExp = (org.abs_models.xtext.abs.LetExp) value;
            LetExp exp = new LetExp();

            // FIXME implementation missing --> matching fields and 1:n relations?

            result = exp;
        }
        else if(value instanceof org.abs_models.xtext.abs.IfExp) {
            org.abs_models.xtext.abs.IfExp xtextExp = (org.abs_models.xtext.abs.IfExp) value;
            IfExp exp = new IfExp();
            exp.setCondExp(pureExpFromXtext(xtextExp.getCondition()));
            exp.setThenExp(pureExpFromXtext(xtextExp.getConsequence()));
            exp.setElseExp(pureExpFromXtext(xtextExp.getAlternate()));

            result = exp;
        }
        else if(value instanceof org.abs_models.xtext.abs.FunctionAppExp) {
            org.abs_models.xtext.abs.FunctionAppExp xtextExp = (FunctionAppExp) value;
            // FIXME implementation missing --> target class? FnApp??
        }
        else if(value instanceof org.abs_models.xtext.abs.VariadicFunctionAppExp) {
            org.abs_models.xtext.abs.VariadicFunctionAppExp xtextExp = (VariadicFunctionAppExp) value;
            // FIXME implementation missing --> target class?
        }
        else if(value instanceof org.abs_models.xtext.abs.PartialFunctionAppExp) {
            org.abs_models.xtext.abs.PartialFunctionAppExp xtextExp = (PartialFunctionAppExp) value;
            // FIXME implementation missing --> target class? ParFnApp??
        }
        else if(value instanceof org.abs_models.xtext.abs.ConstructorAppExp) {
            org.abs_models.xtext.abs.ConstructorAppExp xtextExp = (ConstructorAppExp) value;
            DataConstructorExp exp = new DataConstructorExp();
            exp.setConstructor(xtextExp.getName());

            List<PureExp> params = new List<>();
            for(org.abs_models.xtext.abs.Exp param : xtextExp.getArgs()) {
                params.add(pureExpFromXtext(param));
            }
            exp.setParamList(params);

            result = exp;
        }
        else if(value instanceof org.abs_models.xtext.abs.TemplateStringExp) {
            org.abs_models.xtext.abs.TemplateStringExp xtextExp = (TemplateStringExp) value;
            // FIXME implementation missing --> target class?
        }
        else if(value instanceof org.abs_models.xtext.abs.OrExp) {
            org.abs_models.xtext.abs.OrExp xtextExp = (OrExp) value;
            OrBoolExp exp = new OrBoolExp();
            exp.setLeft(pureExpFromXtext(xtextExp.getLeft()));
            exp.setRight(pureExpFromXtext(xtextExp.getRight()));

            result = exp;
        }
        else if(value instanceof org.abs_models.xtext.abs.AndExp) {
            org.abs_models.xtext.abs.AndExp xtextExp = (AndExp) value;
            AndBoolExp exp = new AndBoolExp();
            exp.setLeft(pureExpFromXtext(xtextExp.getLeft()));
            exp.setRight(pureExpFromXtext(xtextExp.getRight()));

            result = exp;
        }
        else if(value instanceof org.abs_models.xtext.abs.EqExp) {
            org.abs_models.xtext.abs.EqExp xtextExp = (org.abs_models.xtext.abs.EqExp) value;

            if("==".equals(xtextExp.getOperator())) {
                EqExp exp = new EqExp();
                exp.setLeft(pureExpFromXtext(xtextExp.getLeft()));
                exp.setRight(pureExpFromXtext(xtextExp.getRight()));

                result = exp;
            }
            else if("!=".equals(xtextExp.getOperator())) {
                NotEqExp exp = new NotEqExp();
                exp.setLeft(pureExpFromXtext(xtextExp.getLeft()));
                exp.setRight(pureExpFromXtext(xtextExp.getRight()));

                result = exp;
            }
            else {
                throw new NotImplementedYetException(new ASTNode(),
                    "Unknown operator " + xtextExp.getOperator() + " in expression" + value.getClass().toString());
            }
        }
        else if(value instanceof org.abs_models.xtext.abs.CompareExp) {
            org.abs_models.xtext.abs.CompareExp xtextExp = (CompareExp) value;

            if("<".equals(xtextExp.getOperator())) {
                LTExp exp = new LTExp();
                exp.setLeft(pureExpFromXtext(xtextExp.getLeft()));
                exp.setRight(pureExpFromXtext(xtextExp.getRight()));

                result = exp;
            }
            else if(">".equals(xtextExp.getOperator())) {
                GTExp exp = new GTExp();
                exp.setLeft(pureExpFromXtext(xtextExp.getLeft()));
                exp.setRight(pureExpFromXtext(xtextExp.getRight()));

                result = exp;
            }
            else if("<=".equals(xtextExp.getOperator())) {
                LTEQExp exp = new LTEQExp();
                exp.setLeft(pureExpFromXtext(xtextExp.getLeft()));
                exp.setRight(pureExpFromXtext(xtextExp.getRight()));

                result = exp;
            }
            else if(">=".equals(xtextExp.getOperator())) {
                GTEQExp exp = new GTEQExp();
                exp.setLeft(pureExpFromXtext(xtextExp.getLeft()));
                exp.setRight(pureExpFromXtext(xtextExp.getRight()));

                result = exp;
            }
            else {
                throw new NotImplementedYetException(new ASTNode(),
                    "Unknown operator " + xtextExp.getOperator() + " in expression" + value.getClass().toString());
            }
        }
        else if(value instanceof org.abs_models.xtext.abs.PlusMinusExp) {
            org.abs_models.xtext.abs.PlusMinusExp xtextExp = (PlusMinusExp) value;

            if("+".equals(xtextExp.getOperator())) {
                AddAddExp exp = new AddAddExp();
                exp.setLeft(pureExpFromXtext(xtextExp.getLeft()));
                exp.setRight(pureExpFromXtext(xtextExp.getRight()));

                result = exp;
            }
            else if("-".equals(xtextExp.getOperator())) {
                SubAddExp exp = new SubAddExp();
                exp.setLeft(pureExpFromXtext(xtextExp.getLeft()));
                exp.setRight(pureExpFromXtext(xtextExp.getRight()));

                result = exp;
            }
            else {
                throw new NotImplementedYetException(new ASTNode(),
                    "Unknown operator " + xtextExp.getOperator() + " in expression" + value.getClass().toString());
            }
        }
        // FIXME class MulDivModExp unknown
//        else if(value instanceof MulDivModE) {
//
//        }
        else if(value instanceof org.abs_models.xtext.abs.UnaryExp) {
            org.abs_models.xtext.abs.UnaryExp xtextExp = (UnaryExp) value;

            if("!".equals(xtextExp.getOperator())) {
                // FIXME implementation missing --> target class?
            }
            else if("-".equals(xtextExp.getOperator())) {
                // FIXME implementation missing --> target class?
            }
            else if("+".equals(xtextExp.getOperator())) {
                // FIXME implementation missing --> target class?
            }
            else {
                throw new NotImplementedYetException(new ASTNode(),
                    "Unknown operator " + xtextExp.getOperator() + " in expression" + value.getClass().toString());
            }
        }
        else if(value instanceof org.abs_models.xtext.abs.ConversionExp) {
            org.abs_models.xtext.abs.ConversionExp xtextExp = (ConversionExp) value;

            if("implements".equals(xtextExp.getOperator())) {
                // FIXME implementation missing --> target class?
            }
            else if("as".equals(xtextExp.getOperator())) {
                // FIXME implementation missing --> target class?
            }
            else {
                throw new NotImplementedYetException(new ASTNode(),
                    "Unknown operator " + xtextExp.getOperator() + " in expression" + value.getClass().toString());
            }
        }
        // FIXME class PrimaryExp unknown
//        else if(value instanceof PrimaryE) {
//
//        }
        // FIXME class AtomicExp unknown
//        else if(value instanceof AtomicE) {
//
//        }
        else if(value instanceof org.abs_models.xtext.abs.VarOrFieldExp) {
            org.abs_models.xtext.abs.VarOrFieldExp xtextExp = (VarOrFieldExp) value;
            // FIXME implementation missing --> target class?
        }
        else {
            throw new NotImplementedYetException(new ASTNode(),
                "No conversion to JastAdd implemented for Xtext node "
                    + value.getClass().toString());
        }

        return nodeWithLocation(result, value);
    }

    private static DataConstructor fromXtext(org.abs_models.xtext.abs.DataConstructorDecl xtext_d) {
        DataConstructor constructor = new DataConstructor();
        constructor.setName(xtext_d.getName());

        final EList<DataConstructorParamDecl> args = xtext_d.getArgs();
        if(!args.isEmpty()) {
            List<ConstructorArg> constructorArgs = constructorArgsFromXtext(args);
            constructor.setConstructorArgList(constructorArgs);
        }

        return nodeWithLocation(constructor, xtext_d);
    }

    private static List<ConstructorArg> constructorArgsFromXtext(EList<DataConstructorParamDecl> args) {
        List<ConstructorArg> constructorArgs = new List<>();
        for(DataConstructorParamDecl arg : args) {
            ConstructorArg constructorArg = new ConstructorArg();
            if(arg.getName() != null) {
                constructorArg.setSelectorName(new Name(arg.getName()));
            }
            nodeWithLocation(constructorArg, arg);

            TypeUse typeUse = new DataTypeUse();
            nodeWithLocation(typeUse, arg.getType());
            constructorArg.setTypeUse(typeUse);

            constructorArgs.add(constructorArg);
        }
        return constructorArgs;
    }

    static TypeSynDecl fromXtext(org.abs_models.xtext.abs.TypeSynonymDecl xtext_decl) {
        TypeSynDecl result = new TypeSynDecl();
        result.setName(xtext_decl.getName());

        result.setAnnotationList(annotationsfromXtext(xtext_decl.getAnnotations()));

        TypeUse typeUse = new DataTypeUse();
        nodeWithLocation(typeUse, xtext_decl.getType());
        result.setValue(typeUse);

        return nodeWithLocation(result, xtext_decl);
    }

    static ExceptionDecl fromXtext(org.abs_models.xtext.abs.ExceptionDecl xtext_decl) {
        ExceptionDecl result = new  ExceptionDecl();
        result.setName(xtext_decl.getName());

        result.setAnnotationList(annotationsfromXtext(xtext_decl.getAnnotations()));

        DataConstructor constructor = new DataConstructor();
        constructor.setName(xtext_decl.getName());

        List<ConstructorArg> constructorArgs = constructorArgsFromXtext(xtext_decl.getArgs());
        constructor.setConstructorArgList(constructorArgs);

        result.setDataConstructorList(new List<>(constructor));

        return nodeWithLocation(result, xtext_decl);
    }

    static FunctionDecl fromXtext(org.abs_models.xtext.abs.FunctionDecl xtext_decl) {
        FunctionDecl result = new  FunctionDecl();
        result.setName(xtext_decl.getName());

        result.setAnnotationList(annotationsfromXtext(xtext_decl.getAnnotations()));

        TypeUse typeUse = new DataTypeUse();
        nodeWithLocation(typeUse, xtext_decl.getType());
        result.setTypeUse(typeUse);

        List<ParamDecl> params = paramDeclsFromXtext(xtext_decl.getArgs());
        result.setParamList(params);

        // FIXME how to handle xtext_decl.getTypeparams() here?
        // FIXME xtext_decl.getBody() ?
        // FIXME how to fill result.setFunctionDef()? The 2 concrete subclasses don't seem to offer things todo?
        // FIXME My guess would be that xtext_decl.getBody() is matched to result.setFunctionDef() via expressions, but I don't see how... (also see PartialFunctionDecl below)

        return nodeWithLocation(result, xtext_decl);
    }

    private static List<ParamDecl> paramDeclsFromXtext(EList<org.abs_models.xtext.abs.ParamDecl> xtext_decl) {
        List<ParamDecl> params = new List<>();
        for(org.abs_models.xtext.abs.ParamDecl decl : xtext_decl) {
            ParamDecl paramDecl = new ParamDecl();
            paramDecl.setName(decl.getName());
            paramDecl.setAnnotationList(annotationsfromXtext(decl.getAnnotations()));

            // FIXME decl.getType() ?
            // FIXME paramDecl.setAccess() ?

            nodeWithLocation(paramDecl, decl);
            params.add(paramDecl);
        }
        return params;
    }

    static PartialFunctionDecl fromXtext(org.abs_models.xtext.abs.PartialFunctionDecl xtext_decl) {
        PartialFunctionDecl result = new  PartialFunctionDecl();
        result.setName(xtext_decl.getName());

        result.setAnnotationList(annotationsfromXtext(xtext_decl.getAnnotations()));

        TypeUse typeUse = new DataTypeUse();
        nodeWithLocation(typeUse, xtext_decl.getType());
        result.setTypeUse(typeUse);

        List<ParamDecl> params = paramDeclsFromXtext(xtext_decl.getArgs());
        result.setParamList(params);

        List<FunctionParamDecl> paramDeclList = new List<>();
        for(String fArg : xtext_decl.getFunction_args()) {
            FunctionParamDecl decl = new FunctionParamDecl();
            decl.setName(fArg);
            paramDeclList.add(decl);
        }
        result.setFuncParamList(paramDeclList);

        // FIXME how to handle xtext_decl.getTypeparams() here?

        // FIXME this seems too easy...
        PartialFunctionDef functionDef = new PartialFunctionDef();
        functionDef.setPureExp(pureExpFromXtext(xtext_decl.getBody()));
        nodeWithLocation(functionDef, xtext_decl.getBody());

        return nodeWithLocation(result, xtext_decl);
    }

    static InterfaceDecl fromXtext(org.abs_models.xtext.abs.InterfaceDecl xtext_decl) {
        InterfaceDecl result = new  InterfaceDecl();
        result.setName(xtext_decl.getName());

        result.setAnnotationList(annotationsfromXtext(xtext_decl.getAnnotations()));

        List<InterfaceTypeUse> interfaces = interfaceTypeUsesFromXtext(xtext_decl.getSuperinterfaces());
        result.setExtendedInterfaceUseList(interfaces);

        List<MethodSig> methodSigList = methodSigsFromXtext(xtext_decl.getMethods());
        result.setBodyList(methodSigList);

        return nodeWithLocation(result, xtext_decl);
    }

    private static List<MethodSig> methodSigsFromXtext(EList<MethodSignature> sigs) {
        List<MethodSig> methodSigList = new List<>();
        for(MethodSignature signature : sigs) {
            MethodSig sig = methodSigFromXtext(signature);
            methodSigList.add(sig);
        }
        return methodSigList;
    }

    private static MethodSig methodSigFromXtext(MethodSignature signature) {
        MethodSig sig = new MethodSig();
        sig.setName(sig.getName());
        sig.setAnnotationList(annotationsfromXtext(signature.getAnnotations()));
        sig.setParamList(paramDeclsFromXtext(signature.getArgs()));

        TypeUse typeUse = new DataTypeUse();
        nodeWithLocation(typeUse, signature.getType());
        sig.setReturnType(typeUse);

        nodeWithLocation(sig, signature);
        return sig;
    }

    private static List<InterfaceTypeUse> interfaceTypeUsesFromXtext(EList<String> xtext_decl) {
        List<InterfaceTypeUse> interfaces = new List<>();
        for(String name : xtext_decl) {
            InterfaceTypeUse typeUse = new InterfaceTypeUse();
            typeUse.setName(name);
            interfaces.add(typeUse);
        }
        return interfaces;
    }

    static ClassDecl fromXtext(org.abs_models.xtext.abs.ClassDecl xtext_decl) {
        ClassDecl result = new  ClassDecl();
        result.setName(xtext_decl.getName());

        result.setAnnotationList(annotationsfromXtext(xtext_decl.getAnnotations()));

        List<ParamDecl> params = paramDeclsFromXtext(xtext_decl.getArgs());
        result.setParamList(params);

        List<InterfaceTypeUse> interfaces = interfaceTypeUsesFromXtext(xtext_decl.getInterfaces());
        result.setImplementedInterfaceUseList(interfaces);


        List<FieldDecl> astFieldDelcDeclList = new List<>();
        for(org.abs_models.xtext.abs.FieldDecl fieldDecl : xtext_decl.getFields()) {
            FieldDecl astFieldDecl = new FieldDecl();
            astFieldDecl.setName(fieldDecl.getName());

            astFieldDecl.setAnnotationList(annotationsfromXtext(fieldDecl.getAnnotations()));

            astFieldDecl.setInitExp(pureExpFromXtext(fieldDecl.getInit()));

            // FIXME fieldDecl.getType() ?
            // FIXME astFieldDecl.setAccess() ?
            // FIXME astFieldDecl.setPort() ?

            nodeWithLocation(astFieldDecl, fieldDecl);
            astFieldDelcDeclList.add(astFieldDecl);
        }
        result.setFieldList(astFieldDelcDeclList);


        InitBlock astInitBlock = new InitBlock();

        for(org.abs_models.xtext.abs.Stmt statement : xtext_decl.getInitblockstmts()) {
            astInitBlock.addStmt(statementFromXtext(statement));
        }

        result.setInitBlock(astInitBlock);


        List<MethodImpl> methodList = new List<>();
        for(MethodDecl methodDecl : xtext_decl.getMethods()) {
            MethodImpl method = new MethodImpl();

            createMethodSigFromMethodDecl(methodDecl, method);

            Block block = new Block();
            block.setAnnotationList(annotationsfromXtext(methodDecl.getAnnotations()));

            List<Stmt> astStatements = new List<>();
            for(org.abs_models.xtext.abs.Stmt stmt : methodDecl.getStatements()) {
                astStatements.add(statementFromXtext(stmt));
            }

            block.setStmtList(astStatements);

            method.setBlock(block);

            nodeWithLocation(method, methodDecl);
            methodList.add(method);
        }
        result.setMethodList(methodList);


        List<CaseBranchStmt> branchStmts = caseBranchStmtsFromXtext(xtext_decl.getRecoverbranches());
        result.setRecoverBranchList(branchStmts);

        // FIXME result.setTraitUseList() ?

        return nodeWithLocation(result, xtext_decl);
    }

    private static List<CaseBranchStmt> caseBranchStmtsFromXtext(EList<CaseStmtBranch> statements) {
        List<CaseBranchStmt> branchStmts = new List<>();
        for(CaseStmtBranch branch : statements) {
            CaseBranchStmt astBranch = new CaseBranchStmt();
            astBranch.setLeft(patternFromXtext(branch.getPattern()));

            Stmt stmt = statementFromXtext(branch.getBody());
            Block block = new Block();
            block.setStmtList(new List<>(stmt));
            astBranch.setRight(block);

            nodeWithLocation(astBranch, branch);
            branchStmts.add(astBranch);
        }
        return branchStmts;
    }

    private static Stmt statementFromXtext(org.abs_models.xtext.abs.Stmt stmt) {
        Stmt result = null;

        if(stmt instanceof org.abs_models.xtext.abs.VarDeclStmt) {
            org.abs_models.xtext.abs.VarDeclStmt value = (org.abs_models.xtext.abs.VarDeclStmt) stmt;
            List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());

            VarDecl varDecl = new VarDecl();
            varDecl.setName(value.getName());
            varDecl.setInitExp(pureExpFromXtext(value.getInit()));
            // FIXME varDecl.setAccess() ?

            result = new VarDeclStmt(annotations, varDecl);
        }
        else if(stmt instanceof org.abs_models.xtext.abs.AssignStmt) {
            org.abs_models.xtext.abs.AssignStmt value = (org.abs_models.xtext.abs.AssignStmt) stmt;
            List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());

            Exp lhsExp = expFromXtext(value.getLhs());
            VarOrFieldUse varOrFieldUse;
            // FIXME how to get the data over? VarUse/FieldUse takes only 1 string?
            if(lhsExp instanceof VarUse) {
                varOrFieldUse = new VarUse();
            }
            else {
                varOrFieldUse = new FieldUse();
            }

            Exp expresssion = expFromXtext(value.getExp());

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

            result = new ReturnStmt(annotations, expFromXtext(value.getExp()));
        }
        else if(stmt instanceof org.abs_models.xtext.abs.AssertStmt) {
            org.abs_models.xtext.abs.AssertStmt value = (org.abs_models.xtext.abs.AssertStmt) stmt;
            List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());

            result = new AssertStmt(annotations, pureExpFromXtext(value.getExp()));
        }
        else if(stmt instanceof org.abs_models.xtext.abs.Block) {
            org.abs_models.xtext.abs.Block value = (org.abs_models.xtext.abs.Block) stmt;
            List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());

            List<Stmt> subStatements = new List<>();
            for(org.abs_models.xtext.abs.Stmt subStmt : value.getStmts()) {
                subStatements.add(statementFromXtext(subStmt));
            }

            result = new Block(annotations, subStatements);
        }
        else if(stmt instanceof org.abs_models.xtext.abs.IfStmt) {
            org.abs_models.xtext.abs.IfStmt value = (org.abs_models.xtext.abs.IfStmt) stmt;
            List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());

            PureExp condition = pureExpFromXtext(value.getCondition());

            Block consequence = (Block) statementFromXtext(value.getConsequence());

            Block alternateBlock = (Block) statementFromXtext(value.getAlternate());
            Opt<Block> alternate = new Opt<>(alternateBlock);

            result = new IfStmt(annotations, condition, consequence, alternate);
        }
        else if(stmt instanceof org.abs_models.xtext.abs.WhileStmt) {
            org.abs_models.xtext.abs.WhileStmt value = (org.abs_models.xtext.abs.WhileStmt) stmt;
            List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());

            PureExp condition = pureExpFromXtext(value.getCondition());

            Block body = (Block) statementFromXtext(value.getBody());

            result = new WhileStmt(annotations, condition, body);
        }
        else if(stmt instanceof org.abs_models.xtext.abs.ForeachStmt) {
            org.abs_models.xtext.abs.ForeachStmt value = (org.abs_models.xtext.abs.ForeachStmt) stmt;
            List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());

            // FIXME the whole VarOrFieldUse tree is likely to be done differently to get the data over (also see VarDeclStmt & AssignStmt), just cannot see it right now
            LoopVarDecl var = new LoopVarDecl(value.getVar());

            PureExp list = pureExpFromXtext(value.getList());

            Block body = (Block) statementFromXtext(value.getBody());

            result = new ForeachStmt(annotations, var, list, body);
        }
        else if(stmt instanceof org.abs_models.xtext.abs.TryCatchFinallyStmt) {
            org.abs_models.xtext.abs.TryCatchFinallyStmt value = (org.abs_models.xtext.abs.TryCatchFinallyStmt) stmt;
            List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());

            Block body = (Block) statementFromXtext(value.getBody());

            List<CaseBranchStmt> branches = caseBranchStmtsFromXtext(value.getBranches());

            Block finallyBlock = (Block) statementFromXtext(value.getFinally());
            Opt<Block> finallyOpt = new Opt<>(finallyBlock);

            result = new TryCatchFinallyStmt(annotations, body, branches, finallyOpt);
        }
        else if(stmt instanceof org.abs_models.xtext.abs.AwaitStmt) {
            org.abs_models.xtext.abs.AwaitStmt value = (org.abs_models.xtext.abs.AwaitStmt) stmt;
            List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());

            Guard guard = guardFromXtext(value.getGuard());

            result = new AwaitStmt(annotations, guard);
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
            List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());

            Exp exp = expFromXtext(value.getExp());

            result = new ExpressionStmt(annotations, exp);
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

    private static Guard guardFromXtext(org.abs_models.xtext.abs.Guard guard) {
        Guard result = null;

        // FIXME xtext-Guard classes (SingleGuard, ClaimGuard, DurationGuard, ExpGuard) unknown?
//        if(guard instanceof org.abs_models.xtext.abs.) {
//
//        }
//        else if() {
//
//        }
//        else if() {
//
//        }
//        else if() {
//
//        }
//        else {
//            throw new NotImplementedYetException(new ASTNode(),
//                "No conversion to JastAdd implemented for Xtext node "
//                    + guard.getClass().toString());
//        }
        return result;
    }

    private static Pattern patternFromXtext(org.abs_models.xtext.abs.Pattern pattern) {
        Pattern result;
        if(pattern instanceof org.abs_models.xtext.abs.WildcardPattern) {
            org.abs_models.xtext.abs.WildcardPattern value = (WildcardPattern) pattern;
            result = new UnderscorePattern();
            // FIXME really no data to transfer?
        }
        else if(pattern instanceof org.abs_models.xtext.abs.IntLiteralPattern) {
            org.abs_models.xtext.abs.IntLiteralPattern value = (IntLiteralPattern) pattern;
            result = null;
            // FIXME target class & how to transfer information?
        }
        else if(pattern instanceof org.abs_models.xtext.abs.StringLiteralPattern) {
            org.abs_models.xtext.abs.StringLiteralPattern value = (StringLiteralPattern) pattern;
            result = null;
            // FIXME target class & how to transfer information?
        }
        else if(pattern instanceof org.abs_models.xtext.abs.FloatLiteralPattern) {
            org.abs_models.xtext.abs.FloatLiteralPattern value = (FloatLiteralPattern) pattern;
            result = null;
            // FIXME target class & how to transfer information?
        }
        else if(pattern instanceof org.abs_models.xtext.abs.VariablePattern) {
            org.abs_models.xtext.abs.VariablePattern value = (VariablePattern) pattern;
            result = null;
            // FIXME target class & how to transfer information?
        }
        else if(pattern instanceof org.abs_models.xtext.abs.ConstructorPattern) {
            org.abs_models.xtext.abs.ConstructorPattern value = (org.abs_models.xtext.abs.ConstructorPattern) pattern;
            List<Pattern> astPatterns = new List<>();
            for(org.abs_models.xtext.abs.Pattern p : value.getArgs()) {
                astPatterns.add(patternFromXtext(p));
            }
            result = new ConstructorPattern(value.getName(), astPatterns);
        }
        else {
            throw new NotImplementedYetException(new ASTNode(),
                "No conversion to JastAdd implemented for Xtext node "
                    + pattern.getClass().toString());
        }
        return nodeWithLocation(result, pattern);
    }

    private static void createMethodSigFromMethodDecl(MethodDecl methodDecl, MethodImpl method) {
        MethodSig sig = new MethodSig();
        sig.setName(methodDecl.getName());
        sig.setAnnotationList(annotationsfromXtext(methodDecl.getAnnotations()));
        sig.setParamList(paramDeclsFromXtext(methodDecl.getArgs()));

        TypeUse typeUse = new DataTypeUse();
        nodeWithLocation(typeUse, methodDecl.getType());
        sig.setReturnType(typeUse);

        nodeWithLocation(sig, methodDecl);
        method.setMethodSig(sig);
    }

}
