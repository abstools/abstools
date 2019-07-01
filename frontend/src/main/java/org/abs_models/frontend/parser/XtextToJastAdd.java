package org.abs_models.frontend.parser;

import org.abs_models.common.NotImplementedYetException;
import org.abs_models.frontend.ast.*;
import org.abs_models.frontend.ast.Annotation;
import org.abs_models.frontend.ast.Block;
import org.abs_models.frontend.ast.CaseExp;
import org.abs_models.frontend.ast.ClassDecl;
import org.abs_models.frontend.ast.CompilationUnit;
import org.abs_models.frontend.ast.ConstructorPattern;
import org.abs_models.frontend.ast.DataTypeDecl;
import org.abs_models.frontend.ast.EqExp;
import org.abs_models.frontend.ast.ExceptionDecl;
import org.abs_models.frontend.ast.Exp;
import org.abs_models.frontend.ast.FieldDecl;
import org.abs_models.frontend.ast.FunctionDecl;
import org.abs_models.frontend.ast.GetExp;
import org.abs_models.frontend.ast.IfExp;
import org.abs_models.frontend.ast.InterfaceDecl;
import org.abs_models.frontend.ast.LetExp;
import org.abs_models.frontend.ast.ModuleDecl;
import org.abs_models.frontend.ast.NewExp;
import org.abs_models.frontend.ast.ParamDecl;
import org.abs_models.frontend.ast.PartialFunctionDecl;
import org.abs_models.frontend.ast.Pattern;
import org.abs_models.frontend.ast.Stmt;
import org.abs_models.frontend.ast.TypeUse;
import org.abs_models.xtext.abs.*;
import org.abs_models.xtext.abs.AssignStmt;
import org.abs_models.xtext.abs.ReturnStmt;
import org.abs_models.xtext.abs.SkipStmt;
import org.abs_models.xtext.abs.VarDeclStmt;
import org.abs_models.xtext.abs.impl.VarDeclStmtImpl;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.nodemodel.INode;
import org.eclipse.xtext.nodemodel.util.NodeModelUtils;
import org.eclipse.xtext.resource.ILocationInFileProvider;
import org.eclipse.xtext.resource.XtextResourceSet;
import org.eclipse.xtext.util.ITextRegionWithLineInformation;
import org.eclipse.xtext.util.LineAndColumn;

import java.util.Arrays;

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
            // FIXME: handle non-file URLs here (abslang.abs is a resource so
            // doesn’t have a filename)
            node.setFileName(obj.eResource().getURI().toFileString());
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
        // TODO
        // if (xtext_module.getMainBlock() != null) {
        //     result.setBlock(fromXtext(xtext_module.getMainBlock()));
        // }
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
            Annotation astAnnotation = new Annotation();

            PureExp exp = pureExpFromXtext(annotation.getValue());
            astAnnotation.setValue(exp);
        }

        return annotationList;
    }

    private static Exp fromXtext(org.abs_models.xtext.abs.Exp value) {
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

            // FIXME implementation missing

            result = exp;
        }
        else if(value instanceof org.abs_models.xtext.abs.OriginalCallExp) {
            org.abs_models.xtext.abs.OriginalCallExp xtextExp = (OriginalCallExp) value;
            OriginalCall exp = new OriginalCall();

            // FIXME implementation missing

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

            // FIXME implementation missing

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

            // FIXME implementation missing

            result = exp;
        }
        else if(value instanceof org.abs_models.xtext.abs.LetExp) {
            org.abs_models.xtext.abs.LetExp xtextExp = (org.abs_models.xtext.abs.LetExp) value;
            LetExp exp = new LetExp();

            // FIXME implementation missing

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

            for(org.abs_models.xtext.abs.Exp param : xtextExp.getArgs()) {
                // FIXME implementation missing
            }

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


        org.abs_models.xtext.abs.Block initBlock = xtext_decl.getInitblock();
        InitBlock astInitBlock = new InitBlock();

        for(org.abs_models.xtext.abs.Stmt statement : initBlock.getStmts()) {
            astInitBlock.addStmt(statementFromXtext(statement));
        }

        nodeWithLocation(astInitBlock, initBlock);
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


        List<CaseBranchStmt> branchStmts = new List<>();
        for(CaseStmtBranch branch : xtext_decl.getRecoverbranches()) {
            CaseBranchStmt astBranch = new CaseBranchStmt();
            astBranch.setLeft(patternFromXtext(branch));

            Stmt stmt = statementFromXtext(branch.getBody());
            Block block = new Block();
            block.setStmtList(new List<>(stmt));
            astBranch.setRight(block);

            nodeWithLocation(astBranch, branch);
            branchStmts.add(astBranch);
        }
        result.setRecoverBranchList(branchStmts);

        // FIXME result.setTraitUseList() ?

        return nodeWithLocation(result, xtext_decl);
    }

    private static Stmt statementFromXtext(org.abs_models.xtext.abs.Stmt stmt) {
        Stmt result = null;
        List<Annotation> annotations = annotationsfromXtext(stmt.getAnnotations());

        if(stmt instanceof org.abs_models.xtext.abs.VarDeclStmt) {
            org.abs_models.xtext.abs.VarDeclStmt value = (VarDeclStmt) stmt;

            VarDecl varDecl = new VarDecl();
            varDecl.setName(value.getName());
            varDecl.setInitExp(pureExpFromXtext(value.getInit()));
            // FIXME varDecl.setAccess() ?

            result = new org.abs_models.frontend.ast.VarDeclStmt(annotations, varDecl);
        }
        // TODO implement
//        else if(stmt instanceof org.abs_models.xtext.abs.AssignStmt) {
//            org.abs_models.xtext.abs.AssignStmt value = (AssignStmt) stmt;
//
//            result = new org.abs_models.frontend.ast.AssignStmt();
//        }
//        else if(stmt instanceof org.abs_models.xtext.abs.SkipStmt) {
//            org.abs_models.xtext.abs.SkipStmt value = (SkipStmt) stmt;
//            result = new org.abs_models.frontend.ast.SkipStmt();
//        }
//        else if(stmt instanceof org.abs_models.xtext.abs.ReturnStmt) {
//            org.abs_models.xtext.abs.ReturnStmt value = (ReturnStmt) stmt;
//            result = new org.abs_models.frontend.ast.ReturnStmt();
//        }
//        else if(stmt instanceof ) {
//            value = stmt;
//            result =
//        }
//        else if(stmt instanceof ) {
//            value = stmt;
//            result =
//        }
        else {
            throw new NotImplementedYetException(new ASTNode(),
                "No conversion to JastAdd implemented for Xtext node "
                    + stmt.getClass().toString());
        }

        return nodeWithLocation(result, stmt);
    }

    private static Pattern patternFromXtext(CaseStmtBranch branch) {
        Pattern result;
        if(branch instanceof org.abs_models.xtext.abs.WildcardPattern) {
            org.abs_models.xtext.abs.WildcardPattern value = (WildcardPattern) branch;
            result = new UnderscorePattern();
            // FIXME correct target class & how to transfer information?
        }
        else if(branch instanceof org.abs_models.xtext.abs.IntLiteralPattern) {
            org.abs_models.xtext.abs.IntLiteralPattern value = (IntLiteralPattern) branch;
            result = null;
            // FIXME target class & how to transfer information?
        }
        else if(branch instanceof org.abs_models.xtext.abs.StringLiteralPattern) {
            org.abs_models.xtext.abs.StringLiteralPattern value = (StringLiteralPattern) branch;
            result = null;
            // FIXME target class & how to transfer information?
        }
        else if(branch instanceof org.abs_models.xtext.abs.FloatLiteralPattern) {
            org.abs_models.xtext.abs.FloatLiteralPattern value = (FloatLiteralPattern) branch;
            result = null;
            // FIXME target class & how to transfer information?
        }
        else if(branch instanceof org.abs_models.xtext.abs.VariablePattern) {
            org.abs_models.xtext.abs.VariablePattern value = (VariablePattern) branch;
            result = null;
            // FIXME target class & how to transfer information?
        }
        else if(branch instanceof org.abs_models.xtext.abs.ConstructorPattern) {
            org.abs_models.xtext.abs.ConstructorPattern value = (org.abs_models.xtext.abs.ConstructorPattern) branch;
            result = new ConstructorPattern();
            // FIXME how to transfer information?
        }
        else {
            throw new NotImplementedYetException(new ASTNode(),
                "No conversion to JastAdd implemented for Xtext node "
                    + branch.getClass().toString());
        }
        return nodeWithLocation(result, branch);
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
