package org.abs_models.frontend.parser;

import org.abs_models.common.NotImplementedYetException;
import org.abs_models.frontend.ast.*;
import org.abs_models.xtext.abs.AndExp;
import org.abs_models.xtext.abs.CompareExp;
import org.abs_models.xtext.abs.ConstructorAppExp;
import org.abs_models.xtext.abs.ConversionExp;
import org.abs_models.xtext.abs.DataConstructorParamDecl;
import org.abs_models.xtext.abs.FunctionAppExp;
import org.abs_models.xtext.abs.MethodCallExp;
import org.abs_models.xtext.abs.OrExp;
import org.abs_models.xtext.abs.OriginalCallExp;
import org.abs_models.xtext.abs.PartialFunctionAppExp;
import org.abs_models.xtext.abs.PlusMinusExp;
import org.abs_models.xtext.abs.TemplateStringExp;
import org.abs_models.xtext.abs.UnaryExp;
import org.abs_models.xtext.abs.VarOrFieldExp;
import org.abs_models.xtext.abs.VariadicFunctionAppExp;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;
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

        }
        else if(value instanceof org.abs_models.xtext.abs.OriginalCallExp) {

        }
        else if(value instanceof org.abs_models.xtext.abs.MethodCallExp) {

        }
        else if(value instanceof org.abs_models.xtext.abs.NewExp) {

        }
        else {
            throw new NotImplementedYetException(new ASTNode(),
                "No conversion to JastAdd implemented for Xtext node "
                    + value.getClass().toString());
        }
        return result;
    }

    private static PureExp pureExpFromXtext(org.abs_models.xtext.abs.Exp value) {
        PureExp result = null;
        // TODO may be 1:n relation defined by contents of xtext Exp
        if(value instanceof org.abs_models.xtext.abs.CaseExp) {

        }
        else if(value instanceof org.abs_models.xtext.abs.LetExp) {

        }
        else if(value instanceof org.abs_models.xtext.abs.IfExp) {

        }
        else if(value instanceof org.abs_models.xtext.abs.FunctionAppExp) {

        }
        else if(value instanceof org.abs_models.xtext.abs.VariadicFunctionAppExp) {

        }
        else if(value instanceof org.abs_models.xtext.abs.PartialFunctionAppExp) {

        }
        else if(value instanceof org.abs_models.xtext.abs.ConstructorAppExp) {

        }
        else if(value instanceof org.abs_models.xtext.abs.TemplateStringExp) {

        }
        else if(value instanceof org.abs_models.xtext.abs.OrExp) {

        }
        else if(value instanceof org.abs_models.xtext.abs.AndExp) {

        }
        else if(value instanceof org.abs_models.xtext.abs.EqExp) {

        }
        else if(value instanceof org.abs_models.xtext.abs.CompareExp) {

        }
        else if(value instanceof org.abs_models.xtext.abs.PlusMinusExp) {

        }
        // FIXME class MulDivModExp unknown
//        else if(value instanceof MulDivModE) {
//
//        }
        else if(value instanceof org.abs_models.xtext.abs.UnaryExp) {

        }
        else if(value instanceof org.abs_models.xtext.abs.ConversionExp) {

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

        }
        else {
            throw new NotImplementedYetException(new ASTNode(),
                "No conversion to JastAdd implemented for Xtext node "
                    + value.getClass().toString());
        }
        return result;
    }

    // TODO for every new object: nodeWithLocation()

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
        // TODO type, typeparams, args, body, annotations
        // TODO result: annotations, params, functionDef, typeUse

        return result;
    }

    static PartialFunctionDecl fromXtext(org.abs_models.xtext.abs.PartialFunctionDecl xtext_decl) {
        PartialFunctionDecl result = new  PartialFunctionDecl();
        result.setName(xtext_decl.getName());
        // TODO type, typeparams, args, body, annotations, function_args
        // TODO result: annotations, functionParams, params, partialFunctionDef, typeUse
        return result;
    }

    static InterfaceDecl fromXtext(org.abs_models.xtext.abs.InterfaceDecl xtext_decl) {
        InterfaceDecl result = new  InterfaceDecl();
        result.setName(xtext_decl.getName());
        // TODO annotations
        // TODO result: annotations, body, extendedInterfaces
        return result;
    }

    static ClassDecl fromXtext(org.abs_models.xtext.abs.ClassDecl xtext_decl) {
        ClassDecl result = new  ClassDecl();
        result.setName(xtext_decl.getName());
        // TODO args, interfaces, fields, init_block, recover_branches, methods, annotations
        // TODO result: annotations, fields, implementedInterfaces, initBlock, methods, params, recoverBranches, traitUses
        return result;
    }

}
