package org.abs_models.frontend.parser;

import org.abs_models.common.NotImplementedYetException;
import org.abs_models.frontend.ast.ASTNode;
import org.abs_models.frontend.ast.ClassDecl;
import org.abs_models.frontend.ast.CompilationUnit;
import org.abs_models.frontend.ast.DataTypeDecl;
import org.abs_models.frontend.ast.Decl;
import org.abs_models.frontend.ast.ExceptionDecl;
import org.abs_models.frontend.ast.Export;
import org.abs_models.frontend.ast.FromExport;
import org.abs_models.frontend.ast.FromImport;
import org.abs_models.frontend.ast.FunctionDecl;
import org.abs_models.frontend.ast.Import;
import org.abs_models.frontend.ast.InterfaceDecl;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.ast.ModuleDecl;
import org.abs_models.frontend.ast.Name;
import org.abs_models.frontend.ast.NamedExport;
import org.abs_models.frontend.ast.NamedImport;
import org.abs_models.frontend.ast.Opt;
import org.abs_models.frontend.ast.ParametricDataTypeDecl;
import org.abs_models.frontend.ast.PartialFunctionDecl;
import org.abs_models.frontend.ast.StarExport;
import org.abs_models.frontend.ast.StarImport;
import org.abs_models.frontend.ast.TypeParameterDecl;
import org.abs_models.frontend.ast.TypeSynDecl;
import org.abs_models.xtext.abs.AbsPackage;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.generator.trace.ILocationData;
import org.eclipse.xtext.nodemodel.INode;
import org.eclipse.xtext.nodemodel.util.NodeModelUtils;
import org.eclipse.xtext.resource.ILocationInFileProvider;
import org.eclipse.xtext.resource.XtextResourceSet;
import org.eclipse.xtext.util.ITextRegion;
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
        // result.setAnnotationList(fromXtext(xtext_decl.getAnnotations()));
        for (org.abs_models.xtext.abs.DataConstructorDecl xtext_d : xtext_decl.getConstructors()) {
            // result.addDataConstructor(DataConstructor.fromXtext(xtext_d));
        }
        return result;
    }

    static TypeSynDecl fromXtext(org.abs_models.xtext.abs.TypeSynonymDecl xtext_decl) {
        TypeSynDecl result = new TypeSynDecl();
        result.setName(xtext_decl.getName());
        return result;
    }

    static ExceptionDecl fromXtext(org.abs_models.xtext.abs.ExceptionDecl xtext_decl) {
        ExceptionDecl result = new  ExceptionDecl();
        result.setName(xtext_decl.getName());
        return result;
    }

    static FunctionDecl fromXtext(org.abs_models.xtext.abs.FunctionDecl xtext_decl) {
        FunctionDecl result = new  FunctionDecl();
        result.setName(xtext_decl.getName());
        return result;
    }

    static PartialFunctionDecl fromXtext(org.abs_models.xtext.abs.PartialFunctionDecl xtext_decl) {
        PartialFunctionDecl result = new  PartialFunctionDecl();
        result.setName(xtext_decl.getName());
        return result;
    }

    static InterfaceDecl fromXtext(org.abs_models.xtext.abs.InterfaceDecl xtext_decl) {
        InterfaceDecl result = new  InterfaceDecl();
        result.setName(xtext_decl.getName());
        return result;
    }

    static ClassDecl fromXtext(org.abs_models.xtext.abs.ClassDecl xtext_decl) {
        ClassDecl result = new  ClassDecl();
        result.setName(xtext_decl.getName());
        return result;
    }

}
