package org.abs_models.frontend.parser;

import org.abs_models.common.NotImplementedYetException;
import org.abs_models.frontend.ast.*;
import org.abs_models.frontend.ast.AndGuard;
import org.abs_models.frontend.ast.Annotation;
import org.abs_models.frontend.ast.Block;
import org.abs_models.frontend.ast.CompilationUnit;
import org.abs_models.frontend.ast.ConstructorPattern;
import org.abs_models.frontend.ast.DurationGuard;
import org.abs_models.frontend.ast.Feature;
import org.abs_models.frontend.ast.FloatLiteral;
import org.abs_models.frontend.ast.Guard;
import org.abs_models.frontend.ast.IntLiteral;
import org.abs_models.frontend.ast.Pattern;
import org.abs_models.frontend.ast.StringLiteral;
import org.abs_models.frontend.ast.TypeUse;
import org.abs_models.frontend.typechecker.InterfaceType;
import org.abs_models.xtext.abs.*;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.nodemodel.INode;
import org.eclipse.xtext.nodemodel.util.NodeModelUtils;
import org.eclipse.xtext.resource.ILocationInFileProvider;
import org.eclipse.xtext.resource.IResourceDescriptions;
import org.eclipse.xtext.resource.XtextResourceSet;
import org.eclipse.xtext.resource.impl.ResourceDescriptionsProvider;
import org.eclipse.xtext.util.ITextRegionWithLineInformation;
import org.eclipse.xtext.util.LineAndColumn;

import java.util.ArrayList;
//import java.util.*;

// The methods in this class are roughly in the same order as the grammar rule
// / class definitions in Abs.xtext
public class XtextToJastAdd {

    private static ILocationInFileProvider location_provider = Main.absinjector.getInstance(ILocationInFileProvider.class);
    private static ResourceDescriptionsProvider rdp = Main.absinjector.getInstance(ResourceDescriptionsProvider.class);

    private static <T extends ASTNode<?>> T nodeWithLocation(final T node, final EObject obj) {
        final INode n = NodeModelUtils.findActualNodeFor(obj); // do we want .getNode() instead?
        if (n != null) {
            final ITextRegionWithLineInformation location = (ITextRegionWithLineInformation)location_provider.getFullTextRegion(obj);
            // End location is untested since we only print beginning
            // locations in error output
            final LineAndColumn beg = NodeModelUtils.getLineAndColumn(n, location.getOffset());
            final LineAndColumn end = NodeModelUtils.getLineAndColumn(n, location.getOffset() + location.getLength());
            // Xtext column values are 1-based, we want 0-based
            node.setPosition(beg.getLine(), beg.getColumn() - 1,
                             end.getLine(), end.getColumn() - 1);
            String filename = obj.eResource().getURI().toFileString();
            if (filename == null) filename = obj.eResource().getURI().toPlatformString(false);
            node.setFileName(filename);
        }
        return node;
    }

    private static <T extends ASTNode<?>> T nodeWithLocation(final T node, final EObject obj, final EStructuralFeature feature) {
        // FIXME: there’s a constant defined somewhere deep in xtext for this
        // magic 0 value -- INSIGNIFICANT_INDEX or similar name
        return nodeWithLocation(node, obj, feature, 0);
    }


    private static <T extends ASTNode<?>> T nodeWithLocation(final T node, final EObject obj, final EStructuralFeature feature, final int indexInList) {
        final INode n = NodeModelUtils.findActualNodeFor(obj); // do we want .getNode() instead?
        if (n != null) {
            final ITextRegionWithLineInformation location = (ITextRegionWithLineInformation)location_provider.getFullTextRegion(obj, feature, indexInList);
            // End location is untested since we only print beginning
            // locations in error output
            final LineAndColumn beg = NodeModelUtils.getLineAndColumn(n, location.getOffset());
            final LineAndColumn end = NodeModelUtils.getLineAndColumn(n, location.getOffset() + location.getLength());
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
     * Calculate the JastAdd-compliant name.  At this point, we only have
     * references, but JastAdd wants either "A" or "M.A" depending on how we
     * imported A into our current module.
     *
     * How to do this?  Our location is either inside a module or inside a
     * delta.  If we’re in a delta without ‘uses’ clause, use the qualified
     * name.  If we’re in a module (or delta with ‘uses’ clause), check if we
     * have an ‘import’ clause giving the fully-qualified name; if yes, use
     * it.  If no, use the simple name.
     *
     * @param location the context in which we want to resolve the reference
     * @param reference the object for which we want to generate a name
     * @param simpleName the simple name of the reference
     * @return the qualified name of reference if reference is imported by its qualified name; simpleName otherwise.
     */
    private static String accessibleName(EObject location, EObject reference, String simpleName) {
        final IResourceDescriptions index = rdp.getResourceDescriptions(location.eResource());
        // Assume that reference is in the index -- true for everything we
        // want to generate a qualified name for.
        final String qualifiedName = index.getExportedObjectsByObject(reference).iterator().next().getName().toString();
        ModuleDeclaration decl = EcoreUtil2.getContainerOfType(location, ModuleDeclaration.class);
        if (decl == null) {
            DeltaDeclaration delta = EcoreUtil2.getContainerOfType(location, DeltaDeclaration.class);
            if (delta.getUsedModulename() == null) {
                return qualifiedName;
            } else {
                // Assume that the validator has complained about a non-existent
                // module already if there’s a typo in the ‘uses’ clause
                decl = (ModuleDeclaration)
                    index.getExportedObjects(AbsPackage.eINSTANCE.getModuleDeclaration(),
                                             QualifiedName.create(delta.getUsedModulename().split("\\.")), false)
                    .iterator().next().getEObjectOrProxy();
            }
        }
        if (qualifiedName.startsWith(decl.getName())) {
            // reference is defined in the current module
            return simpleName;
        } else {
            for (final ModuleImport imp : decl.getImports()) {
                if (imp.getModulename() == null
                    && imp.getIdentifiers().contains(qualifiedName)) {
                    // reference was imported via qualifed name: use that
                    return qualifiedName;
                }
            }
            // use simple name
            return simpleName;
        }
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
    public static Model fromResourceSet(final XtextResourceSet resourceSet) {
        final Model result = new Model();
        for (final Resource r : resourceSet.getResources()) {
            for (final EObject unit : r.getContents()) {
                final CompilationUnit jastadd_unit = fromXtext((org.abs_models.xtext.abs.CompilationUnit) unit);
                new ASTPreProcessor().preprocess(jastadd_unit);
                result.addCompilationUnitNoTransform(jastadd_unit);
            }
        }
        return result;
    }

    static CompilationUnit fromXtext(final org.abs_models.xtext.abs.CompilationUnit xtext_unit) {
        final CompilationUnit result = new CompilationUnit();
        result.setName(xtext_unit.eResource().getURI().toFileString());
        for (final org.abs_models.xtext.abs.ModuleDeclaration module : xtext_unit.getModules()) {
            result.addModuleDeclNoTransform(fromXtext(module));
        }
        for (final org.abs_models.xtext.abs.DeltaDeclaration delta : xtext_unit.getDeltas()) {
            result.addDeltaDeclNoTransform(fromXtext(delta));
        }
        if (xtext_unit.getProductline() != null) {
            result.setProductLine(fromXtext(xtext_unit.getProductline()));
        }
        for (final org.abs_models.xtext.abs.ProductDeclaration product : xtext_unit.getProducts()) {
            result.addProductDeclNoTransform(fromXtext(product));
        }
        for (final org.abs_models.xtext.abs.MTVLFeatureRoot f : xtext_unit.getRootFeatures()) {
            result.addFeatureDeclNoTransform(fromXtext(f));
        }
        for (final org.abs_models.xtext.abs.MTVLFeatureExtension e : xtext_unit.getFeatureExtensions()) {
            result.addFExtNoTransform(fromXtext(e));
        }
        return nodeWithLocation(result, xtext_unit);
    }

    static ModuleDecl fromXtext(final org.abs_models.xtext.abs.ModuleDeclaration xtext_module) {
        final ModuleDecl result = new ModuleDecl();
        result.setName(xtext_module.getName());
        for (final org.abs_models.xtext.abs.ModuleExport export : xtext_module.getExports()) {
            result.addExportNoTransform(fromXtext(export));
        }
        for (final org.abs_models.xtext.abs.ModuleImport imp : xtext_module.getImports()) {
            result.addImportNoTransform(fromXtext(imp));
        }
        for (final org.abs_models.xtext.abs.Declaration decl : xtext_module.getDeclarations()) {
            result.addDeclNoTransform(fromXtext(decl));
        }

        if (xtext_module.getProductLineDecl() != null){
             result.setProductLine(fromXtext(xtext_module.getProductLineDecl()));
        }

         for(org.abs_models.xtext.abs.LocalProductDeclaration product : xtext_module.getLocalProductDecls()) {
        	result.addLocalProductDeclNoTransform(fromXtext(product));
        }

        if(xtext_module.isMain()) {
            final List<Stmt> statements = new List<>();
            for(final org.abs_models.xtext.abs.Statement stmt : xtext_module.getMainblockStatements()) {
                statements.add(fromXtext(stmt));
            }

            final MainBlock mainBlock = new MainBlock();
            mainBlock.setStmtList(statements);
            mainBlock.setAnnotationList(annotationsfromXtext(xtext_module.getMainblockAnnotations()));
            result.setBlock(mainBlock);
        }
        return nodeWithLocation(result, xtext_module);
    }

    private static Export fromXtext(final org.abs_models.xtext.abs.ModuleExport xtext_export) {
        Export result;
        if (xtext_export.isStar()) {
            // "export *;"
            // "export * from OtherModule;"
            final StarExport se = new StarExport();
            result = se;
            if (xtext_export.getModulename() != null)
                se.setModuleName(nodeWithLocation(new Name(xtext_export.getModulename()), xtext_export, AbsPackage.eINSTANCE.getModuleExport_Modulename()));
        } else if (xtext_export.getModulename() != null) {
            // "export a, b from OtherModule;"
            final FromExport fe = new FromExport();
            result = fe;
            fe.setModuleName(xtext_export.getModulename());
            for (int i = 0; i < xtext_export.getIdentifiers().size(); i++) {
                final String id = xtext_export.getIdentifiers().get(i);
                fe.addNameNoTransform(nodeWithLocation(new Name(id), xtext_export, AbsPackage.eINSTANCE.getModuleExport_Identifiers(), i));
            }
        } else {
            // "export a, b;"
            final NamedExport ne = new NamedExport();
            result = ne;
            for (int i = 0; i < xtext_export.getIdentifiers().size(); i++) {
                final String id = xtext_export.getIdentifiers().get(i);
                ne.addNameNoTransform(nodeWithLocation(new Name(id), xtext_export, AbsPackage.eINSTANCE.getModuleExport_Identifiers(), i));
            }
        }
        return nodeWithLocation(result, xtext_export);
    }

    private static Import fromXtext(final org.abs_models.xtext.abs.ModuleImport xtext_import) {
        Import result;
        if (xtext_import.isStar()) {
            // "import * from OtherModule;"
            final StarImport si = new StarImport(xtext_import.getModulename());
            result = si;
        } else if (xtext_import.getModulename() != null) {
            // "import a, b from OtherModule;"
            final FromImport fi = new FromImport();
            result = fi;
            fi.setModuleName(xtext_import.getModulename());
            for (int i = 0; i < xtext_import.getIdentifiers().size(); i++) {
                final String id = xtext_import.getIdentifiers().get(i);
                fi.addNameNoTransform(nodeWithLocation(new Name(id), xtext_import, AbsPackage.eINSTANCE.getModuleImport_Identifiers(), i));
            }
        } else {
            // "import OtherModule.a, OtherModule.b;"
            final NamedImport ni = new NamedImport();
            result = ni;
            for (int i = 0; i < xtext_import.getIdentifiers().size(); i++) {
                final String id = xtext_import.getIdentifiers().get(i);
                ni.addNameNoTransform(nodeWithLocation(new Name(id), xtext_import, AbsPackage.eINSTANCE.getModuleImport_Identifiers(), i));
            }
        }
        return nodeWithLocation(result, xtext_import);
    }

    private static List<Annotation> annotationsfromXtext(final org.abs_models.xtext.abs.Annotations annotations) {
        final List<Annotation> annotationList = new List<>();
        for(final org.abs_models.xtext.abs.Annotation annotation : annotations.getAnnotations()) {
            Annotation astAnnotation;
            final PureExp exp = pureExpFromXtext(annotation.getValue());
            if (annotation.getId() != null) {
                astAnnotation = new TypedAnnotation(exp, nodeWithLocation(new UnresolvedTypeUse(annotation.getId(), new List<>()), AbsPackage.eINSTANCE.getAnnotation_Id()));
            } else {
                astAnnotation = new Annotation(exp);
            }
            annotationList.add(nodeWithLocation(astAnnotation, annotation));
        }
        return annotationList;
    }

    static Decl fromXtext(final org.abs_models.xtext.abs.Declaration xtext_decl) {
        Decl result = null;
        if (xtext_decl.getDatatypeDeclaration() != null) {
            result = fromXtext(xtext_decl.getDatatypeDeclaration());
            ((DataTypeDecl)result).setAnnotationList(annotationsfromXtext(xtext_decl.getAnnotations()));
        } else if (xtext_decl.getTypesynonymDeclaration() != null) {
            result = fromXtext(xtext_decl.getTypesynonymDeclaration());
            ((TypeSynDecl)result).setAnnotationList(annotationsfromXtext(xtext_decl.getAnnotations()));
        } else if (xtext_decl.getExceptionDeclaration() != null) {
            result = fromXtext(xtext_decl.getExceptionDeclaration());
            ((ExceptionDecl)result).setAnnotationList(annotationsfromXtext(xtext_decl.getAnnotations()));
        } else if (xtext_decl.getFunctionDeclaration() != null) {
            result = fromXtext(xtext_decl.getFunctionDeclaration());
            if (result instanceof PartialFunctionDecl) {
                ((PartialFunctionDecl)result).setAnnotationList(annotationsfromXtext(xtext_decl.getAnnotations()));
            } else {
                ((FunctionDecl)result).setAnnotationList(annotationsfromXtext(xtext_decl.getAnnotations()));
            }
        } else if (xtext_decl.getTraitDeclaration() != null) {
            result = fromXtext(xtext_decl.getTraitDeclaration());
            ((TraitDecl)result).setAnnotationList(annotationsfromXtext(xtext_decl.getAnnotations()));
        } else if (xtext_decl.getInterfaceDeclaration() != null) {
            result = fromXtext(xtext_decl.getInterfaceDeclaration());
            ((InterfaceDecl)result).setAnnotationList(annotationsfromXtext(xtext_decl.getAnnotations()));
        } else if (xtext_decl.getClassDeclaration() != null) {
            result = fromXtext(xtext_decl.getClassDeclaration());
            ((ClassDecl)result).setAnnotationList(annotationsfromXtext(xtext_decl.getAnnotations()));
        } else {
            throw new NotImplementedYetException(new ASTNode(),
                                                 "No conversion to JastAdd implemented for Xtext declaration node "
                                                 + xtext_decl.getClass().toString());
        }
        return result;
    }

    static DataTypeDecl fromXtext(final org.abs_models.xtext.abs.DataTypeDeclaration xtext_decl) {
        DataTypeDecl result;
        if (xtext_decl.getTypeParameters().isEmpty()) {
            result = new DataTypeDecl();
        } else {
            result = new ParametricDataTypeDecl();
            for (int i = 0; i < xtext_decl.getTypeParameters().size(); i++) {
                final String tp = xtext_decl.getTypeParameters().get(i);
                ((ParametricDataTypeDecl)result).addTypeParameterNoTransform(nodeWithLocation(new TypeParameterDecl(tp), xtext_decl, AbsPackage.eINSTANCE.getDataTypeDeclaration_TypeParameters(), i));
            }
        }
        result.setName(xtext_decl.getName());

        for (final org.abs_models.xtext.abs.DataConstructorDeclaration xtext_d : xtext_decl.getConstructors()) {
            final DataConstructor constructor = new DataConstructor();
            constructor.setName(xtext_d.getName());

            for (final DataConstructorParameter arg : xtext_d.getArguments()) {
                constructor.addConstructorArgNoTransform(fromXtext(arg));
            }

            result.addDataConstructor(nodeWithLocation(constructor, xtext_d));
        }
        return nodeWithLocation(result, xtext_decl);
    }

    private static ConstructorArg fromXtext(final DataConstructorParameter xtext_arg) {
        final ConstructorArg constructorArg = new ConstructorArg();
        if(xtext_arg.getName() != null) {
            constructorArg.setSelectorName(nodeWithLocation(new Name(xtext_arg.getName()), xtext_arg, AbsPackage.eINSTANCE.getDataConstructorParameter_Name()));
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
        final TypeUse tu = fromXtext(xtext_arg.getType());
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

    static TypeSynDecl fromXtext(final org.abs_models.xtext.abs.TypesynonymDeclaration xtext_decl) {
        final TypeSynDecl result = new TypeSynDecl();
        result.setName(xtext_decl.getName());
        result.setValue(fromXtext(xtext_decl.getType()));
        return nodeWithLocation(result, xtext_decl);
    }

    static ExceptionDecl fromXtext(final org.abs_models.xtext.abs.ExceptionDeclaration xtext_decl) {
        final ExceptionConstructor constructor = new ExceptionConstructor();
        constructor.setName(xtext_decl.getName());
        for (final DataConstructorParameter arg : xtext_decl.getArguments()) {
            constructor.addConstructorArgNoTransform(fromXtext(arg));
        }

        final ExceptionDecl result = new ExceptionDecl();
        result.setName(xtext_decl.getName());
        result.setDataConstructorList(new List<DataConstructor>(constructor));
        return nodeWithLocation(result, xtext_decl);
    }

    static Decl fromXtext(final org.abs_models.xtext.abs.FunctionDeclaration xtext_decl) {
        // This parses FunctionDecl and PartialFunctionDecl; these are
        // unfortunately distinct classes in JastAdd, so we return type Decl
        // instead.
        if (xtext_decl.getFunctionArguments().size() > 0) {
            PartialFunctionDecl result;
            if (!xtext_decl.getTypeParameters().isEmpty()) {
                final ParametricPartialFunctionDecl presult = new ParametricPartialFunctionDecl();
                result = presult;
                for (int i = 0; i < xtext_decl.getTypeParameters().size(); i++) {
                    final String tp = xtext_decl.getTypeParameters().get(i);
                    presult.addTypeParameterNoTransform(nodeWithLocation(new TypeParameterDecl(tp), xtext_decl, AbsPackage.eINSTANCE.getFunctionDeclaration_TypeParameters(), i));
                }
            } else {
                result = new  PartialFunctionDecl();
            }
            result.setName(xtext_decl.getName());
            result.setTypeUse(fromXtext(xtext_decl.getResulttype()));

            for(final org.abs_models.xtext.abs.Parameter arg : xtext_decl.getArguments()) {
                result.addParamNoTransform(fromXtext(arg));
            }

            for (int i = 0; i < xtext_decl.getFunctionArguments().size(); i++) {
                final String fArg = xtext_decl.getFunctionArguments().get(i);
                result.addFuncParamNoTransform(nodeWithLocation(new FunctionParamDecl(fArg), xtext_decl, AbsPackage.eINSTANCE.getFunctionDeclaration_FunctionArguments(), i));
            }

            final PartialFunctionDef functionDef = new PartialFunctionDef(pureExpFromXtext(xtext_decl.getBody()));
            result.setPartialFunctionDef(nodeWithLocation(functionDef, xtext_decl.getBody()));

            return nodeWithLocation(result, xtext_decl);
        } else {
            FunctionDecl result;
            if (!xtext_decl.getTypeParameters().isEmpty()) {
                final ParametricFunctionDecl presult = new ParametricFunctionDecl();
                result = presult;
                for (int i = 0; i < xtext_decl.getTypeParameters().size(); i++) {
                    final String tp = xtext_decl.getTypeParameters().get(i);
                    presult.addTypeParameterNoTransform(nodeWithLocation(new TypeParameterDecl(tp), xtext_decl, AbsPackage.eINSTANCE.getFunctionDeclaration_TypeParameters(), i));
                }
            } else {
                result = new  FunctionDecl();
            }
            result.setName(xtext_decl.getName());
            result.setTypeUse(fromXtext(xtext_decl.getResulttype()));

            for(final org.abs_models.xtext.abs.Parameter arg : xtext_decl.getArguments()) {
                result.addParamNoTransform(fromXtext(arg));
            }

            if (xtext_decl.isBuiltin()) {
                final BuiltinFunctionDef bd = new BuiltinFunctionDef();
                for (int i = 0; i < xtext_decl.getBuiltinArguments().size(); i++) {
                    final String barg = xtext_decl.getBuiltinArguments().get(i);
                    bd.addStringArgNoTransform(nodeWithLocation(new StringLiteral(ASTPreProcessor.preprocessStringLiteral(barg)), xtext_decl, AbsPackage.eINSTANCE.getFunctionDeclaration_BuiltinArguments(), i));
                }
                result.setFunctionDef(nodeWithLocation(bd, xtext_decl, AbsPackage.eINSTANCE.getFunctionDeclaration_Builtin()));
            } else {
                final PureExp exp = pureExpFromXtext(xtext_decl.getBody());
                result.setFunctionDef(nodeWithLocation(new ExpFunctionDef(exp), xtext_decl.getBody()));
            }
            return nodeWithLocation(result, xtext_decl);
        }
    }

    static TraitDecl fromXtext(final org.abs_models.xtext.abs.TraitDeclaration xtext_decl) {
        final TraitDecl result = new TraitDecl();
        result.setName(xtext_decl.getName());
        result.setTraitExpr(fromXtext(xtext_decl.getTraitExpression()));
        return nodeWithLocation(result, xtext_decl);
    }

    static TraitExpr fromXtext(final org.abs_models.xtext.abs.TraitExpression xtext_exp) {
        TraitExpr result = fromXtext(xtext_exp.getBasicExpression());
        for (final org.abs_models.xtext.abs.TraitOperation op : xtext_exp.getTraitOperations()) {
            result = new TraitModifyExpr(result, fromXtext(op));
        }
        return nodeWithLocation(result, xtext_exp);
    }

    static TraitExpr fromXtext(final org.abs_models.xtext.abs.BasicTraitExpression xtext_exp) {
        TraitExpr result = null;
        if (xtext_exp.isMethodSet() || xtext_exp.getMethods().size() > 0) {
            final TraitSetExpr fresult = new TraitSetExpr();
            for (final org.abs_models.xtext.abs.MethodDeclaration m : xtext_exp.getMethods()) {
                fresult.addMethodImplNoTransform(fromXtext(m));
            }
            result = fresult;
        } else {
            result = new TraitNameExpr(xtext_exp.getRef());
        }
        return nodeWithLocation(result, xtext_exp);
    }

    static MethodModifier fromXtext(final org.abs_models.xtext.abs.TraitOperation xtext_exp) {
        MethodModifier result = null;
        if (xtext_exp.isRemoveMethodModifier() || xtext_exp.getRemovedSignatures().size() > 0) {
            final List<MethodSig> l = new List<>();
            for (final org.abs_models.xtext.abs.MethodSignature s : xtext_exp.getRemovedSignatures()) {
                l.add(fromXtext(s));
            }
            result = new RemoveMethodModifier(l);
        } else if (xtext_exp.getAddedExpression() != null) {
            result = new AddMethodModifier(fromXtext(xtext_exp.getAddedExpression()));
        } else {
            result = new ModifyMethodModifier(fromXtext(xtext_exp.getModifiedExpression()));
        }
        return nodeWithLocation(result, xtext_exp);
    }

    static InterfaceDecl fromXtext(final org.abs_models.xtext.abs.InterfaceDeclaration xtext_decl) {
        final InterfaceDecl result = new  InterfaceDecl();
        result.setName(xtext_decl.getName());

        for (int i = 0; i < xtext_decl.getSuperinterfaces().size(); i++) {
            result.addExtendedInterfaceUseNoTransform(nodeWithLocation(fromXtext(xtext_decl.getSuperinterfaces().get(i)), xtext_decl, AbsPackage.eINSTANCE.getInterfaceDeclaration_Superinterfaces(), i));
        }

        for (final MethodSignature ms : xtext_decl.getMethods()) {
            result.addBodyNoTransform(fromXtext(ms));
        }
        
        result.setUnique(xtext_decl.isUnique());
        
        return nodeWithLocation(result, xtext_decl);
    }

    private static MethodSig fromXtext(final MethodSignature xtext_decl) {
        final MethodSig result = new MethodSig();
        result.setName(xtext_decl.getName());
        result.setAnnotationList(annotationsfromXtext(xtext_decl.getAnnotations()));
        for(final org.abs_models.xtext.abs.Parameter arg : xtext_decl.getArguments()) {
            result.addParamNoTransform(fromXtext(arg));
        }
        result.setReturnType(fromXtext(xtext_decl.getResulttype()));
        return nodeWithLocation(result, xtext_decl);
    }

    static ClassDecl fromXtext(final org.abs_models.xtext.abs.ClassDeclaration xtext_decl) {
        final ClassDecl result = new  ClassDecl();
        result.setName(xtext_decl.getName());

        result.setUnique(xtext_decl.isUnique());

        for(org.abs_models.xtext.abs.Parameter arg : xtext_decl.getArguments()) {
            result.addParamNoTransform(fromXtext(arg));
        }

        for (int i = 0; i < xtext_decl.getInterfaces().size(); i++) {
            String iname = xtext_decl.getInterfaces().get(i).getName();
            InterfaceTypeUse it = fromXtext(xtext_decl.getInterfaces().get(i));
            result.addImplementedInterfaceUseNoTransform(nodeWithLocation(it, xtext_decl, AbsPackage.eINSTANCE.getClassDeclaration_Interfaces(), i));
        }

        for(final org.abs_models.xtext.abs.FieldDeclaration fieldDecl : xtext_decl.getFields()) {
            result.addFieldNoTransform(fromXtext(fieldDecl));
        }

        // TODO treat the case of an empty init block
        if (xtext_decl.getInitblockStatements().size() > 0) {
            final InitBlock astInitBlock = new InitBlock();
            for(final org.abs_models.xtext.abs.Statement statement : xtext_decl.getInitblockStatements()) {
                astInitBlock.addStmt(fromXtext(statement));
            }
            result.setInitBlock(astInitBlock);
        }

        for(final MethodDeclaration methodDecl : xtext_decl.getMethods()) {
            result.addMethodNoTransform(fromXtext(methodDecl));
        }

        for (final SwitchStatementBranch recover_branch : xtext_decl.getRecoverbranches()) {
            result.addRecoverBranchNoTransform(fromXtext(recover_branch));
        }
        for (final org.abs_models.xtext.abs.TraitExpression trait_exp : xtext_decl.getUsedTraits()) {
            result.addTraitUseNoTransform(new TraitUse(fromXtext(trait_exp)));
        }

        return nodeWithLocation(result, xtext_decl);
    }

    private static FieldDecl fromXtext(final org.abs_models.xtext.abs.FieldDeclaration xtext_decl) {
        final FieldDecl result = new FieldDecl();
        result.setName(xtext_decl.getName());
        result.setAnnotationList(annotationsfromXtext(xtext_decl.getAnnotations()));
        if (xtext_decl.getInit() != null) {
            result.setInitExp(pureExpFromXtext(xtext_decl.getInit()));
        }
        result.setAccess(fromXtext(xtext_decl.getType()));
        return nodeWithLocation(result, xtext_decl);
    }

    private static MethodImpl fromXtext(final org.abs_models.xtext.abs.MethodDeclaration xtext_decl) {
        // Initialize position here already so we can use it for the embedded
        // MethodSig
        final MethodImpl result = nodeWithLocation(new MethodImpl(), xtext_decl);

        final MethodSig sig = new MethodSig();
        sig.setName(xtext_decl.getName());
        sig.setAnnotationList(annotationsfromXtext(xtext_decl.getAnnotations()));
        for(final org.abs_models.xtext.abs.Parameter arg : xtext_decl.getArguments()) {
            sig.addParamNoTransform(fromXtext(arg));
        }
        sig.setReturnType(fromXtext(xtext_decl.getResulttype()));
        // Handle position of embedded MethodSig: first set position to
        // closing ")", then adjust start position to start of MethodDecl
        nodeWithLocation(sig, xtext_decl, AbsPackage.eINSTANCE.getMethodDeclaration_SigEndPosition());
        sig.setPosition(result.getStartLine(), result.getStartColumn(),
                        sig.getEndLine(), sig.getEndColumn());
        result.setMethodSig(sig);

        final Block block = new Block();
        for(final org.abs_models.xtext.abs.Statement stmt : xtext_decl.getStatements()) {
            block.addStmtNoTransform(fromXtext(stmt));
        }
        result.setBlock(block);

        return nodeWithLocation(result, xtext_decl);
    }

    private static ParamDecl fromXtext(final org.abs_models.xtext.abs.Parameter xtext_decl) {
        final ParamDecl result = new ParamDecl();
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
    private static Block blockFromXtext(final org.abs_models.xtext.abs.Statement stmt) {
        final Stmt result = fromXtext(stmt);
        if (result instanceof Block) {
            return (Block)result;
        } else {
            final Block block = new Block();
            block.addStmtNoTransform(result);
            return nodeWithLocation(block, stmt);
        }
    }

    private static Stmt fromXtext(final org.abs_models.xtext.abs.Statement stmt) {
        Stmt result = null;

        if(stmt instanceof org.abs_models.xtext.abs.VariableDeclarationStatement) {
            final org.abs_models.xtext.abs.VariableDeclarationStatement value = (org.abs_models.xtext.abs.VariableDeclarationStatement) stmt;
            final List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());
            final VarDecl varDecl = new VarDecl();
            varDecl.setName(value.getVariablename());
            varDecl.setAccess(fromXtext(value.getType()));
            if (value.getInit() != null) {
                varDecl.setInitExp(fromXtext(value.getInit()));
            }
            result = new VarDeclStmt(annotations, varDecl);
        }
        else if(stmt instanceof org.abs_models.xtext.abs.AssignStatement) {
            final org.abs_models.xtext.abs.AssignStatement value = (org.abs_models.xtext.abs.AssignStatement) stmt;
            final List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());
            final org.abs_models.xtext.abs.Expression lhsExp = value.getLhs();
            VarOrFieldUse varOrFieldUse;
            if(lhsExp instanceof VariableOrFieldExpression) {
                final VariableOrFieldExpression lhs = (VariableOrFieldExpression) lhsExp;
                if (lhs.isField()) {
                    varOrFieldUse = new FieldUse(lhs.getVariablename());
                } else {
                    // might still get rewritten to FieldUse by JastAdd
                    // TODO: make this more precise once scoping / linking is
                    // implemented
                    varOrFieldUse = new VarUse(lhs.getVariablename());
                }
            } else {
                assert false : "Invalid left-hand side expression in Xtext AST reached XtextToJastAdd -- check validation rules";
                varOrFieldUse = null;
            }
            final Exp expresssion = fromXtext(value.getExpression());
            result = new AssignStmt(annotations, varOrFieldUse, expresssion);
        }
        else if(stmt instanceof org.abs_models.xtext.abs.SkipStatement) {
            final org.abs_models.xtext.abs.SkipStatement value = (org.abs_models.xtext.abs.SkipStatement) stmt;
            final List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());
            result = new SkipStmt(annotations);
        }
        else if(stmt instanceof org.abs_models.xtext.abs.ReturnStatement) {
            final org.abs_models.xtext.abs.ReturnStatement value = (org.abs_models.xtext.abs.ReturnStatement) stmt;
            final List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());

            result = new ReturnStmt(annotations, fromXtext(value.getExpression()));
        }
        else if(stmt instanceof org.abs_models.xtext.abs.AssertStatement) {
            final org.abs_models.xtext.abs.AssertStatement value = (org.abs_models.xtext.abs.AssertStatement) stmt;
            final List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());

            result = new AssertStmt(annotations, pureExpFromXtext(value.getExpression()));
        }
        else if(stmt instanceof org.abs_models.xtext.abs.Block) {
            final org.abs_models.xtext.abs.Block value = (org.abs_models.xtext.abs.Block) stmt;
            final Block block = new Block();
            final List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());

            for(final org.abs_models.xtext.abs.Statement subStmt : value.getStatements()) {
                block.addStmtNoTransform(fromXtext(subStmt));
            }
            block.setAnnotationList(annotations);
            result = block;
        }
        else if(stmt instanceof org.abs_models.xtext.abs.IfStatement) {
            final org.abs_models.xtext.abs.IfStatement value = (org.abs_models.xtext.abs.IfStatement) stmt;
            final IfStmt ifstmt = new IfStmt();
            ifstmt.setAnnotationList(annotationsfromXtext(value.getAnnotations()));
            ifstmt.setCondition(pureExpFromXtext(value.getCondition()));
            ifstmt.setThen(blockFromXtext(value.getConsequence()));
            if (value.getAlternate() != null) {
                ifstmt.setElse(blockFromXtext(value.getAlternate()));
            }
            result = ifstmt;
        }
        else if(stmt instanceof org.abs_models.xtext.abs.WhileStatement) {
            final org.abs_models.xtext.abs.WhileStatement value = (org.abs_models.xtext.abs.WhileStatement) stmt;
            final List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());
            final PureExp condition = pureExpFromXtext(value.getCondition());
            final Block body = blockFromXtext(value.getBody());
            result = new WhileStmt(annotations, condition, body);
        }
        else if(stmt instanceof org.abs_models.xtext.abs.ForeachStatement) {
            final org.abs_models.xtext.abs.ForeachStatement value = (org.abs_models.xtext.abs.ForeachStatement) stmt;
            final List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());
            final LoopVarDecl loopvar = new LoopVarDecl(value.getLoopvar());
            final Opt<LoopVarDecl> indexvar = value.getIndexvar() == null ? new Opt<>() : new Opt<>(new LoopVarDecl(value.getIndexvar()));
            final PureExp list = pureExpFromXtext(value.getList());
            final Block body = blockFromXtext(value.getBody());
            result = new ForeachStmt(annotations, loopvar, indexvar, list, body);
        }
        else if(stmt instanceof org.abs_models.xtext.abs.TryCatchFinallyStatement) {
            final org.abs_models.xtext.abs.TryCatchFinallyStatement value = (org.abs_models.xtext.abs.TryCatchFinallyStatement) stmt;
            final List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());
            final Block body = blockFromXtext(value.getBody());
            final List<CaseBranchStmt> branches = caseBranchStmtsFromXtext(value.getBranches());
            final Opt<Block> finallyOpt = new Opt<>();
            if (value.getFinally() != null) {
                final Block finallyBlock = blockFromXtext(value.getFinally());
                finallyOpt.setChild(finallyBlock, 0);
            }
            result = new TryCatchFinallyStmt(annotations, body, branches, finallyOpt);
        }
        else if(stmt instanceof org.abs_models.xtext.abs.SuspendStatement) {
            final org.abs_models.xtext.abs.SuspendStatement value = (org.abs_models.xtext.abs.SuspendStatement) stmt;
            final List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());
            result = new SuspendStmt(annotations);
        }
        else if(stmt instanceof org.abs_models.xtext.abs.DurationStatement) {
            final org.abs_models.xtext.abs.DurationStatement value = (org.abs_models.xtext.abs.DurationStatement) stmt;
            final List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());
            final PureExp min = pureExpFromXtext(value.getMin());
            final PureExp max = pureExpFromXtext(value.getMax());
            result = new DurationStmt(annotations, min, max);
        }
        else if(stmt instanceof org.abs_models.xtext.abs.ThrowStatement) {
            final org.abs_models.xtext.abs.ThrowStatement value = (org.abs_models.xtext.abs.ThrowStatement) stmt;
            final List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());
            final PureExp exception = pureExpFromXtext(value.getException());
            result = new ThrowStmt(annotations, exception);
        }
        else if(stmt instanceof org.abs_models.xtext.abs.DieStatement) {
            final org.abs_models.xtext.abs.DieStatement value = (org.abs_models.xtext.abs.DieStatement) stmt;
            final List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());
            final PureExp exception = pureExpFromXtext(value.getException());
            result = new DieStmt(annotations, exception);
        }
        else if(stmt instanceof org.abs_models.xtext.abs.MoveCogToStatement) {
            final org.abs_models.xtext.abs.MoveCogToStatement value = (org.abs_models.xtext.abs.MoveCogToStatement) stmt;
            final List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());
            final PureExp target = pureExpFromXtext(value.getTarget());
            result = new MoveCogToStmt(annotations, target);
        }
        else if(stmt instanceof org.abs_models.xtext.abs.ExpressionStatement) {
            final org.abs_models.xtext.abs.ExpressionStatement value = (org.abs_models.xtext.abs.ExpressionStatement) stmt;
            final org.abs_models.xtext.abs.Expression xtextExp = value.getExpression();
            final List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());
            if (xtextExp instanceof AwaitExpression
                && (!(((AwaitExpression)xtextExp).getGuard() instanceof ExpressionGuard)
                    || !(((org.abs_models.xtext.abs.ExpressionGuard)(((AwaitExpression)xtextExp).getGuard())).getExpression() instanceof MethodCallExpression))) {
                // Are we an expression that is NOT await + single expression
                // "guard" with an asynchronous method call expression?  Then
                // generate a JastAdd AwaitStatement.  (The "await o!m()" case is
                // handled as part of fromXtext(Expression) below.)
                final Guard guard = fromXtext(((AwaitExpression)xtextExp).getGuard());
                result = new AwaitStmt(annotations, guard);
            } else {
                final Exp exp = fromXtext(xtextExp);
                result = new ExpressionStmt(annotations, exp);
            }
        }
        else if(stmt instanceof org.abs_models.xtext.abs.SwitchStatement) {
            final org.abs_models.xtext.abs.SwitchStatement value = (org.abs_models.xtext.abs.SwitchStatement) stmt;
            final List<Annotation> annotations = annotationsfromXtext(value.getAnnotations());
            final PureExp condition = pureExpFromXtext(value.getCondition());
            final List<CaseBranchStmt> branches = caseBranchStmtsFromXtext(value.getBranches());
            result = new CaseStmt(annotations, condition, branches);
        }
        else {
            throw new NotImplementedYetException(new ASTNode(),
                "No conversion to JastAdd implemented for Xtext statement node "
                    + stmt.getClass().toString());
        }
        return nodeWithLocation(result, stmt);
    }

    private static Guard fromXtext(final org.abs_models.xtext.abs.Guard guard) {
        Guard result = null;
        if (guard instanceof org.abs_models.xtext.abs.DurationGuard) {
            final org.abs_models.xtext.abs.DurationGuard dguard = (org.abs_models.xtext.abs.DurationGuard) guard;
            result = nodeWithLocation(new org.abs_models.frontend.ast.DurationGuard(pureExpFromXtext(dguard.getMin()), pureExpFromXtext(dguard.getMax())), guard);
        } else if (guard instanceof org.abs_models.xtext.abs.ExpressionGuard) {
            final org.abs_models.xtext.abs.ExpressionGuard eguard = (org.abs_models.xtext.abs.ExpressionGuard) guard;
            if (eguard.isClaim()) {
                result = nodeWithLocation(new org.abs_models.frontend.ast.ClaimGuard(pureExpFromXtext(eguard.getExpression())), guard);
            } else {
                result = nodeWithLocation(new org.abs_models.frontend.ast.ExpGuard(pureExpFromXtext(eguard.getExpression())), guard);
            }
        } else if (guard instanceof org.abs_models.xtext.abs.AndGuard) {
            final org.abs_models.xtext.abs.AndGuard aguard = (org.abs_models.xtext.abs.AndGuard)guard;
            result = nodeWithLocation(new org.abs_models.frontend.ast.AndGuard(fromXtext(aguard.getLeft()), fromXtext(aguard.getRight())), guard);
        } else {
            throw new NotImplementedYetException(new ASTNode(),
               "No conversion to JastAdd implemented for Xtext guard node "
                   + guard.getClass().toString());
        }
        return result;
    }

    private static List<CaseBranchStmt> caseBranchStmtsFromXtext(final EList<SwitchStatementBranch> statements) {
        final List<CaseBranchStmt> branchStmts = new List<>();
        for(final SwitchStatementBranch branch : statements) {
            branchStmts.add(fromXtext(branch));
        }
        return branchStmts;
    }

    private static CaseBranchStmt fromXtext(final SwitchStatementBranch xtext_branch) {
        final CaseBranchStmt result = new CaseBranchStmt();
        result.setLeft(fromXtext(xtext_branch.getPattern()));

        final Block block = blockFromXtext(xtext_branch.getBody());
        result.setRight(block);

        return nodeWithLocation(result, xtext_branch);
    }

    private static Pattern fromXtext(final org.abs_models.xtext.abs.Pattern pattern) {
        Pattern result;
        if (pattern instanceof org.abs_models.xtext.abs.WildcardPattern) {
            result = nodeWithLocation(new UnderscorePattern(), pattern);
        } else if (pattern instanceof org.abs_models.xtext.abs.IntLiteralPattern) {
            final org.abs_models.xtext.abs.IntLiteralPattern value = (IntLiteralPattern) pattern;
            final LiteralExp exp = nodeWithLocation(new org.abs_models.frontend.ast.IntLiteral(value.getValue().toString()), value, AbsPackage.eINSTANCE.getIntLiteralPattern_Value());
            result = nodeWithLocation(new LiteralPattern(exp), pattern);
        } else if (pattern instanceof org.abs_models.xtext.abs.StringLiteralPattern) {
            final org.abs_models.xtext.abs.StringLiteralPattern value = (StringLiteralPattern) pattern;
            final LiteralExp exp = nodeWithLocation(new org.abs_models.frontend.ast.StringLiteral(ASTPreProcessor.preprocessStringLiteral(value.getValue())), value, AbsPackage.eINSTANCE.getStringLiteralPattern_Value());
            result = nodeWithLocation(new LiteralPattern(exp), pattern);
        }
        else if(pattern instanceof org.abs_models.xtext.abs.FloatLiteralPattern) {
            final org.abs_models.xtext.abs.FloatLiteralPattern value = (FloatLiteralPattern) pattern;
            final LiteralExp exp = nodeWithLocation(new org.abs_models.frontend.ast.FloatLiteral(Double.toString(value.getValue())), value, AbsPackage.eINSTANCE.getFloatLiteralPattern_Value());
            result = nodeWithLocation(new LiteralPattern(exp), pattern);
        } else if (pattern instanceof org.abs_models.xtext.abs.VariablePattern) {
            final org.abs_models.xtext.abs.VariablePattern value = (VariablePattern) pattern;
            result = nodeWithLocation(new PatternVarUse(value.getValue()), pattern);
        } else if (pattern instanceof org.abs_models.xtext.abs.ConstructorPattern) {
            // TODO: once Xtext linking is in place, create ExceptionPattern
            // here?
            final org.abs_models.xtext.abs.ConstructorPattern value = (org.abs_models.xtext.abs.ConstructorPattern) pattern;
            final ConstructorPattern presult = new ConstructorPattern();
            presult.setConstructor(value.getConstructorname());
            for(final org.abs_models.xtext.abs.Pattern p : value.getArguments()) {
                presult.addParamNoTransform(fromXtext(p));
            }
            result = nodeWithLocation(presult, pattern);
        } else {
            throw new NotImplementedYetException(new ASTNode(),
                "No conversion to JastAdd implemented for Xtext pattern node "
                    + pattern.getClass().toString());
        }
        return nodeWithLocation(result, pattern);
    }

    private static Exp fromXtext(final org.abs_models.xtext.abs.Expression value) {
        Exp result;

        if (value instanceof org.abs_models.xtext.abs.GetExpression) {

            final org.abs_models.xtext.abs.GetExpression xtextExp = (org.abs_models.xtext.abs.GetExpression) value;
            final GetExp exp = new GetExp(pureExpFromXtext(xtextExp.getFutureExpression()));
            result = nodeWithLocation(exp, value);

        } else if (value instanceof org.abs_models.xtext.abs.OriginalCallExpression) {

            final org.abs_models.xtext.abs.OriginalCallExpression xtextExp = (OriginalCallExpression) value;
            final List<PureExp> paramList = new List<>();
            for(final org.abs_models.xtext.abs.Expression e : xtextExp.getArguments()) {
                paramList.add(pureExpFromXtext(e));
            }
            OriginalCall exp;
            if (xtextExp.isCore()) {
                // ‘core.original()’
                exp = new TargetedOriginalCall(paramList, nodeWithLocation(new DeltaID("core"), xtextExp, AbsPackage.eINSTANCE.getOriginalCallExpression_Core()));
            } else if (xtextExp.getDelta() != null) {
                // ‘Delta.original()’
                exp = new TargetedOriginalCall(paramList, nodeWithLocation(new DeltaID(xtextExp.getDelta()), xtextExp, AbsPackage.eINSTANCE.getOriginalCallExpression_Delta()));
            } else {
                // ‘original()’
                exp = new OriginalCall(paramList);
            }
            result = nodeWithLocation(exp, value);

        } else if (value instanceof org.abs_models.xtext.abs.MethodCallExpression) {

            final org.abs_models.xtext.abs.MethodCallExpression xtextExp = (MethodCallExpression) value;
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
            for (final org.abs_models.xtext.abs.Expression e : xtextExp.getArguments()) {
                exp.addParamNoTransform(pureExpFromXtext(e));
            }
            result = exp;

        } else if (value instanceof org.abs_models.xtext.abs.AwaitExpression) {

            final org.abs_models.xtext.abs.AwaitExpression awaitExp = (org.abs_models.xtext.abs.AwaitExpression) value;
            // Are we an await + single expression "guard" with an
            // asynchronous method call expression?  Then generate a JastAdd
            // AwaitAsyncCallExpression.  We rely on validation having blocked all
            // invalid constructs; fix the validator if there’s a class cast
            // exception in the code below.
            final ExpressionGuard guard = (ExpressionGuard)awaitExp.getGuard();
            final org.abs_models.xtext.abs.MethodCallExpression xtextExp = (MethodCallExpression) guard.getExpression();
            final AwaitAsyncCall exp = new AwaitAsyncCall();
            exp.setMethod(xtextExp.getMethodname());
            exp.setCallee(pureExpFromXtext(xtextExp.getTarget()));
            for (final org.abs_models.xtext.abs.Expression e : xtextExp.getArguments()) {
                exp.addParamNoTransform(pureExpFromXtext(e));
            }
            result = exp;
        } else if (value instanceof org.abs_models.xtext.abs.NewExpression) {

            final org.abs_models.xtext.abs.NewExpression xtextExp = (org.abs_models.xtext.abs.NewExpression)value;
            final NewExp exp = new NewExp();
            exp.setClassName(accessibleName(xtextExp, xtextExp.getClass_(),
                                            xtextExp.getClass_().getName()));
            for(final org.abs_models.xtext.abs.Expression e : xtextExp.getArguments()) {
                exp.addParamNoTransform(pureExpFromXtext(e));
            }
            if (xtextExp.isLocal()) {
                exp.setLocal(nodeWithLocation(new Local(), xtextExp, AbsPackage.eINSTANCE.getNewExpression_Local()));
            }
            if (xtextExp.getProductExpr() != null) {
                ProductDecl product = new ProductDecl();
                exp.setProductDecl(product.setProductExpr(fromXtext(xtextExp.getProductExpr())));
            }
            result = nodeWithLocation(exp, xtextExp);

        } else {
            result = pureExpFromXtext(value);
        }
        return nodeWithLocation(result, value);
    }

    private static PureExp pureExpFromXtext(final org.abs_models.xtext.abs.Expression value) {
        PureExp result = null;

        if(value instanceof org.abs_models.xtext.abs.OrExpression) {
            final org.abs_models.xtext.abs.OrExpression xtextExp = (OrExpression) value;
            result = new OrBoolExp(pureExpFromXtext(xtextExp.getLeft()),
                                   pureExpFromXtext(xtextExp.getRight()));
        } else if(value instanceof org.abs_models.xtext.abs.AndExpression) {
            final org.abs_models.xtext.abs.AndExpression xtextExp = (AndExpression) value;
            result = new AndBoolExp(pureExpFromXtext(xtextExp.getLeft()),
                                    pureExpFromXtext(xtextExp.getRight()));
        } else if(value instanceof org.abs_models.xtext.abs.EqExpression) {
            final org.abs_models.xtext.abs.EqExpression xtextExp = (org.abs_models.xtext.abs.EqExpression) value;
            result = new EqExp(pureExpFromXtext(xtextExp.getLeft()),
                               pureExpFromXtext(xtextExp.getRight()));
        } else if(value instanceof org.abs_models.xtext.abs.NotEqExpression) {
            final org.abs_models.xtext.abs.NotEqExpression xtextExp = (org.abs_models.xtext.abs.NotEqExpression) value;
            result = new NotEqExp(pureExpFromXtext(xtextExp.getLeft()),
                                  pureExpFromXtext(xtextExp.getRight()));
        } else if(value instanceof org.abs_models.xtext.abs.LTExpression) {
            final org.abs_models.xtext.abs.LTExpression xtextExp = (org.abs_models.xtext.abs.LTExpression) value;
            result = new LTExp(pureExpFromXtext(xtextExp.getLeft()),
                               pureExpFromXtext(xtextExp.getRight()));
        } else if(value instanceof org.abs_models.xtext.abs.GTExpression) {
            final org.abs_models.xtext.abs.GTExpression xtextExp = (org.abs_models.xtext.abs.GTExpression) value;
            result = new GTExp(pureExpFromXtext(xtextExp.getLeft()),
                               pureExpFromXtext(xtextExp.getRight()));
        } else if(value instanceof org.abs_models.xtext.abs.LTEQExpression) {
            final org.abs_models.xtext.abs.LTEQExpression xtextExp = (org.abs_models.xtext.abs.LTEQExpression) value;
            result = new LTEQExp(pureExpFromXtext(xtextExp.getLeft()),
                                 pureExpFromXtext(xtextExp.getRight()));
        } else if(value instanceof org.abs_models.xtext.abs.GTEQExpression) {
            final org.abs_models.xtext.abs.GTEQExpression xtextExp = (org.abs_models.xtext.abs.GTEQExpression) value;
            result = new GTEQExp(pureExpFromXtext(xtextExp.getLeft()),
                                 pureExpFromXtext(xtextExp.getRight()));
        } else if(value instanceof org.abs_models.xtext.abs.PlusExpression) {
            final org.abs_models.xtext.abs.PlusExpression xtextExp = (org.abs_models.xtext.abs.PlusExpression) value;
            result = new AddAddExp(pureExpFromXtext(xtextExp.getLeft()),
                                   pureExpFromXtext(xtextExp.getRight()));
        } else if(value instanceof org.abs_models.xtext.abs.MinusExpression) {
            final org.abs_models.xtext.abs.MinusExpression xtextExp = (org.abs_models.xtext.abs.MinusExpression) value;
            result = new SubAddExp(pureExpFromXtext(xtextExp.getLeft()),
                                   pureExpFromXtext(xtextExp.getRight()));
        } else if(value instanceof org.abs_models.xtext.abs.MulExpression) {
            final org.abs_models.xtext.abs.MulExpression xtextExp = (org.abs_models.xtext.abs.MulExpression) value;
            result = new MultMultExp(pureExpFromXtext(xtextExp.getLeft()),
                                     pureExpFromXtext(xtextExp.getRight()));
        } else if(value instanceof org.abs_models.xtext.abs.DivExpression) {
            final org.abs_models.xtext.abs.DivExpression xtextExp = (org.abs_models.xtext.abs.DivExpression) value;
            result = new DivMultExp(pureExpFromXtext(xtextExp.getLeft()),
                                    pureExpFromXtext(xtextExp.getRight()));
        } else if(value instanceof org.abs_models.xtext.abs.ModExpression) {
            final org.abs_models.xtext.abs.ModExpression xtextExp = (org.abs_models.xtext.abs.ModExpression) value;
            result = new ModMultExp(pureExpFromXtext(xtextExp.getLeft()),
                                    pureExpFromXtext(xtextExp.getRight()));
        }
        // MethodCallExpression, GetExpression are handled in fromXtext(Expression)
        else if(value instanceof org.abs_models.xtext.abs.ImplementsExpression) {
            final org.abs_models.xtext.abs.ImplementsExpression xtextExp = (org.abs_models.xtext.abs.ImplementsExpression) value;
            result = new ImplementsExp(pureExpFromXtext(xtextExp.getBody()),
                                       new InterfaceTypeUse(xtextExp.getInterface(), new List<>()));
        } else if(value instanceof org.abs_models.xtext.abs.AsExpression) {
            final org.abs_models.xtext.abs.AsExpression xtextExp = (org.abs_models.xtext.abs.AsExpression) value;
            result = new AsExp(pureExpFromXtext(xtextExp.getBody()),
                               new InterfaceTypeUse(xtextExp.getInterface(), new List<>()));
        } else if(value instanceof org.abs_models.xtext.abs.NotExpression) {
            final org.abs_models.xtext.abs.NotExpression xtextExp = (org.abs_models.xtext.abs.NotExpression) value;
            result = new NegExp(pureExpFromXtext(xtextExp.getBody()));
        } else if(value instanceof org.abs_models.xtext.abs.NegExpression) {
            final org.abs_models.xtext.abs.NegExpression xtextExp = (org.abs_models.xtext.abs.NegExpression) value;
            result = new MinusExp(pureExpFromXtext(xtextExp.getBody()));
        }
        // AwaitExpression is handled in fromXtext(ExpressionStatement),
        // fromXtext(Expression) NewExpression, OriginalCallExpression are
        // handled in fromXtext(Expression)
        else  if(value instanceof org.abs_models.xtext.abs.CaseExpression) {
            final org.abs_models.xtext.abs.CaseExpression xtextExp = (org.abs_models.xtext.abs.CaseExpression) value;
            final List<CaseBranch> branches = new List<>();
            for(final CaseExpressionBranch cb : xtextExp.getCasebranches()) {
                final CaseBranch branch = new CaseBranch(fromXtext(cb.getPattern()),
                                                   pureExpFromXtext(cb.getExpression()));
                branches.add(branch);
            }
            result = new CaseExp(pureExpFromXtext(xtextExp.getCondition()),
                                 branches);
        } else if(value instanceof org.abs_models.xtext.abs.LetExpression) {
            final org.abs_models.xtext.abs.LetExpression xtextExp = (org.abs_models.xtext.abs.LetExpression) value;
            result = pureExpFromXtext(xtextExp.getBody());
            // Construct the wrapping LetExpression’s from the inside out -- later
            // expressions have access to the identifiers of earlier ones so
            // are on the inside
            for (int i = xtextExp.getVariables().size() - 1; i >= 0; i--) {
                // getVariables(), getExps() have the same size
                result = new LetExp(fromXtext(xtextExp.getVariables().get(i)),
                                    pureExpFromXtext(xtextExp.getExpressions().get(i)),
                                    result);
            }
        } else if(value instanceof org.abs_models.xtext.abs.WhenExpression) {
            final org.abs_models.xtext.abs.WhenExpression xtextExp = (org.abs_models.xtext.abs.WhenExpression) value;
            result = new IfExp(pureExpFromXtext(xtextExp.getCondition()),
                               pureExpFromXtext(xtextExp.getConsequence()),
                               pureExpFromXtext(xtextExp.getAlternate()));
        } else if(value instanceof org.abs_models.xtext.abs.FunctionAppExpression) {
            final org.abs_models.xtext.abs.FunctionAppExpression xtextExp = (FunctionAppExpression) value;
            if (xtextExp.getFunctionArguments().size() > 0) {
                final ParFnApp exp = new ParFnApp();
                exp.setName(xtextExp.getFunctionname());
                for (final org.abs_models.xtext.abs.Expression arg : xtextExp.getArguments()) {
                    exp.addParamNoTransform(pureExpFromXtext(arg));
                }
                for (final org.abs_models.xtext.abs.PartialFunctionParam p : xtextExp.getFunctionArguments()) {
                    exp.addFuncParamNoTransform(fromXtext(p));
                }
                result = exp;
            } else {
                final FnApp exp = new FnApp();
                exp.setName(xtextExp.getFunctionname());
                for (final org.abs_models.xtext.abs.Expression arg : xtextExp.getArguments()) {
                    exp.addParamNoTransform(pureExpFromXtext(arg));
                }
                result = exp;
            }
        } else if(value instanceof org.abs_models.xtext.abs.VariadicFunctionAppExpression) {
            final org.abs_models.xtext.abs.VariadicFunctionAppExpression xtextExp = (VariadicFunctionAppExpression) value;
            final List<PureExp> arglist = new List<PureExp>();
            for (final org.abs_models.xtext.abs.Expression arg : xtextExp.getArguments()) {
                arglist.add(pureExpFromXtext(arg));
            }
            PureExp args;
            if (xtextExp.getArguments().isEmpty()) {
                args = new DataConstructorExp("Nil", new List<>());
            } else {
                args = new ListLiteral(arglist);
            }
            result = new FnApp(xtextExp.getFunctionname(), new List<PureExp>(args));
        } else if(value instanceof org.abs_models.xtext.abs.ConstructorAppExpression) {
            final org.abs_models.xtext.abs.ConstructorAppExpression xtextExp = (ConstructorAppExpression) value;
            final List<PureExp> params = new List<>();
            for(final org.abs_models.xtext.abs.Expression param : xtextExp.getArguments()) {
                params.add(pureExpFromXtext(param));
            }
            result = new DataConstructorExp(xtextExp.getConstructorname(), params);
        } else if(value instanceof org.abs_models.xtext.abs.TemplateStringSimpleExpression){
            final org.abs_models.xtext.abs.TemplateStringSimpleExpression xtextExp = (org.abs_models.xtext.abs.TemplateStringSimpleExpression) value;
            result = new StringLiteral(ASTPreProcessor.preprocessTemplateStringLiteral(xtextExp.getString()));
        } else if(value instanceof org.abs_models.xtext.abs.TemplateStringExpression) {
            final org.abs_models.xtext.abs.TemplateStringExpression xtextExp = (org.abs_models.xtext.abs.TemplateStringExpression) value;
            PureExp exp = new AddAddExp(new StringLiteral(ASTPreProcessor.preprocessTemplateStringLiteral(xtextExp.getStartString())),
                                        new FnApp("toString", new List<>(pureExpFromXtext(xtextExp.getFirstExpression()))));
            for (int i = 0; i < xtextExp.getExpressions().size(); i++) {
                final PureExp part = new AddAddExp(new StringLiteral(ASTPreProcessor.preprocessTemplateStringLiteral(xtextExp.getBetweenStrings().get(i))),
                                             new FnApp("toString", new List<>(pureExpFromXtext(xtextExp.getExpressions().get(i)))));
                exp = new AddAddExp(exp, part);
            }
            exp = new AddAddExp(exp, new StringLiteral(ASTPreProcessor.preprocessTemplateStringLiteral(xtextExp.getEndString())));
            result = exp;
        } else if (value instanceof org.abs_models.xtext.abs.IntLiteral) {
            final org.abs_models.xtext.abs.IntLiteral xtextExp = (org.abs_models.xtext.abs.IntLiteral) value;
            result = new IntLiteral(xtextExp.getValue().toString());
        } else if (value instanceof org.abs_models.xtext.abs.FloatLiteral) {
            final org.abs_models.xtext.abs.FloatLiteral xtextExp = (org.abs_models.xtext.abs.FloatLiteral) value;
            result = new FloatLiteral(Double.toString(xtextExp.getValue()));
        } else if (value instanceof org.abs_models.xtext.abs.StringLiteral) {
            final org.abs_models.xtext.abs.StringLiteral xtextExp = (org.abs_models.xtext.abs.StringLiteral) value;
            result = new StringLiteral(ASTPreProcessor.preprocessStringLiteral(xtextExp.getValue()));
        } else if(value instanceof org.abs_models.xtext.abs.VariableOrFieldExpression) {
            final org.abs_models.xtext.abs.VariableOrFieldExpression xtextExp = (org.abs_models.xtext.abs.VariableOrFieldExpression) value;
            if (xtextExp.isField()) {
                result = new FieldUse(xtextExp.getVariablename());
            } else {
                result = new VarUse(xtextExp.getVariablename());
            }
        } else if(value instanceof org.abs_models.xtext.abs.ThisExpression) {
            result = new ThisExp();
        } else if(value instanceof org.abs_models.xtext.abs.NullLiteral) {
            result = new NullExp();
        } else {
            throw new NotImplementedYetException(new ASTNode(),
                "No conversion to JastAdd implemented for Xtext expression node "
                    + value.getClass().toString());
        }
        return nodeWithLocation(result, value);
    }

    private static ParFnAppParam fromXtext(final org.abs_models.xtext.abs.PartialFunctionParam p) {
        if (p.getFunctionname() != null) {
            return nodeWithLocation(new NamedParFnAppParam(p.getFunctionname()),
                                    p, AbsPackage.eINSTANCE.getPartialFunctionParam_Functionname());
        } else {
            final List<ParamDecl> params = new List<ParamDecl>();
            for(final org.abs_models.xtext.abs.Parameter arg : p.getParameters()) {
                params.add(fromXtext(arg));
            }

            return nodeWithLocation(new AnonymousFunctionDecl(params, pureExpFromXtext(p.getBody())),
                                    p);
        }
    }

    private static TypeUse fromXtext(final org.abs_models.xtext.abs.TypeUse type) {
        final TypeUse result = fromXtext(type.getType());
        result.setAnnotationList(annotationsfromXtext(type.getAnnotations()));
        return result;
    }

    private static TypeUse fromXtext(final org.abs_models.xtext.abs.TypeUseNoAnnotations type) {
        TypeUse result;
        if (!type.getParameters().isEmpty()) {
            final ParametricDataTypeUse presult = new ParametricDataTypeUse();
            result = (ParametricDataTypeUse) presult;
            for (final org.abs_models.xtext.abs.TypeUse param : type.getParameters()) {
                presult.addParamNoTransform(fromXtext(param));
            }
        } else if (type.getProducExpr() != null) {
            VariableInterfaceTypeUse ipresult = new VariableInterfaceTypeUse();
            ProductDecl product =  new ProductDecl();
            product.setProductExpr(fromXtext(type.getProducExpr()));
            ipresult.setProductDecl(product);
            product.setParent(ipresult);
            result = ipresult;
        }else {
            // will be rewritten by JastAdd -- also once we implement scoping
            // and linking, we can use the exact class here
            result = new UnresolvedTypeUse();
        }
        result.setName(type.getTypename());
        return nodeWithLocation(result, type);
    }

    private static InterfaceTypeUse fromXtext(org.abs_models.xtext.abs.InterfaceTypeIdentifier xtext_interface) {
        InterfaceTypeUse itu;
        if (xtext_interface.getProduct() != null) {
            VariableInterfaceTypeUse ipresult = new VariableInterfaceTypeUse();
            ProductDecl product = new ProductDecl();
            product.setProductExpr(fromXtext(xtext_interface.getProduct()));
            ipresult.setProductDecl(product);
            product.setParent(ipresult);
            itu = ipresult;
        }
        else {
            itu = new InterfaceTypeUse();
        }
        itu.setName(xtext_interface.getName());
        return itu;
    }

    // ========== end of Core ABS ==========

    private static DeltaDecl fromXtext(final org.abs_models.xtext.abs.DeltaDeclaration xtext_delta) {
        final DeltaDecl result = new DeltaDecl();
        result.setName(xtext_delta.getName());
        for (final org.abs_models.xtext.abs.DeltaParameter arg : xtext_delta.getArguments()) {
            if (arg.getNormalParameter() != null) {
                result.addParam(nodeWithLocation(new DeltaFieldParam(fromXtext(arg.getNormalParameter())), arg));
            } else {
                result.addParam(nodeWithLocation(new DeltaClassParam(arg.getClassModifier(), fromXtext(arg.getCondition())), arg));
            }
        }
        if (xtext_delta.getUsedModulename() != null) {
            final String iname = xtext_delta.getUsedModulename();
            result.setImportedModule(nodeWithLocation(new DeltaAccess(iname), xtext_delta, AbsPackage.eINSTANCE.getDeltaDeclaration_UsedModulename()));
        }
        for (final org.abs_models.xtext.abs.DeltaModuleModifier m : xtext_delta.getModifiers()) {
            result.addModuleModifier(fromXtext(m));
        }
        return nodeWithLocation(result, xtext_delta);
    }

    private static HasCondition fromXtext(final org.abs_models.xtext.abs.DeltaCondition xtext_cond) {
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

    private static ModuleModifier fromXtext(final org.abs_models.xtext.abs.DeltaModuleModifier xtext_mod) {
        ModuleModifier result = null;
        if (xtext_mod.getAddedDeclaration() != null) {
            final Decl d = fromXtext(xtext_mod.getAddedDeclaration());
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
        } else if (xtext_mod.getAddedImport() != null) {
            result = new AddImportModifier(fromXtext(xtext_mod.getAddedImport()));
        } else if (xtext_mod.getAddedExport() != null) {
            result = new AddExportModifier(fromXtext(xtext_mod.getAddedExport()));
        } else if (xtext_mod.getRemovedClassName() != null) {
            result = new RemoveClassModifier(xtext_mod.getRemovedClassName());
        } else if (xtext_mod.getRemovedInterfaceName() != null) {
            result = new RemoveInterfaceModifier(xtext_mod.getRemovedInterfaceName());
        } else if (xtext_mod.getModifiedClassName() != null) {
            final ModifyClassModifier mresult = new ModifyClassModifier();
            mresult.setName(xtext_mod.getModifiedClassName());
            for (int i = 0; i < xtext_mod.getAddedInterfaces().size(); i++) {
                final String iname = xtext_mod.getAddedInterfaces().get(i);
                mresult.addAddedInterfaceNoTransform(nodeWithLocation(new InterfaceTypeUse(iname, new List<>()), xtext_mod, AbsPackage.eINSTANCE.getDeltaModuleModifier_AddedInterfaces(), i));
            }
            for (int i = 0; i < xtext_mod.getRemovedInterfaces().size(); i++) {
                final String iname = xtext_mod.getRemovedInterfaces().get(i);
                mresult.addRemovedInterfaceNoTransform(nodeWithLocation(new InterfaceTypeUse(iname, new List<>()), xtext_mod, AbsPackage.eINSTANCE.getDeltaModuleModifier_RemovedInterfaces(), i));
            }
            for (final org.abs_models.xtext.abs.ClassModifier mod : xtext_mod.getClassModifiers()) {
                if (mod.getAddedField() != null) {
                    mresult.addModifierNoTransform(new AddFieldModifier(fromXtext(mod.getAddedField())));
                } else if (mod.getAddedMethods().size() > 0) {
                    final TraitSetExpr tse = new TraitSetExpr();
                    for (final org.abs_models.xtext.abs.MethodDeclaration m : mod.getAddedMethods()) {
                        tse.addMethodImplNoTransform(fromXtext(m));
                    }
                    mresult.addModifierNoTransform(new DeltaTraitModifier(new AddMethodModifier(tse)));
                } else if (mod.getAddedTrait() != null) {
                    mresult.addModifierNoTransform(new DeltaTraitModifier(new AddMethodModifier(new TraitNameExpr(mod.getAddedTrait()))));
                } else if (mod.getRemovedField() != null) {
                    mresult.addModifierNoTransform(new RemoveFieldModifier(fromXtext(mod.getRemovedField())));
                } else if (mod.getRemovedMethods().size() > 0) {
                    final List<MethodSig> methodlist = new List<MethodSig>();
                    for (final org.abs_models.xtext.abs.MethodSignature s : mod.getRemovedMethods()) {
                        methodlist.add(fromXtext(s));
                    }
                    mresult.addModifierNoTransform(new DeltaTraitModifier(new RemoveMethodModifier(methodlist)));
                } else if (mod.getModifiedMethods().size() > 0) {
                    final TraitSetExpr tse = new TraitSetExpr();
                    for (final org.abs_models.xtext.abs.MethodDeclaration m : mod.getModifiedMethods()) {
                        tse.addMethodImplNoTransform(fromXtext(m));
                    }
                    mresult.addModifierNoTransform(new DeltaTraitModifier(new ModifyMethodModifier(tse)));
                } else if (mod.getModifiedTrait() != null) {
                    mresult.addModifierNoTransform(new DeltaTraitModifier(new ModifyMethodModifier(new TraitNameExpr(mod.getModifiedTrait()))));
                }
            }
            result = mresult;
        } else if (xtext_mod.getModifiedInterfaceName() != null) {
            final ModifyInterfaceModifier mresult = new ModifyInterfaceModifier();
            mresult.setName(xtext_mod.getModifiedInterfaceName());
            for (final org.abs_models.xtext.abs.InterfaceModifier mod : xtext_mod.getInterfaceModifiers()) {
                if (mod.getAddedMethod() != null) {
                    mresult.addMethodSigModifier(new AddMethodSigModifier(fromXtext(mod.getAddedMethod())));
                } else {
                    mresult.addMethodSigModifier(new RemoveMethodSigModifier(fromXtext(mod.getRemovedMethod())));
                }
            }
            result = mresult;
        } else if (xtext_mod.getModifiedTypesynonym() != null) {
            result = new ModifyTypeSynModifier(fromXtext(xtext_mod.getModifiedTypesynonym()));
        } else if (xtext_mod.getModifiedDatatype() != null) {
            result = new ModifyDataTypeModifier(fromXtext(xtext_mod.getModifiedDatatype()));
        }
        return nodeWithLocation(result, xtext_mod);
    }

    // Product lines

    private static ProductLine fromXtext(final org.abs_models.xtext.abs.ProductlineDeclaration xtext_decl) {
        final ProductLine result = new ProductLine();
        result.setName(xtext_decl.getName());
        for (final org.abs_models.xtext.abs.ProductFeature feature : xtext_decl.getFeatures()) {
            result.addFeatureNoTransform(fromXtext(feature));
        }
        for (final org.abs_models.xtext.abs.ProductlineDeltaClause xtext_clause : xtext_decl.getDeltaClauses()) {
            result.addDeltaClause(fromXtext(xtext_clause));
        }
        return nodeWithLocation(result, xtext_decl);
    }

    private static Feature fromXtext(final org.abs_models.xtext.abs.ProductFeature xtext_feature) {
        final Feature result = new Feature();
        result.setName(xtext_feature.getName());
        for (final org.abs_models.xtext.abs.AttributeAssignment xtext_attr : xtext_feature.getAttributeAssignments()) {
            final AttrAssignment attr = new AttrAssignment();
            attr.setName(xtext_attr.getAttributename());
            final org.abs_models.xtext.abs.AttributeAssignmentValue xtext_value = xtext_attr.getValue();
            if (xtext_value instanceof org.abs_models.xtext.abs.AttributeAssignmentValue_Int) {
                final int value = ((org.abs_models.xtext.abs.AttributeAssignmentValue_Int)xtext_value).getValue().intValue();
                attr.setValue(nodeWithLocation(new IntVal(value), xtext_value));
            } else if (xtext_value instanceof org.abs_models.xtext.abs.AttributeAssignmentValue_Bool) {
                final boolean value = ((org.abs_models.xtext.abs.AttributeAssignmentValue_Bool)xtext_value).getValue().equals("True");
                attr.setValue(nodeWithLocation(new BoolVal(value), xtext_value));
            } else if (xtext_value instanceof org.abs_models.xtext.abs.AttributeAssignmentValue_String) {
                final String value = ((org.abs_models.xtext.abs.AttributeAssignmentValue_String)xtext_value).getValue();
                attr.setValue(nodeWithLocation(new StringVal(value), xtext_value));
            } else {
                // The antlr code created an UnknownValue here and rewrote it
                // into BoolVal later -- we catch invalid constructor names
                // during xtext validation instead.
                throw new NotImplementedYetException(new ASTNode(),
                                                     "No conversion to JastAdd implemented for Xtext product feature node "
                                                     + xtext_value.getClass().toString());
            }
            result.addAttrAssignment(nodeWithLocation(attr, xtext_attr));
        }
        return nodeWithLocation(result, xtext_feature);
    }

    private static DeltaClause fromXtext(final org.abs_models.xtext.abs.ProductlineDeltaClause xtext_clause) {
        final DeltaClause result = new DeltaClause();
        final Deltaspec result_name = new Deltaspec();
        result_name.setDeltaID(xtext_clause.getName());
        for (final org.abs_models.xtext.abs.DeltaClauseParam param : xtext_clause.getParameters()) {
            result_name.addDeltaparam(fromXtext(param));
        }
        result.setDeltaspec(result_name);
        for (final String after_id : xtext_clause.getAfterIds()) {
            result.addAfterDeltaID(new DeltaID(after_id));
        }
        if (xtext_clause.getFromCondition() != null) {
            result.setFromAppCond(fromXtext(xtext_clause.getFromCondition()));
        }
        if (xtext_clause.getWhenCondition() != null) {
            result.setAppCond(fromXtext(xtext_clause.getWhenCondition()));
        }
        return nodeWithLocation(result, xtext_clause);
    }

    private static Deltaparam fromXtext(final org.abs_models.xtext.abs.DeltaClauseParam xtext_param) {
        Deltaparam result = null;
        if (xtext_param instanceof org.abs_models.xtext.abs.DeltaClauseParam_Int) {
            result = new Const(new IntVal(((org.abs_models.xtext.abs.DeltaClauseParam_Int)xtext_param).getIntParameter().intValue()));
        } else if (xtext_param instanceof org.abs_models.xtext.abs.DeltaClauseParam_Id) {
            final org.abs_models.xtext.abs.DeltaClauseParam_Id id_param = (org.abs_models.xtext.abs.DeltaClauseParam_Id) xtext_param;
            if (id_param.getFidaidParameter() == null) {
                if (id_param.getIdParameter().equals("True")) {
                    result = new Const(new BoolVal(true));
                } else if (id_param.getIdParameter().equals("False")) {
                    result = new Const(new BoolVal(false));
                } else {
                    result = new FID(id_param.getIdParameter());
                }
            } else {
                result = new FIDAID(id_param.getIdParameter(), id_param.getFidaidParameter());
            }
        } else {
            throw new NotImplementedYetException(new ASTNode(),
                                                 "No conversion to JastAdd implemented for Xtext delta clause node "
                                                 + xtext_param.getClass().toString());
        }
        return nodeWithLocation(result, xtext_param);
    }

    private static AppCond fromXtext(final org.abs_models.xtext.abs.DeltaClauseApplicationClause xtext_clause) {
        AppCond result = null;
        if (xtext_clause instanceof org.abs_models.xtext.abs.DeltaClauseApplicationClauseBiImpl) {
            AppCond left = fromXtext(xtext_clause.getLeft());
            AppCond right = fromXtext(xtext_clause.getRight());
            result = new AppCondOr( new AppCondAnd(left, right),new AppCondAnd(new AppCondNot(left),
                new AppCondNot(right)));
        } else if (xtext_clause instanceof org.abs_models.xtext.abs.DeltaClauseApplicationClauseImpl) {
            result = new AppCondOr(new AppCondNot(fromXtext(xtext_clause.getLeft())),
                fromXtext(xtext_clause.getRight()));
        }
        else if (xtext_clause instanceof org.abs_models.xtext.abs.DeltaClauseApplicationClauseOr) {
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
                result = new AppCondFeature(xtext_clause.getFeatureName());
            }
        }
        return nodeWithLocation(result, xtext_clause);
    }

    //Local Product lines
    private static LocalProductLine fromXtext(org.abs_models.xtext.abs.LocalProductLineDeclaration xtext_decl){
        LocalProductLine result = new LocalProductLine();

        for (org.abs_models.xtext.abs.Feature ft: xtext_decl.getFeatures()) {
            Feature feature = new Feature();
            feature.setName(ft.getId());
            result.addFeatureNoTransform(nodeWithLocation(feature, ft));
        }
        if (xtext_decl.getCond() != null){
            result.setFeatCond(fromXtext(xtext_decl.getCond()));
        }

        for(org.abs_models.xtext.abs.LocalDeltaDeclaration delta : xtext_decl.getDeltas()){
            result.addDeltaDeclNoTransform(fromXtext(delta));
        }

        for(org.abs_models.xtext.abs.ActivationCondition cond: xtext_decl.getActivationConditions()){
            DeltaClause deltaClause = new DeltaClause();
            Deltaspec deltaSpec = new Deltaspec();
            deltaSpec.setDeltaID(cond.getDeltaName());
            deltaClause.setDeltaspec(deltaSpec);
            if (cond.getCond() != null) {
                deltaClause.setAppCond(fromXtext(cond.getCond()));
            }
            if (xtext_decl.getApplicationOrder() != null && xtext_decl.getApplicationOrder().size() > 0) {
                deltaClause.setAfterDeltaIDList(fromXtext(xtext_decl.getApplicationOrder(), cond.getDeltaName()));
            }
            if (cond.getAfterIds() != null) {
                for (String after_id : cond.getAfterIds()) {
                    deltaClause.addAfterDeltaID(new DeltaID(after_id));
                }
            }
            result.addDeltaClauseNoTransform(deltaClause);
        }

        if (xtext_decl.getActivationConditions() == null || xtext_decl.getActivationConditions().size() == 0) {
            for (org.abs_models.xtext.abs.ApplicationOrder order : xtext_decl.getApplicationOrder()){
                fromXtext(order).forEach(dc -> result.addDeltaClauseNoTransform(dc));
            }
        }
        return nodeWithLocation(result, xtext_decl);
    }
    private static List<DeltaClause> fromXtext(org.abs_models.xtext.abs.ApplicationOrder xtext_order){
        List <DeltaClause> result = new List<>();
        DeltaSet lastSet = xtext_order.getOrder().get(xtext_order.getOrder().size() - 1);
        for (String delta : lastSet.getElems()) {
            DeltaClause deltaClause = new DeltaClause();
            Deltaspec deltaSpec = new Deltaspec();
            deltaSpec.setDeltaID(delta);
            deltaClause.setDeltaspec(deltaSpec);
            for (int i = 0; i< xtext_order.getOrder().size() - 1; ++i){
                xtext_order.getOrder().get(i).getElems().forEach(d -> deltaClause.addAfterDeltaID(new DeltaID(d)));
            }
            result.add(deltaClause);
        }
        return result;
    }

    private static DeltaDecl fromXtext(org.abs_models.xtext.abs.LocalDeltaDeclaration delta){
        DeltaDecl result = new DeltaDecl();
        result.setName(delta.getName());
        for(org.abs_models.xtext.abs.DeltaModuleModifier modifier : delta.getOps()){
            result.addModuleModifier(fromXtext(modifier));
        }
        return nodeWithLocation(result, delta);
    }

    private static List<DeltaID> fromXtext(EList<org.abs_models.xtext.abs.ApplicationOrder> orderList, String deltaId){
        List<DeltaID> afterDeltas = new List<>();
        ArrayList<DeltaID> deltas = new ArrayList<>();
        boolean found = false; //found the delta id in a order declaration
        int i =0 ;

        //check all order declarations
        for (org.abs_models.xtext.abs.ApplicationOrder order : orderList) {
            while (i < order.getOrder().size() && ! (found = order.getOrder().get(i).getElems().contains(deltaId))) {
                order.getOrder().get(i).getElems().forEach(d -> deltas.add(new DeltaID(d)));
                i++;
            }
            if (found) {
                //if the delta id was found in a order declaration, add the accumulated deltas to the final list
                afterDeltas.addAll(deltas);
            }
            // reset for next iteration
            found = false;
            deltas.clear();
            i = 0;
        }
        return afterDeltas;
    }

    private static LocalProductDecl fromXtext(org.abs_models.xtext.abs.LocalProductDeclaration xtext_decl) {
        final LocalProductDecl result = new LocalProductDecl();
        result.setName(xtext_decl.getName());
        result.setProductExpr(fromXtext(xtext_decl.getExpression()));
        return nodeWithLocation(result, xtext_decl);
    }

    // Products
    private static ProductDecl fromXtext(org.abs_models.xtext.abs.ProductDeclaration xtext_decl) {
        final ProductDecl result = new ProductDecl();
        result.setName(xtext_decl.getName());
        if (xtext_decl.getExpression() != null) {
            // new syntax
            result.setProductExpr(fromXtext(xtext_decl.getExpression()));
        } else {
            // old syntax
            final ProductFeatureSet fs = new ProductFeatureSet();
            for (final org.abs_models.xtext.abs.ProductFeature feat : xtext_decl.getFeatures()) {
                fs.addFeature(fromXtext(feat));
            }
            result.setProductExpr(fs);
        }
        return nodeWithLocation(result, xtext_decl);
    }

    private static ProductExpr fromXtext(final org.abs_models.xtext.abs.ProductExpression xtext_exp) {
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
                for (final org.abs_models.xtext.abs.ProductFeature feat : xtext_exp.getFeatures()) {
                    ((ProductFeatureSet)result).addFeature(fromXtext(feat));
                }
            }
        }
        return nodeWithLocation(result, xtext_exp);
    }

    // mTVL

    private static FeatureDecl fromXtext(final org.abs_models.xtext.abs.MTVLFeatureRoot xtext_root) {
        final FeatureDecl result = new FeatureDecl();
        result.setName(xtext_root.getName());
        if (xtext_root.getGroup() != null) {
            result.setGroup(fromXtext(xtext_root.getGroup()));
        }
        final AttrConstraints constraints = new AttrConstraints();
        for (final org.abs_models.xtext.abs.MTVLAttributeDeclaration attr : xtext_root.getAttributes()) {
            constraints.addAttributeNoTransform(fromXtext(attr));
        }
        for (final org.abs_models.xtext.abs.MTVLConstraint constraint : xtext_root.getConstraints()) {
            constraints.addConstrNoTransform(fromXtext(constraint));
        }
        result.setAttrConstraints(constraints);
        return nodeWithLocation(result, xtext_root);
    }

    private static FeatureDecl fromXtext(final org.abs_models.xtext.abs.MTVLFeatureDeclaration xtext_feature) {
        final FeatureDecl result = new FeatureDecl();
        result.setName(xtext_feature.getName());
        if (xtext_feature.getGroup() != null) {
            result.setGroup(fromXtext(xtext_feature.getGroup()));
        }
        final AttrConstraints constraints = new AttrConstraints();
        for (final org.abs_models.xtext.abs.MTVLAttributeDeclaration attr : xtext_feature.getAttributes()) {
            constraints.addAttributeNoTransform(fromXtext(attr));
        }
        for (final org.abs_models.xtext.abs.MTVLConstraint constraint : xtext_feature.getConstraints()) {
            constraints.addConstrNoTransform(fromXtext(constraint));
        }
        result.setAttrConstraints(constraints);
        return nodeWithLocation(result, xtext_feature);
    }

    private static FExt fromXtext(final org.abs_models.xtext.abs.MTVLFeatureExtension xtext_ext) {
        final FExt result = new FExt();
        result.setName(xtext_ext.getName());
        if (xtext_ext.getGroup() != null) {
            result.setGroup(fromXtext(xtext_ext.getGroup()));
        }
        final AttrConstraints constraints = new AttrConstraints();
        for (final org.abs_models.xtext.abs.MTVLAttributeDeclaration attr : xtext_ext.getAttributes()) {
            constraints.addAttributeNoTransform(fromXtext(attr));
        }
        for (final org.abs_models.xtext.abs.MTVLConstraint constraint : xtext_ext.getConstraints()) {
            constraints.addConstrNoTransform(fromXtext(constraint));
        }
        result.setAttrConstraints(constraints);
        return nodeWithLocation(result, xtext_ext);
    }

    private static Group fromXtext(final org.abs_models.xtext.abs.MTVLFeatureGroup xtext_group) {
        final Group result = new Group();
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
        for (final org.abs_models.xtext.abs.MTVLChildFeature f : xtext_group.getChildren()) {
            result.addFNodeNoTransform(fromXtext(f));
        }
        return nodeWithLocation(result, xtext_group);
    }

    private static Attribute fromXtext(final org.abs_models.xtext.abs.MTVLAttributeDeclaration xtext_attr) {
        final Attribute result = new Attribute();
        result.setName(xtext_attr.getName());
        if (xtext_attr.getType().equals("Int")) {
            if (xtext_attr.isInterval()) {
                result.setAType(new IntMType("Int",
                                             fromXtext(xtext_attr.getLower()),
                                             fromXtext(xtext_attr.getUpper())));
            } else if (xtext_attr.isSet()) {
                final IntListMType mt = new IntListMType("Int", new List<>());
                for (final org.abs_models.xtext.abs.MTVLIntValue v : xtext_attr.getContent()) {
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

    private static BoundaryInt fromXtext(final org.abs_models.xtext.abs.MTVLIntLimit xtext_lim) {
        BoundaryInt result = null;
        if (xtext_lim.isStar()) {
            result = new Limit();
        } else {
            result = new BoundaryVal((xtext_lim.isMinus() ? -1 : +1)
                                     * xtext_lim.getValue().intValue());
        }
        return nodeWithLocation(result, xtext_lim);
    }

    private static Constr fromXtext(final org.abs_models.xtext.abs.MTVLConstraint xtext_constr) {
        Constr result = null;
        if (xtext_constr instanceof org.abs_models.xtext.abs.MTVLIfInConstraint) {
            result = new IfIn(fromXtext(((org.abs_models.xtext.abs.MTVLIfInConstraint)xtext_constr).getExpression()));
        } else if (xtext_constr instanceof org.abs_models.xtext.abs.MTVLIfOutConstraint) {
            result = new IfOut(fromXtext(((org.abs_models.xtext.abs.MTVLIfOutConstraint)xtext_constr).getExpression()));
        } else if (xtext_constr instanceof org.abs_models.xtext.abs.MTVLRequireConstraint) {
            result = new Require(new FeatVar(((org.abs_models.xtext.abs.MTVLRequireConstraint)xtext_constr).getRequire()));
        } else if (xtext_constr instanceof org.abs_models.xtext.abs.MTVLExcludeConstraint) {
            result = new Exclude(new FeatVar(((org.abs_models.xtext.abs.MTVLExcludeConstraint)xtext_constr).getExclude()));
        } else {
            throw new NotImplementedYetException(new ASTNode(),
                                                 "No conversion to JastAdd implemented for Xtext mTVL constraint node "
                                                 + xtext_constr.getClass().toString());
        }
        return nodeWithLocation(result, xtext_constr);
    }

    private static FNode fromXtext(final org.abs_models.xtext.abs.MTVLChildFeature xtext_feat) {
        FNode result = null;
        if (xtext_feat.isOpt()) {
            result = new OptFeat(fromXtext(xtext_feat.getFeature()));
        } else {
            result = new MandFeat(fromXtext(xtext_feat.getFeature()));
        }
        return nodeWithLocation(result, xtext_feat);
    }

    private static MExp fromXtext(final org.abs_models.xtext.abs.MTVLConstraintExpression xtext_exp) {
        MExp result = null;

        if (xtext_exp instanceof org.abs_models.xtext.abs.MTVLConstraintOrExpression) {
            result = new MOrBoolExp(fromXtext(xtext_exp.getLeft()),
                                    fromXtext(xtext_exp.getRight()));
        } else if (xtext_exp instanceof org.abs_models.xtext.abs.MTVLConstraintAndExpression) {
            result = new MAndBoolExp(fromXtext(xtext_exp.getLeft()),
                                     fromXtext(xtext_exp.getRight()));
        } else if (xtext_exp instanceof org.abs_models.xtext.abs.MTVLConstraintImplExpression) {
            result = new MImpliesExp(fromXtext(xtext_exp.getLeft()),
                                     fromXtext(xtext_exp.getRight()));
        } else if (xtext_exp instanceof org.abs_models.xtext.abs.MTVLConstraintEqvExpression) {
            result = new MEquivExp(fromXtext(xtext_exp.getLeft()),
                                   fromXtext(xtext_exp.getRight()));
        } else if (xtext_exp instanceof org.abs_models.xtext.abs.MTVLConstraintEqExpression) {
            result = new MEqExp(fromXtext(xtext_exp.getLeft()),
                                fromXtext(xtext_exp.getRight()));
        } else if (xtext_exp instanceof org.abs_models.xtext.abs.MTVLConstraintNeqExpression) {
            result = new MNotEqExp(fromXtext(xtext_exp.getLeft()),
                                   fromXtext(xtext_exp.getRight()));
        } else if (xtext_exp instanceof org.abs_models.xtext.abs.MTVLConstraintLTExpression) {
            result = new MLTExp(fromXtext(xtext_exp.getLeft()),
                                fromXtext(xtext_exp.getRight()));
        } else if (xtext_exp instanceof org.abs_models.xtext.abs.MTVLConstraintGTExpression) {
            result = new MGTExp(fromXtext(xtext_exp.getLeft()),
                                fromXtext(xtext_exp.getRight()));
        } else if (xtext_exp instanceof org.abs_models.xtext.abs.MTVLConstraintLTEQExpression) {
            result = new MLTEQExp(fromXtext(xtext_exp.getLeft()),
                                  fromXtext(xtext_exp.getRight()));
        } else if (xtext_exp instanceof org.abs_models.xtext.abs.MTVLConstraintGTEQExpression) {
            result = new MGTEQExp(fromXtext(xtext_exp.getLeft()),
                                  fromXtext(xtext_exp.getRight()));
        } else if (xtext_exp instanceof org.abs_models.xtext.abs.MTVLConstraintPlusExpression) {
            result = new MAddAddExp(fromXtext(xtext_exp.getLeft()),
                                    fromXtext(xtext_exp.getRight()));
        } else if (xtext_exp instanceof org.abs_models.xtext.abs.MTVLConstraintMinusExpression) {
            result = new MSubAddExp(fromXtext(xtext_exp.getLeft()),
                                    fromXtext(xtext_exp.getRight()));
        } else if (xtext_exp instanceof org.abs_models.xtext.abs.MTVLConstraintMulExpression) {
            result = new MMultMultExp(fromXtext(xtext_exp.getLeft()),
                                      fromXtext(xtext_exp.getRight()));
        } else if (xtext_exp instanceof org.abs_models.xtext.abs.MTVLConstraintDivExpression) {
            result = new MDivMultExp(fromXtext(xtext_exp.getLeft()),
                                     fromXtext(xtext_exp.getRight()));
        } else if (xtext_exp instanceof org.abs_models.xtext.abs.MTVLConstraintModExpression) {
            result = new MModMultExp(fromXtext(xtext_exp.getLeft()),
                                     fromXtext(xtext_exp.getRight()));
        }
        // MTVLConstraintPrimaryExpression
        else if (xtext_exp.getParenExpression() != null) {
            return fromXtext(xtext_exp.getParenExpression());
        } else if (xtext_exp.getMinusExpression() != null) {
            result = new MMinusExp(fromXtext(xtext_exp.getMinusExpression()));
        } else if (xtext_exp.getNegExpression() != null) {
            result = new MNegExp(fromXtext(xtext_exp.getNegExpression()));
        } else if (xtext_exp.getIntExpression() != null) {
            result = new MValue(new IntVal(xtext_exp.getIntExpression().intValue()));
        } else if (xtext_exp.getIdExpression() != null) {
            result = new AttVar(xtext_exp.getIdExpression());
        } else {
            if (xtext_exp.getDotId() != null) {
                result = new FAVar(xtext_exp.getTypeExpression(),
                                   xtext_exp.getDotId());
            } else {
                if (xtext_exp.getTypeExpression().equals("True")) {
                    result = new MValue(new BoolVal(true));
                } else if (xtext_exp.getTypeExpression().equals("False")) {
                    result = new MValue(new BoolVal(false));
                } else {
                    result = new FeatVar(xtext_exp.getTypeExpression());
                }
            }
        }
        return nodeWithLocation(result, xtext_exp);
    }

}
