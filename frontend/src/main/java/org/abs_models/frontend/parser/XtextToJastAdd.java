package org.abs_models.frontend.parser;

import org.abs_models.frontend.ast.CompilationUnit;
import org.abs_models.frontend.ast.Export;
import org.abs_models.frontend.ast.FromExport;
import org.abs_models.frontend.ast.FromImport;
import org.abs_models.frontend.ast.Import;
import org.abs_models.frontend.ast.ModuleDecl;
import org.abs_models.frontend.ast.Name;
import org.abs_models.frontend.ast.NamedExport;
import org.abs_models.frontend.ast.NamedImport;
import org.abs_models.frontend.ast.Opt;
import org.abs_models.frontend.ast.StarExport;
import org.abs_models.frontend.ast.StarImport;
import org.abs_models.xtext.abs.AbsPackage;
import org.eclipse.xtext.EcoreUtil2;

public class XtextToJastAdd {
    // TODO add locations to JastAdd AST nodes
    static CompilationUnit fromXtext(org.abs_models.xtext.abs.CompilationUnit xtext_unit) {
        CompilationUnit result = new CompilationUnit();
        result.setName(xtext_unit.eResource().getURI().toFileString());
        for (org.abs_models.xtext.abs.ModuleDecl module : xtext_unit.getModules()) {
            result.addModuleDeclNoTransform(fromXtext(module));
        }
        return result;
    }

    static ModuleDecl fromXtext(org.abs_models.xtext.abs.ModuleDecl xtext_module) {
        ModuleDecl result = new ModuleDecl();
        result.setName(xtext_module.getName());
        for (org.abs_models.xtext.abs.ModuleExport export : xtext_module.getExports()) {
            Export e;
            if (export.isStar()) {
                // export *;
                // export * from OtherModule;
                StarExport se = new StarExport();
                e = se;
                if (export.getModulename() != null)
                    se.setModuleName(new Name(export.getModulename()));
            } else if (export.getModulename() != null) {
                // export a, b from OtherModule;
                FromExport fe = new FromExport();
                e = fe;
                fe.setModuleName(export.getModulename());
                for (String id : export.getIdentifiers()) {
                    fe.addNameNoTransform(new Name(id));
                }
            } else {
                // export a, b;
                NamedExport ne = new NamedExport();
                e = ne;
                for (String id : export.getIdentifiers()) {
                    ne.addNameNoTransform(new Name(id));
                }
            }
            result.addExport(e);
        }
        for (org.abs_models.xtext.abs.ModuleImport imp : xtext_module.getImports()) {
            Import i;
            if (imp.isStar()) {
                // import * from OtherModule;
                StarImport si = new StarImport(imp.getModulename());
                i = si;
            } else if (imp.getModulename() != null) {
                // import a, b from OtherModule;
                FromImport fi = new FromImport();
                i = fi;
                fi.setModuleName(imp.getModulename());
                for (String id : imp.getIdentifiers()) {
                    fi.addNameNoTransform(new Name(id));
                }
            } else {
                // import OtherModule.a, OtherModule.b ;
                NamedImport ni = new NamedImport();
                i = ni;
                for (String id : imp.getIdentifiers()) {
                    ni.addNameNoTransform(new Name(id));
                }
            }
            result.addImport(i);
        }
        // for (org.abs_models.xtext.abs.Declaration decl : xtext_module.getDeclarations()) {
        //     result.addDeclNoTransform(fromXtext(decl));
        // }
        // if (xtext_module.getMainBlock() != null) {
        //     result.setBlock(fromXtext(xtext_module.getMainBlock()));
        // }
        return result;
    }
}
