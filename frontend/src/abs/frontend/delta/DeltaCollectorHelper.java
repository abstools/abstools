/*
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */

package abs.frontend.delta;

import abs.common.Constants;
import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.TypeError;
import abs.frontend.ast.*;
import abs.frontend.typechecker.*;

import java.util.ArrayList;
import java.util.Collection;

/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 24/06/11
 * Time: 11:56
 * To change this template use File | Settings | File Templates.
 * 
 * TODO: Some documentation, e.g. why are ClassDecls returned?
 */

public class DeltaCollectorHelper {        

  public static ResolvedMap getDefinedDeltas(ModuleDecl mod) {
       return getDefinedDeltas(mod, new ArrayList<KindedName>());
   }

  public static ResolvedMap getDefinedDeltas(ModuleDecl mod,
           Collection<KindedName> foundDuplicates) {
       ResolvedMap res = new ResolvedMap();
       ResolvedModuleName moduleName = new ResolvedModuleName(mod);

       for (Decl d : mod.getDeclList()) {
           if (d instanceof DeltaDecl || d instanceof ClassDecl) { // XXX ClassDecls as well?!
             ResolvedDeclName rn = new ResolvedDeclName(moduleName, d);
             if (res.put(rn.getSimpleName(), rn) != null) 
                 foundDuplicates.add(rn.getSimpleName());
             res.put(rn.getQualifiedName(), rn);
           }
       }

       return res;
   }

  public static ResolvedMap getVisibleDeltas(ModuleDecl mod) {
      ResolvedMap res = new ResolvedMap(mod.getDefinedDeltas());
      res.putAll(mod.getImportedDeltas());
      return res;
  }

  public static ResolvedMap getImportedDeltas(ModuleDecl mod) {
      ResolvedMap res = new ResolvedMap();

      for (Import i : mod.getImports()) {
          if (i instanceof StarImport) {
              StarImport si = (StarImport) i;
              ModuleDecl md = mod.lookupModule(si.getModuleName());
              if (md == null) {
                  if (!Constants.STDLIB_NAME.equals(si.getModuleName()))
                      throw new TypeCheckerException(new TypeError(si, ErrorMessage.MODULE_NOT_RESOLVABLE,
                              si.getModuleName()));
              } else {
                  res.putAll(md.getExportedDeltas());
              }
          } else if (i instanceof NamedImport) {
              NamedImport ni = (NamedImport) i;
              for (Name n : ni.getNames()) {
                  ModuleDecl md = mod.lookupModule(n.getModuleName());
                  if (md == null) {
                      if (!Constants.STDLIB_NAME.equals(n.getModuleName()))
                          throw new TypeCheckerException(new TypeError(n, ErrorMessage.MODULE_NOT_RESOLVABLE,
                                  n.getModuleName()));
                  }
                  res.addAllNames(md.getExportedDeltas(), n);
              }
          } else if (i instanceof FromImport) {
              FromImport fi = (FromImport) i;
              ModuleDecl md = mod.lookupModule(fi.getModuleName());
              if (md != null) {
                  ResolvedMap en = md.getExportedDeltas();
                  for (Name n : fi.getNames()) {
                      res.putKindedNames(n.getString(), en);
                      res.putKindedNames(fi.getModuleName() + "." + n.getString(), en);
                  }
              }
          }
      }
      return res;
  }

  public static ResolvedMap getExportedDeltas(ModuleDecl mod) {
      ResolvedMap res = new ResolvedMap();
      for (Export e : mod.getExports()) {
          if (e instanceof StarExport) {
              StarExport se = (StarExport) e;
              if (!se.hasModuleName()) {
                  res.putAll(mod.getDefinedDeltas());
              } else {
                  String moduleName = se.getModuleName().getName();
                  res.putNamesOfModule(mod, moduleName, null);
              }
          } else if (e instanceof FromExport) {
              FromExport fe = (FromExport) e;
              String moduleName = fe.getModuleName();
              for (Name n : fe.getNames()) {
                  String simpleName = n.getSimpleName();
                  res.putNamesOfModule(mod, moduleName, simpleName);
              }
          } else if (e instanceof NamedExport) {
              NamedExport ne = (NamedExport) e;
              for (Name n : ne.getNames()) {
                  String simpleName = TypeCheckerHelper.getSimpleName(n.getString());
                  res.putKindedNames(simpleName, mod.getVisibleDeltas());
                  res.putKindedNames(mod.getName() + "." + simpleName, mod.getVisibleDeltas());
              }
          }
      }
      return res;
  }
}
