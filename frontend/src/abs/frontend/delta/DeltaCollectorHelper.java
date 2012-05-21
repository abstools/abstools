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
import java.util.HashMap;
import java.util.Map;

/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 24/06/11
 * Time: 11:56
 * To change this template use File | Settings | File Templates.
 * 
 * TODO: Some documentation, e.g. why are ClassDecls returned?
 * TODO: Encapsulating e.g. in a public static class ResolvedMap extends HashMap<KindedName, ResolvedDeclName> might read nicer.
 */

public class DeltaCollectorHelper {        

  public static Map<KindedName, ResolvedDeclName> getDefinedDeltas(ModuleDecl mod) {
       return getDefinedDeltas(mod, new ArrayList<KindedName>());
   }

   public static Map<KindedName, ResolvedDeclName> getDefinedDeltas(ModuleDecl mod,
           Collection<KindedName> foundDuplicates) {
       HashMap<KindedName, ResolvedDeclName> res = new HashMap<KindedName, ResolvedDeclName>();
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

  public static Map<KindedName, ResolvedDeclName> getVisibleDeltas(ModuleDecl mod) {
      HashMap<KindedName, ResolvedDeclName> res = new HashMap<KindedName, ResolvedDeclName>(mod.getDefinedDeltas());
      res.putAll(mod.getImportedDeltas());
      return res;
  }

  public static Map<KindedName, ResolvedDeclName> getImportedDeltas(ModuleDecl mod) {
      HashMap<KindedName, ResolvedDeclName> res = new HashMap<KindedName, ResolvedDeclName>();

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
                  Map<KindedName, ResolvedDeclName> allNames = TypeCheckerHelper.getAllNames(n.getSimpleName(), md.getExportedDeltas());
                  if (!allNames.isEmpty()) {
                      for (Map.Entry<KindedName, ResolvedDeclName> e : allNames.entrySet()) {
                          res.put(new KindedName(e.getKey().getKind(), n.getModuleName() + "." + e.getKey().getName()),
                                  e.getValue());
                      }
                  } else {
                      throw new TypeCheckerException(new TypeError(n, ErrorMessage.NAME_NOT_EXPORTED_BY_MODULE,
                              n.getSimpleName(), n.getModuleName()));
                  }
              }
          } else if (i instanceof FromImport) {
              FromImport fi = (FromImport) i;
              ModuleDecl md = mod.lookupModule(fi.getModuleName());
              if (md != null) {
                  Map<KindedName, ResolvedDeclName> en = md.getExportedDeltas();
                  for (Name n : fi.getNames()) {
                      TypeCheckerHelper.putKindedNames(n.getString(), en, res);
                      TypeCheckerHelper.putKindedNames(fi.getModuleName() + "." + n.getString(), en, res);
                  }
              }
          }
      }
      return res;
  }

  public static Map<KindedName, ResolvedDeclName> getExportedDeltas(ModuleDecl mod) {
      HashMap<KindedName, ResolvedDeclName> res = new HashMap<KindedName, ResolvedDeclName>();
      for (Export e : mod.getExports()) {
          if (e instanceof StarExport) {
              StarExport se = (StarExport) e;
              if (!se.hasModuleName()) {
                  res.putAll(mod.getDefinedDeltas());
              } else {
                  String moduleName = se.getModuleName().getName();

                  putNamesOfModule(mod, res, moduleName, null);
              }
          } else if (e instanceof FromExport) {
              FromExport fe = (FromExport) e;
              String moduleName = fe.getModuleName();
              for (Name n : fe.getNames()) {
                  String simpleName = n.getSimpleName();
                  putNamesOfModule(mod, res, moduleName, simpleName);
              }
          } else if (e instanceof NamedExport) {
              NamedExport ne = (NamedExport) e;
              for (Name n : ne.getNames()) {
                  String simpleName = TypeCheckerHelper.getSimpleName(n.getString());
                  TypeCheckerHelper.putKindedNames(simpleName, mod.getVisibleDeltas(), res);
                  TypeCheckerHelper.putKindedNames(mod.getName() + "." + simpleName, mod.getVisibleDeltas(), res);
              }
          }

      }
      return res;
  }

  private static void putNamesOfModule(ModuleDecl mod, HashMap<KindedName, ResolvedDeclName> res, String moduleName,
          String simpleNamePattern) {
      for (Map.Entry<KindedName, ResolvedDeclName> entry : mod.getVisibleDeltas().entrySet()) {
          KindedName kn = entry.getKey();
          if (TypeCheckerHelper.isQualified(kn.getName())) {
              if (TypeCheckerHelper.getModuleName(kn.getName()).equals(moduleName)) {
                  String simpleName = TypeCheckerHelper.getSimpleName(kn.getName());
                  if (simpleNamePattern == null || simpleNamePattern.equals(simpleName)) {
                      res.put(new KindedName(kn.getKind(), mod.getName() + "." + simpleName), entry.getValue());
                      res.put(new KindedName(kn.getKind(), simpleName), entry.getValue());
                  }
              }
          }
      }
  }

}
