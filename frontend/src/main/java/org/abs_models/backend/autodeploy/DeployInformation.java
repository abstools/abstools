package org.abs_models.backend.autodeploy;

import org.abs_models.frontend.ast.*;
import org.abs_models.frontend.ast.List;

import java.io.PrintWriter;
import java.util.*;

public class DeployInformation {

  private Map<String, DeployInformationClass> _map;
  private Map<String, Set<String>> _extends;
  private Map<String, Set<String>> _implements;

  public DeployInformation() {
    _map = new HashMap<>();
    _extends = new HashMap<>();
    _implements = new HashMap<>();
  }

  public void extractInformation(Model model) {
    extractHierarchy(model);
    extractDeployInformationClasses(model);
  }

  private void extractHierarchy(Model model) {
    for (Decl decl : model.getDecls()) {
      List<InterfaceTypeUse> list = null;
      if (decl instanceof InterfaceDecl) {
        list = ((InterfaceDecl) decl).getExtendedInterfaceUseList();
        if(list != null) {
          Set<String> extended = new HashSet<>();
          for (InterfaceTypeUse use : list) {
            extended.add(use.getType().getQualifiedName());
          }
          _extends.put(decl.getType().getQualifiedName(), extended);
        }
      }
      if (decl instanceof ClassDecl) {
        list =  ((ClassDecl) decl).getImplementedInterfaceUseList();
        if(list != null) {
          Set<String> extended = new HashSet<>();
          for (InterfaceTypeUse use : list) {
            extended.add(use.getType().getQualifiedName());
          }
          _implements.put(decl.getType().getQualifiedName(), extended);
        }
      }
    }
  }

  private void extractDeployInformationClasses(Model model) {
    int i = 0;
    for (Decl decl : model.getDecls()) {
      if (decl instanceof ClassDecl) {
        DeployInformationClass dic = new DeployInformationClass(((ClassDecl) decl).getParamList());
        for(Annotation ann: ((ClassDecl) decl).getAnnotationListNoTransform()) {
          if(ann instanceof TypedAnnotation) {
            System.out.println(i++ + ": \"" + ann.getType().getSimpleName()
                    + "\" vs \"" + ((TypedAnnotation)ann).getAccess().getType().getSimpleName()
                    + "\" vs \"" + ((TypeUse)((TypedAnnotation)ann).getAccess()).getName() + "\"");
            if(((TypeUse)((TypedAnnotation)ann).getAccess()).getName().equals("Deploy")) {
              dic.addAnn(ann.getValue());
            }
          }
        }
        if(!dic.isEmpty()) { _map.put(decl.getType().getQualifiedName(), dic); }
      }
    }
  }

  public void generateJSON(PrintWriter f) {
    f.write("{\n");
    f.write("  \"classes\": [\n");
    Iterator<Map.Entry<String, DeployInformationClass>> iClass = _map.entrySet().iterator();
    while(iClass.hasNext()) {
      Map.Entry<String, DeployInformationClass> entry = iClass.next();
      System.out.println("Generating JSON for the class \"" + entry.getKey() + "\"");
      entry.getValue().generateJSON(entry.getKey(), f);
      if(iClass.hasNext()) f.write(",\n");
    }
    f.write("  ],\n");
    f.write("  \"hierarchy\": {\n");
    Iterator<String> iClassName = _map.keySet().iterator();
    while(iClassName.hasNext()) {
      String className = iClassName.next();
      f.write("    \"" + className + "\": [");
      Set<String> implemented =  _implements.get(className);
      if ((implemented != null) && (!implemented.isEmpty())) { generateImplemented(implemented, f); }
      f.write("]");
      if (iClassName.hasNext()) { f.write(",\n"); }
      else { f.write("\n  }\n"); }
    }
    f.write("}");
  }

  private void generateImplemented(Set<String> names, PrintWriter f) {
    Iterator<String> iName = names.iterator();
    while(iName.hasNext()) {
      String name = iName.next();
      // BUG: when an interface has the same name as the class
      f.write("\"" + name + "\"");
      Set<String> extended = _extends.get(name);
      if ((extended != null) && (!extended.isEmpty())) {
        f.write(", ");
        generateImplemented(extended, f);
      }
      if (iName.hasNext()) { f.write(", "); }
    }
  }
}
