package autodeploy;

import abs.frontend.ast.*;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.PrintWriter;
import java.util.*;
import java.util.List;

public class DeployInformation {

  private Map<String, DeployInformationClass> _map;
  private Map<String, List<String>> _hierarchy;

  public DeployInformation() {
    _map = new HashMap<String, DeployInformationClass>();
    _hierarchy = new HashMap<String, List<String>>();
  }

  public void extractInformation(Model model) {
    extractHierarchy(model);
    extractDeployInformationClasses(model);
  }

  private void extractHierarchy(Model model) {
    for (Decl decl : model.getDecls()) {
      if (decl instanceof InterfaceDecl) {
        List<String> extended = new LinkedList<String>();
        for (InterfaceTypeUse use : ((InterfaceDecl) decl).getExtendedInterfaceUseList()) {
          extended.add(use.getType().getQualifiedName());
        }
        _hierarchy.put(decl.getType().getQualifiedName(), extended);
      }
    }
  }

  private void extractDeployInformationClasses(Model model) {
    int i = 0;
    for (Decl decl : model.getDecls()) {
      if (decl instanceof ClassDecl) {
        DeployInformationClass dic = new DeployInformationClass(((ClassDecl) decl).getParamList());
        for(Annotation ann: ((ClassDecl) decl).getAnnotationList()) {
          System.out.println(i++);
          if(ann.getType().getSimpleName().equals("Aeolus")) { dic.addAnn(ann.getValue()); }
        }
        _map.put(decl.getType().getQualifiedName(), dic);
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
    f.write("  \"hierarchy\": []\n");
    f.write("}");
  }
}
