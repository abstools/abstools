/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.configurator.visualizer;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.PrintWriter;
import java.util.ArrayList;

import abs.frontend.ast.CompilationUnit;
import abs.frontend.ast.FExt;
import abs.frontend.ast.FNode;
import abs.frontend.ast.FeatureDecl;
import abs.frontend.ast.List;
import abs.frontend.ast.Model;

public class FMVisualizer {

    public List<FeatureDecl> lFD = new List<FeatureDecl>();

    public List<FeatureDecl> lFD1 = new List<FeatureDecl>();

    List<FExt> lFE = new List<FExt>();

    List<FExt> lFE1 = new List<FExt>();

    public static ArrayList<String> arlNames = new ArrayList<String>();

    public static ArrayList<String> arlAttributes = new ArrayList<String>();

    public boolean isOpt;

    public Boolean ParseMicroTVLFile(final Model m)
    {
        Boolean IsFileGeneratedSuccessfully = false;
        try
        {
            List<CompilationUnit> lsCompilationUnits = new List<CompilationUnit>();
            List<FNode> lsFnode = new List<FNode>();

            lsCompilationUnits = m.getCompilationUnits();

            for (CompilationUnit compilationUnit : lsCompilationUnits)
            {
                if(!compilationUnit.getName().contains(".abs"))
                {
                    lFE = compilationUnit.getFExtList();
                    lFD = compilationUnit.getFeatureDeclList();
                }

                for (FeatureDecl oFD : lFD) {
                    lFD1.add(oFD.copy());

                    if(oFD.hasGroup())
                    {
                        lsFnode = oFD.getGroup().getFNodeList();
                        for (FNode oFnode : lsFnode) {
                            //System.out.print("\nFeature Declaration Child Name: " + oFnode.getFeat().getName() + "\n");
                            lFD.add(oFnode.getFeatureDecl().copy());
                        }
                    }
                    else
                    {
                        //System.out.print("\n Else part for this Feature Declaration: " + oFD.getName());
                    }
                }
            }

            boolean IsRoot = true;
            for (FeatureDecl oFD : lFD1)
            {
                if(IsRoot)
                {
                    System.out.print("\nFD : " + oFD.getName());
                    PrintWriter writer = new PrintWriter("hats\\FMSource.xml");
                    writer.print("");
                    writer.close();
                    WriteToXMLFile("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n\t<featureModel chosenLayoutAlgorithm=\"1\">\n\t\t<struct>");
                    Recursion(oFD);
                    WriteToXMLFile("\n\t\t</struct>\n\t\t<constraints/>\n\t\t<comments/>\n\t\t<featureOrder userDefined=\"false\"/>\n\t</featureModel>");
                    IsRoot = false;
                }
            }
            System.out.print("\n\nPre-processing has been done successfully!!");

            IsFileGeneratedSuccessfully = true;
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
        return IsFileGeneratedSuccessfully;
    }

    private Boolean WriteToXMLFile(String sLine)
    {
        String sFilePath = "hats\\FMSource.xml";
        Boolean IsWrittenSuccessfully = false;
        try
        {
            FileWriter fstream = new FileWriter(sFilePath, true);
            BufferedWriter out = new BufferedWriter(fstream);
            out.write(sLine);
            out.close();
            IsWrittenSuccessfully = true;
        }
        catch(Exception e)
        {
            IsWrittenSuccessfully = false;
        }
        return IsWrittenSuccessfully;
    }

    private void Recursion(FeatureDecl oFD)
    {

        List<FNode> lFnode = new List<FNode>();
        try
        {
            if(oFD.hasGroup())
            {
                System.out.print("\nCardinality of Feature " + oFD.getName() + ": " + oFD.getGroup().getCard().toString());

                if(oFD.getGroup().getCard().toString().contains("AllOf()") || oFD.getGroup().getCard().toString().contains("CRange()"))
                {
                    if(isOpt)
                    {
                        WriteToXMLFile("\n\t\t\t<and abstract=\"true\" mandatory=\"false\" name=\""+ oFD.getName() +"\">");
                        isOpt = false;
                    }
                    else
                        WriteToXMLFile("\n\t\t\t<and abstract=\"true\" mandatory=\"true\" name=\""+ oFD.getName() +"\">");
                }

                lFnode = oFD.getGroup().getFNodeList();
                for (FNode fnode : lFnode)
                {
                    //System.out.print("\nOpt : " + fnode.getFeat().getName() + " opt: " + fnode.isOpt());
                    if(fnode.isOpt())
                        isOpt = true;
                    Recursion(fnode.getFeatureDecl());
                }
                WriteToXMLFile("\n\t\t\t</and>");
            }
            else
            {
                if(isOpt)
                {
                    WriteToXMLFile("\n\t\t\t<feature mandatory=\"false\" name=\""+ oFD.getName() +"\"/>");
                    isOpt = false;
                }
                else
                    WriteToXMLFile("\n\t\t\t<feature name=\""+ oFD.getName() +"\"/>");
            }
        }
        catch(Exception e)
        {
            e.printStackTrace();
        }
    }
}
