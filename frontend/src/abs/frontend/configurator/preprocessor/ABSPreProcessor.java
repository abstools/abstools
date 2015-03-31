/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.configurator.preprocessor;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import abs.frontend.ast.AttrConstraints;
import abs.frontend.ast.Attribute;
import abs.frontend.ast.CompilationUnit;
import abs.frontend.ast.Constr;
import abs.frontend.ast.FExt;
import abs.frontend.ast.FNode;
import abs.frontend.ast.FeatureDecl;
import abs.frontend.ast.Group;
import abs.frontend.ast.List;
import abs.frontend.ast.Model;


/**
 * @author Ajit
 *
 */
public class ABSPreProcessor {

    public List<FeatureDecl> lFD = new List<FeatureDecl>();

    public List<FeatureDecl> lFD1 = new List<FeatureDecl>();

    List<FExt> lFE = new List<FExt>();

    List<FExt> lFE1 = new List<FExt>();

    public static ArrayList<String> arlNames = new ArrayList<String>();

    public static ArrayList<String> arlAttributes = new ArrayList<String>();

    //Ajit
    public void preProcessModel(Model m) throws ParserConfigurationException, FileNotFoundException
    {
        {
            List<CompilationUnit> lsCompilationUnits = new List<CompilationUnit>();

            List<FNode> lsFnode = new List<FNode>();

            lsCompilationUnits = m.getCompilationUnits();
            arlAttributes = GetXmlTagValues("Attribute");

            for (CompilationUnit compilationUnit : lsCompilationUnits)
            {
                if(!compilationUnit.getName().contains(".abs"))
                {
                lFE = compilationUnit.getFExtList();
                lFD = compilationUnit.getFeatureDeclList();
                }

                for(FExt oFeatureExt : lFE)
                {
                    lFE1.add(oFeatureExt.copy());
                    System.out.print("\n Feature Ext : " + oFeatureExt.getName());
                }

                for (FeatureDecl oFD : lFD) {
                    lFD1.add(oFD.copy());
                    //System.out.print("\n Feature Declaration: " + oFD.getName());
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

            boolean IsExtensionPresent = false;
            for (FeatureDecl oFD : lFD1)
            {
                IsExtensionPresent = false;
                for(FExt oFE: lFE1)
                {
                    if(oFE.getName().equals(oFD.getName().toString()))
                    {
                        WriteExistingExtension(oFE, oFD);
                        IsExtensionPresent = true;
                    }
                }
                if(!IsExtensionPresent)
                {
                    WriteMissingExtension(oFD);
                }
            }
            System.out.print("Pre-processing has been done successfully!!");
        }
    }

    //Ajit
    private Boolean WriteToMvtlFile(String sLine)
    {
        //String sFilePath = "E:\\Hiwi\\FM Configurator Code\\output\\extensions.mtvl";
        String sFilePath = "hats\\extensions.mtvl";
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

    //Ajit
    private boolean WriteExistingExtension(FExt oFE, FeatureDecl oFD) throws FileNotFoundException
    {
        try
        {
            AttrConstraints attrconstr = oFE.getAttrConstraints();
            List<Attribute> lAtt = new List<Attribute>();
            List<FNode> lFnode = new List<FNode>();
            lAtt = attrconstr.getAttributeList();

            String sAtt = "";
            for (Attribute att : lAtt) {
                System.out.print("\n Attribute :  " + att.getName());
                sAtt = att.getName();
            }
            WriteToMvtlFile("extension " + oFE.getName() + "{");

            if(oFD.hasGroup())
                {
                    System.out.print("\n" + oFD.getName() + " has a group");
                    System.out.print("\n" + "Cardinality getstart : " + oFD.getGroup().getCard());
                    Group oGroup = oFD.getGroup();
                    lFnode = oGroup.getFNodeList();

                    String sExpression = "";
                    for (Attribute att : lAtt) {
                        if(arlAttributes.contains(att.getName().trim()))
                        {
                            sExpression = "";
                            for (FNode fnode : lFnode) {
                                //System.out.print("\n Fnode: " + fnode.getFeat().getName() + "\n");
                                sExpression = fnode.getFeatureDecl().getName() + "." + att.getName() + " + " + sExpression;
                            }

                        WriteToMvtlFile("\nifout: " + att.getName() + " == 0");
                        sExpression = sExpression.substring(0, sExpression.length() - 2);
                        WriteToMvtlFile("\n" + att.getName() + " = " + sExpression.trim());
                        }
                    }
                }
            else
                {
                    for (Attribute att : lAtt) {
                        if(arlAttributes.contains(att.getName().trim()))
                        {
                            WriteToMvtlFile("\nifout: " + att.getName() + " == 0");
                        }
                    }
                }

            List<Constr> oConstr = new List<Constr>();
            oConstr = oFE.getAttrConstraints().getConstrs();
            WriteToMvtlFile("\n}\n");
        }
        catch (Exception e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        return true;
    }

    //Ajit
    private boolean WriteMissingExtension(FeatureDecl oFD)
    {
        WriteToMvtlFile("extension " + oFD.getName() + "{");
        List<FNode> lFnode = new List<FNode>();
        String sAtt= "cost";

        if(oFD.hasGroup())
        {
            Group oGroup = oFD.getGroup();
            lFnode = oGroup.getFNodeList();
            String sExpression = "";
            for (FNode fnode : lFnode) {
                sExpression = fnode.getFeatureDecl().getName() + "." + sAtt + " + " + sExpression;
            }

            sExpression = sExpression.substring(0, sExpression.length() - 2);
            WriteToMvtlFile("\nifout: " + sAtt + " == 0");
            WriteToMvtlFile("\n" + sAtt + " = " + sExpression.trim());
        }
        else
        {
            WriteToMvtlFile("\nifout: " + sAtt + " == 0");
        }

        WriteToMvtlFile("\n}\n");
        return true;
    }

    //Ajit
    private ArrayList<String> GetXmlTagValues(String sTagName) throws ParserConfigurationException
    {
        try {
            //File file = new File("C:\\app\\FeatureIDE\\workspace\\ABSFrontend\\PreProcessorConfig.xml");
            File file = new File("hats\\PreProcessorConfig.xml");

            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            DocumentBuilder db = dbf.newDocumentBuilder();
            Document doc;

            doc = db.parse(file);

            Element e = doc.getDocumentElement();
            NodeList nodeList = doc.getElementsByTagName(sTagName);
            for (int i = 0; i < nodeList.getLength(); i++) {
                Node node = nodeList.item(i);
                if (node.getNodeType() == Node.ELEMENT_NODE) {
                    Element element = (Element) node;
                    NodeList nodelist = element.getElementsByTagName("Item");
                    System.out.print("\nLength  : " + nodelist.getLength());

                    for(int j = 0; j<nodelist.getLength();j++)
                    {
                        Element element1 = (Element) nodelist.item(j);
                        NodeList fstNm = element1.getChildNodes();
                        System.out.print("\n Item: " + fstNm.item(0).getNodeValue());
                        arlNames.add(fstNm.item(0).getNodeValue());
                    }
                }
            }
        }
        catch (SAXException e1) {
            // TODO Auto-generated catch block
            e1.printStackTrace();
        } catch (IOException e1) {
            // TODO Auto-generated catch block
            e1.printStackTrace();
        }
        return arlNames;
    }
}
