package abs.backend.java;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import abs.backend.java.lib.runtime.ABSFut;
import abs.backend.java.lib.types.ABSBool;
import abs.backend.java.lib.types.ABSInteger;
import abs.backend.java.lib.types.ABSString;
import abs.backend.java.lib.types.ABSUnit;
import abs.frontend.ast.DataTypeUse;
import abs.frontend.ast.Model;
import abs.frontend.parser.Main;
import abs.frontend.typechecker.BoundedType;
import abs.frontend.typechecker.DataTypeType;
import abs.frontend.typechecker.InterfaceType;
import abs.frontend.typechecker.Type;
import abs.frontend.typechecker.TypeParameter;
import abs.frontend.typechecker.UnionType;

public class JavaBackend extends Main {
    
    public static void main(final String[] args)  {
        
        try {
            new JavaBackend().compile(args);
        } catch(Exception e) {
            System.err.println("An error occurred during compilation: "+e.getMessage());
            
            if (Arrays.asList(args).contains("-debug")) {
                e.printStackTrace();
            }
            
            System.exit(1);
        }
    }
    
    private File destDir = new File(".");
    
    @Override
    public List<String> parseArgs(String[] args) throws Exception {
        List<String> restArgs = super.parseArgs(args);
        List<String> remaindingArgs = new ArrayList<String>();
        
        for (int i = 0; i < restArgs.size(); i++) {
            String arg = restArgs.get(i);
            if (arg.equals("-d")) {
                i++;
                if (i == restArgs.size()) {
                    System.err.println("Please provide a destination directory");
                    System.exit(1);
                } else {
                    destDir = new File(args[i]);
                }
            } else {
                remaindingArgs.add(arg);
            }
        }

        return remaindingArgs;
    }
    
    protected void printUsage() {
        super.printUsage();
        System.out.println("Java Backend:");
        System.out.println("  -d <dir>  generate files to <dir>");
    }
    
    private void compile(String[] args) throws Exception {
        final Model model = parse(args);
        
        if (!destDir.exists()) {
            System.err.println("Destination directory "+destDir.getAbsolutePath()+" does not exist!");
            System.exit(1);
        } 

        if (!destDir.canWrite()) {
            System.err.println("Destination directory "+destDir.getAbsolutePath()+" cannot be written to!");
            System.exit(1);
        } 
    
        compile(model,destDir);
        
    }

    private static void compile(Model m, File destDir) throws IOException {
        File tmpFile = generateJavaToTmpFile(m);
        JavaCompiler.compile("-classpath",System.getProperty("java.class.path"), "-d", destDir.getAbsolutePath(), tmpFile.getAbsolutePath());
    }


    private static File generateJavaToTmpFile(Model model) throws IOException {
        File tmpFile = File.createTempFile("abs", "javabackend");
        PrintStream s = new PrintStream(new BufferedOutputStream(new FileOutputStream(tmpFile)));
        model.generateJava(s);
        s.close();
        //tmpFile.deleteOnExit();
        
        return tmpFile;
    }

    private static final Map<String, String> dataTypeMap = initDataTypeMap();
    
    private static Map<String, String> initDataTypeMap() {
        final Map<String, String> res = new HashMap<String,String>();
        res.put("Int",ABSInteger.class.getName());
        res.put("Bool",ABSBool.class.getName());
        res.put("String",ABSString.class.getName());
        res.put("Fut",ABSFut.class.getName());
        res.put("Unit",ABSUnit.class.getName());
        return res;
    }


    public static String getJavaType(DataTypeUse absType) {
        return getQualifiedString(absType.getType());
    }
    
    public static String getQualifiedString(Type absType) {
   	 String res = null;
   	 if (absType.isDataType()) {
   		 DataTypeType dt = (DataTypeType) absType;
      	 res = dataTypeMap.get(dt.getDecl().getName());
   		 if (res != null)
   			 return res;
   		 StringBuffer sb = new StringBuffer(dt.getDecl().getName());
   		 if (dt.hasTypeArgs() && !containsUnboundedType(dt.getTypeArgs())) {
   		     
   			 sb.append("<");
   			 boolean first = true;
   			 for (Type t : dt.getTypeArgs()) {
   				 if (first) first = false;
   				 else sb.append(',');
   				 sb.append(getQualifiedString(t));
   			 }
   			 sb.append(">");
   		 }
   		 
   		 return sb.toString();
   	 } else if (absType.isInterfaceType()) {
   		 InterfaceType it = (InterfaceType) absType;
   		 return it.getDecl().getName();
   	 } else if (absType.isTypeParameter()) {
   		 TypeParameter tp = (TypeParameter) absType;
   		 return tp.getDecl().getName();
   	 } else if (absType.isBoundedType()) {
   		 BoundedType bt = (BoundedType) absType;
   		 if (bt.hasBoundType())
   			 return getQualifiedString(bt.getBoundType());
   		 return "?";
   	 } else if (absType.isAnyType()) {
   		 return "Object";
   	 } else if (absType.isUnionType()) {
   	     return ((UnionType) absType).getOriginatingClassName();
   	 }

   	 throw new RuntimeException("Type "+absType.getClass().getName()+" not yet supported by Java backend");
    }


    private static boolean containsUnboundedType(List<Type> typeArgs) {
        for (Type t : typeArgs) {
            if (t.isBoundedType()) {
                if (!((BoundedType)t).hasBoundType())
                    return true;
            }
        }
        return false;
    }
    
    
}
