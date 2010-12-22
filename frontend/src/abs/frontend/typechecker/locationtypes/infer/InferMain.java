package abs.frontend.typechecker.locationtypes.infer;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import abs.common.FileUtils;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.CompilationUnit;
import abs.frontend.ast.Model;
import abs.frontend.parser.Main;
import abs.frontend.typechecker.locationtypes.LocationType;

public class InferMain extends Main {
    
    public static void main(final String... args) {

        try {
            new InferMain().compile(args);
        } catch (Exception e) {
            System.err.println("An error occurred during compilation: " + e.getMessage());
            e.printStackTrace();

            System.exit(1);
        }
    }

    private File destDir = new File(".");

    @Override
    public List<String> parseArgs(String[] args) throws Exception {
        List<String> restArgs = super.parseArgs(args);
        List<String> remainingArgs = new ArrayList<String>();

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
                remainingArgs.add(arg);
            }
        }

        return remainingArgs;
    }

    protected void printUsage() {
        super.printUsage();
        System.out.println("Location Type Inferrer:");
        System.out.println("  -d <dir>     generate files to <dir>\n");
    }

    private void compile(String[] args) throws Exception {
        locationTypeInferenceEnabled = true;
        final Model model = parse(args);
        if (model.hasParserErrors() || model.hasErrors() || model.hasTypeErrors())
            return;

        if (!destDir.exists()) {
            System.err.println("Destination directory " + destDir.getAbsolutePath() + " does not exist!");
            System.exit(1);
        }

        if (!destDir.canWrite()) {
            System.err.println("Destination directory " + destDir.getAbsolutePath() + " cannot be written to!");
            System.exit(1);
        }

        LocationTypeInferrerExtension ltie = (LocationTypeInferrerExtension)model.getTypeExt().getFirstRegisteredTypeExtension(LocationTypeInferrerExtension.class);
        writeInferenceResultsBack(ltie.getResults());

    }
    
    public void writeInferenceResultsBack(Map<LocationTypeVariable, LocationType> results) throws IOException {
        Map<CompilationUnit, List<LocationTypeVariable>> m = new HashMap<CompilationUnit, List<LocationTypeVariable>>();
        for (LocationTypeVariable ltv : results.keySet()) {
            ASTNode<?> node = ltv.getNode();
            if (node == null) continue;
            CompilationUnit cu = node.getCompilationUnit();
            if (cu.getName().equals("ABS.StdLib")) continue;
            List<LocationTypeVariable> list = m.get(cu);
            if (list == null) {
                list = new ArrayList<LocationTypeVariable>();
                m.put(cu, list);
            }
            list.add(ltv);
        }
        
        for (Entry<CompilationUnit, List<LocationTypeVariable>> e : m.entrySet()) {
            CompilationUnit cu = e.getKey();
            List<LocationTypeVariable> l = e.getValue();
            Collections.sort(l, new Comparator<LocationTypeVariable>() {
                
                @Override
                public int compare(LocationTypeVariable o1, LocationTypeVariable o2) {
                    int pos1 = o1.getTypeNode().getAbsolutePosition();
                    int pos2 = o2.getTypeNode().getAbsolutePosition();
                    if (pos1 == -1 || pos2 == -1) {
                        throw new RuntimeException("Absolute position not defined");
                    }
                    return Integer.valueOf(pos1).compareTo(pos2);
                }
            });
            File file = new File(cu.getFileName());
            StringBuilder sb = FileUtils.fileToStringBuilder(file);
            int offset = 0;
            for (LocationTypeVariable ltv : l) {
                int pos = offset + ltv.getTypeNode().getAbsolutePosition();
                String s = results.get(ltv).toAnnoString();
                sb.insert(pos, s);
                offset += s.length();
            }
            FileUtils.writeStringBuilderToFile(sb, file);
        }
    }

}
