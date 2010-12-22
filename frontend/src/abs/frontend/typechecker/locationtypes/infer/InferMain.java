package abs.frontend.typechecker.locationtypes.infer;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import abs.backend.java.lib.runtime.ABSFut;
import abs.backend.java.lib.types.ABSBool;
import abs.backend.java.lib.types.ABSInteger;
import abs.backend.java.lib.types.ABSString;
import abs.backend.java.lib.types.ABSUnit;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.DataConstructor;
import abs.frontend.ast.DataTypeDecl;
import abs.frontend.ast.DataTypeUse;
import abs.frontend.ast.Decl;
import abs.frontend.ast.FunctionDecl;
import abs.frontend.ast.InterfaceDecl;
import abs.frontend.ast.Model;
import abs.frontend.ast.Name;
import abs.frontend.ast.TypeUse;
import abs.frontend.parser.Main;
import abs.frontend.typechecker.BoundedType;
import abs.frontend.typechecker.DataTypeType;
import abs.frontend.typechecker.InterfaceType;
import abs.frontend.typechecker.Type;
import abs.frontend.typechecker.TypeParameter;
import abs.frontend.typechecker.UnionType;

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
        ltie.writeInferenceResultsBack();

    }

}
