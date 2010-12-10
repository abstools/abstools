package abs.backend.prolog;

import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import abs.frontend.ast.*;
import abs.frontend.parser.Main;

public class PrologBackend extends Main {

    private File destDir = new File(".");
    protected File outFile;
    protected PrintStream outStream;
    private String outFilename = "abs.pl";
    public static int awaitId = 0;

    public static void main(final String[] args) {
        PrologBackend prologBE = new PrologBackend();
        try {
            prologBE.absToPrologTerms(args);
            System.out.println("ABS file parsed to Prolog terms in " + prologBE.outFile.getAbsolutePath());
        } catch (Exception e) {
            System.err.println("An error occurred during compilation: " + e.getMessage());

            if (Arrays.asList(args).contains("-debug")) {
                e.printStackTrace();
            }
            System.exit(1);
        } finally {
            prologBE.outStream.close();
        }
    }

    protected void printUsage() {
        super.printUsage();
        System.out.println("Prolog Backend:");
        System.out.println("  -d <dir>     generate files to <dir>");
        System.out.println("  -fn <dir>    output file name");
    }

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
            } else if (arg.equals("-fn")) {
                i++;
                if (i == restArgs.size()) {
                    System.err.println("Please provide a file name");
                    System.exit(1);
                } else {
                    outFilename = args[i];
                }
            } else {
                remainingArgs.add(arg);
            }
        }
        return remainingArgs;
    }

    private void absToPrologTerms(String[] args) throws Exception {
        final Model model = parse(args); // This parses the ABS producing an AST
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
        if (verbose)
            printAST(model, 0);
        outFile = new File(destDir, outFilename);
        outStream = new PrintStream(new BufferedOutputStream(new FileOutputStream(outFile)));
        model.generateProlog(outStream);
    }

    private void printAST(ASTNode<?> ast, int level) {
        if (ast != null) {
            printTab(level);
            System.out.println(level + ":" + ast.getClass().getName() + "@" + ast.getId());
            int n = ast.getNumChild();
            for (int i = 0; i < n; i++)
                printAST(ast.getChild(i), level + 1);
        }
    }

    private void printTab(int n) {
        for (int i = 0; i < n; i++)
            System.out.print("    ");
    }
}
