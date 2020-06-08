/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.scala;

import org.abs_models.Absc;
import org.abs_models.backend.common.InternalBackendException;
import org.abs_models.common.NotImplementedYetException;
import org.abs_models.common.WrongProgramArgumentException;
import org.abs_models.frontend.delta.DeltaModellingException;
import org.abs_models.frontend.parser.Main;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.ast.ModuleDecl;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.List;

public class ScalaBackend extends Main {

    /**
     * The default relative directory to generate Java files.
     */
    public static final String DEFAULT_OUTPUT_DIRECTORY_NAME = "generated-sources/jabsc";

    private static String FILE_EXTENSION = "scala";
    public static int minVersion = 12;

    public String outputDirectory;
    public String source;

    public static int doMain(Absc args) {
        int result = 0;
        ScalaBackend backEnd = new ScalaBackend();
        backEnd.arguments = args;
        try {
            result = backEnd.compile();
        } catch (NotImplementedYetException e) {
            System.err.println(e.getMessage());
            return 1;
        } catch (Exception e) {
            System.err.println("An error occurred during compilation:\n");

            e.printStackTrace();

            return 1;
        }
        return result;
    }

    public static void printUsage() {
        System.out.println("Scala Backend (-scala):");
        System.out.println("  -scala <source.abs>    the source abs file");
        System.out.println("  [-dep]     generate with DC Support");
        System.out.println();
    }

    @Override
    public List<String> parseArgs(String[] args) throws InternalBackendException {
        List<String> restArgs = super.parseArgs(args);
        List<String> remainingArgs = new ArrayList<String>();

        for (int i = 0; i < restArgs.size(); i++) {
            String arg = restArgs.get(i);
            //System.out.println(arg);
            if (arg.equals("-scala")) {
                // if (i == restArgs.size()) {
                // System.err.println("Please provide a source file");
                // System.exit(1);
                // } else {
                // i++;
                // source = restArgs.get(i);
                // if (!(source.endsWith(".abs"))) {
                // System.err.println(source+ " is not .abs file");
                // printUsage();
                // System.exit(1);
                //
                // remainingArgs.add(source);
                // }
                // }

            } else if (arg.equals("-dep")) {
                ScalaVisitor.withDC = true;
            } else {
                source = arg;
                remainingArgs.add(arg);
            }
        }
        return remainingArgs;
    }

    public int compile() throws Exception {
        final Model model = parse(arguments.files);
        System.out.println("analyzed model");
        if ((model.hasParserErrors() || model.hasErrors() || model.hasTypeErrors())) {
            printErrorMessage();
        }


        source = arguments.files.get(0).getName();
        final Path outputDirPath = createOutputDirectoryPath(outputDirectory, createPath(source));
        Files.createDirectories(outputDirPath);

        ScalaTypeTranslator typeTranslator = new ScalaTypeTranslator();

        for (ModuleDecl pack : model.getModuleDecls()) {
            DefaultScalaWriterSupplier supplier = new DefaultScalaWriterSupplier(PathResolver.DEFAULT_PATH_RESOLVER,
                    pack.getName(), outputDirPath);
            final ScalaVisitor v = new ScalaVisitor(pack.getName(), model, supplier, typeTranslator,
                    outputDirPath);


            final Path packagePath = resolveOutputDirectory(pack.getName(), outputDirPath);
            Files.createDirectories(packagePath);
            final ScalaWriter progWriter = supplier.apply(pack.getName());
            System.out.println("Visiting model");

            v.preVisit(pack);
            System.out.println("Completed model");
        }

        for (ModuleDecl pack : model.getModuleDecls()) {
            if (pack.getName().equals("ABS.StdLib")) {
                DefaultScalaWriterSupplier supplier = new DefaultScalaWriterSupplier(PathResolver.DEFAULT_PATH_RESOLVER,
                        pack.getName(), outputDirPath);
                final ScalaVisitor v = new ScalaVisitor(pack.getName(), model, supplier, typeTranslator,
                        outputDirPath);

                final Path packagePath = resolveOutputDirectory(pack.getName(), outputDirPath);
                Files.createDirectories(packagePath);
                final ScalaWriter progWriter = supplier.apply(pack.getName());
                System.out.println("Visiting model");

                pack.accept(v, progWriter);
                System.out.println("Completed model");
            }
        }

        for (ModuleDecl pack : model.getModuleDecls()) {

            if (!(pack.getName().equals("ABS.StdLib"))) {
                DefaultScalaWriterSupplier supplier = new DefaultScalaWriterSupplier(PathResolver.DEFAULT_PATH_RESOLVER,
                        pack.getName(), outputDirPath);
                final ScalaVisitor v = new ScalaVisitor(pack.getName(), model, supplier, typeTranslator,
                        outputDirPath);

                final Path packagePath = resolveOutputDirectory(pack.getName(), outputDirPath);
                Files.createDirectories(packagePath);
                final ScalaWriter progWriter = supplier.apply(pack.getName());
                System.out.println("Visiting model");

                pack.accept(v, progWriter);
                System.out.println("Completed model");
            }
        }
        final String loc;
        return 0;

    }

    private static Path createPath(String source) {
        return Paths.get(source).toAbsolutePath().normalize();
    }

    /**
     * @param packageName
     * @param source
     * @param outputDirectory
     * @return
     */
    protected Path createSourcePath(String packageName, Path source, Path outputDirectory) {
        final String fullFileName = source.getFileName().toString();
        final int dotIndex = fullFileName.lastIndexOf('.');
        final String fileName = dotIndex == -1 ? fullFileName : fullFileName.substring(0, dotIndex);
        outputDirectory = resolveOutputDirectory(packageName, outputDirectory);
        return outputDirectory.resolve(fileName + "." + FILE_EXTENSION);
    }

    /**
     * @param sourcePath
     * @return
     * @throws IOException
     */
    protected BufferedWriter createWriter(final Path sourcePath) throws IOException {
        return Files.newBufferedWriter(sourcePath, StandardOpenOption.CREATE, StandardOpenOption.WRITE);
    }

    /**
     * @param packageName
     * @param outputDirectory
     * @return
     */
    protected Path resolveOutputDirectory(String packageName, Path outputDirectory) {
        return PathResolver.DEFAULT_PATH_RESOLVER.resolveOutputDirectory(packageName, outputDirectory);
    }

    protected static Path createOutputDirectoryPath(String outputDirectory, Path source) {
        final boolean isSourceDirectory = Files.isDirectory(source) && Files.isReadable(source);
        if (outputDirectory != null) {
            return createPath(outputDirectory);
        } else {
            return isSourceDirectory ? source.getParent().resolve(DEFAULT_OUTPUT_DIRECTORY_NAME).toAbsolutePath()
                    : source.getParent().getParent().resolve(DEFAULT_OUTPUT_DIRECTORY_NAME).toAbsolutePath();
        }
    }

    public static int getScalaVersion() {
        return minVersion;
    }
}
