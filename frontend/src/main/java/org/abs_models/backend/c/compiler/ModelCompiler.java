package org.abs_models.backend.c.compiler;

import org.abs_models.backend.c.codegen.CFile;
import org.abs_models.backend.c.codegen.CFunctionDecl;
import org.abs_models.backend.c.codegen.CProject;
import org.abs_models.backend.rvsdg.abs.DataConstructNode;
import org.abs_models.backend.rvsdg.abs.SetVarNode;
import org.abs_models.backend.rvsdg.abs.StateOutput;
import org.abs_models.backend.rvsdg.abs.Variable;
import org.abs_models.backend.rvsdg.builder.Function;
import org.abs_models.backend.rvsdg.builder.ModelBuilder;
import org.abs_models.backend.rvsdg.core.*;
import org.abs_models.frontend.typechecker.Type;

import java.io.File;
import java.io.IOException;
import java.util.*;

/**
 * ModelCompiler is responsible for compiling a single module to a single C file.
 */
public class ModelCompiler {
    private final CFile cFile;
    private final TypeRepresentationBuilder types = new TypeRepresentationBuilder();
    private final HashMap<Object, String> values = new HashMap<>();
    private int idGenerator = 0;

    private ModelCompiler(CFile cFile) {
        this.cFile = cFile;
    }

    /**
     * Compiles the model built by the ModelBuilder into C and returns the project.
     */
    public static CProject compile(File outdir, ModelBuilder modelBuilder) throws IOException {
        CProject project = new CProject(outdir);
        project.copyFromResources("/c");

        CFile cFile = project.openFile("main.c");
        ModelCompiler compiler = new ModelCompiler(cFile);
        compiler.compile(modelBuilder);
        cFile.close();
        return project;
    }

    private final Set<Node> compiled = new HashSet<>();

    private void compile(ModelBuilder builder) throws IOException {
        Function func = builder.mainFunction;

        // Go over all nodes and make sure we can represent all types.
        buildTypes(func);

        // Declare all types used.
        types.declare(cFile);

        CFunctionDecl mainFuncDecl = new CFunctionDecl("main", "int", List.of());
        cFile.startFunction(mainFuncDecl);
        for (Variable variable : func.variables) {
            declareVariabel(variable);
        }
        compileOutput(func.state);
        cFile.writeLine("return 0;");
        cFile.stopFunction();
        cFile.close();
    }

    private void buildTypes(Function func) {
        for (Node node : func.region.nodes) {
            for (Output output : node.outputs) {
                types.get(output.type);
            }
        }

        for (Variable variable : func.variables) {
            types.get(variable.type);
        }
    }

    private String compileOutput(Output output) throws IOException {
        if (output instanceof SimpleOutput) {
            compileNode(((SimpleOutput) output).node);
        } else if (output instanceof GammaOutput) {
            compileNode(((GammaOutput) output).gammaNode);
        } else if (output instanceof GammaArgument) {
            // Nothing to do.
        } else if (output instanceof StateOutput) {
            return null;
        } else {
            throw new RuntimeException("Unhandled output: " + output);
        }

        return useValue(output);
    }

    private void compileNode(Node node) throws IOException {
        if (compiled.contains(node)) return;
        compiled.add(node);

        for (Output output : node.outputs) {
            declareOutput(output);
        }

        if (node instanceof DataConstructNode) {
            DataConstructNode dataConstructNode = (DataConstructNode) node;

            ArrayList<String> params = new ArrayList<>();
            for (Input input : node.inputs) {
                params.add(compileInput(input));
            }

            Output result = dataConstructNode.getResult();
            String resultIdent = useValue(result);
            DataTypeRepresentation repr = (DataTypeRepresentation) types.get(result.type);
            DataTypeRepresentation.Variant variant = repr.findVariant(dataConstructNode.dataConstructor);
            repr.setVariant(cFile, resultIdent, variant);
            int fieldIdx = 0;
            for (String param : params) {
                repr.setVariantField(cFile, resultIdent, variant, fieldIdx, param);
                fieldIdx++;
            }
            return;
        }

        if (node instanceof SetVarNode) {
            SetVarNode setVarNode = (SetVarNode) node;
            compileInput(setVarNode.getState());
            String value = compileInput(setVarNode.getValue());
            String ident = useValue(setVarNode.var);
            TypeRepresentation repr = types.get(setVarNode.var.type);
            repr.setValue(cFile, ident, value);
            return;
        }

        if (node instanceof GammaNode) {
            GammaNode gammaNode = ((GammaNode) node);
            String predicate = compileInput(gammaNode.getPredicate());

            for (int i = 1; i < node.inputs.size(); i++) {
                GammaInput input = (GammaInput) node.inputs.get(i);
                String value = compileInput(input);
                for (GammaArgument argument : input.arguments) {
                    values.put(argument, value);
                }
            }

            cFile.writeLine("if (" + predicate + ".tag == 0) {");
            compileGammaBranch(gammaNode, 0);
            cFile.writeLine("} else {");
            compileGammaBranch(gammaNode, 1);
            cFile.writeLine("}");
            return;
        }

        throw new RuntimeException("Unhandled node: " + node);
    }

    private void compileGammaBranch(GammaNode gammaNode, int idx) throws IOException {
        for (Output output : gammaNode.outputs) {
            GammaOutput gammaOutput = (GammaOutput) output;
            String result = compileOutput(gammaOutput.branchOutputs.get(idx));
            TypeRepresentation repr = types.get(output.type);

            if (repr.isRepresentable()) {
                repr.setValue(cFile, useValue(output), result);
            }
        }
    }

    private void declareOutput(Output output) throws IOException {
        declareValue(output.type, output);
    }

    private void declareVariabel(Variable var) throws IOException {
        declareValue(var.type, var);
    }

    private void declareValue(Type type, Object val) throws IOException {
        String ident = "val" + idGenerator++;
        TypeRepresentation repr = types.get(type);
        if (repr.isRepresentable()) {
            cFile.writeLine(repr.getCType() + " " + ident + ";");
        }
        values.put(val, ident);
    }

    private String useValue(Object val) {
        String ident = values.get(val);
        if (ident == null) throw new RuntimeException("Could not find value: " + val);
        return ident;
    }

    private String compileInput(Input input) throws IOException {
        if (input instanceof SimpleInput) {
            return compileOutput(((SimpleInput) input).output);
        }

        throw new RuntimeException("Unhandled input: " + input);
    }
}
