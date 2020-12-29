package org.abs_models.backend.c.compiler;

import org.abs_models.backend.c.codegen.CFile;
import org.abs_models.backend.c.codegen.CFunctionDecl;
import org.abs_models.backend.c.codegen.CProject;
import org.abs_models.backend.rvsdg.abs.*;
import org.abs_models.backend.rvsdg.builder.Function;
import org.abs_models.backend.rvsdg.builder.ModelBuilder;
import org.abs_models.backend.rvsdg.core.*;
import org.abs_models.frontend.typechecker.Type;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.*;

/**
 * ModelCompiler is responsible for compiling a single module to a single C file.
 */
public class ModelCompiler {
    private final CFile cFile;
    private final TypeRepresentationBuilder types = new TypeRepresentationBuilder();
    private final Region mainRegion = new Region();
    private int idGenerator = 0;

    static private class Register {
        TypeRepresentation repr;
        String ident;

        public Register(TypeRepresentation repr, String ident) {
            this.repr = repr;
            this.ident = ident;
        }
    }

    static private class Region {
        final HashMap<Object, Register> registers = new HashMap<>();
    }

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
        cFile.writeLine("#include \"abslib.h\"");
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
            Register reg = declareVariabel(mainRegion, variable);
            reg.repr.initZero(cFile, reg.ident);
        }
        compileOutput(mainRegion, func.state);
        deinitValues(mainRegion);
        cFile.writeLine("return 0;");
        cFile.stopFunction();
        cFile.close();
    }

    private void deinitValues(Region region) throws IOException {
        for (Register reg : region.registers.values()) {
            if (reg.repr.isRepresentable()) {
                reg.repr.deinit(cFile, reg.ident);
            }
        }
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

    private String compileOutput(Region region, Output output) throws IOException {
        if (output instanceof SimpleOutput) {
            compileNode(region, ((SimpleOutput) output).node);
        } else if (output instanceof GammaOutput) {
            compileNode(region, ((GammaOutput) output).gammaNode);
        } else if (output instanceof GammaArgument) {
            // Nothing to do.
        } else if (output instanceof StateOutput) {
            return null;
        } else {
            throw new RuntimeException("Unhandled output: " + output);
        }

        return useValue(region, output);
    }

    private void compileNode(Region region, Node node) throws IOException {
        if (compiled.contains(node)) return;
        compiled.add(node);

        for (Output output : node.outputs) {
            declareOutput(region, output);
        }

        if (node instanceof DataConstructNode) {
            DataConstructNode dataConstructNode = (DataConstructNode) node;

            ArrayList<String> params = new ArrayList<>();
            for (Input input : node.inputs) {
                params.add(compileInput(region, input));
            }

            Output result = dataConstructNode.getResult();
            String resultIdent = useValue(region, result);
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

        if (node instanceof GetVarNode) {
            GetVarNode getVarNode = (GetVarNode) node;
            compileInput(region, getVarNode.getState());
            String resultIdent = useValue(region, getVarNode.getResult());
            String varIdent = useValue(mainRegion, getVarNode.var);
            TypeRepresentation repr = types.get(getVarNode.var.type);
            repr.initCopy(cFile, resultIdent, varIdent);
            return;
        }

        if (node instanceof SetVarNode) {
            SetVarNode setVarNode = (SetVarNode) node;
            compileInput(region, setVarNode.getState());
            String value = compileInput(region, setVarNode.getValue());
            String ident = useValue(mainRegion, setVarNode.var);
            TypeRepresentation repr = types.get(setVarNode.var.type);
            repr.deinit(cFile, ident);
            repr.initCopy(cFile, ident, value);
            return;
        }

        if (node instanceof GammaNode) {
            GammaNode gammaNode = ((GammaNode) node);
            String predicate = compileInput(region, gammaNode.getPredicate());

            List<Region> regions = List.of(new Region(), new Region());

            for (int i = 1; i < node.inputs.size(); i++) {
                GammaInput input = (GammaInput) node.inputs.get(i);
                String value = compileInput(region, input);
                for (GammaArgument argument : input.arguments) {
                    Register reg = declareOutput(regions.get(argument.regionIdx), argument);
                    if (reg.repr.isRepresentable()) {
                        reg.repr.initCopy(cFile, reg.ident, value);
                    }
                }
            }

            cFile.writeLine("if (" + predicate + ".tag == 1) {");
            compileGammaBranch(regions.get(0), gammaNode, 0);
            deinitValues(regions.get(0));
            cFile.writeLine("} else {");
            compileGammaBranch(regions.get(1), gammaNode, 1);
            deinitValues(regions.get(1));
            cFile.writeLine("}");
            return;
        }

        if (node instanceof PrintNode) {
            PrintNode printNode = (PrintNode) node;
            compileInput(region, printNode.getState());
            String value = compileInput(region, printNode.getValue());
            String funcName = printNode.withNewline ? "absstr_println" : "absstr_print";
            cFile.writeLine(funcName + "(" + value + ");");
            return;
        }

        if (node instanceof StringConcatNode) {
            StringConcatNode stringConcatNode = (StringConcatNode) node;
            String leftIdent = compileInput(region, stringConcatNode.getLeft());
            String rightIdent = compileInput(region, stringConcatNode.getRight());
            String resultIdent = useValue(region, stringConcatNode.getResult());
            cFile.writeLine("absstr_concat(&" + resultIdent + "," + leftIdent + "," + rightIdent + ");");
            return;
        }

        if (node instanceof StringLiteralNode) {
            StringLiteralNode stringLiteralNode = (StringLiteralNode) node;
            String resultIdent = useValue(region, stringLiteralNode.getResult());
            byte[] bytes = stringLiteralNode.content.getBytes(StandardCharsets.UTF_8);
            String cString = cFile.encodeCString(bytes);
            cFile.writeLine("absstr_literal(&" + resultIdent + "," + cString + "," + bytes.length + ");");
            return;
        }

        if (node instanceof IntLiteralNode) {
            IntLiteralNode intLiteralNode = (IntLiteralNode) node;
            String resultIdent = useValue(region, intLiteralNode.getResult());
            byte[] bytes = intLiteralNode.content.getBytes(StandardCharsets.UTF_8);
            String cString = cFile.encodeCString(bytes);
            cFile.writeLine("absint_literal(&" + resultIdent + "," + cString + "," + bytes.length + ");");
            return;
        }

        if (node instanceof ToStringNode) {
            ToStringNode toStringNode = (ToStringNode) node;
            Input value = toStringNode.getValue();
            String valueIdent = compileInput(region, value);
            TypeRepresentation repr = types.get(value.getType());
            String resultIdent = useValue(region, toStringNode.getResult());
            repr.convertToString(cFile, "&" + resultIdent, valueIdent);
            return;
        }

        if (node instanceof ComparisonNode) {
            ComparisonNode comparisonNode = (ComparisonNode) node;
            String leftIdent = compileInput(region, comparisonNode.getLeft());
            String rightIdent = compileInput(region, comparisonNode.getRight());
            TypeRepresentation repr = types.get(comparisonNode.getLeft().getType());
            String resultIdent = useValue(region, comparisonNode.getResult());
            repr.writeCompare(cFile, resultIdent + ".tag", comparisonNode.operator, leftIdent, rightIdent);
            return;
        }

        if (node instanceof BinaryArithmeticNode) {
            BinaryArithmeticNode binNode = (BinaryArithmeticNode) node;
            String leftIdent = compileInput(region, binNode.getLeft());
            String rightIdent = compileInput(region, binNode.getRight());
            String resultIdent = useValue(region, binNode.getResult());
            TypeRepresentation repr = types.get(binNode.getLeft().getType());
            String postfix = cFile.encodeBinaryArithmeticOperator(binNode.operator);
            cFile.writeLine(repr.getCType() + "_" + postfix + "(&" + resultIdent + ", " + leftIdent + ", " + rightIdent + ");");
            return;
        }

        if (node instanceof ToRationalNode) {
            ToRationalNode toRatNode = (ToRationalNode) node;
            String numIdent = compileInput(region, toRatNode.getNumerator());
            String denIdent = compileInput(region, toRatNode.getDenominator());
            String resultIdent = useValue(region, toRatNode.getResult());
            cFile.writeLine("absrat_initfromints(&" + resultIdent + ", " + numIdent + ", " + denIdent + ");");
            return;
        }

        if (node instanceof IntToRationalNode) {
            IntToRationalNode toRatNode = (IntToRationalNode) node;
            String intIdent = compileInput(region, toRatNode.getInt());
            String resultIdent = useValue(region, toRatNode.getResult());
            cFile.writeLine("absrat_initfromint(&" + resultIdent + ", " + intIdent + ");");
            return;
        }

        throw new RuntimeException("Unhandled node: " + node);
    }

    private void compileGammaBranch(Region region, GammaNode gammaNode, int idx) throws IOException {
        for (Output output : gammaNode.outputs) {
            GammaOutput gammaOutput = (GammaOutput) output;
            String result = compileOutput(region, gammaOutput.branchOutputs.get(idx));
            TypeRepresentation repr = types.get(output.type);

            if (repr.isRepresentable()) {
                repr.initCopy(cFile, useValue(region, output), result);
            }
        }
    }

    private Register declareOutput(Region region, Output output) throws IOException {
        return declareValue(region, output.type, output);
    }

    private Register declareVariabel(Region region, Variable var) throws IOException {
        return declareValue(region, var.type, var);
    }

    private Register declareValue(Region region, Type type, Object val) throws IOException {
        String ident = "val" + idGenerator++;
        TypeRepresentation repr = types.get(type);
        Register reg = new Register(repr, ident);
        if (repr.isRepresentable()) {
            cFile.writeLine(repr.getCType() + " " + ident + ";");
        }
        region.registers.put(val, reg);
        return reg;
    }

    private String useValue(Region region, Object val) {
        Register reg = region.registers.get(val);
        if (reg == null) throw new RuntimeException("Could not find value: " + val);
        return reg.ident;
    }

    private String compileInput(Region region, Input input) throws IOException {
        if (input instanceof SimpleInput) {
            return compileOutput(region, ((SimpleInput) input).output);
        }

        throw new RuntimeException("Unhandled input: " + input);
    }
}
