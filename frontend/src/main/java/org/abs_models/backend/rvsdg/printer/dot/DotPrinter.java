package org.abs_models.backend.rvsdg.printer.dot;

import org.abs_models.backend.rvsdg.abs.StateOutput;
import org.abs_models.backend.rvsdg.core.*;

import java.io.IOException;
import java.io.Writer;
import java.util.HashMap;
import java.util.Map;

public class DotPrinter {
    private final Writer writer;
    private final Map<Object, String> idMapping = new HashMap<>();
    private int idNonce = 0;

    private DotPrinter(Writer writer) {
        this.writer = writer;
    }

    public static void print(Writer writer, Region region) throws IOException {
        DotPrinter printer = new DotPrinter(writer);
        printer.printHeader();
        printer.printRegion(region);
        printer.printFooter();
    }

    void printHeader() throws IOException {
        writeLine("digraph {");
        writeExpr("compound=true");
        writeExpr("rankdir=BT");
        writeExpr("node [shape=record]");
    }

    void printFooter() throws IOException {
        writeLine("}");
    }

    void printRegion(Region region) throws IOException {
        for (Node node : region.nodes) {
            if (node instanceof GammaNode) {
                printGammaNode((GammaNode) node);
            } else {
                printNode(node);
            }
        }
    }

    void printNode(Node node) throws IOException {
        // Build node:
        String id = idFor(node);
        RecordBuilder record = new RecordBuilder();
        record.nest();
        addInputsToRecord(record, node);
        record.entry(node.getClass().getSimpleName());
        addOutputsToRecord(record, node);
        record.unnest();

        Attributes nodeAttrs = new Attributes();
        nodeAttrs.add("label", record.toString());
        writeExpr(id + nodeAttrs.toString());

        // Add edges for input:
        for (int i = 0; i < node.inputs.size(); i++) {
            String source = id + ":" + "i" + i;
            Input input = node.inputs.get(i);
            printInputEdge(source, input);
        }
    }

    void printInputEdge(String source, Input input) throws IOException {
        if (input instanceof SimpleInput) {
            String target = outputEdgeName(((SimpleInput) input).output);
            writeExpr(source + " -> " + target);
        } else {
            throw new RuntimeException("Unknown input: " + input);
        }
    }

    String outputEdgeName(Output output) {
        if (output instanceof SimpleOutput) {
            SimpleOutput simpleOutput = (SimpleOutput) output;
            return idFor(simpleOutput.node) + ":" + "o" + simpleOutput.idx + ":s";
        } else if (output instanceof GammaArgument) {
            GammaArgument arg = (GammaArgument) output;
            GammaNode gammaNode = arg.input.gammaNode;
            return idFor(gammaNode) + "_context_" + arg.regionIdx + ":" + "i" + arg.input.inputIdx;
        } else if (output instanceof GammaOutput) {
            GammaOutput gammaOutput = (GammaOutput)output;
            return idFor(gammaOutput.gammaNode) + ":" + "o" + gammaOutput.idx + ":s";
        } else if (output instanceof StateOutput) {
            return "State";
        } else {
            throw new RuntimeException("Unknown output: " + output);
        }
    }

    void printGammaNode(GammaNode node) throws IOException {
        String id = idFor(node);

        // We create a separate node for the inputs only since these need to be connected to the subregions.
        String startId = id + "_start";

        RecordBuilder startRecord = new RecordBuilder();
        startRecord.nest();
        addInputsToRecord(startRecord, node);
        startRecord.entry("main", "GammaNode");
        startRecord.unnest();

        Attributes startAttrs = new Attributes();
        startAttrs.add("label", startRecord.toString());
        writeExpr(startId + startAttrs.toString());

        for (int i = 0; i < node.getBranchCount(); i++) {
            Region branchRegion = node.branchRegions.get(i);
            writeLine("subgraph cluster_" + id + i + " {");

            String contextId = id + "_context_" + i;

            RecordBuilder argumentRecord = new RecordBuilder();
            argumentRecord.entry(String.valueOf(i));
            for (int j = 1; j < node.inputs.size(); j++) {
                argumentRecord.entry("i" + j, "I");
            }
            Attributes attrs = new Attributes();
            attrs.add("label", argumentRecord.toString());
            writeExpr(contextId + attrs.toString());

            printRegion(branchRegion);

            writeLine("}");

            writeExpr(contextId + " -> " + startId + ":main");
        }

        RecordBuilder endRecord = new RecordBuilder();
        endRecord.nest();
        addOutputsToRecord(endRecord, node);
        endRecord.unnest();

        Attributes endAttrs = new Attributes();
        endAttrs.add("label", endRecord.toString());
        writeExpr(id + endAttrs.toString());

        // Add edges for input:
        for (int i = 0; i < node.inputs.size(); i++) {
            String source = startId + ":" + "i" + i;
            Input input = node.inputs.get(i);
            printInputEdge(source, input);
        }

        // Connect outputs:
        for (int i = 0; i < node.outputs.size(); i++) {
            GammaOutput gammaOutput = (GammaOutput) node.outputs.get(i);
            String source = id + ":" + "o" + i;
            for (Output branchOutput : gammaOutput.branchOutputs) {
                String target = outputEdgeName(branchOutput);
                writeExpr(source + " -> " + target);
            }
        }
    }

    private void addInputsToRecord(RecordBuilder record, Node node) {
        if (node.inputs.isEmpty()) return;

        record.nest();
        for (int i = 0; i < node.inputs.size(); i++) {
            record.entry("i" + i, "I");
        }
        record.unnest();
    }

    private void addOutputsToRecord(RecordBuilder record, Node node) {
        if (node.outputs.isEmpty()) return;

        record.nest();
        for (int i = 0; i < node.outputs.size(); i++) {
            record.entry("o" + i, "O");
        }
        record.unnest();
    }

    private String idFor(Object obj) {
        String id = idMapping.get(obj);
        if (id == null) {
            id = "obj_" + ++idNonce;
            idMapping.put(obj, id);
        }
        return id;
    }

    private void write(String text) throws IOException {
        writer.write(text);
    }

    private void writeLine(String text) throws IOException {
        write(text);
        write("\n");
    }

    private void writeExpr(String text) throws IOException {
        write(text);
        write(";\n");
    }
}
