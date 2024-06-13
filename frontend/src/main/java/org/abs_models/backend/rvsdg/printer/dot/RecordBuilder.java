package org.abs_models.backend.rvsdg.printer.dot;

/**
 * Builds a record: https://graphviz.org/doc/info/shapes.html#record
 */
public class RecordBuilder {
    private final StringBuilder stringBuilder = new StringBuilder();
    private boolean needsPipe = false;

    @Override
    public String toString() {
        return stringBuilder.toString();
    }

    private static String escapeLabel(String label) {
        return label.replaceAll("<", "\\\\<").replaceAll(">", "\\\\>");
    }

    public RecordBuilder entry(String label) {
        return entry(null, label);
    }

    public RecordBuilder entry(String port, String label) {
        if (needsPipe) addPipe();
        if (port != null) {
            stringBuilder.append("<").append(port).append("> ");
        }
        stringBuilder.append(label);
        needsPipe = true;
        return this;
    }

    public RecordBuilder nest() {
        if (needsPipe) addPipe();
        stringBuilder.append("{");
        needsPipe = false;
        return this;
    }

    public RecordBuilder unnest() {
        stringBuilder.append("}");
        needsPipe = true;
        return this;
    }

    private void addPipe() {
        stringBuilder.append("|");
    }
}
