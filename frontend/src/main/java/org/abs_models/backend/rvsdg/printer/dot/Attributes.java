package org.abs_models.backend.rvsdg.printer.dot;

import java.util.ArrayList;

public class Attributes {
    private final ArrayList<String> data = new ArrayList<>();

    public void add(String key, String value) {
        // TODO: Escape value
        data.add(key + "=\"" + value + "\"");
    }

    @Override
    public String toString() {
        return "[" + String.join(",", data) + "]";
    }
}
