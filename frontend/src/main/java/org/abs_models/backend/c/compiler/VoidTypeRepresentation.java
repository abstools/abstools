package org.abs_models.backend.c.compiler;

public class VoidTypeRepresentation implements TypeRepresentation {
    @Override
    public boolean isRepresentable() {
        return false;
    }

    @Override
    public String getCType() {
        throw new Error("not representable");
    }
}
