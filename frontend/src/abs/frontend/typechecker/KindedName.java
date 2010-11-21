package abs.frontend.typechecker;

public class KindedName {
    public enum Kind {
        FUN, CLASS, TYPE_DECL, DATA_CONSTRUCTOR, MODULE;
    }

    private final String name;
    private final Kind kind;

    public KindedName(Kind kind, String name) {
        this.kind = kind;
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public Kind getKind() {
        return kind;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null || !(obj instanceof KindedName))
            return false;
        KindedName o = (KindedName) obj;
        return this.name.equals(o.name) && this.kind.equals(o.kind);
    }

    @Override
    public int hashCode() {
        return name.hashCode() * kind.hashCode();
    }

    @Override
    public String toString() {
        return name + "[" + kind.name() + "]";
    }
}
