package abs.backend.java.lib.types;


public interface ABSFunction<R> extends ABSType {
    R apply();
}
