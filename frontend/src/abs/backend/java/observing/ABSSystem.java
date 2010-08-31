package abs.backend.java.observing;

public interface ABSSystem {
    
    void registerObjectCreationListener(ObjectCreationListener e);
    void registerObjectCreationListener(String className, ObjectCreationListener e);
}

