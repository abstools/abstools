package abs.frontend.analyser;

import java.util.ArrayList;

public class SemanticErrorList extends ArrayList<SemanticError> {
    public SemanticError getFirst() {
        return get(0);
    }
}
