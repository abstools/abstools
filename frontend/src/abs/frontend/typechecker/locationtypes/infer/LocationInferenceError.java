package abs.frontend.typechecker.locationtypes.infer;

import abs.frontend.analyser.SemanticError;
import abs.frontend.ast.Model;

public class LocationInferenceError extends SemanticError {

    public LocationInferenceError() {
        
    }
    
    @Override
    public String getMessage() {
        return "Location Type Inference failed, could not find a valid typing";
    }

    @Override
    public int getColumn() {
        return 0;
    }

    @Override
    public int getLine() {
        return 0;
    }
    
    @Override
    public String getFileName() {
        return "";
    }

    
}
