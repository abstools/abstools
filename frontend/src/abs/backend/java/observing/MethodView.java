package abs.backend.java.observing;


/**
 * Represents a method or a block, e.g. an init block or a main block.
 * In case it represents main block getClassView() returns null 
 * @author Jan Schäfer
 *
 */
public interface MethodView {
    /**
     * Returns the class of this method or null if this is a main block
     * @return the class of this method or null if this is a main block
     */
    public ClassView getClassView();
    
    /**
     * Returns the name of this method or block
     * @return the name of this method or block
     */
    public String getName();
}
