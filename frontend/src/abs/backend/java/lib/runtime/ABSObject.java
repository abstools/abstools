package abs.backend.java.lib.runtime;

import java.lang.reflect.Field;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;

import abs.backend.java.lib.types.ABSBool;
import abs.backend.java.lib.types.ABSRef;
import abs.backend.java.lib.types.ABSValue;
import abs.backend.java.observing.COGView;
import abs.backend.java.observing.ClassView;
import abs.backend.java.observing.ObjectObserver;
import abs.backend.java.observing.ObjectView;
import static abs.backend.java.lib.runtime.ABSRuntime.*;

public abstract class ABSObject implements ABSRef {
    private final COG cog;
    protected final long id; 
    
    public ABSObject(long id) {
        this(getCurrentCOG(),id);
    }
    
    public String toString() {
        return getClassName()+" "+id;
    }
    
    public abstract String getClassName();

    protected ABSObject(COG cog, long id) {
        this.cog = cog;
        this.id = id;
    }
    
    public final COG getCOG() {
        return cog;
    }

    protected final void __ABS_checkSameCOG() {
        if (cog != getCurrentCOG()) {
            throw new ABSIllegalSynchronousCallException();
        }
    }
    
    @Override
    public ABSBool eq(ABSValue o) {
        return ABSBool.fromBoolean(this == o);
    }

    @Override
    public ABSBool notEq(ABSValue o) {
        return eq(o).negate();
    }

    @Override
    public boolean isDataType() {
        return false;
    }
    
    @Override
    public boolean isReference() {
        return true;
    }
    
    
    protected volatile ObjectView view;
    public synchronized ObjectView getView() {
        if (view == null) {
            view = new View();
        }
        return view;
    }
    
    protected ABSValue getFieldValue(String fieldName) throws NoSuchFieldException {
        throw new NoSuchFieldException(fieldName); 
    }
    
    private class View implements ObjectView {

        @Override
        public COGView getCOG() {
            return cog.getView();
        }

        @Override
        public ClassView getClassView() {
            return null;
        }

        @Override
        public String getClassName() {
            return ABSObject.this.getClassName();
        }

        @Override
        public ABSValue getFieldValue(String fieldName) throws NoSuchFieldException {
            return ABSObject.this.getFieldValue(fieldName);
        }

        @Override
        public void registerObjectObserver(ObjectObserver l) {
            // TODO Auto-generated method stub
            
        }
        
        @Override
        public String toString() {
            return ABSObject.this.toString();
        }
        
        public List<String> getFieldNames() {
            return ABSObject.this.getFieldNames(); 
        }
        
    }

    public abstract List<String> getFieldNames();
}
