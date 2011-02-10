package abs.frontend.delta;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Collection;
import abs.frontend.ast.ClassModifier;

/*
 * A Delta stores a collection of ClassModifiers for each module
 * These are applied "simultaneously", so an order is not important
 */
public class Delta {

    private String name;
    // mapping module-name -> ClassModifier
    private HashMap<String, Collection<ClassModifier>> modules;
    
    public Delta() {
        modules = new HashMap<String, Collection<ClassModifier>>();
    }

    public Delta(String n) {
        modules = new HashMap<String, Collection<ClassModifier>>();
        name = n;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public boolean appliesTo(String id) {
        return modules.containsKey(id);
    }
    
    /*
     * add a ClassModifier to the collection; id specifies the module name
     */
    public void addClassModifier(String id, ClassModifier cm) {
        if (! modules.containsKey(id))
            modules.put(id, new HashSet<ClassModifier>());
        
        modules.get(id).add(cm);
    }

    /*
     * returns all ClassModifiers that apply to given module name
     */
    public Collection<ClassModifier> getClassModifiers(String id) {
        if (modules.containsKey(id))
            return modules.get(id);
        else
            return Collections.emptySet();
    }

    public String toString() {
        String msg = name + ": ";
        for (String key : modules.keySet())
            msg += key + "=>" + modules.get(key);
        return msg;
    }

}
