package abs.common;

import java.util.ArrayList;
import java.util.Collection;

import abs.frontend.ast.List;
import abs.frontend.ast.Name;
import abs.frontend.ast.QualifiedName;
import abs.frontend.ast.SimpleName;

public class QualifiedNameUtil {
    public static final Name create(String... args) {
    	if (args.length == 1)
    		return new SimpleName(args[0]);
    	
        List<SimpleName> names = new List<SimpleName>();
        for (String s : args) {
            names = names.add(new SimpleName(s));
        }
        return new QualifiedName(names);
    }

    public static Name createFromDottedString(String s) {
    	return create(s.split("\\.")); 
    }
    
    public static Name create(Collection<String> name) {
    	return create(name.toArray(new String[0]));
    }
    
	public static QualifiedName create(Name moduleName, SimpleName simpleName) {
		return create(moduleName, simpleName.getName());
	}

	public static QualifiedName create(Name moduleName, String name) {
		return create(toList(moduleName), name);
	}

	public static QualifiedName create(java.util.List<String> moduleName, String name) {
		List list = new List();
		for (String s : moduleName) {
			list.add(new SimpleName(s));
		}
		
		list.add(new SimpleName(name));
		return new QualifiedName(list);
	}
	
	public static java.util.List<String> toList(Name n) {
		if (n.isSimple()) {
			ArrayList<String> al = new ArrayList<String>(1);
			al.add(((SimpleName)n).getName());
			return al;
		} else {
			QualifiedName qn = (QualifiedName) n;
			ArrayList<String> al = new ArrayList<String>(1);
			for (SimpleName sn : qn.getSimpleNames()) {
				al.add(sn.getName());
			}
			return al;
		}
	}
	
}
