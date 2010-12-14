package abs.frontend.typechecker.locationtypes;

import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.TypeError;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.AsyncCall;
import abs.frontend.ast.Call;
import abs.frontend.ast.DataConstructorExp;
import abs.frontend.ast.Model;
import abs.frontend.ast.NewExp;
import abs.frontend.ast.NullExp;
import abs.frontend.ast.SyncCall;
import abs.frontend.ast.ThisExp;
import abs.frontend.typechecker.Type;
import abs.frontend.typechecker.TypeAnnotation;
import abs.frontend.typechecker.ext.DefaultTypeSystemExtension;

public class LocationTypeExtension extends DefaultTypeSystemExtension {

    private LocationType defaultType = LocationType.SOMEWHERE;
    
    public LocationTypeExtension(Model m) {
        super(m);
    }
    
    public void setDefaultType(LocationType newDefault) {
        defaultType = newDefault;
    }
 
    @Override
    public void checkAssignable(Type adaptTo, Type rht, Type lht, ASTNode<?> n) {
        LocationType rhtl = getLocationType(rht);
        LocationType lhtl = getLocationType(lht);
        LocationType adaptedRht = rhtl;
        if (adaptTo != null) {
            rhtl.adaptTo(getLocationType(adaptTo));
        }
        if (!adaptedRht.isSubtypeOf(lhtl)) {
            errors.add(new TypeError(n,ErrorMessage.LOCATION_TYPE_CANNOT_ASSIGN,adaptedRht.toString(),lhtl.toString()));
        }
    }

    @Override
    public void annotateType(Type t, ASTNode<?> n) {
        if (n instanceof AsyncCall) {
            AsyncCall ac = (AsyncCall) n;
            adaptTo(t,ac.getCallee().getType());
        } else
        if (n instanceof ThisExp) {
            setLocationType(t,LocationType.NEAR);
        } else            
        if (n instanceof NewExp) {
            NewExp newExp = (NewExp)n;
            LocationType type = LocationType.NEAR;
            if (newExp.hasCog()) {
                type = LocationType.FAR;
            } 
            setLocationType(t,type);
        } else
        if (n instanceof NullExp) {
            setLocationType(t, LocationType.BOTTOM);
        } else if (t.isReferenceType()) {
            setAnnotatedType(t);
        }
        
    }

    private void setAnnotatedType(Type t) {
        LocationType lt = getLocationTypeFromAnnotations(t);
        if (lt == null)
            lt = defaultType;
        setLocationType(t, lt);
    }
    
    public static LocationType getLocationTypeFromAnnotations(Type t) {
        for (TypeAnnotation an : t.getTypeAnnotations()) {
            if (an.getType().getQualifiedName().equals("ABS.StdLib.LocationType")) {
                DataConstructorExp de = (DataConstructorExp) an.getValue();
                String name = de.getDecl().getName();
                return LocationType.createFromName(name);
            }
        }
        return null;
    }
    

    @Override
    public void checkMethodCall(Call call) {
        LocationType lt = getLocationType(call.getCallee().getType());
        if (call instanceof SyncCall) {
            if (!lt.isNear()) {
                errors.add(new TypeError(call,ErrorMessage.LOCATION_TYPE_SYNC_CALL_ON_NON_NEAR,new String[0]));
            }
        }
    }

    public LocationType getLocationType(Type type) {
        return (LocationType) type.getMetaData(LocationType.LOCATION_KEY);
    }
    
    public void setLocationType(Type type, LocationType lt) {
        type.addMetaData(LocationType.LOCATION_KEY, lt);
    }
    
    public void adaptTo(Type type, Type to) {
        setLocationType(type,getLocationType(type).adaptTo(getLocationType(to)));
    }
    
}
