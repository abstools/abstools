package abs.frontend.typechecker.locationtypes;

import java.util.Map;

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
import abs.frontend.typechecker.locationtypes.infer.LocationTypeInferrerExtension;
import abs.frontend.typechecker.locationtypes.infer.LocationTypeVariable;

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
        
        if (n instanceof NewExp && ((NewExp)n).hasCog()) {
            if (!rhtl.isSubtypeOfFarAdapted(lhtl)) {
                errors.add(new TypeError(n,ErrorMessage.LOCATION_TYPE_CANNOT_ASSIGN,rhtl.toString(),lhtl.toString()));
            }
        } else {
            LocationType adaptedRht = rhtl;
            if (adaptTo != null) {
                adaptedRht = rhtl.adaptTo(getLocationType(adaptTo));
            }
            if (!adaptedRht.isSubtypeOf(lhtl)) {
                errors.add(new TypeError(n,ErrorMessage.LOCATION_TYPE_CANNOT_ASSIGN,adaptedRht.toString(),lhtl.toString()));
            }
        }
        
    }

    @Override
    public void annotateType(Type t, ASTNode<?> origNode, ASTNode<?> typeNode) {
        if (origNode instanceof AsyncCall) {
            AsyncCall ac = (AsyncCall) origNode;
            adaptTo(t,ac.getCallee().getType());
        } else
        if (origNode instanceof ThisExp) {
            setLocationType(t,LocationType.NEAR);
        } else            
        if (origNode instanceof NewExp) {
            NewExp newExp = (NewExp)origNode;
            LocationType type = LocationType.NEAR;
            if (newExp.hasCog()) {
                type = LocationType.FAR;
            } 
            setLocationType(t,type);
        } else
        if (origNode instanceof NullExp) {
            setLocationType(t, LocationType.BOTTOM);
        } else if (t.isReferenceType()) {
            setAnnotatedType(t);
        }
        
    }

    private void setAnnotatedType(Type t) {
        try {
            LocationType lt = getLocationTypeFromAnnotations(t);
            if (lt == null)
                lt = defaultType;
            setLocationType(t, lt);
        } catch (LocationTypeCheckerException e) {
            errors.add(e.getTypeError());
        }
    }
    
    public static LocationType getLocationTypeFromAnnotations(Type t) {
        LocationType res = null;
        for (TypeAnnotation an : t.getTypeAnnotations()) {
            if (an.getType().getQualifiedName().equals("ABS.StdLib.LocationType")) {
                DataConstructorExp de = (DataConstructorExp) an.getValue();
                String name = de.getDecl().getName();
                if (res != null) {
                    throw new LocationTypeCheckerException(new TypeError(an.getValue(),ErrorMessage.LOCATION_TYPE_MULTIPLE, new String[0]));
                } else {
                    res = LocationType.createFromName(name); 
                }
            }
        }
        return res;
    }
    

    @Override
    public void checkMethodCall(Call call) {
        LocationType lt = getLocationType(call.getCallee().getType());
        if (lt.isBottom()) {
            errors.add(new TypeError(call,ErrorMessage.LOCATION_TYPE_CALL_ON_BOTTOM,new String[0]));
        } else
        if (call instanceof SyncCall) {
            if (!lt.isNear()) {
                errors.add(new TypeError(call,ErrorMessage.LOCATION_TYPE_SYNC_CALL_ON_NON_NEAR,new String[0]));
            }
        }
    }

    public LocationType getLocationType(Type type) {
        Map<LocationTypeVariable, LocationType> locationTypeInferenceResult = model.getLocationTypeInferenceResult();
        if (locationTypeInferenceResult != null) {
            LocationTypeVariable lv = LocationTypeInferrerExtension.getLV(type);
            if (lv != null) {
                return locationTypeInferenceResult.get(lv);
            }
        }
        return (LocationType) type.getMetaData(LocationType.LOCATION_KEY);
    }
    
    public void setLocationType(Type type, LocationType lt) {
        type.addMetaData(LocationType.LOCATION_KEY, lt);
    }
    
    public void adaptTo(Type type, Type to) {
        setLocationType(type,getLocationType(type).adaptTo(getLocationType(to)));
    }
}
