/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.typechecker.locationtypes;

import java.util.Map;

import org.abs_models.frontend.analyser.ErrorMessage;
import org.abs_models.frontend.analyser.TypeError;
import org.abs_models.frontend.ast.*;
import org.abs_models.frontend.typechecker.Type;
import org.abs_models.frontend.typechecker.TypeAnnotation;
import org.abs_models.frontend.typechecker.ext.DefaultTypeSystemExtension;
import org.abs_models.frontend.typechecker.ext.AdaptDirection;
import org.abs_models.frontend.typechecker.locationtypes.infer.LocationTypeInferrerExtension;
import org.abs_models.frontend.typechecker.locationtypes.infer.LocationTypeVariable;

public class LocationTypeExtension extends DefaultTypeSystemExtension {

    private LocationType defaultType = LocationType.SOMEWHERE;
    
    private LocationTypeInferrerExtension ltie;
    
    public LocationTypeExtension(Model m) {
        super(m);
    }
    
    public LocationTypeExtension(Model m, LocationTypeInferrerExtension ltie) {
        super(m);
        this.ltie = ltie;
        defaultType = ltie.getDefaultType();
    }

    @Override
    public void checkAssignable(Type adaptTo, AdaptDirection dir, Type rht, Type lht, ASTNode<?> n) {
        LocationType rhtl = getLocationType(rht);
        LocationType lhtl = getLocationType(lht);
        
        if (n instanceof NewExp && !((NewExp)n).hasLocal()) {
            if (!rhtl.isSubtypeOfFarAdapted(lhtl)) {
                errors.add(new TypeError(n,ErrorMessage.LOCATION_TYPE_CANNOT_ASSIGN,rhtl.toString(),lhtl.toString()));
            }
        } else {
            LocationType adaptedRht = rhtl;
            if (adaptTo != null) {
                adaptedRht = rhtl.adaptTo(getLocationType(adaptTo), dir);
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
            adaptTo(t,AdaptDirection.FROM,ac.getCallee().getType());
        } else
        if (origNode instanceof ThisExp) {
            setLocationType(t,LocationType.NEAR);
        } else            
        if (origNode instanceof NewExp) {
            NewExp newExp = (NewExp)origNode;
            LocationType type = LocationType.FAR;
            if (newExp.hasLocal()) {
                type = LocationType.NEAR;
            } 
            setLocationType(t,type);
        } else
        if (origNode instanceof NullExp) {
            setLocationType(t, LocationType.BOTTOM);
        } else if (t.isReferenceType()) {
            setAnnotatedType(t, origNode);
        }
        
    }

    private void setAnnotatedType(Type t, ASTNode<?> origNode) {
        try {
            LocationType lt = getLocationTypeFromAnnotations(t, origNode);
            if (lt == null)
                lt = defaultType;
            setLocationType(t, lt);
        } catch (LocationTypeCheckerException e) {
            errors.add(e.getTypeError());
        }
    }
    
    public static LocationType getLocationTypeFromAnnotations(Type t) {
        return getLocationTypeFromAnnotations(t, null);
    }
    
    public static LocationType getLocationTypeFromAnnotations(Type t, ASTNode<?> originatingNode) {
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
        /*
        if (originatingNode != null && originatingNode instanceof ConstructorArg) {
            if (res == null || !res.isSomewhere()) {
                throw new LocationTypeCheckerException(new TypeError(originatingNode,ErrorMessage.LOCATION_TYPE_DATACONSTR_MUST_BE_SOMEWHERE, new String[0]));
            }
        }*/
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
        if (ltie != null) {
            Map<LocationTypeVariable, LocationType> locationTypeInferenceResult = ltie.getResults();
            if (locationTypeInferenceResult != null) {
                LocationTypeVariable lv = LocationTypeInferrerExtension.getLV(type);
                if (lv != null) {
                    return defaultIfNull(locationTypeInferenceResult.get(lv));
                }
            }
        }
        return defaultIfNull((LocationType) type.getMetaData(LocationType.LOCATION_KEY));
    }
    
    private LocationType defaultIfNull(LocationType l) {
        if (l == null)
          return defaultType;
        else
            return l;
    }

    public void setLocationType(Type type, LocationType lt) {
        type.addMetaData(LocationType.LOCATION_KEY, lt);
    }
    
    public void adaptTo(Type type, AdaptDirection dir, Type to) {
        setLocationType(type,getLocationType(type).adaptTo(getLocationType(to), dir));
    }
}
