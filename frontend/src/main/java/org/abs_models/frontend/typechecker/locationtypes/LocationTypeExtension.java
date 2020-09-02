/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.typechecker.locationtypes;

import org.abs_models.frontend.analyser.ErrorMessage;
import org.abs_models.frontend.analyser.TypeError;
import org.abs_models.frontend.ast.*;
import org.abs_models.frontend.typechecker.Type;
import org.abs_models.frontend.typechecker.TypeAnnotation;
import org.abs_models.frontend.typechecker.ext.DefaultTypeSystemExtension;
import org.abs_models.frontend.typechecker.ext.AdaptDirection;

import java.util.Map;

public class LocationTypeExtension extends DefaultTypeSystemExtension {

    private LocationType defaultType = LocationType.SOMEWHERE;

    private Map<LocationTypeVar, LocationType> inferred;

    public LocationTypeExtension(Model m) {
        super(m);
    }

    public LocationTypeExtension(Model m, Map<LocationTypeVar, LocationType> inferred) {
        super(m);
        this.inferred = inferred;
    }

    public void setDefaultType(LocationType defaultType) {
        this.defaultType = defaultType;
    }

    @Override
    public void checkAssignable(Type adaptTo, AdaptDirection dir, Type rht, Type lht, ASTNode<?> n) {
        LocationType lhlt = getLocationType(lht);
        LocationType rhlt = getLocationType(rht);

        LocationType adaptedRhlt = rhlt;
        if (adaptTo != null) {
            adaptedRhlt = rhlt.adaptTo(getLocationType(adaptTo), dir);
        }
        if (!adaptedRhlt.isSubtypeOf(lhlt)) {
            errors.add(new TypeError(n, ErrorMessage.LOCATION_TYPE_CANNOT_ASSIGN, adaptedRhlt.toString(), lhlt.toString()));
        }
    }

    @Override
    public void annotateType(Type t, ASTNode<?> origNode, ASTNode<?> typeNode) {
        if (origNode instanceof AsyncCall) {
            AsyncCall ac = (AsyncCall) origNode;
            adaptTo(t, AdaptDirection.FROM, ac.getCallee().getType());
        } else if (origNode instanceof AwaitAsyncCall) {
            AwaitAsyncCall ac = (AwaitAsyncCall) origNode;
            adaptTo(t, AdaptDirection.FROM, ac.getCallee().getType());
        } else if (origNode instanceof ThisExp) {
            setLocationType(t, LocationType.NEAR);
        } else if (origNode instanceof NewExp) {
            NewExp newExp = (NewExp) origNode;
            LocationType type = LocationType.FAR;
            if (newExp.hasLocal()) {
                type = LocationType.NEAR;
            }
            setLocationType(t, type);
        } else if (origNode instanceof NullExp) {
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
                    throw new LocationTypeCheckerException(new TypeError(an.getValue(), ErrorMessage.LOCATION_TYPE_MULTIPLE, new String[0]));
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
            errors.add(new TypeError(call, ErrorMessage.LOCATION_TYPE_CALL_ON_BOTTOM, new String[0]));
        } else if (call instanceof SyncCall) {
            if (!lt.isNear()) {
                errors.add(new TypeError(call, ErrorMessage.LOCATION_TYPE_SYNC_CALL_ON_NON_NEAR, new String[0]));
            }
        }
    }

    public LocationType getLocationType(Type type) {
        if (inferred != null) {
            LocationTypeVar lv = LocationTypeVar.getVar(type);
            LocationType lt = inferred.get(lv);
            if (lv != null)
                return defaultIfNull(inferred.get(lv));
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
        setLocationType(type, getLocationType(type).adaptTo(getLocationType(to), dir));
    }


}
