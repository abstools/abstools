package org.abs_models.frontend.typechecker.locationtypes;

import org.abs_models.frontend.analyser.SemanticConditionList;
import org.abs_models.frontend.ast.*;
import org.abs_models.frontend.typechecker.Type;
import org.abs_models.frontend.typechecker.ext.AdaptDirection;
import org.abs_models.frontend.typechecker.ext.DefaultTypeSystemExtension;
import org.abs_models.frontend.typechecker.ext.TypeSystemExtension;

import java.util.List;
import java.util.Map;


public class LocationTypeInferenceExtension extends DefaultTypeSystemExtension {
    private final ConstraintCollection constraints = new ConstraintCollection();

    private LocationType defaultType = LocationType.INFER;

    private boolean debug = false;

    public Map<LocationTypeVar, LocationType> getResults() {
        return results;
    }

    private Map<LocationTypeVar, LocationType> results;

    public LocationType getDefaultType() {
        return defaultType;
    }

    public void setDefaultType(LocationType defaultType) {
        this.defaultType = defaultType;
    }

    public void enableDebug() {
        this.debug = true;
        constraints.enableDebug();
    }

    public LocationTypeInferenceExtension(Model m) {
        super(m);
    }

    private LocationTypeVar adaptTo(LocationTypeVar expLocType, AdaptDirection dir, LocationTypeVar adaptTo, ASTNode<?> typeNode, ASTNode<?> originatingNode) {
        LocationTypeVar tv = new LocationTypeVar(typeNode);
        constraints.add(Constraint.adapt(tv, expLocType, dir, adaptTo, originatingNode));
        return tv;
    }

    @Override
    public void checkEq(Type lt, Type t, ASTNode<?> origin) {
        LocationTypeVar lv1 = getVarSafe(lt, origin);
        LocationTypeVar lv2 = getVarSafe(t, origin);
        Constraint c = Constraint.eq(lv1, lv2, origin);
        constraints.add(c);
    }

    @Override
    public void checkAssignable(Type adaptTo, AdaptDirection dir, Type rht, Type lht, ASTNode<?> n) {
        LocationTypeVar lhv = getVarSafe(lht, n);
        LocationTypeVar rhv = getVarSafe(rht, n);

        if (rhv == null) {
            rhv = addNewVar(rht, n, null);
        }

        if (adaptTo != null && getVar(adaptTo) != LocationTypeVar.NEAR) {
            rhv = adaptTo(rhv, dir, getVar(adaptTo), null, n);
        }
        constraints.add(Constraint.sub(rhv, lhv, n));
    }

    private LocationTypeVar addNewVar(Type t, ASTNode<?> originatingNode, ASTNode<?> typeNode) {
        LocationTypeExtension.getLocationTypeFromAnnotations(t); // check consistency of annotations
        LocationTypeVar ltv = getVar(t);
        if (ltv != null)
            return ltv;
        LocationType lt = getLocationTypeOrDefault(t, originatingNode);
        LocationTypeVar tv;
        if (lt.isInfer()) {
            tv = new LocationTypeVar(originatingNode);
        } else {
            tv = LocationTypeVar.getFromLocationType(lt);
        }
        annotateVar(t, tv);
        return tv;
    }

    private void adaptAndSet(Type rht, AdaptDirection dir, LocationTypeVar adaptTo, ASTNode<?> origin) {
        if (adaptTo != LocationTypeVar.NEAR) {
            LocationTypeVar rlv = getVarSafe(rht, origin);
            LocationTypeVar adapted = adaptTo(rlv, dir, adaptTo, null, origin);
            annotateVar(rht, adapted);
        }
    }

    private void annotateVar(Type t, LocationTypeVar lv) {
        LocationTypeVar.setVar(t, lv);
    }

    @Override
    public void annotateType(Type t, ASTNode<?> on, ASTNode<?> typeNode) {
        if (on instanceof SyncCall) {
        } else if (on instanceof AsyncCall) {
            AsyncCall ac = (AsyncCall) on;
            adaptAndSet(t, AdaptDirection.FROM, getVarSafe(ac.getCallee().getType(), on), on);
        } else if (on instanceof AwaitAsyncCall) {
            AwaitAsyncCall ac = (AwaitAsyncCall) on;
            adaptAndSet(t, AdaptDirection.FROM, getVarSafe(ac.getCallee().getType(), on), on);
        } else if (on instanceof ThisExp) {
            annotateVar(t, LocationTypeVar.NEAR);
        } else if (on instanceof NewExp) {
            NewExp ne = (NewExp) on;
            LocationTypeVar lv;
            if (ne.hasLocal()) {
                lv = LocationTypeVar.NEAR;
            } else {
                lv = LocationTypeVar.FAR;
                // TODO
            }
            annotateVar(t, lv);
        } else {
            if (t.isReferenceType()) {
                addNewVar(t, on, typeNode);
            }
            if (t.isNullType()) {
                annotateVar(t, LocationTypeVar.BOTTOM);
            }
        }
    }

    // Fix bug #290: synccalls can be on non-near objects (see
    // https://github.com/abstools/abstools/issues/290)

    // @Override
    // public void checkMethodCall(Call call) {
    //     LocationTypeVar lv = getVarSafe(call.getCallee().getType(), call);
    //     assert lv != null;
    //     if (call instanceof SyncCall) {
    //         constraints.add(Constraint.eq(lv, LocationTypeVar.NEAR, call));
    //     }
    // }

    public LocationTypeVar getVarSafe(Type t, ASTNode<?> n) {
        LocationTypeVar v = getVar(t);

        if (v == null) {
            return addNewVar(t, n, null);
        }

        return v;
    }

    public static LocationTypeVar getVar(Type t) {
        return LocationTypeVar.getVar(t);
    }

    private LocationType getLocationTypeOrDefault(Type t, ASTNode<?> originatingNode) {
        LocationType lt = LocationTypeExtension.getLocationTypeFromAnnotations(t);
        if (lt == null) {
            return defaultType;
        }
        return lt;
    }

    @Override
    public void finished() {
        ConstraintSolver solver = new ConstraintSolver(constraints, debug);
        results = solver.solve();

        if (debug) {
            for (Map.Entry<LocationTypeVar, LocationType> e : results.entrySet()) {
                System.out.println("" + e.getKey() + " := " + e.getValue());
            }
        }

        if (!errors.containsErrors()) {
            SemanticConditionList sel = new SemanticConditionList();
            List<TypeSystemExtension> exts = model.getTypeExt().getTypeSystemExtensionList();
            model.getTypeExt().clearTypeSystemExtensions();
            LocationTypeExtension lte = new LocationTypeExtension(model, results);
            lte.setDefaultType(defaultType);
            model.getTypeExt().register(lte);
            model.typeCheck(sel);
            errors.addAll(sel);
            model.getTypeExt().clearTypeSystemExtensions();
            model.getTypeExt().registerAll(exts);
        }
    }
}
