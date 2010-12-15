package abs.frontend.typechecker.locationtypes.infer;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.AsyncCall;
import abs.frontend.ast.Call;
import abs.frontend.ast.Model;
import abs.frontend.ast.NewExp;
import abs.frontend.ast.SyncCall;
import abs.frontend.ast.ThisExp;
import abs.frontend.typechecker.Type;
import abs.frontend.typechecker.ext.DefaultTypeSystemExtension;
import abs.frontend.typechecker.locationtypes.LocationType;
import abs.frontend.typechecker.locationtypes.LocationTypeExtension;

public class LocationTypeInferrerExtension extends DefaultTypeSystemExtension {
    Map<LocationTypeVariable, LocationType> results;
    
    public LocationTypeInferrerExtension(Model m) {
        super(m);
    }

    private Set<Constraint> constraints = new HashSet<Constraint>();
    
    public Set<Constraint> getConstraints() {
        return constraints;
    }
    
    public Map<LocationTypeVariable, LocationType> getResults() {
        return results;
    }
    
    public static LocationTypeVariable getLV(Type t) {
        LocationTypeVariable ltv = (LocationTypeVariable) t.getMetaData(LocationTypeVariable.VAR_KEY); 
        return ltv;
    }
    
    private LocationTypeVariable adaptTo(LocationTypeVariable expLocType, LocationTypeVariable adaptTo, ASTNode<?> n) {
        LocationTypeVariable tv = LocationTypeVariable.newVar(constraints, n, false);
        constraints.add(Constraint.adaptConstraint(tv, expLocType, adaptTo));
        //System.out.println("Require " + tv + " = " + expLocType + " |>" + adaptTo);
        return tv;
    }
    
    private void adaptAndSet(Type rht, LocationTypeVariable adaptTo) {
        if (adaptTo != LocationTypeVariable.ALWAYS_NEAR) { // Optimization
            LocationTypeVariable rhtlv = getLV(rht);
            LocationTypeVariable tv = adaptTo(rhtlv, adaptTo, null);
            annotateVar(rht, tv);
        }
    }
    
    private void annotateVar(Type t, LocationTypeVariable tv) {
        t.addMetaData(LocationTypeVariable.VAR_KEY,tv);
    }
    /*
    private void setConstantConstraint(LocationTypeVariable tv, Type t) {
        if (t.isReferenceType()) {
            LocationType lt = LocationTypeExtension.getLocationTypeFromAnnotations(t);
            if (lt != null) {
                constraints.add(Constraint.constConstraint(tv, lt, Constraint.MUST_HAVE));
            }
        }
    }*/

    private LocationTypeVariable addNewVar(Type t, ASTNode<?> n) {
        LocationTypeVariable ltv = getLV(t);
        if (ltv != null) 
            return ltv;
        LocationType lt = LocationTypeExtension.getLocationTypeFromAnnotations(t);
        LocationTypeVariable tv;
        if (lt != null) {
            tv = LocationTypeVariable.getFromLocationType(lt);
        } else {
            tv = LocationTypeVariable.newVar(constraints, n, true);
        } 
        annotateVar(t, tv);
        return tv;
    }
    
    @Override
    public void checkAssignable(Type adaptTo, Type rht, Type lht, ASTNode<?> n) {
        LocationTypeVariable sub = getLV(rht);
        LocationTypeVariable tv = getLV(lht);
        if (n instanceof NewExp && ((NewExp)n).hasCog()) {
            constraints.add(Constraint.farConstraint(sub, tv));
        } else {
            if (adaptTo != null && getLV(adaptTo) != LocationTypeVariable.ALWAYS_NEAR) { // Optimization
                sub = adaptTo(getLV(rht), getLV(adaptTo), null);
            }
            constraints.add(Constraint.subConstraint(sub, tv));
        } 
    }

    @Override
    public void annotateType(Type t, ASTNode<?> originatingNode, ASTNode<?> typeNode) {
        if (originatingNode instanceof SyncCall) {
        } else
        if (originatingNode instanceof AsyncCall) {
            AsyncCall call = (AsyncCall)originatingNode;
            adaptAndSet(t, getLV(call.getCallee().getType()));
        } else
        if (originatingNode instanceof ThisExp) {
            annotateVar(t, LocationTypeVariable.ALWAYS_NEAR);
        } else
        if (originatingNode instanceof NewExp) {
            NewExp newExp = (NewExp)originatingNode;
            LocationTypeVariable ltv;
            if (newExp.hasCog()) {
                ltv = LocationTypeVariable.ALWAYS_FAR;
            } else {
                ltv = LocationTypeVariable.ALWAYS_NEAR;
            }
            annotateVar(t, ltv);
        } else {
            if (t.isReferenceType()) {
                addNewVar(t, typeNode);
            } 
            if (t.isNullType()) {
                annotateVar(t, LocationTypeVariable.ALWAYS_BOTTOM);
            }
        }
    }

    @Override
    public void checkMethodCall(Call call) {
        LocationTypeVariable lv = getLV(call.getCallee().getType());
        if (call instanceof SyncCall) {
            //checkEq(lv, LocationTypeVariable.ALWAYS_NEAR, Constraint.SHOULD_HAVE);
            constraints.add(Constraint.constConstraint(lv, LocationType.NEAR, Constraint.SHOULD_HAVE));
        } else {
        //checkNeq(getLV(call.getCallee().getType()), LocationTypeVariable.ALWAYS_BOTTOM, Constraint.SHOULD_HAVE);
            constraints.add(Constraint.constConstraint(lv, LocationType.ALLVISTYPES, Constraint.SHOULD_HAVE));
        }
    }

  private void checkEq(LocationTypeVariable ltv1, LocationTypeVariable ltv2, Integer importance) {
        constraints.add(Constraint.eqConstraint(ltv1, ltv2, importance));
    }

    @Override
    public void checkEq(Type t1, Type t2) {
        checkEq(getLV(t1), getLV(t2), Constraint.SHOULD_HAVE);
    }
    
    @Override
    public void finished() {
        results = new SatGenerator(constraints).generate(errors);
        if (errors.isEmpty()) {
            SemanticErrorList sel = new SemanticErrorList();
            model.getTypeExt().unregister(this);
            model.getTypeExt().register(new LocationTypeExtension(model, this));
            model.typeCheck(sel);
            errors.addAll(sel);
        }
     }
}
