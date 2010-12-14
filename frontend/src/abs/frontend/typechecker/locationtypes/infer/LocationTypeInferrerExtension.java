package abs.frontend.typechecker.locationtypes.infer;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import abs.frontend.ast.ASTNode;
import abs.frontend.ast.AsyncCall;
import abs.frontend.ast.Call;
import abs.frontend.ast.Model;
import abs.frontend.ast.NewExp;
import abs.frontend.ast.SyncCall;
import abs.frontend.ast.ThisExp;
import abs.frontend.typechecker.DataTypeType;
import abs.frontend.typechecker.Type;
import abs.frontend.typechecker.ext.DefaultTypeSystemExtension;
import abs.frontend.typechecker.locationtypes.LocationType;
import abs.frontend.typechecker.locationtypes.LocationTypeCheckerHelper;

public class LocationTypeInferrerExtension extends DefaultTypeSystemExtension {
    public LocationTypeInferrerExtension(Model m) {
        super(m);
    }

    private Set<Constraint> constraints = new HashSet<Constraint>();
    
    public Set<Constraint> getConstraints() {
        return constraints;
    }
    
    public static LocationTypeVariable getLV(Type t) {
        LocationTypeVariable ltv = (LocationTypeVariable) t.getMetaData(LocationTypeVariable.VAR_KEY); 
        return ltv;
    }
    
    private LocationTypeVariable adaptTo(LocationTypeVariable expLocType, LocationTypeVariable adaptTo, ASTNode<?> n) {
        LocationTypeVariable tv = LocationTypeVariable.newVar(constraints, n);
        constraints.add(Constraint.adaptConstraint(tv, expLocType, adaptTo));
        //System.out.println("Require " + tv + " = " + expLocType + " |>" + adaptTo);
        return tv;
    }
    
    private void adaptAndSet(Type rht, LocationTypeVariable adaptTo) {
        if (rht.isDataType()) {
            DataTypeType dtr = (DataTypeType) rht;
            if (dtr.hasTypeArgs()) {
                for (int i = 0; i < dtr.getTypeArgs().size(); i++) {
                    adaptAndSet(dtr.getTypeArg(i), adaptTo);
                }
            }
        }
        if (rht.isReferenceType()) {
            if (adaptTo != LocationTypeVariable.ALWAYS_NEAR) { // Optimization
                LocationTypeVariable rhtlv = getLV(rht);
                LocationTypeVariable tv = adaptTo(rhtlv, adaptTo, null);
                annotateVar(rht, tv);
            }
        }
    }
    
    private void annotateVar(Type t, LocationTypeVariable tv) {
        t.addMetaData(LocationTypeVariable.VAR_KEY,tv);
    }
    
    private void setConstantConstraint(LocationTypeVariable tv, Type t) {
        if (t.isReferenceType()) {
            LocationType lt = LocationTypeCheckerHelper.getLocationType(t, null);
            if (lt != null) {
                constraints.add(Constraint.constConstraint(tv, lt));
            }
        }
    }

    private LocationTypeVariable addNewVar(Type t, ASTNode<?> n) {
        LocationTypeVariable ltv = getLV(t);
        if (ltv != null) 
            return ltv;
        LocationTypeVariable tv = LocationTypeVariable.newVar(constraints, n);
        setConstantConstraint(tv, t);
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
    public void annotateType(Type t, ASTNode<?> n) {
        if (n instanceof SyncCall) {
        } else
        if (n instanceof AsyncCall) {
            AsyncCall call = (AsyncCall)n;
            adaptAndSet(t, getLV(call.getCallee().getType()));
        } else
        if (n instanceof ThisExp) {
            annotateVar(t, LocationTypeVariable.ALWAYS_NEAR);
        } else
        if (n instanceof NewExp) {
            NewExp newExp = (NewExp)n;
            LocationTypeVariable ltv;
            if (newExp.hasCog()) {
                ltv = LocationTypeVariable.ALWAYS_FAR;
            } else {
                ltv = LocationTypeVariable.ALWAYS_NEAR;
            }
            annotateVar(t, ltv);
        } else {
            if (t.isReferenceType()) {
                addNewVar(t, n);
            } 
            if (t.isNullType()) {
                annotateVar(t, LocationTypeVariable.ALWAYS_BOTTOM);
            }
        }
    }

    @Override
    public void checkMethodCall(Call call) {
        if (call instanceof SyncCall) {
            checkEq(getLV(call.getCallee().getType()), LocationTypeVariable.ALWAYS_NEAR);
        }
    }
    
    private void checkEq(LocationTypeVariable ltv1, LocationTypeVariable ltv2) {
        constraints.add(Constraint.eqConstraint(ltv1, ltv2));
    }

    @Override
    public void checkEq(Type t1, Type t2) {
        checkEq(getLV(t1), getLV(t2));
    }
    
    @Override
    public void finished() {
        Map<LocationTypeVariable, LocationType> generated = new SatGenerator(constraints).generate(errors);
        model.setLocationTypeInferenceResult(generated);
    }
}
