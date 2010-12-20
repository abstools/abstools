package abs.frontend.typechecker.locationtypes.infer;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.AsyncCall;
import abs.frontend.ast.Block;
import abs.frontend.ast.Call;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.Decl;
import abs.frontend.ast.Exp;
import abs.frontend.ast.FieldDecl;
import abs.frontend.ast.MethodImpl;
import abs.frontend.ast.Model;
import abs.frontend.ast.NewExp;
import abs.frontend.ast.Stmt;
import abs.frontend.ast.SyncCall;
import abs.frontend.ast.ThisExp;
import abs.frontend.ast.VarDecl;
import abs.frontend.typechecker.DataTypeType;
import abs.frontend.typechecker.Type;
import abs.frontend.typechecker.ext.DefaultTypeSystemExtension;
import abs.frontend.typechecker.locationtypes.LocationType;
import abs.frontend.typechecker.locationtypes.LocationTypeExtension;

public class LocationTypeInferrerExtension extends DefaultTypeSystemExtension {
    public static enum LocationTypingPrecision {
        BASIC,
        METHOD_LOCAL_FAR,
        CLASS_LOCAL_FAR
    }
    
    LocationTypingPrecision LOCATION_TYPING_PRECISION = LocationTypingPrecision.CLASS_LOCAL_FAR;
    
    private Map<LocationTypeVariable, LocationType> results;
    private LocationType defaultType = LocationType.INFER;
    private Map<Block, LocationType[]> methodLocalFarTypes = new HashMap<Block, LocationType[]>();
    private Map<ClassDecl, LocationType[]> classLocalFarTypes = new HashMap<ClassDecl, LocationType[]>();
    
    public LocationTypeInferrerExtension(Model m) {
        super(m);
    }
    
    public void setDefaultType(LocationType type) {
        defaultType = type;
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
    
    private LocationTypeVariable adaptTo(LocationTypeVariable expLocType, LocationTypeVariable adaptTo, ASTNode<?> typeNode, ASTNode<?> originatingNode) {
        LocationTypeVariable tv = LocationTypeVariable.newVar(constraints, typeNode, false, getLocalFarTypes(originatingNode));
        constraints.add(Constraint.adaptConstraint(tv, expLocType, adaptTo));
        //System.out.println("Require " + tv + " = " + expLocType + " |>" + adaptTo);
        return tv;
    }
    
    
    private void adaptAndSet(Type rht, LocationTypeVariable adaptTo, ASTNode<?> originatingNode) {
        if (adaptTo != LocationTypeVariable.ALWAYS_NEAR) { // Optimization
            LocationTypeVariable rhtlv = getLV(rht);
            LocationTypeVariable tv = adaptTo(rhtlv, adaptTo, null, originatingNode);
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

    private LocationTypeVariable addNewVar(Type t, ASTNode<?> originatingNode, ASTNode<?> typeNode) {
        LocationTypeVariable ltv = getLV(t);
        if (ltv != null) 
            return ltv;
        LocationType lt = getLocationTypeOrDefault(t);
        LocationTypeVariable tv;
        if (lt.isInfer()) {
            tv = LocationTypeVariable.newVar(constraints, typeNode, true, getLocalFarTypes(originatingNode));
        } else {
            tv = LocationTypeVariable.getFromLocationType(lt);
        } 
        annotateVar(t, tv);
        return tv;
    }
    
    private LocationType getLocationTypeOrDefault(Type t) {
        LocationType lt = LocationTypeExtension.getLocationTypeFromAnnotations(t);
        if (lt == null) {
            return defaultType;
        }
        return lt;
    }

    @Override
    public void checkAssignable(Type adaptTo, Type rht, Type lht, ASTNode<?> n) {
        LocationTypeVariable sub = getLV(rht);
        LocationTypeVariable tv = getLV(lht);
        if (n instanceof NewExp && ((NewExp)n).hasCog()) {
            constraints.add(Constraint.farConstraint(sub, tv));
        } else {
            if (adaptTo != null && getLV(adaptTo) != LocationTypeVariable.ALWAYS_NEAR) { // Optimization
                sub = adaptTo(getLV(rht), getLV(adaptTo), null, n);
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
            adaptAndSet(t, getLV(call.getCallee().getType()), originatingNode);
        } else
        if (originatingNode instanceof ThisExp) {
            annotateVar(t, LocationTypeVariable.ALWAYS_NEAR);
        } else
        if (originatingNode instanceof NewExp) {
            NewExp newExp = (NewExp)originatingNode;
            LocationTypeVariable ltv;
            if (newExp.hasCog()) {
                //ltv = LocationTypeVariable.ALWAYS_FAR;
                LocationType[] parFarTypes = getLocalFarTypes(originatingNode);
                ltv = LocationTypeVariable.newVar(constraints, typeNode, false, parFarTypes);
                constraints.add(Constraint.constConstraint(ltv, append(parFarTypes, LocationType.FAR), Constraint.MUST_HAVE));
            } else {
                ltv = LocationTypeVariable.ALWAYS_NEAR;
            }
            annotateVar(t, ltv);
        } else {
            if (t.isReferenceType()) {
                addNewVar(t, originatingNode, typeNode);
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
            LocationType[] lts = append(LocationType.ALLCONCRETEUSERTYPES, lv.parametricFarTypes());
            constraints.add(Constraint.constConstraint(lv, lts, Constraint.SHOULD_HAVE));
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
    
    private LocationType[] getMethodLocalFarTypes(ASTNode<?> n) {
        LocationType[] result = new LocationType[0];
        if (LOCATION_TYPING_PRECISION == LocationTypingPrecision.BASIC)
            return result;
        Block b = null;
        if (n instanceof Stmt) {
            b = ((Stmt)n).getContextBlock();
        }
        if (n instanceof Exp) {
            b = ((Exp)n).getContextBlock();
        }
        if (n instanceof VarDecl) {
            b = ((VarDecl)n).getAccess().getContextBlock();
        }
        if (n instanceof MethodImpl) {
            b = ((MethodImpl)n).getBlock().getContextBlock();
        }
        if (b != null) {
            if (methodLocalFarTypes.containsKey(b)) {
                result = methodLocalFarTypes.get(b);
            } else {
                int num = b.countNumberOfNewCogExpr();
                result = new LocationType[num];
                for (int i = 0; i < num; i++) {
                    result[i] = LocationType.createParametricFar("M"+i);
                }
                methodLocalFarTypes.put(b, result);
            }
        }
        return result;
    }
    
    private LocationType[] getClassLocalFarTypes(ASTNode<?> n) {
        LocationType[] result = new LocationType[0];
        if (LOCATION_TYPING_PRECISION == LocationTypingPrecision.BASIC || LOCATION_TYPING_PRECISION == LocationTypingPrecision.METHOD_LOCAL_FAR)
            return result;
        Decl d = null;
        if (n instanceof Stmt) {
            d = ((Stmt)n).getContextDecl();
        }
        if (n instanceof Exp) {
            d = ((Exp)n).getContextDecl();
        }
        if (n instanceof VarDecl) {
            d = ((VarDecl)n).getAccess().getContextDecl();
        }
        if (n instanceof MethodImpl) {
            d = ((MethodImpl)n).getBlock().getContextDecl();
        }
        if (n instanceof FieldDecl) {
            d = ((FieldDecl)n).getAccess().getContextDecl();
        }
        if (d instanceof ClassDecl) {
            ClassDecl cd = (ClassDecl)d;
            if (classLocalFarTypes.containsKey(cd)) {
                result = classLocalFarTypes.get(cd);
            } else {
                int num = countReferenceTypesOfFields(cd);
                result = new LocationType[num];
                for (int i = 0; i < num; i++) {
                    result[i] = LocationType.createParametricFar("C"+i);
                }
                classLocalFarTypes.put(cd, result);
            }
        }
        return result;
    }
    
    private LocationType[] getLocalFarTypes(ASTNode<?> originatingNode) {
        return append(getClassLocalFarTypes(originatingNode), getMethodLocalFarTypes(originatingNode));
    }


    private int countReferenceTypesOfFields(ClassDecl cd) {
        /*int result = 0;
        for (FieldDecl f : cd.getFields()) {
           result += countReferenceTypes(f.getAccess().getType()); 
        }
        return result;*/
        return 10;
    }

    private int countReferenceTypes(Type type) {
        int result = 0;
        if (type.isReferenceType()) return 1;
        if (type.isDataType()) {
            DataTypeType dtt = (DataTypeType)type;
            for (Type t : dtt.getTypeArgs()) {
                result += countReferenceTypes(t);
            }
        }
        return result;
    }
    
    public static LocationType[] append(LocationType[] lt1, LocationType... lt2) {
        LocationType[] result = new LocationType[lt1.length + lt2.length];
        System.arraycopy(lt1, 0, result, 0, lt1.length);
        System.arraycopy(lt2, 0, result, lt1.length, lt2.length);
        return result;
    }
}
