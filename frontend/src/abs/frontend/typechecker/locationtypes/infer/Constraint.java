/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.typechecker.locationtypes.infer;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

import abs.frontend.typechecker.ext.AdaptDirection;
import abs.frontend.typechecker.locationtypes.LocationType;

import static abs.frontend.typechecker.locationtypes.LocationType.*;
import static abs.frontend.typechecker.locationtypes.infer.MultiListIterable.fromIterable;

public abstract class Constraint {
    public final static Integer MUST_HAVE = 2147483647;
    public final static Integer NICE_TO_HAVE = 1;
    public final static Integer SHOULD_HAVE = 1337;

    protected void prependAll(Integer i, List<List<Integer>> l) {
        for (List<Integer> subl : l) {
            subl.add(0, i);
        }
    }

    protected List<LocationType> commonTypes(List<LocationType> lt1, List<LocationType> lt2) {
        List<LocationType> result = new ArrayList<>(lt1);
        result.retainAll(lt2);
        return result;
    }

    protected List<LocationType> diffTypes(List<LocationType> lt1, List<LocationType> lt2) {
        List<LocationType> result = new ArrayList<>(lt1);
        result.removeAll(commonTypes(lt1, lt2));
        return result;
    }

    public abstract List<List<Integer>> generateSat(Environment e);

    public abstract void variables(Set<LocationTypeVariable> vars);


    private static List<Integer> generate(LocationTypeVariable v, Environment e, LocationType... types) {
        return generate(v,e,Arrays.asList(types));
    }

    private static List<Integer> generate(LocationTypeVariable v, Environment e, List<LocationType> types) {
        List<Integer> values = new ArrayList<>();
        for (LocationType t : types) {
            values.add(e.get(v, t));
        }
        return values;
    }

    private static class SubConstraint extends Constraint {
        LocationTypeVariable tv1, tv2;

        public SubConstraint(LocationTypeVariable tv1, LocationTypeVariable tv2) {
            this.tv1 = tv1;
            this.tv2 = tv2;
        }

        @Override
        public String toString() {
            return tv1+ " <: " + tv2;
        }

        @Override
        public List<List<Integer>> generateSat(Environment e) {
            List<List<Integer>> result = new ArrayList<>();
            List<Integer> values;
            values = new ArrayList<>();
            values.add(- e.get(tv1, BOTTOM));
            values.addAll(generate(tv2, e, ALLVISTYPES));
            result.add(values);
            values = new ArrayList<>();
            values.add(- e.get(tv1, NEAR));
            values.addAll(generate(tv2, e, NEAR, SOMEWHERE));
            result.add(values);
            values = new ArrayList<>();
            values.add(- e.get(tv1, FAR));
            values.addAll(generate(tv2, e, FAR, SOMEWHERE));
            result.add(values);
            values = new ArrayList<>();
            values.add(- e.get(tv1, SOMEWHERE));
            values.addAll(generate(tv2, e, SOMEWHERE));
            result.add(values);

            for (LocationType pft : tv1.parametricFarTypes()) {
                if (tv2.parametricFarTypes().contains(pft)) {
                    values = new ArrayList<>();
                    values.add(- e.get(tv1, pft));
                    values.addAll(generate(tv2, e, pft, FAR, SOMEWHERE));
                    result.add(values);
                } else {
                    values = new ArrayList<>();
                    values.add(- e.get(tv1, pft));
                    values.addAll(generate(tv2, e, FAR, SOMEWHERE));
                    result.add(values);
                }

            }
            prependAll(SHOULD_HAVE, result);
            return result;
        }

        @Override
        public void variables(Set<LocationTypeVariable> vars) {
            vars.add(tv1);
            vars.add(tv2);
        }

    }

    private static class FarConstraint extends Constraint {
        LocationTypeVariable tv1, tv2;

        public FarConstraint(LocationTypeVariable tv1, LocationTypeVariable tv2) {
            /*if (tv2.parametricFarTypes().size() > 0) {
                throw new IllegalArgumentException("tv2 should not contain parametric Far Types");
            }*/
            this.tv1 = tv1;
            this.tv2 = tv2;
        }

        @Override
        public String toString() {
            return tv1+ " <:_FAR " + tv2;
        }

        @Override
        public List<List<Integer>> generateSat(Environment e) {
            List<List<Integer>> result = new ArrayList<>();
            List<Integer> values;
            values = new ArrayList<>();
            values.add(- e.get(tv1, BOTTOM));
            values.addAll(generate(tv2, e, tv2.allTypes()));
            result.add(values);
            values = new ArrayList<>();
            values.add(- e.get(tv1, NEAR));
            values.addAll(generate(tv2, e, FAR, SOMEWHERE));
            result.add(values);
            values = new ArrayList<>();
            values.add(- e.get(tv1, FAR));
            values.addAll(generate(tv2, e, FAR, SOMEWHERE));
            result.add(values);
            values = new ArrayList<>();
            values.add(- e.get(tv1, SOMEWHERE));
            values.addAll(generate(tv2, e, FAR, SOMEWHERE));
            result.add(values);
            for (LocationType pft : tv1.parametricFarTypes()) {
                if (tv2.parametricFarTypes().contains(pft)) {
                    values = new ArrayList<>();
                    values.add(- e.get(tv1, pft));
                    values.addAll(generate(tv2, e, pft, FAR, SOMEWHERE));
                    result.add(values);
                } else {
                    values = new ArrayList<>();
                    values.add(- e.get(tv1, pft));
                    values.addAll(generate(tv2, e, FAR, SOMEWHERE));
                    result.add(values);
                }
            }
            prependAll(SHOULD_HAVE, result);
            return result;
        }

        @Override
        public void variables(Set<LocationTypeVariable> vars) {
            vars.add(tv1);
            vars.add(tv2);
        }

    }

    public static class DeclConstraint extends Constraint {
        LocationTypeVariable tv;

        public DeclConstraint(LocationTypeVariable tv) {
            this.tv = tv;
        }

        @Override
        public List<List<Integer>> generateSat(Environment e) {
            List<List<Integer>> result = new ArrayList<>();
            List<Integer> values = new ArrayList<>();

            // vt must have at least a type
            for (LocationType it : tv.allTypes()) {
                values.add(e.get(tv, it));
            }
            result.add(values);

            // vt must have maximally one type
            for (LocationType it1 : tv.allTypes()) {
                for (LocationType it2 : tv.allTypes()) {
                    if (!it1.equals(it2)) {
                        // !tv = it1 or !tv = it2  ===  ! (tv = it1 && tv = it2)
                        result.add(new CL(e).not(tv).is(it1).orNot(tv).is(it2).getValues());
                    }
                }
            }
            prependAll(MUST_HAVE, result);
            return result;
        }


        @Override
        public String toString() {
            return tv + " def= " + tv.allTypes();
        }

        @Override
        public void variables(Set<LocationTypeVariable> vars) {
            vars.add(tv);
        }

    }

    private static class CL {
        private ArrayList<Integer> values = new ArrayList<>();
        private Environment e;
        public CL(Environment e) {
            this.e = e;
        }

        class Predicate {
            LocationTypeVariable tv;
            private boolean not;
            Predicate(LocationTypeVariable v, boolean not) {
               tv = v;
               this.not = not;
            }

            CL is(LocationType t) {
                if (not) {
                    return orNot(tv,t);
                } else {
                    return or(tv,t);
                }

            }
        }

        public Predicate or(LocationTypeVariable tv) {
            return new Predicate(tv,false);
        }

        public Predicate then(LocationTypeVariable tv) {
            return new Predicate(tv,false);
        }

        public Predicate orNot(LocationTypeVariable tv) {
            return new Predicate(tv,true);
        }

        public Predicate not(LocationTypeVariable tv) {
            return orNot(tv);
        }

        public Predicate if_(LocationTypeVariable tv) {
            return orNot(tv);
        }

        public Predicate andIf(LocationTypeVariable tv) {
            return orNot(tv);
        }

        public CL not(LocationTypeVariable tv, LocationType t) {
            return orNot(tv,t);
        }

        public CL orNot(LocationTypeVariable tv, LocationType t) {
            values.add(- e.get(tv, t));
            return this;
        }

        public CL or(LocationTypeVariable tv, LocationType t) {
            values.add(e.get(tv, t));
            return this;
        }

        public CL if_(LocationTypeVariable tv, LocationType t) {
            return not(tv, t);
        }

        public CL andIf(LocationTypeVariable tv, LocationType t) {
            return not(tv, t);
        }

        public CL then(LocationTypeVariable tv, LocationType t) {
            return or(tv, t);
        }

        public ArrayList<Integer> getValues() {
            return values;
        }
    }

    private static class AdaptConstraint extends Constraint {
        LocationTypeVariable resultTv, tv, adaptToTv;
        AdaptDirection dir;

        public AdaptConstraint(LocationTypeVariable resultTv, LocationTypeVariable tv, AdaptDirection dir, LocationTypeVariable adaptToTv) {
            if (resultTv == null || tv == null || adaptToTv == null)
                throw new IllegalArgumentException("some variable is null: "+resultTv+", "+tv+", "+adaptToTv);
            if (dir == null) {
                throw new IllegalArgumentException("direction is null");
            }
            /*if (!Arrays.equals(adaptToTv.parametricFarTypes(), resultTv.parametricFarTypes())) {
                throw new IllegalArgumentException("Parametric Far Types for adaptToTv and resultTv should be equal");
            }*/
            /*
            if (adaptToTv.parametricFarTypes().length > 0 && !Arrays.equals(adaptToTv.parametricFarTypes(), resultTv.parametricFarTypes())) {
                throw new IllegalArgumentException("Parametric Far Types for adaptToTv and resultTv should be equal");
            }*/
            this.resultTv = resultTv;
            this.tv = tv;
            this.adaptToTv = adaptToTv;
            this.dir = dir;
        }

        @Override
        public String toString() {
            return tv + " >>_" + dir + " " + adaptToTv + " = " + resultTv;
        }

        @Override
        public List<List<Integer>> generateSat(Environment e) {
            List<List<Integer>> result = new ArrayList<>();

            // if adaptToTv = NEAR
            result.add(new CL(e).if_(adaptToTv).is(NEAR).andIf(tv).is(BOTTOM).then(resultTv).is(BOTTOM).getValues());
            result.add(new CL(e).if_(adaptToTv).is(NEAR).andIf(tv).is(NEAR).then(resultTv).is(NEAR).getValues());
            result.add(new CL(e).if_(adaptToTv).is(NEAR).andIf(tv).is(FAR).then(resultTv).is(FAR).getValues());
            result.add(new CL(e).if_(adaptToTv).is(NEAR).andIf(tv).is(SOMEWHERE).then(resultTv).is(SOMEWHERE).getValues());
            for (LocationType t : commonTypes(tv.parametricFarTypes(), resultTv.parametricFarTypes())) {
                result.add(new CL(e).if_(adaptToTv).is(NEAR).andIf(tv).is(t).then(resultTv).is(t).getValues());
            }
            for (LocationType t : diffTypes(tv.parametricFarTypes(), resultTv.parametricFarTypes())) {
                result.add(new CL(e).if_(adaptToTv).is(NEAR).andIf(tv).is(t).then(resultTv).is(FAR).getValues());
            }

            // if adaptToTv = FAR
            result.add(new CL(e).if_(adaptToTv).is(FAR).andIf(tv).is(BOTTOM).then(resultTv).is(BOTTOM).getValues());
            result.add(new CL(e).if_(adaptToTv).is(FAR).andIf(tv).is(NEAR).then(resultTv).is(FAR).getValues());
            result.add(new CL(e).if_(adaptToTv).is(FAR).andIf(tv).is(FAR).then(resultTv).is(SOMEWHERE).getValues());
            result.add(new CL(e).if_(adaptToTv).is(FAR).andIf(tv).is(SOMEWHERE).then(resultTv).is(SOMEWHERE).getValues());
            for (LocationType t : tv.parametricFarTypes()) {
                result.add(new CL(e).if_(adaptToTv).is(FAR).andIf(tv).is(t).then(resultTv).is(SOMEWHERE).getValues());
            }

            // if adaptToTv = FAR(i)
            for (LocationType t : adaptToTv.parametricFarTypes()) {
                result.add(new CL(e).if_(adaptToTv).is(t).andIf(tv).is(BOTTOM).then(resultTv).is(BOTTOM).getValues());
                if (dir.isFrom())
                    result.add(new CL(e).if_(adaptToTv).is(t).andIf(tv).is(NEAR).then(resultTv).is(t).getValues());
                else
                    result.add(new CL(e).if_(adaptToTv).is(t).andIf(tv).is(NEAR).then(resultTv).is(FAR).getValues());
                result.add(new CL(e).if_(adaptToTv).is(t).andIf(tv).is(FAR).then(resultTv).is(SOMEWHERE).getValues());
                result.add(new CL(e).if_(adaptToTv).is(t).andIf(tv).is(SOMEWHERE).then(resultTv).is(SOMEWHERE).getValues());
            }
            //List<LocationType> commonAdaptTvAndTv = commonTypes(adaptToTv.parametricFarTypes(), tv.parametricFarTypes());
            for (LocationType t : adaptToTv.parametricFarTypes()) {
                for (LocationType t2 : tv.parametricFarTypes()) {
                    if (t != t2) {
                        if (dir.isFrom()) {
                            result.add(new CL(e).if_(adaptToTv).is(t).andIf(tv).is(t2).then(resultTv).is(SOMEWHERE).getValues());
                        } else {
                            if (resultTv.parametricFarTypes().contains(t2)) {
                                result.add(new CL(e).if_(adaptToTv).is(t).andIf(tv).is(t2).then(resultTv).is(t2).getValues());
                            } else {
                                result.add(new CL(e).if_(adaptToTv).is(t).andIf(tv).is(t2).then(resultTv).is(FAR).getValues());
                            }
                        }
                    } else {
                        result.add(new CL(e).if_(adaptToTv).is(t).andIf(tv).is(t2).then(resultTv).is(SOMEWHERE).getValues());
                    }
                }
            }
            /*for (LocationType t : diffTypes(adaptToTv.parametricFarTypes(), tv.parametricFarTypes())) {
                for (LocationType t2 : diffTypes(tv.parametricFarTypes(), adaptToTv.parametricFarTypes())) {
                    result.add(new CL(e).if_(adaptToTv).is(t).andIf(tv).is(t2).then(resultTv).is(SOMEWHERE).getValues());
                }
            }*/

            // if adaptToTv = SOMEWHERE
            for (LocationType t : tv.allTypes()) {
                if (t != BOTTOM)
                    result.add(new CL(e).if_(adaptToTv).is(SOMEWHERE).andIf(tv).is(t).then(resultTv).is(SOMEWHERE).getValues());
            }
            result.add(new CL(e).if_(adaptToTv).is(SOMEWHERE).andIf(tv).is(BOTTOM).then(resultTv).is(BOTTOM).getValues());

            prependAll(MUST_HAVE, result);
            // adaptToTv should always be unequal to BOTTOM
            //result.addAll(Constraint.neqConstraint(adaptToTv, LocationTypeVariable.ALWAYS_BOTTOM, SHOULD_HAVE).generateSat(e));
            return result;
        }

        @Override
        public void variables(Set<LocationTypeVariable> vars) {
            vars.add(resultTv);
            vars.add(tv);
            vars.add(adaptToTv);
        }

    }

    private static class EqConstraint extends Constraint {
        LocationTypeVariable tv1, tv2;
        Integer importance;

        public EqConstraint(LocationTypeVariable tv1, LocationTypeVariable tv2, Integer importance) {
            /*if (!Arrays.equals(tv1.parametricFarTypes(), tv2.parametricFarTypes())) {
                throw new IllegalArgumentException("Parametric Far Types for tv1 and tv2 should be equal");
            }*/
            this.tv1 = tv1;
            this.tv2 = tv2;
            this.importance = importance;
        }

        @Override
        public List<List<Integer>> generateSat(Environment e) {
            List<List<Integer>> result = new ArrayList<>();
            List<Integer> values;
            //boolean b = tv1.allTypes().length > tv2.allTypes().length;
            for (LocationType t : commonTypes(tv1.allTypes(), tv2.allTypes())) {
                values = new ArrayList<>();
                values.add(- e.get(tv1, t));
                values.add(e.get(tv2, t));
                result.add(values);
            }
            for (LocationType lt : diffTypes(tv1.parametricFarTypes(), tv2.parametricFarTypes())) {
                values = new ArrayList<>();
                values.add(- e.get(tv1, lt));
                result.add(values);
            }
            for (LocationType lt : diffTypes(tv2.parametricFarTypes(), tv1.parametricFarTypes())) {
                values = new ArrayList<>();
                values.add(- e.get(tv2, lt));
                result.add(values);
            }

            prependAll(importance, result);
            return result;
        }

        @Override
        public String toString() {
            return tv1 + " = " + tv2;
        }

        @Override
        public void variables(Set<LocationTypeVariable> vars) {
            vars.add(tv1);
            vars.add(tv2);
        }

    }

    private static class ConstConstraint extends Constraint {
        LocationTypeVariable tv;
        Iterable<LocationType> t;
        Integer importance;

        public ConstConstraint(LocationTypeVariable tv, Iterable<LocationType> list, Integer importance) {
            this.tv = tv;
            this.t = list;
            this.importance = importance;
        }

        @Override
        public List<List<Integer>> generateSat(Environment e) {
            if (!tv.allTypes().containsAll(fromIterable(t))) {
                throw new IllegalArgumentException("t should be a subset of types in tv");
            }
            List<List<Integer>> result = new ArrayList<>();
            List<Integer> values;
            values = new ArrayList<>();
            for (LocationType lt : t) {
                values.add(e.get(tv, lt));
            }
            result.add(values);
            /*for (LocationType t2 : LocationType.ALLTYPES) {
                if (!t.equals(t2)) {
                    values = new ArrayList<Integer>();
                    values.add(- e.get(tv, t2));
                    result.add(values);
                }
            }*/
            prependAll(importance, result);
            return result;
        }

        @Override
        public String toString() {
            return tv + " := " + t;
        }

        @Override
        public void variables(Set<LocationTypeVariable> vars) {
            vars.add(tv);
        }

    }

    // tv1 < tv2
    public static Constraint subConstraint(LocationTypeVariable tv1, LocationTypeVariable tv2) {
        return new SubConstraint(tv1, tv2);
    }

    // tv1 = tv2
    public static Constraint eqConstraint(LocationTypeVariable tv1, LocationTypeVariable tv2, Integer importance) {
        return new EqConstraint(tv1, tv2, importance);
    }

    // tv := t
    public static Constraint constConstraint(LocationTypeVariable tv, LocationType t, Integer importance) {
        return new ConstConstraint(tv, Arrays.asList(t), importance);
    }

    /*public static Constraint constConstraint(LocationTypeVariable tv, LocationType[] tl, Integer importance) {
        return new ConstConstraint(tv, Arrays.asList(tl), importance);
    }*/

    public static Constraint constConstraint(LocationTypeVariable tv, Iterable<LocationType> tl, Integer importance) {
        return new ConstConstraint(tv, tl, importance);
    }

    // tv1 = tv2 |> tv3
    public static Constraint adaptConstraint(LocationTypeVariable resultTv, LocationTypeVariable tv, AdaptDirection dir, LocationTypeVariable adaptTo) {
        return new AdaptConstraint(resultTv, tv, dir, adaptTo);
    }

    public static Constraint declConstraint(LocationTypeVariable tv) {
        return new DeclConstraint(tv);
    }

    // tv1 <_far tv2
    public static Constraint farConstraint(LocationTypeVariable tv1, LocationTypeVariable tv2) {
        return new FarConstraint(tv1, tv2);
    }
}
