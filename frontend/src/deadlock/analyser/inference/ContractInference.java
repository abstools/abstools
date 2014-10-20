/**
 * This class contains the implementation of the contract inference
 */
package deadlock.analyser.inference;

import abs.frontend.ast.*;
import abs.frontend.typechecker.*;
import abs.frontend.typechecker.KindedName.Kind;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;

import choco.kernel.common.util.tools.ArrayUtils;
import deadlock.constraints.term.*;
import deadlock.analyser.factory.*;
import deadlock.analyser.generation.*;

/**
 * @author Abel, Michael
 * 
 */
public class ContractInference {

    // Code description
    /*
     * between //Map Interface_name to Class_name and //END Map Interface_name
     * to Class_name there are attribute for calculate a mapping between
     * Interface_name and Class_name
     * 
     * between //OLD ENVIRONMENT CREATION and //END OLD ENVIRONMENT CREATION
     * there are attribute for create the initial environmant, this is the old
     * code based on old unification algorithm
     * 
     * between //NEW ENVIRONMENT CREATION and //END NEW ENVIRONMENT CREATION
     * there are new aspect for calculate the initial environment based on new
     * semiUnification algorithm, what before was attribute, now is aspect
     * becouse of the fact that we need to pass a Factory for the creation of
     * Term, in this case freshTermVariable to put into the Environment
     * 
     * between //TYPEINFERENCE and //END TYPEINFERENCE there are the aspetc for
     * implement the inference rule, all the inference rule inside paper are
     * written, more than these there are also rule for AST node, necessary for
     * the computanion, e.g. PureExp.typeInference( ... ), we need to discuss
     * how perform it, rule are empty now. More than there rule there are also
     * the "super" rule of the biggest node of AST, e.g. Model.typeInference(
     * ... ); I suppose (and comment) that Model only need Factory to work, and
     * inside Model could be recover the Environment and the mapping between
     * InterfaceName and ClassName.
     */

    /************************************/
    /* Helper function */
    /************************************/
    public RecordPresent createInstance(ClassDecl cd, Factory df, GroupName a) {
        // TODO: deal with field assignement, with init, and run
        LinkedList<RecordField> l = new LinkedList<RecordField>();
        Environment env = new Environment();
        for (ParamDecl f : cd.getParams()) {
            RecordVariable X = df.newRecordVariable();
            env.putVariable(f.getName(), X);
            l.add(df.newRecordField(f.getName(), X));
        }
        for (FieldDecl f : cd.getFields()) {
            if (f.hasInitExp()) {
                l.add(df.newRecordField(f.getName(), typeInferenceAsPure(f.getInitExp(), null, env, a, null, df, cd)
                        .getRepresentative()));
            } else {
                l.add(df.newRecordField(f.getName(), df.newRecordVariable()));
            } // no need to test for datatype: they all are assign
        }
        return df.newRecordPresent(a, l);
    }

    // eAsPure(String ident, Environment env, GroupName a, Map<InterfaceDecl,
    // ClassDecl> intertoclass, Factory df, ClassDecl cl)

    /************************************/
    /* Map Interface_name to Class_name */
    /************************************/

    /*
     * This mapping is used so we can associate a contract to an interface, not
     * only a class. In the original paper, there was no interfaces, and we
     * could directly get the contract of a method call by looking to the type
     * of the called object. In ABS, this is not true anymore, so we use this
     * trick. This means that two classes cannot implement the same interface,
     * otherwise we once again don't know what is the contract of the interface.
     */

    public Map<InterfaceDecl, ClassDecl> getMapInterfaceToClass(Model m) {
        Map<InterfaceDecl, ClassDecl> res = new HashMap<InterfaceDecl, ClassDecl>();
        for (Decl decl : m.getDecls()) {
            if (decl instanceof ClassDecl) {
                // 1. Computes recursively the set of all interfaces the class
                // extends
                Set<InterfaceTypeUse> toAdd = new HashSet<InterfaceTypeUse>();
                Set<InterfaceTypeUse> set = new HashSet<InterfaceTypeUse>();
                Set<InterfaceTypeUse> tmp = new HashSet<InterfaceTypeUse>();
                for (InterfaceTypeUse it : ((ClassDecl) decl).getImplementedInterfaceUses()) {
                    toAdd.add(it);
                }
                while (!toAdd.isEmpty()) {
                    Iterator<InterfaceTypeUse> i = toAdd.iterator();
                    while (i.hasNext()) {
                        InterfaceTypeUse it = i.next();
                        if ((!set.contains(it)) && (it.getType() instanceof InterfaceType)) {
                            set.add(it);
                            for (InterfaceTypeUse itin : ((InterfaceType) it.getType()).getDecl()
                                    .getExtendedInterfaceUses()) {
                                tmp.add(itin);
                            }
                        }
                    }
                    toAdd = tmp;
                    tmp = new HashSet<InterfaceTypeUse>();
                }
                // 2. add these interfaces to the map
                for (InterfaceTypeUse it : set) {
                    InterfaceDecl d = ((InterfaceType) it.getType()).getDecl();
                    if (res.containsKey(d)) {
                        System.out.println("WARNING: the class \"" + res.get(d).getName() + "\" and \""
                                + ((ClassDecl) decl).getName() + "\" both implement the interface \"" + d.getName()
                                + "\"." + "This will probably cause an erroneous deadlock analysis");
                    } else {
                        res.put(d, (ClassDecl) decl);
                    }
                }
            }
        }
        return res;
    }

    /************************************/
    /* New Environment Creation */
    /************************************/

    /*
     * this method computes the initial environment of the contract inference.
     * An environment is a mapping from methods to a behavior specification of
     * the form r(\vect{r'}) -> r'', where - r is the record of the object
     * executing the method - \vect{r'} are the records of the parameters of the
     * method - r'' is the record of the result. As contract inference wasn't
     * performed yet, the initial mapping does not know the structure of these
     * r's, and so put variables in their place.
     * 
     * Let remark that this initial environment is still necessary to perform
     * contract inference. Another remark concerns Datatypes. To be able to deal
     * with them like objects, we need to give them a record. As a good
     * approximation, we can say that all datatypes live in the same cog that is
     * different from those used by the objects.
     */

    public Environment environment(Model m, Factory df, boolean verbose) {
        Environment res = new Environment();
        for (CompilationUnit c : m.getCompilationUnits()) {
            res.add(environment(c, df, verbose));
        }
        return res;
    }

    public Environment environment(CompilationUnit cu, Factory df, boolean verbose) {
        Environment res = new Environment();
        for (ModuleDecl d : cu.getModuleDecls()) {
            res.add(environment(d, df, verbose));
        }
        return res;
    }

    public Environment environment(ModuleDecl md, Factory df, boolean verbose) {
        Environment res = new Environment();
        for (Decl d : md.getDecls()) {
            if (d instanceof ClassDecl) {
                res.add(environment(((ClassDecl) d), df, verbose));
            }
        }
        return res;
    }

    public Environment environment(ClassDecl cd, Factory df, boolean verbose) {
        Environment res = new Environment();
        // 1. Methods
        for (MethodImpl m : cd.getMethods()) {
            res.add(environment(m, df, cd, verbose));
        }
        // 2. init
        MethodInterface mi = df.newMethodInterface(createInstance(cd, df, df.newGroupName()), new LinkedList<Record>(),
                df.newRecordVariable());
        res.putMethod(cd.getName(), "!init!", mi);
        return res;
    }

    public Environment environment(MethodImpl mi, Factory df, ClassDecl c, boolean verbose) {
        if (verbose) {
            System.out.println("Generating initial environment for the method \"" + c.getName() + "."
                    + mi.getMethodSig().getName() + "\"");
        }
        Environment res = new Environment();

        // 1. Record of "this"
        Record rthis = createInstance(c, df, df.newGroupName());
        // 2. Simple variables for the method parameters
        LinkedList<Record> rparam = new LinkedList<Record>();
        for (ParamDecl p : mi.getMethodSig().getParams()) {
            rparam.add(df.newRecordVariable());
        }
        // 3. Simple variable for the return object (or datatype)
        RecordVariable rres = df.newRecordVariable();
        // 4. Finalize
        MethodInterface mIntf = df.newMethodInterface(rthis, rparam, rres);
        res.putMethod(c.getName(), mi.getMethodSig().getName(), mIntf);
        return res;
    }

    /************************************/
    /* TYPE INFERENCE */
    /************************************/

    // ResultInferenceStmt stands for
    // 'deadlock.constraints.constraint.ConstraintGeneration_output'.
    // Suppose that 'ResultInferenceStmt' is class type containing three
    // value (Record r, Contract c,
    // deadlock.constraints.constraint.ConstraintStore U)
    // Suppose that ResultInferenceStmt has method getRecord() setRecord(),
    // getContract setContract addContract accumulateContract()
    // "the )( operator",
    // getdeadlock.constraints.constraint.ConstraintStore,
    // setdeadlock.constraints.constraint.ConstraintStore,
    // adddeadlock.constraints.constraint.Constraint(),
    // adddeadlock.constraints.constraint.ConstraintStore()

    // because of all the methods different from get/set needs the Factory, we
    // perform that operation outside ResultInferenceStmt class.
    // itfToClass is an HashMap that bind InterfaceName to ClassDelc, this is
    // done because in ABS Interface are type and Class are not.

    // ////////////////////////////////////////////////////////////////////////////
    // 1. Model, Compilation Units (files), Classes and methods
    // ////////////////////////////////////////////////////////////////////////////

    public ResultInference typeInference(Model m, String ident, Environment env, Factory df,
            Map<InterfaceDecl, ClassDecl> intertoclass) {
        ResultInference res = new ResultInference();
        for (CompilationUnit c : m.getCompilationUnits()) {
            res.add(typeInference(c, ident, env, intertoclass, df));
        }
        ResultInferenceStmt resMain = typeInference(m.getMainBlock(), ident, env, df.newGroupName(), intertoclass, df,
                null);
        res.add(resMain.getConstraint());
        res.setMain(resMain.getContract());
        return res;
    }

    public ResultInference typeInference(CompilationUnit cu, String ident, Environment env,
            Map<InterfaceDecl, ClassDecl> intertoclass, Factory df) {
        ResultInference res = new ResultInference();
        String nident = null;
        if (ident != null) {
            System.out.println(ident + "Contract Inference for the file \"" + cu.getName() + "\"");
            nident = " " + ident;
        }
        for (ModuleDecl d : cu.getModuleDecls()) {
            res.add(typeInference(d, nident, env, intertoclass, df));
        }
        return res;
    }

    public ResultInference typeInference(ModuleDecl md, String ident, Environment env,
            Map<InterfaceDecl, ClassDecl> intertoclass, Factory df) {
        ResultInference res = new ResultInference();
        String nident = null;
        if (ident != null) {
            System.out.println(ident + "Contract Inference for the module \"" + md.getName() + "\"");
            nident = " " + ident;
        }
        for (Decl d : md.getDecls()) {
            if (d instanceof ClassDecl) {
                res.add(typeInference(((ClassDecl) d), nident, env, intertoclass, df));
            }
        }
        return res;
    }

    public ResultInference typeInference(ClassDecl cd, String ident, Environment env,
            Map<InterfaceDecl, ClassDecl> intertoclass, Factory df) {
        ResultInference res = new ResultInference();
        String nident = null;
        if (ident != null) {
            System.out.println(ident + "Contract Inference for the class \"" + cd.getName() + "\"");
            nident = " " + ident;
        }
        // 1. Methods
        for (MethodImpl m : cd.getMethods()) {
            res.add(typeInference(m, nident, env, intertoclass, df, cd));
        }
        // 2. Init
        MethodInterface mi = env.getMethod(cd.getName(), "!init!");
        Record thisRecord = mi.getThis();
        deadlock.constraints.constraint.Constraint c;
        Contract contract;
        if (cd.hasInitBlock()) {
            Environment envInitBlock = env.clone();
            GroupName a = ((RecordPresent) thisRecord).getRoot();
            envInitBlock.putVariable("this", thisRecord);
            ResultInferenceStmt resInitBlock = typeInference(cd.getInitBlock(), nident, envInitBlock, a, intertoclass,
                    df, cd);
            contract = resInitBlock.getContract();
            c = resInitBlock.getConstraint();
        } else {
            contract = df.newContractEmpty();
            c = df.newConstraint();
        }
        // 2.2. add the call to run if the method exists.
        if (env.getMethod(cd.getName(), "run") != null) {
            ASTNode node = (cd.hasInitBlock() ? cd.getInitBlock() : cd);

            MethodInterface mirun = df.newMethodInterface(thisRecord, new LinkedList<Record>(), df.newRecordVariable());
            c.addSemiEquation(new ASTNodeInformation(node), env.getMethod(cd.getName(), "run"), mirun);
            contract.add(df.newContractInvk(node, cd.getName(), "run", mirun));
        }

        res.add(c);
        res.add(cd.getName(), "!init!", df.newMethodContract(mi, contract));

        if (ident != null) {
            System.out.println(ident + "Inference for the class \"" + cd.getName() + "\" Finished");
        }
        return res;
    }

    public ResultInference typeInference(MethodImpl mImp, String ident, Environment env,
            Map<InterfaceDecl, ClassDecl> intertoclass, Factory df, ClassDecl cl) {
        String nident = null;
        if (ident != null) {
            System.out.println(ident + "Contract Inference for the method \"" + cl.getName() + "."
                    + mImp.getMethodSig().getName() + "\"");
            nident = " " + ident;
        }

        // 1. Generate the environment for the contract inference
        Environment envMethod = env.clone();

        // add to the environment the record for [this], the parameters, and
        // [destiny]
        MethodInterface mi = env.getMethod(cl.getName(), mImp.getMethodSig().getName());
        Record thisRecord = mi.getThis();
        java.util.List<Record> params = mi.getParameters();
        Record result = mi.getResult();
        GroupName a = ((RecordPresent) thisRecord).getRoot(); // by
                                                              // construction,
                                                              // we know that
                                                              // [thisRecord] is
                                                              // RecordPresent

        envMethod.putVariable("this", thisRecord);
        Iterator<Record> it = params.iterator();
        for (ParamDecl p : mImp.getMethodSig().getParams()) {
            envMethod.putVariable(p.getName(), it.next());
        }
        envMethod.putVariable("!destiny!", result);

        // 2. perform the inference
        ResultInferenceStmt resBlock = typeInference(mImp.getBlock(), nident, envMethod, a, intertoclass, df, cl);
        if (ident != null) {
            System.out.println(ident + "Method Block Finished");
        }

        // 3. link the inferred contract with the method interface in the
        // environment
        // deadlock.constraints.constraint.Constraint c =
        // resBlock.getConstraint();

        // 4. give back the result.
        ResultInference res = new ResultInference();
        res.add(cl.getName(), mImp.getMethodSig().getName(), df.newMethodContract(mi, resBlock.getContract()));
        res.add(resBlock.getConstraint());
        return res;
    }

    // ////////////////////////////////////////////////////////////////////////////
    // 2. Rules
    // ////////////////////////////////////////////////////////////////////////////

    // ////////////////////////////////////////////////////////////////////////////
    // 2.1. Generic inference rules, for nodes that are not yet managed by the
    // inference

    private Method getInferenceMethod(Class dynamicClass, Class staticClass, Class... parameters) {
        return getInferenceMethod("typeInference", dynamicClass, staticClass, parameters);
    }

    private Method getInferenceMethod(String methodName, Class dynamicClass, Class staticClass, Class... parameters) {
        Class newc = dynamicClass;
        Method m = null;

        Class[] params = new Class[parameters.length + 1];
        for (int i = 0; i < parameters.length; i++)
            params[i + 1] = parameters[i];

        // Try the superclasses
        while (m == null && newc != staticClass) {

            try {
                params[0] = newc;
                m = getClass().getMethod(methodName, params);
            } catch (NoSuchMethodException e) {
                newc = newc.getSuperclass();
            }
        }

        // /*NO NEED OF THIS */
        // // Try the interfaces. If necessary, you
        // // can sort them first to define 'visitable' interface wins
        // // in case an object implements more than one.
        // if (newc == Object.class) {
        // Class[] interfaces = c.getInterfaces();
        // for (int i = 0; i < interfaces.length; i++) {
        // String method = interfaces[i].getName();
        // method = "visit" + method.substring(method.lastIndexOf('.') + 1);
        // try {
        // m = getClass().getMethod(method, new Class[] {interfaces[i]});
        // } catch (NoSuchMethodException e) {}
        // }
        // }

        // /*NO NEED OF THIS EITHER*/
        //
        // if (m == null) {
        // try {
        // m = thisclass.getMethod("visitObject", new Class[] {Object.class});
        // } catch (Exception e) {
        // // Can't happen
        // }
        // }
        return m;
    }

    public ResultInferenceStmt typeInference(Decl d, String ident, Environment env, GroupName a,
            Map<InterfaceDecl, ClassDecl> intertoclass, Factory df, ClassDecl cl) {
        Method m = getInferenceMethod(d.getClass(), Decl.class, String.class, Environment.class, GroupName.class,
                Map.class, Factory.class, ClassDecl.class);

        if (m != null)
            try {
                return (ResultInferenceStmt) m.invoke(this, d, ident, env, a, intertoclass, df, cl);
            } catch (IllegalAccessException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            } catch (IllegalArgumentException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            } catch (InvocationTargetException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }

        System.out.println("WARNING: Contract inference not implemented for Declaration \"" + d.getClass().getName()
                + "\"");
        return new ResultInferenceStmt(null, df.newContractEmpty(), df.newConstraint(), env);
    }

    public ResultInferenceStmt typeInference(Stmt s, String ident, Environment env, GroupName a,
            Map<InterfaceDecl, ClassDecl> intertoclass, Factory df, ClassDecl cl) {
        Method m = getInferenceMethod(s.getClass(), Stmt.class, String.class, Environment.class, GroupName.class,
                Map.class, Factory.class, ClassDecl.class);

        if (m != null)
            try {
                return (ResultInferenceStmt) m.invoke(this, s, ident, env, a, intertoclass, df, cl);
            } catch (IllegalAccessException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            } catch (IllegalArgumentException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            } catch (InvocationTargetException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }

        System.out.println("WARNING: Contract inference not implemented for Statment \"" + s.getClass().getName()
                + "\"");
        return new ResultInferenceStmt(null, df.newContractEmpty(), df.newConstraint(), env);
    }

    public ResultInferenceEffExp typeInference(Exp e, String ident, Environment env, GroupName a,
            Map<InterfaceDecl, ClassDecl> intertoclass, Factory df, ClassDecl cl) {
        Method m = getInferenceMethod(e.getClass(), Exp.class, String.class, Environment.class, GroupName.class,
                Map.class, Factory.class, ClassDecl.class);

        if (m != null)
            try {
                return (ResultInferenceEffExp) m.invoke(this, e, ident, env, a, intertoclass, df, cl);
            } catch (IllegalAccessException e1) {
                // TODO Auto-generated catch block
                e1.printStackTrace();
            } catch (IllegalArgumentException e1) {
                // TODO Auto-generated catch block
                e1.printStackTrace();
            } catch (InvocationTargetException e1) {
                // TODO Auto-generated catch block
                e1.printStackTrace();
            }

        System.out.println("WARNING: Contract inference not implemented for Expression \"" + e.getClass().getName()
                + "\"");
        return new ResultInferenceEffExp(null, df.newRecordVariable(), df.newContractEmpty(), df.newConstraint(), env);
    }

    public ResultInferencePureExp typeInferenceAsPure(PureExp exp, String ident, Environment env, GroupName a,
            Map<InterfaceDecl, ClassDecl> intertoclass, Factory df, ClassDecl cl) {

        
        Method m = getInferenceMethod("typeInferenceAsPure", exp.getClass(), PureExp.class, String.class,
                Environment.class, GroupName.class, Map.class, Factory.class, ClassDecl.class);

        if (m != null)
            try {
                return (ResultInferencePureExp) m.invoke(this, exp, ident, env, a, intertoclass, df, cl);
            } catch (IllegalAccessException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            } catch (IllegalArgumentException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            } catch (InvocationTargetException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }

        System.out.println("WARNING: Contract inference not implemented for Expression \"" + exp.getClass().getName()
                + "\"");
        return new ResultInferencePureExp(df);
    }

    public ResultInferenceEffExp typeInference(PureExp pexp, String ident, Environment env, GroupName a,
            Map<InterfaceDecl, ClassDecl> intertoclass, Factory df, ClassDecl cl) {
        Method m = getInferenceMethod(pexp.getClass(), PureExp.class, String.class, Environment.class, GroupName.class,
                Map.class, Factory.class, ClassDecl.class);

        if (m != null)
            try {
                return (ResultInferenceEffExp) m.invoke(this, pexp, ident, env, a, intertoclass, df, cl);
            } catch (IllegalAccessException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            } catch (IllegalArgumentException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            } catch (InvocationTargetException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }

        ResultInferencePureExp resPure = typeInferenceAsPure(pexp, ident, env, a, intertoclass, df, cl);
        return new ResultInferenceEffExp(resPure.getId(), resPure.getRepresentative(), df.newContractEmpty(),
                df.newConstraint(), env);
    }

    public ResultInferencePureExp typeInference(Guard g, String ident, Environment env, GroupName a,
            Map<InterfaceDecl, ClassDecl> intertoclass, Factory df, ClassDecl cl) {
        Method m = getInferenceMethod(g.getClass(), Guard.class, String.class, Environment.class, GroupName.class,
                Map.class, Factory.class, ClassDecl.class);

        if (m != null)
            try {
                return (ResultInferencePureExp) m.invoke(this, g, ident, env, a, intertoclass, df, cl);
            } catch (IllegalAccessException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            } catch (IllegalArgumentException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            } catch (InvocationTargetException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }

        System.out.println("WARNING: Contract inference not implemented for Guard \"" + g.getClass().getName() + "\"");
        return new ResultInferencePureExp(df);
    }

    // /////////////////////////////////////////////////////////////////////////////
    // 2.2. Declarations

    public ResultInferenceStmt typeInference(VarDecl vd, String ident, Environment prev, GroupName a,
            Map<InterfaceDecl, ClassDecl> intertoclass, Factory df, ClassDecl cl) {
        String nident = null;
        if (ident != null) {
            System.out.println(ident + "Contract Inference for the VarDecl \"" + vd.getName() + "\"");
            nident = " " + ident;
        }

        Contract contract;
        deadlock.constraints.constraint.Constraint c;
        Record X;

        Environment env = prev.clone();

        if (vd.hasInitExp()) {
            Exp exp = vd.getInitExp();
            if (exp instanceof PureExp) {
                ResultInferencePureExp resInitExp = typeInferenceAsPure(((PureExp) exp), nident, prev, a, intertoclass,
                        df, cl);
                contract = df.newContractEmpty();
                c = df.newConstraint();
                X = resInitExp.getRepresentative();
            } else {
                ResultInferenceEffExp resInitExp = typeInference(((EffExp) exp), nident, prev, a, intertoclass, df, cl);
                contract = resInitExp.getContract();
                c = resInitExp.getConstraint();
                X = resInitExp.getRecord();
            }
        } else {
            contract = df.newContractEmpty();
            c = df.newConstraint();
            X = df.newRecordVariable();
        }
        env.putVariable(vd.getName(), X);

        return new ResultInferenceStmt(vd.getName(), contract, c, env);
    }

    // ////////////////////////////////////////////////////////////////////////////
    // 2.3. Statments

    public ResultInferenceStmt typeInference(AssignStmt astmt, String ident, Environment env, GroupName a,
            Map<InterfaceDecl, ClassDecl> intertoclass, Factory df, ClassDecl cl) {
        String nident = null;
        if (ident != null) {
            System.out.println(ident + "Contract Inference for the AssignStmt");
            nident = " " + ident;
        }

        String name = astmt.getVar().getName();

        Contract contract;
        deadlock.constraints.constraint.Constraint c;
        Record X;

        Exp exp = astmt.getValue();
        if (exp instanceof PureExp) {
            ResultInferencePureExp resValue = typeInferenceAsPure(((PureExp) exp), nident, env, a, intertoclass, df, cl);
            contract = df.newContractEmpty();
            c = df.newConstraint();
            X = resValue.getRepresentative(env.getVariable(name));
        } else {
            ResultInferenceEffExp resValue = typeInference(((EffExp) exp), nident, env, a, intertoclass, df, cl);
            contract = resValue.getContract();
            c = resValue.getConstraint();
            X = resValue.getRecord();
        }

        if (ident != null) {
            System.out.println(ident + "AssignStmt Sub-Expression Finished");
        }

        if (env.isField(name)) {
            c.addEquation(new ASTNodeInformation(astmt), env.getVariable(name), X);
        } else {
            env.putVariable(name, X);
        } // implementation of the new environment update

        return new ResultInferenceStmt(name, contract, c, env);
    }

    public ResultInferenceStmt typeInference(AwaitStmt astmt, String ident, Environment env, GroupName a,
            Map<InterfaceDecl, ClassDecl> intertoclass, Factory df, ClassDecl clthis) {
        String nident = null;
        if (ident != null) {
            System.out.println(ident + "Contract Inference for the AwaitStmt ");
            nident = " " + ident;
        }

        Contract contract;
        deadlock.constraints.constraint.Constraint c = df.newConstraint();
        GroupName aprime = df.newGroupName();

        // 1. First look if there is some annotation defined
        Iterator<Annotation> it = astmt.getAnnotations().iterator();
        if (it.hasNext()) {
            PureExp dep = it.next().getValue();
            ResultInferencePureExp resAnn = typeInferenceAsPure(dep, nident, env, a, intertoclass, df, clthis);
            contract = df.newContractAwait(astmt, aprime, a);

            if (ident != null) {
                System.out.println(ident + "AwaitStmt Annotation Finished");
            }
            ClassDecl cl;
            Type t = dep.getType();
            if (t.isInterfaceType()) {
                cl = intertoclass.get(((InterfaceType) t).getDecl());
            } else {
                cl = clthis;
            }
            if (cl == null) {
                System.out.println("Class retrival failed!!!");
            } // should NEVER occur
            c.addEquation(new ASTNodeInformation(astmt), resAnn.getRepresentative(), createInstance(cl, df, aprime));

            return new ResultInferenceStmt(null, contract, c, env);
        } else {
            ResultInferencePureExp resGuard = typeInference(astmt.getGuard(), nident, env, a, intertoclass, df, clthis);
            if (ident != null) {
                System.out.println(ident + "AwaitStmt Sub-Expression Finished");
            }
            contract = df.newContractAwait(astmt, a, aprime);
            c.addEquation(new ASTNodeInformation(astmt), df.newRecordFuture(aprime, df.newRecordVariable()),
                    resGuard.getRepresentative());

            return new ResultInferenceStmt(resGuard.getId(), contract, c, env);
        }

    }

    public ResultInferenceStmt typeInference(SkipStmt skip, String ident, Environment env, GroupName a,
            Map<InterfaceDecl, ClassDecl> intertoclass, Factory df, ClassDecl cl) {
        return new ResultInferenceStmt(null, df.newContractEmpty(), df.newConstraint(), env);
    }

    public ResultInferenceStmt typeInference(SuspendStmt susp, String ident, Environment env, GroupName a,
            Map<InterfaceDecl, ClassDecl> intertoclass, Factory df, ClassDecl cl) {
        return new ResultInferenceStmt(null, df.newContractEmpty(), df.newConstraint(), env);
    }

    public ResultInferenceStmt typeInference(DurationStmt dration, String ident, Environment env, GroupName a,
            Map<InterfaceDecl, ClassDecl> intertoclass, Factory df, ClassDecl cl) {
        return new ResultInferenceStmt(null, df.newContractEmpty(), df.newConstraint(), env);
    }

    public ResultInferenceStmt typeInference(ReturnStmt res, String ident, Environment env, GroupName a,
            Map<InterfaceDecl, ClassDecl> intertoclass, Factory df, ClassDecl cl) {
        String nident = null;
        if (ident != null) {
            System.out.println(ident + "Contract Inference for the ReturnStmt ");
            nident = " " + ident;
        }

        ResultInferenceEffExp resRetExp = typeInference(res.getRetExp(), nident, env, a, intertoclass, df, cl);
        if (ident != null) {
            System.out.println(ident + "ReturnStmt Sub-Expression Finished");
        }

        Contract contract = resRetExp.getContract();
        deadlock.constraints.constraint.Constraint c = resRetExp.getConstraint();
        c.addEquation(new ASTNodeInformation(res), env.getVariable("!destiny!"), resRetExp.getRecord());

        return new ResultInferenceStmt(resRetExp.getId(), contract, c, env); // we
                                                                             // need
                                                                             // to
                                                                             // forward
                                                                             // the
                                                                             // id,
                                                                             // in
                                                                             // case
                                                                             // there
                                                                             // is
                                                                             // a
                                                                             // get
                                                                             // in
                                                                             // the
                                                                             // expression
    }

    public ResultInferenceStmt typeInference(ExpressionStmt exp, String ident, Environment env, GroupName a,
            Map<InterfaceDecl, ClassDecl> intertoclass, Factory df, ClassDecl cl) {
        String nident = null;
        if (ident != null) {
            System.out.println(ident + "Contract Inference for the ExpressionStmt ");
            nident = " " + ident;
        }

        // 1. is called the inference on the value expression of assignment
        ResultInferenceEffExp resExp = typeInference(exp.getExp(), nident, env, a, intertoclass, df, cl);
        if (ident != null) {
            System.out.println(ident + "ExpressionStmt Sub-Expression Finished");
        }
        return new ResultInferenceStmt(resExp.getId(), resExp.getContract(), resExp.getConstraint(), env);
    }

    public ResultInferenceStmt typeInference(AssertStmt ass, String ident, Environment env, GroupName a,
            Map<InterfaceDecl, ClassDecl> intertoclass, Factory df, ClassDecl cl) {
        String nident = null;
        if (ident != null) {
            System.out.println(ident + "Contract Inference for the AssertStmt ");
            nident = " " + ident;
        }
        return new ResultInferenceStmt(null, df.newContractEmpty(), df.newConstraint(), env);
    }

    public ResultInferenceStmt typeInference(VarDeclStmt vd, String ident, Environment env, GroupName a,
            Map<InterfaceDecl, ClassDecl> intertoclass, Factory df, ClassDecl cl) {
        String nident = null;
        if (ident != null) {
            System.out.println(ident + "Contract Inference for the VarDeclStmt ");
            nident = " " + ident;
        }
        ResultInferenceStmt resVarDecl = typeInference(vd.getVarDecl(), nident, env, a, intertoclass, df, cl);
        if (ident != null) {
            System.out.println(ident + "VarDeclStmt Sub-Expression Finished");
        }
        return resVarDecl;
    }

    public ResultInferenceStmt typeInference(IfStmt ifstmt, String ident, Environment env, GroupName a,
            Map<InterfaceDecl, ClassDecl> intertoclass, Factory df, ClassDecl cl) {
        String nident = null;
        if (ident != null) {
            System.out.println(ident + "Contract inference of a Conditional Expression");
            nident = " " + ident;
        }
        // as the condition is a pure expression, it doesn't have contract or
        // constraints
        ResultInferenceStmt resThen = typeInference(ifstmt.getThen(), nident, env.clone(), a, intertoclass, df, cl);

        Contract contract;
        deadlock.constraints.constraint.Constraint c = resThen.getConstraint();

        if (ifstmt.hasElse()) {
            ResultInferenceStmt resElse = typeInference(ifstmt.getElse(), nident, env.clone(), a, intertoclass, df, cl);
            c.add(resElse.getConstraint());
            c.add(resThen.getEnvironment().unify(df, new ASTNodeInformation(ifstmt), resElse.getEnvironment())); // ensure
                                                                                                                 // that
                                                                                                                 // the
                                                                                                                 // two
                                                                                                                 // branches
                                                                                                                 // return
                                                                                                                 // the
                                                                                                                 // same
                                                                                                                 // environment
            contract = df.newContractUnion(ifstmt, resThen.getContract(), resElse.getContract());
        } else {
            contract = df.newContractUnion(ifstmt, resThen.getContract(), df.newContractEmpty());
        }
        if (ident != null) {
            System.out.println(ident + "IfStmt Sub-Expression Finished");
        }
        env.updateValues(resThen.getEnvironment());
        return new ResultInferenceStmt(resThen.getId(), contract, c, env);
    }

    public ResultInferenceStmt typeInference(WhileStmt whilestmt, String ident, Environment env, GroupName a,
            Map<InterfaceDecl, ClassDecl> intertoclass, Factory df, ClassDecl cl) {
        String nident = null;
        if (ident != null) {
            System.out.println(ident + "Contract Inference for the While (Approximated with one iteration) ");
            nident = " " + ident;
        }
        ResultInferenceStmt resStmt = typeInference(whilestmt.getBody(), nident, env, a, intertoclass, df, cl);
        if (ident != null) {
            System.out.println(ident + "While Sub-Statments Finished");
        }
        return resStmt;
    }

    public ResultInferenceStmt typeInference(Block b, String ident, Environment envInit, GroupName a,
            Map<InterfaceDecl, ClassDecl> intertoclass, Factory df, ClassDecl cl) {
        String nident = null;
        if (ident != null) {
            System.out.println(ident + "Contract Inference for the Block ");
            nident = " " + ident;
        }

        // accumulate constracts and deadlock.constraints.constraint.Constraints
        // in the resulting output
        ResultInferenceStmt resStmt = null;
        deadlock.constraints.constraint.Constraint c = df.newConstraint();
        Contract contract = df.newContractEmpty();
        Environment env = envInit;
        String prev_id = null;

        for (Stmt s : b.getStmts()) {
            resStmt = typeInference(s, nident, env, a, intertoclass, df, cl);
            c.add(resStmt.getConstraint());
            env = resStmt.getEnvironment();

            // if(prev_id != null) System.out.println("    previous id = " +
            // prev_id);

            if ((resStmt.getId() != null) && (prev_id != null) && (resStmt.getId().equals(prev_id))) {
                contract.fusion(resStmt.getContract());
            } else {
                prev_id = resStmt.getId();
                contract.add(resStmt.getContract());
            }
            // for debugging
            // if(resStmt.getId() != null)
            // System.out.println("    current id = " + resStmt.getId());
        }
        if (ident != null) {
            System.out.println(ident + "Block Sub-Statments Finished");
        }

        // finish
        if (resStmt == null) {
            return new ResultInferenceStmt(null, df.newContractEmpty(), df.newConstraint(), env);
        } else {
            return new ResultInferenceStmt(resStmt.getId(), contract, c, env);
        }
    }

    // ////////////////////////////////////////////////////////////////////////////
    // 2.4. Expressions

    // 2.4.1. Pure Expressions

    public ResultInferencePureExp typeInferenceAsPure(NullExp exp, String ident, Environment env, GroupName a,
            Map<InterfaceDecl, ClassDecl> intertoclass, Factory df, ClassDecl cl) {
        if (ident != null) {
            System.out.println(ident + "Contract inference of 'null' PureExp ");
        }
        return new ResultInferencePureExp(df, null, df.newRecordVariableFanthom());
    }

    public ResultInferencePureExp typeInferenceAsPure(ThisExp exp, String ident, Environment env, GroupName a,
            Map<InterfaceDecl, ClassDecl> intertoclass, Factory df, ClassDecl cl) {
        if (ident != null) {
            System.out.println(ident + "Contract inference of 'this' PureExp ");
        }
        return new ResultInferencePureExp(df, "this", env.getVariable("this"));
    }

    public ResultInferencePureExp typeInferenceAsPure(VarOrFieldUse var, String ident, Environment env, GroupName a,
            Map<InterfaceDecl, ClassDecl> intertoclass, Factory df, ClassDecl cl) {
        if (ident != null) {
            System.out.println(ident
                    + "Contract Inference for the VarOrFieldUse \""
                    + var.getName()
                    + "\" (considered as a"
                    + ((env.getVariable(var.getName()) instanceof RecordVariable)
                            && (((RecordVariable) env.getVariable(var.getName())).isDataType()) ? " datatype)"
                            : "n object)"));
        }
        return new ResultInferencePureExp(df, var.getName(), env.getVariable(var.getName()));
    }

    public ResultInferencePureExp typeInferenceAsPure(LiteralExp lit, String ident, Environment env, GroupName a,
            Map<InterfaceDecl, ClassDecl> intertoclass, Factory df, ClassDecl cl) {
        if (ident != null) {
            System.out.println(ident + "Contract inference of a Literal ");
        }
        return new ResultInferencePureExp(df, null, df.getDataType());
    }

    public ResultInferencePureExp typeInferenceAsPure(DataConstructorExp exp, String ident, Environment env,
            GroupName a, Map<InterfaceDecl, ClassDecl> intertoclass, Factory df, ClassDecl cl) { // DATATYPES
        String nident = null;
        if (ident != null) {
            System.out.println(ident + "Contract inference of the Data type Constructor \"" + exp.getConstructor()
                    + "\"");
            nident = " " + ident;
        }
        ResultInferencePureExp res = new ResultInferencePureExp(df);
        for (PureExp param : exp.getParams()) {
            res.add(typeInferenceAsPure(param, nident, env, a, intertoclass, df, cl));
        }
        return res;
    }

    public ResultInferencePureExp typeInferenceAsPure(LetExp exp, String ident, Environment env, GroupName a,
            Map<InterfaceDecl, ClassDecl> intertoclass, Factory df, ClassDecl cl) {
        String nident = null;
        if (ident != null) {
            System.out.println(ident + "Contract inference of a Let Expression ");
            nident = " " + ident;
        }
        ResultInferencePureExp res = typeInferenceAsPure(exp.getVal(), nident, env, a, intertoclass, df, cl);
        Record X = res.getRepresentative();
        Environment envExp = env.clone();
        envExp.putVariable(exp.getVar().getName(), X);
        res.add(typeInferenceAsPure(exp.getExp(), nident, envExp, a, intertoclass, df, cl));
        /*
         * Record X = resVal.getRecord(); Environment envExp = env.clone();
         * envExp.putVariable(exp.getVar().getName(), X); ResultInferenceStmt
         * resExp = typeInference(exp.getExp(), nident, envExp, a, intertoclass,
         * df, cl);
         * 
         * Contract contract = resVal.getContract();
         * contract.add(resExp.getContract());
         * deadlock.constraints.constraint.Constraint c =
         * resVal.getConstraint(); c.add(resExp.getConstraint());
         */

        return res;
    }

    public ResultInferencePureExp typeInferenceAsPure(FnApp fn, String ident, Environment env, GroupName a,
            Map<InterfaceDecl, ClassDecl> intertoclass, Factory df, ClassDecl cl) { // DATATYPES
        String nident = null;
        if (ident != null) {
            System.out.println(ident + "Contract inference of a Function Application ");
            nident = " " + ident;
        }
        ResultInferencePureExp res = new ResultInferencePureExp(df);
        for (PureExp param : fn.getParams()) {
            res.add(typeInferenceAsPure(param, nident, env, a, intertoclass, df, cl));
        }
        return res;
    }

    public ResultInferencePureExp typeInferenceAsPure(IfExp ifExp, String ident, Environment env, GroupName a,
            Map<InterfaceDecl, ClassDecl> intertoclass, Factory df, ClassDecl cl) {
        String nident = null;
        if (ident != null) {
            System.out.println(ident + "Contract inference of an Exp Conditional Expression");
            nident = " " + ident;
        }
        // as the condition is a pure expression, its contract and constraint
        // are empty
        ResultInferencePureExp res = typeInferenceAsPure(ifExp.getThenExp(), nident, env, a, intertoclass, df, cl);
        res.add(typeInferenceAsPure(ifExp.getElseExp(), nident, env, a, intertoclass, df, cl));
        return res;
    }

    public ResultInferencePureExp typeInferenceAsPure(Unary un, String ident, Environment env, GroupName a,
            Map<InterfaceDecl, ClassDecl> intertoclass, Factory df, ClassDecl cl) { // DATATYPES
        String nident = null;
        if (ident != null) {
            System.out.println(ident + "Contract Inference for an Unary Pure Expression -> no contract and no record ");
            nident = " " + ident;
        }
        return typeInferenceAsPure(un.getOperand(), nident, env, a, intertoclass, df, cl);
    }

    public ResultInferencePureExp typeInferenceAsPure(Binary bin, String ident, Environment env, GroupName a,
            Map<InterfaceDecl, ClassDecl> intertoclass, Factory df, ClassDecl cl) { // DATATYPES
        String nident = null;
        if (ident != null) {
            System.out.println(ident + "Contract Inference for an Unary Pure Expression -> no contract and no record ");
            nident = " " + ident;
        }
        ResultInferencePureExp res = typeInferenceAsPure(bin.getLeft(), nident, env, a, intertoclass, df, cl);
        res.add(typeInferenceAsPure(bin.getRight(), nident, env, a, intertoclass, df, cl));
        return res;
    }

    // TODO:
    // CaseExp : PureExp ::= Expr:PureExp Branch:CaseBranch* ;
    // CaseBranch ::= Left:Pattern Right:PureExp ;
    // abstract Pattern ;
    // PatternVarUse: Pattern ::= <Name>;
    // PatternVar: Pattern ::= Var:PatternVarDecl;
    // ConstructorPattern: Pattern ::= <Constructor> Param:Pattern*;
    // LiteralPattern: Pattern ::= Literal:LiteralExp;
    // UnderscorePattern: Pattern;

    // 2.4.2. Expressions with side effects.

    public ResultInferenceEffExp typeInference(NewExp newExp, String ident, Environment env, GroupName a,
            Map<InterfaceDecl, ClassDecl> intertoclass, Factory df, ClassDecl clthis) {
        String nident = null;
        if (ident != null) {
            System.out.println(ident + "Contract Inference for the NewExp ");
            nident = " " + ident;
        }

        // 1. Create the record for the new object, and collect contract and
        // constraints from arguments
        ClassDecl cl = (ClassDecl) (newExp.lookup(new KindedName(Kind.CLASS, newExp.getClassName())));
        LinkedList<RecordField> fields = new LinkedList<RecordField>();
        int i = 0;
        Contract contract = df.newContractEmpty();
        deadlock.constraints.constraint.Constraint c = df.newConstraint();
        Environment envFields = env.clone();
        // 1.1. Params
        ResultInferencePureExp resParam;
        for (PureExp p : newExp.getParams()) {
            resParam = typeInferenceAsPure(p, nident, env, a, intertoclass, df, cl);
            fields.add(df.newRecordField(cl.getParam(i).getName(), resParam.getRepresentative()));
            envFields.putVariable(cl.getParam(i).getName(), resParam.getRepresentative());
            i++;
        }
        // 1.2. Fields
        for (FieldDecl f : cl.getFields()) {
            if (f.hasInitExp()) {
                resParam = typeInferenceAsPure(f.getInitExp(), ident, envFields, a, intertoclass, df, cl);
                fields.add(df.newRecordField(f.getName(), resParam.getRepresentative()));
            } else {
                fields.add(df.newRecordField(f.getName(), df.newRecordVariable()));
            }
        }
        // 1.3. Group Name
        GroupName aprime; // depends if the new object is in the same cog or not
        if (!newExp.hasLocal()) {
            aprime = df.newGroupName();
        } else {
            aprime = a;
        }
        Record r = df.newRecordPresent(aprime, fields);

        // 1.4. Calling the init of r
        MethodInterface miinit = df.newMethodInterface(r, new LinkedList<Record>(), df.newRecordVariable());
        c.addSemiEquation(new ASTNodeInformation(newExp), env.getMethod(cl.getName(), "!init!"), miinit);
        contract.add(df.newContractInvk(newExp, cl.getName(), "!init!", miinit));

        return new ResultInferenceEffExp(null, r, contract, c, env);
    }

    public ResultInferenceEffExp typeInference(Call call, String ident, Environment env, GroupName a,
            Map<InterfaceDecl, ClassDecl> intertoclass, Factory df, ClassDecl clthis) {
        String nident = null;
        if (ident != null) {
            System.out.println(ident + "Contract Inference for a "
                    + ((call instanceof SyncCall) ? "Synchronous" : "Asynchronous") + "  method Call");
            nident = " " + ident;
        }

        // 1. Get the method interface
        Type t = call.getCallee().getType();
        ClassDecl cl;
        if (t.isInterfaceType()) {
            cl = intertoclass.get(((InterfaceType) t).getDecl());
        } else {
            cl = clthis;
        }

        if (cl == null) {
            // we are in presence of an non implemented interface
            // in that case, we don't know how the method would behave, and
            // simply put a null contract.
            System.out.println("Class retrival failed!!!");
            return new ResultInferenceEffExp(null, df.newRecordVariable(), df.newContractEmpty(), df.newConstraint(),
                    env);
        } else {

            // 2. Collect contracts and
            // deadlock.constraints.constraint.Constraints from the call
            ResultInferencePureExp resCallee = typeInferenceAsPure(call.getCallee(), nident, env, a, intertoclass, df,
                    cl);
            Contract contract = df.newContractEmpty();
            deadlock.constraints.constraint.Constraint c = df.newConstraint();
            Record callee = resCallee.getRepresentative();

            LinkedList<Record> s = new LinkedList<Record>();
            ResultInferencePureExp resParam;
            for (PureExp p : call.getParams()) {
                resParam = typeInferenceAsPure(p, nident, env, a, intertoclass, df, cl);
                s.add(resParam.getRepresentative());
            }

            // 3. Construct the record for the return value
            Record Y = df.newRecordVariable();

            // 4. pack up the result
            MethodInterface mi = df.newMethodInterface(callee, s, Y);
            c.addSemiEquation(new ASTNodeInformation(call), env.getMethod(cl.getName(), call.getMethod()), mi);

            Record r;
            if (call instanceof SyncCall) {
                r = Y;
                contract.add(df.newContractSyncInvk(call, cl.getName(), call.getMethod(), mi));
            } else {
                contract.add(df.newContractInvk(call, cl.getName(), call.getMethod(), mi));

                GroupName aprime = df.newGroupName();
                Record calleeShape = createInstance(cl, df, aprime);
                c.addEquation(new ASTNodeInformation(call), callee, calleeShape);
                r = df.newRecordFuture(aprime, Y);
            }
            return new ResultInferenceEffExp(null, r, contract, c, env);
        }
    }

    public ResultInferenceEffExp typeInference(GetExp exp, String ident, Environment env, GroupName a,
            Map<InterfaceDecl, ClassDecl> intertoclass, Factory df, ClassDecl cl) {
        String nident = null;
        if (ident != null) {
            System.out.println(ident + "Contract Inference for a GetExp ");
            nident = " " + ident;
        }

        // 1. first is calculate the inference of the expression 'e' of 'e.get'
        ResultInferencePureExp resPureExp = typeInferenceAsPure(exp.getPureExp(), nident, env, a, intertoclass, df, cl);
        if (ident != null) {
            System.out.println(ident + "GetExp Sub-Expression Finished");
        }

        Contract contract = df.newContractEmpty();
        deadlock.constraints.constraint.Constraint c = df.newConstraint();

        // 2. record for the result
        GroupName aprime = df.newGroupName();
        Record X = df.newRecordVariable();
        c.addEquation(new ASTNodeInformation(exp), df.newRecordFuture(aprime, X), resPureExp.getRepresentative());

        // pack up the result
        contract.fusion(df.newContractGet(exp, a, aprime));
        return new ResultInferenceEffExp(resPureExp.getId(), X, contract, c, env);
    }

    // ////////////////////////////////////////////////////////////////////////////
    // 2.5. Guards

    public ResultInferencePureExp typeInference(ClaimGuard cg, String ident, Environment env, GroupName a,
            Map<InterfaceDecl, ClassDecl> intertoclass, Factory df, ClassDecl cl) {
        String nident = null;
        if (ident != null) {
            System.out.println(ident + "Contract Inference for the ClaimGuard ");
            nident = " " + ident;
        }
        return typeInferenceAsPure(cg.getVar(), nident, env, a, intertoclass, df, cl);
    }

    // TODO; AndGuard, ExpGuard, DurationGuard

    // END TYPE INFERENCE

    // TYPE INFERENCE OF MAIN! Done only after right creation of CCT
    /*
     * 
     * //only for Main.main //TI-ModuleDecl public ResultInference
     * ModuleDecl.typeInferenceMain(Environment env, Map<Interface, ClassDecl>
     * intertoclass, Factory df) {
     * System.out.println("Level 2: Contract Inference for the module \"" +
     * this.getName() + "\"");
     * 
     * ResultInference res = new ResultInference();
     * 
     * //1. Calculate the ModuleDecl Inference by collecting together the
     * ClassDecls Inferences for (Decl d : getDecls()){ if(d instanceof
     * ClassDecl){ res.add(((ClassDecl) d).typeInference(env, itfToClass, df,
     * semi)); } }
     * 
     * //2. If the modules has a Main, then we put also a Main.main method with
     * this contract if (hasBlock()) { GroupName amain = df.newGroupName();
     * //Variable name of this into the Main ResultInferenceStmt resBlock =
     * getBlock().typeInference(env, amain, itfToClass, df, semi);
     * MethodInterface mi = df.newMethodInterface(df.newRecordVariable(), new
     * LinkedList<Record>(), df.newRecordVariable()); res.add("!Main!",
     * "!main!", df.newMethodContract(mi, resBlock.getContract())); //I invent
     * this "method name" Main.main to recognize the contract of main
     * res.add(resBlock.getConstraint()); } return res; }
     * 
     * 
     * //only for Main.main //TI-CompilationUnit public ResultInference
     * CompilationUnit.typeInferenceMain(Environment g, HashMap<String,
     * ClassDecl> itfToClass, Factory df, boolean semi) {
     * 
     * System.out.println("Level 1: Contract Inference for the file \"" +
     * this.getName() + "\"");
     * 
     * 
     * ResultInference res = new ResultInference();
     * 
     * //1. Calculate the CompilationUnit Inference by collecting together the
     * ModuleDecls Inferences for (ModuleDecl d : getModuleDecls()){ //Like in
     * Environment generation I avoid ModuleDecl starting with ABS, that are
     * automatically generated and are not part of our analisys
     * //if(!(d.getName()).startsWith("ABS")){ res.add(d.typeInferenceMain(g,
     * itfToClass, df, semi)); //} } return res; }
     * 
     * 
     * //Only for Main.main //TI-Model public ResultInference
     * Model.typeInferenceMain(Environment g, Factory df, boolean semi) {
     * 
     * ResultInference res = new ResultInference(); HashMap<String, ClassDecl>
     * intertoclass = new HashMap<String, ClassDecl>();//interfToClass();
     * 
     * //due to a compilationUnit not part of our analysis we skipped the
     * "abslang.abs" unit //1. Calculate the model Inference by collecting
     * together the CompilationUnits Inferences for (CompilationUnit c :
     * getCompilationUnits()){ if(c.getName().endsWith("abslang.abs")) {
     * System.out.println("  Inference skipped"); } else {
     * res.add(c.typeInferenceMain(g, intertoclass, df, semi)); } } return res;
     * }
     * 
     * //I want the environment outside to reintroduce it for a new inference
     * cycle public Environment Model.getEnvironment(Factory df){ return
     * this.environment(df); }
     */
    // END NEW CODE
    // END TYPEINFERENCE

    /*
     * //TI-Field public ResultInferenceStmt FieldUse.typeInference(Environment
     * g, Term a, HashMap<String, ClassDecl> itfToClass, Factory df, boolean
     * semi) {
     * 
     * System.out.println("Level 5: Contract Inference for the FieldUse ");
     * 
     * ClassDecl classOfThis;
     * 
     * // 1. crazy way to adjust and write more elegant to find the ClassDecl of
     * this
     * 
     * if((this.getParent().getClass().getName()).endsWith("ClassDecl"))
     * classOfThis = (ClassDecl) this.getParent(); else
     * if((this.getParent().getParent
     * ().getClass().getName()).endsWith("ClassDecl")) classOfThis = (ClassDecl)
     * this.getParent().getParent(); else
     * if((this.getParent().getParent().getParent
     * ().getClass().getName()).endsWith("ClassDecl")) classOfThis = (ClassDecl)
     * this.getParent().getParent().getParent(); else
     * if((this.getParent().getParent
     * ().getParent().getParent().getClass().getName()).endsWith("ClassDecl"))
     * classOfThis = (ClassDecl)
     * this.getParent().getParent().getParent().getParent(); else
     * if((this.getParent
     * ().getParent().getParent().getParent().getParent().getClass
     * ().getName()).endsWith("ClassDecl")) classOfThis = (ClassDecl)
     * this.getParent().getParent().getParent().getParent().getParent(); else
     * if(
     * (this.getParent().getParent().getParent().getParent().getParent().getParent
     * ().getClass().getName()).endsWith("ClassDecl")) classOfThis = (ClassDecl)
     * this
     * .getParent().getParent().getParent().getParent().getParent().getParent();
     * else
     * if((this.getParent().getParent().getParent().getParent().getParent().
     * getParent().getParent().getClass().getName()).endsWith("ClassDecl"))
     * classOfThis = (ClassDecl)
     * this.getParent().getParent().getParent().getParent
     * ().getParent().getParent().getParent(); else
     * if((this.getParent().getParent
     * ().getParent().getParent().getParent().getParent
     * ().getParent().getParent().getClass().getName()).endsWith("ClassDecl"))
     * classOfThis = (ClassDecl)
     * this.getParent().getParent().getParent().getParent
     * ().getParent().getParent().getParent().getParent(); else
     * if((this.getParent
     * ().getParent().getParent().getParent().getParent().getParent
     * ().getParent()
     * .getParent().getParent().getClass().getName()).endsWith("ClassDecl"))
     * classOfThis = (ClassDecl)
     * this.getParent().getParent().getParent().getParent
     * ().getParent().getParent().getParent().getParent().getParent(); else
     * if((this
     * .getParent().getParent().getParent().getParent().getParent().getParent
     * ().getParent
     * ().getParent().getParent().getParent().getClass().getName()).endsWith
     * ("ClassDecl")) classOfThis = (ClassDecl)
     * this.getParent().getParent().getParent
     * ().getParent().getParent().getParent
     * ().getParent().getParent().getParent().getParent(); else
     * if((this.getParent
     * ().getParent().getParent().getParent().getParent().getParent
     * ().getParent()
     * .getParent().getParent().getParent().getParent().getClass().
     * getName()).endsWith("ClassDecl")) classOfThis = (ClassDecl)
     * this.getParent
     * ().getParent().getParent().getParent().getParent().getParent
     * ().getParent().getParent().getParent().getParent().getParent(); else
     * if((this
     * .getParent().getParent().getParent().getParent().getParent().getParent
     * ().getParent
     * ().getParent().getParent().getParent().getParent().getParent()
     * .getClass().getName()).endsWith("ClassDecl")) classOfThis = (ClassDecl)
     * this
     * .getParent().getParent().getParent().getParent().getParent().getParent
     * ().getParent
     * ().getParent().getParent().getParent().getParent().getParent(); else
     * if((this
     * .getParent().getParent().getParent().getParent().getParent().getParent
     * ().getParent
     * ().getParent().getParent().getParent().getParent().getParent()
     * .getParent().getClass().getName()).endsWith("ClassDecl")) classOfThis =
     * (ClassDecl)
     * this.getParent().getParent().getParent().getParent().getParent
     * ().getParent
     * ().getParent().getParent().getParent().getParent().getParent()
     * .getParent().getParent(); else classOfThis = null;
     * 
     * 
     * 
     * 
     * 
     * ResultInferenceStmt res = new ResultInferenceStmt(); // 2. here it is
     * created a frash this with fresh variable inside field Term aprime =
     * df.freshTermVariable(); LinkedList<Term> fields = new LinkedList<Term>();
     * 
     * for(FieldDecl f : classOfThis.getFields()){ Term Y =
     * df.freshTermVariable(); fields.add(df.newRecordField(f.getName(), Y)); //
     * 3. the right field f extract in this.f assign the record to the rule
     * T-Field if((f.getName()).equals(this.getName())) {res.setRecord(Y);} }
     * 
     * 
     * //Need to write
     * deadlock.deadlock.constraints.constraint.Constraints.deadlock
     * .constraints.
     * constraint.Constraint.deadlock.constraints.constraint.Constraint instead
     * of deadlock.constraints.constraint.Constraint because of ABS compiler.
     * 
     * // 4. deadlock.constraints.constraint.Constraint returned is an equation
     * between real this of the environment and the new fresh one
     * semiUnification
     * .deadlock.constraints.constraint.Constraint.deadlock.constraints
     * .constraint.Constraint constr = df.newConstraint();
     * constr.addEquation(df.newRecordPresent(aprime, fields),
     * g.getVariable("this")); res.setConstraint(constr); res.setEnvironment(g);
     * 
     * return res; }
     */

}
