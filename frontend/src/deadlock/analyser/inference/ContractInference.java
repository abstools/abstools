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
import java.util.List;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;

import choco.kernel.common.util.tools.ArrayUtils;

import com.gzoumix.semisolver.constraint.Constraint;
import com.gzoumix.semisolver.term.*;

import deadlock.analyser.AnalyserLog;
import deadlock.analyser.factory.*;
import deadlock.analyser.generation.*;

/**
 * @author Abel, Michael
 * 
 */
public class ContractInference {

    private static final String _initName = "!init!";
    private static final String _runName = "run";
    private static final String _destinyName = "!destiny!";
    private static final String _this = "this";

    private static final String _dummyMethod = "!dummy";
    private static final String _dummyClass = "!Dummy";
    private static final String _dummyModule = "!Dummy";

    private static ClassDecl _emptyDecl;

    private AnalyserLog _log;
    private Factory _df;
    private Map<InterfaceDecl, ClassDecl> _intertoclass;
    private Model _model;
    private TypingEnvironment _env;
    private GroupName _a;
    private ClassDecl _cd;

    public ContractInference(AnalyserLog log, Factory df, Model m) {
        _log = log;
        _df = df;
        _intertoclass = null;
        _model = m;
        _env = new TypingEnvironment();
        _a = null;
        _cd = null;

        _emptyDecl = new ClassDecl();
        _emptyDecl.setName(_dummyClass);
        MethodSig methodSig = new MethodSig(_dummyMethod, new abs.frontend.ast.List<Annotation>(), new DataTypeUse(),
                new abs.frontend.ast.List<ParamDecl>());
        MethodImpl node = new MethodImpl(methodSig, new Block(), false);

        _emptyDecl.addMethod(node);
    }

    /************************************/
    /* Helper function */
    /************************************/
    // create a record instance of the particular class, living in the cog a
    public RecordPresent createInstance(ClassDecl cd, GroupName a) {
        LinkedList<RecordField> l = new LinkedList<RecordField>();
        for (ParamDecl f : cd.getParams()) {
            RecordVariable X = _df.newRecordVariable();
            l.add(_df.newRecordField(f.getName(), X));
        }
        for (FieldDecl f : cd.getFields()) {
            l.add(_df.newRecordField(f.getName(), _df.newRecordVariable())); // init
                                                                             // expressions
                                                                             // are
                                                                             // managed
                                                                             // in
                                                                             // the
                                                                             // analysis
                                                                             // of
                                                                             // the
                                                                             // init
                                                                             // block.
        }
        return _df.newRecordPresent(a, l);
    }

    private RecordPresent createInstance(Type t, ClassDecl clthis, GroupName a) {
        ClassDecl cl;
        if (t.isInterfaceType()) {
            cl = _intertoclass.get(((InterfaceType) t).getDecl());
        } else {
            cl = clthis;
        }
        if (cl == null) {
            _log.logError("Class retrival failed!!!");
        } // should NEVER occur
        return this.createInstance(cl, a);
    }

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

    // reviewed //
    public void computeMapInterfaceToClass() {
        Map<InterfaceDecl, ClassDecl> res = new HashMap<InterfaceDecl, ClassDecl>();

        List<InterfaceDecl> allInterfaces = new LinkedList<InterfaceDecl>();

        for (Decl decl : _model.getDecls()) {
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
                        _log.logWarning("WARNING: the class \"" + res.get(d).getName() + "\" and \""
                                + ((ClassDecl) decl).qualifiedName() + "\" both implement the interface \""
                                + d.qualifiedName() + "\"."
                                + "This will probably cause an erroneous deadlock analysis");
                    } else {
                        res.put(d, (ClassDecl) decl);
                    }
                }
            } else if (decl instanceof InterfaceDecl)
                allInterfaces.add((InterfaceDecl) decl);
        }

        allInterfaces.removeAll(res.keySet());

        for (InterfaceDecl notImplInterface : allInterfaces) {
            res.put(notImplInterface, _emptyDecl);
        }

        _intertoclass = res;
    }

    /************************************/
    /* Environment Creation */
    /************************************/

    public void computeEnvironment() {
        for (CompilationUnit cu : _model.getCompilationUnits()) {
            for (ModuleDecl md : cu.getModuleDecls()) {
                for (Decl d : md.getDecls()) {
                    if (d instanceof ClassDecl) {
                        computeEnvironment(((ClassDecl) d), md.getName());
                    } else if (d instanceof FunctionDecl) {
                        computeEnvironment(((FunctionDecl) d), md.getName());
                    } else if (d instanceof DataTypeDecl) {
                        computeEnvironment(((DataTypeDecl) d), md.getName());
                    }
                }
            }
        }

        computeEnvironment(_emptyDecl, _dummyModule);
    }

    // method declaration
    public void computeEnvironment(ClassDecl cd, String moduleName) {

        if (cd == _emptyDecl)
            return;
        // Methods
        for (MethodImpl m : cd.getMethods()) {
            _log.logNormal("Generating initial environment for the method \"" + cd.getName() + "."
                    + m.getMethodSig().getName() + "\"");
            // 1. Record of "this"
            IRecord rthis = createInstance(cd, _df.newGroupName());
            // 2. Simple variables for the method parameters
            LinkedList<IRecord> rparam = new LinkedList<IRecord>();
            for (ParamDecl p : m.getMethodSig().getParams()) {
                rparam.add(_df.newRecordVariable());
            }
            // 3. Simple variable for the return object (or datatype)
            RecordVariable rres = _df.newRecordVariable();
            // 4. Finalize
            MethodInterface mIntf = _df.newMethodInterface(rthis, rparam, rres);
            _env.putMethod(moduleName, cd.getName(), m.getMethodSig().getName(), mIntf);
        }
        // 2. init
        MethodInterface mi = _df.newMethodInterface(createInstance(cd, _df.newGroupName()), new LinkedList<IRecord>(),
                _df.newRecordVariable());
        _env.putMethod(moduleName, cd.getName(), _initName, mi);
    }

    // functions declaration
    public void computeEnvironment(FunctionDecl decl, String moduleName) {
        String name = decl.getName();

        // 1. type parameters
        HashMap<String, RecordVariable> typeParameterMap = new HashMap<>();
        if (decl instanceof ParametricFunctionDecl) {
            for (TypeParameterDecl args : ((ParametricFunctionDecl) decl).getTypeParameterList()) {
                typeParameterMap.put(args.getName(), _df.newRecordVariable());
            }
        }

        // 2. parameters, result record, and put the resulting Function
        // Interface in the typing environment
        List<Term> l = new LinkedList();
        for (ParamDecl pd : decl.getParamList()) {
            Access a = pd.getAccess(); // get the type of the parameter
            if (a instanceof TypeUse) {
                l.add(expandArgs((TypeUse) a, typeParameterMap));
            } else {
                _log.logError("unable to retrieve the type of the parameter \"" + pd.getName() + "\" of Function \""
                        + moduleName + "." + name + "\"");
                return;
            } // should never occur
        }
        _env.putFunction(moduleName, name, new FunctionInterface(l, expandArgs(decl.getTypeUse(), typeParameterMap)));
    }

    private Term expandArgs(TypeUse arg, HashMap<String, RecordVariable> map) {
        if (arg instanceof DataTypeUse) { // datatype
            java.util.List<Term> l = new LinkedList<>();
            if (arg instanceof ParametricDataTypeUse) { // it is recursive
                for (TypeUse subarg : ((ParametricDataTypeUse) arg).getParamList()) {
                    l.add(this.expandArgs(subarg, map));
                }
            }
            return _df.newTerm(arg.getName(), l);
        } else if (arg instanceof InterfaceTypeUse) {
            ClassDecl c = _intertoclass.get(((InterfaceTypeUse) arg).getDecl());
            return createInstance(c, _df.newGroupName());
        } else if (arg instanceof TypeParameterUse) { // we have a variable
            return map.get(((TypeParameterUse) arg).getName());
        }

        // Should never occur...
        _log.logError("Unknown error in method expandArgs of deadlock analysis");
        return null;
    }

    // datatype declaration
    public void computeEnvironment(DataTypeDecl decl, String moduleName) { // TODO
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

    public ResultInference typeInference() {
        ResultInference res = new ResultInference();

        _log.logDebug("Initial Environment:\n" + _env.toString());

        // 1. methods
        for (CompilationUnit cu : _model.getCompilationUnits()) {
            res.add(typeInference(cu));
        }
        // 2. main
        /*
         * _a = _df.newGroupName(); ResultInferenceStmt resMain =
         * typeInference(_model.getMainBlock());
         * 
         * res.add(resMain.getConstraint()); Contract co =
         * _df.newContractEmpty(); for(TypingEnvironment te :
         * resMain.getEnvironment()) co =
         * _df.newContractUnion(_model.getMainBlock(), co,
         * _df.newContract(te.unsync(_model.getMainBlock())));
         * res.setMain(resMain.getContract(), co);
         */

        return res;
    }

    public ResultInference typeInference(CompilationUnit cu) {
        ResultInference res = new ResultInference();
        _log.logDebug("Contract Inference for the file \"" + cu.getName() + "\"");
        _log.beginIndent();
        Set<String> searchPath = new HashSet<>();
        for (ModuleDecl md : cu.getModuleDecls()) {
            searchPath.add(md.getName());
            res.add(typeInference(md));
        }

        if (cu.hasMainBlock()) {
            searchPath.add("ABS.StdLib");
            _env.setSearchPath(searchPath);
            _a = _df.newGroupName();
            ResultInferenceStmt resMain = typeInference(cu.getMainBlock());

            res.add(resMain.getConstraint());
            Contract co = _df.newContractEmpty();
            for (TypingEnvironment te : resMain.getEnvironment())
                co = _df.newContractUnion(cu.getMainBlock(), co, _df.newContract(te.unsync(_model.getMainBlock())));
            res.setMain(resMain.getContract(), co);
        }
        _log.endIndent();
        return res;
    }

    public ResultInference typeInference(ModuleDecl md) {
        ResultInference res = new ResultInference();
        _log.logDebug("Contract Inference for the module \"" + md.getName() + "\"");
        _log.beginIndent();
        // 1. add the import to the visibility list
        _env.addSearchPath(md.getName()); // current library
        _env.addSearchPath("ABS.StdLib"); // default library
        for (Import imp : md.getImportList()) {
            if (imp instanceof NamedImport) {
                for (Name name : ((NamedImport) imp).getNameList())
                    _env.addSearchPath(name.getName());
            } // WARNING: not sure what that is, there are no usage examples
              // from the examples
            else if (imp instanceof StarImport) {
                _env.addSearchPath(((StarImport) imp).getModuleName());
            } else if (imp instanceof FromImport) {
                _env.addSearchPath(((FromImport) imp).getModuleName());
            } // WARNING: we are not precise, we include all declaration from
              // the module
        }

        _log.logDebug("module " + md.getName() + ": initial search path = " + _env.toStringSearchPath());
        for (Decl d : md.getDecls()) {
            if (d instanceof ClassDecl) {
                res.add(typeInference(((ClassDecl) d)));
            } else if (d instanceof FunctionDecl) {
            } // TODO: add function definition
        }
        _env.resetSearchPath();
        _log.endIndent();
        return res;
    }

    public ResultInference typeInference(ClassDecl cd) {
        ResultInference res = new ResultInference();
        if (cd == _emptyDecl)
            return res;
        _log.logDebug("Contract Inference for the class \"" + cd.getName() + "\"");
        _log.beginIndent();

        _cd = cd;

        // 1. Methods
        for (MethodImpl m : cd.getMethods()) {
            res.add(typeInference(m));
        }

        // 2. Init
        MethodInterface mi = _env.getMethod(cd.getName(), _initName);
        IRecord thisRecord = mi.getThis();
        _a = ((RecordPresent) thisRecord).getRoot();
        Constraint c = _df.newConstraint();
        Contract cp, cf;

        _env.newScope();
        _env.putVariable(_this, thisRecord);

        // 2.1. Field assignments
        for (FieldDecl f : cd.getFields()) {
            if (f.hasInitExp()) {
                ResultInferencePureExp tmp = typeInferenceAsPure(f.getInitExp());
                c.add(tmp.getConstraint());
                c.addEquation(new ASTNodeInformation(f), ((RecordPresent) thisRecord).getField(f.getName()),
                        _env.getRecord(tmp.getVariableType()));
            }
        }

        // 2.2. Init block
        if (cd.hasInitBlock()) {
            ResultInferenceStmt resInitBlock = typeInference(cd.getInitBlock());
            cp = resInitBlock.getContract();
            c.add(resInitBlock.getConstraint());

            cf = _df.newContractEmpty();
            for (TypingEnvironment te : resInitBlock.getEnvironment())
                cf = _df.newContractUnion(cd.getInitBlock(), cf, _df.newContract(te.unsync(cd.getInitBlock())));
        } else {
            cp = _df.newContractEmpty();
            cf = _df.newContractEmpty();
        }
        // 2.3. add the call to run if the method exists.
        if (_env.getMethod(cd.getName(), _runName) != null) {
            ASTNode node = (cd.hasInitBlock() ? cd.getInitBlock() : cd);

            MethodInterface mirun = _df.newMethodInterface(thisRecord, new LinkedList<IRecord>(),
                    _df.newRecordVariable());
            c.addSemiEquation(new ASTNodeInformation(node), _env.getMethod(cd.getName(), _runName), mirun);

            List<Contract> tmp = new LinkedList<>();
            tmp.add(cf);
            tmp.add(_df.newContractInvk(node, cd.getName(), _runName, mirun));
            cf = _df.newContractParallel(node, tmp);
        }

        res.add(c);
        res.add(cd.getName(), _initName, _df.newMethodContract(mi, cp, cf));

        _env.clearFutures();
        _env.clearVariables();

        _log.endIndent();
        _log.logDebug("Inference for the class \"" + cd.getName() + "\" Finished");
        return res;
    }

    public ResultInference typeInference(MethodImpl mImp) {
        _log.logDebug(
                "Contract Inference for the method \"" + _cd.getName() + "." + mImp.getMethodSig().getName() + "\"");
        _log.beginIndent();

        // 1. Generate the environment for the contract inference
        _env.newScope();
        MethodInterface mi = _env.getMethod(_cd.getName(), mImp.getMethodSig().getName());

        IRecord thisRecord = mi.getThis();
        List<IRecord> params = mi.getParameters();
        IRecord result = mi.getResult();
        _a = ((RecordPresent) thisRecord).getRoot();
        _env.putVariable(_this, thisRecord); // WARNING: problem with inference
                                             // if futures in this
        Iterator<IRecord> it = params.iterator();
        for (ParamDecl p : mImp.getMethodSig().getParams()) {
            if (p.getType().isFutureType()) {
                TypingEnvironmentVariableTypeFuture f = new TypingEnvironmentVariableTypeFuture();
                _env.putVariable(p.getName(), f);
                _env.putFuture(f, new TypingEnvironmentFutureTypeTick(it.next()));
            } else {
                _env.putVariable(p.getName(), it.next());
            }
        }
        _env.putVariable(_destinyName, result);

        // 2. perform the inference
        ResultInferenceStmt resBlock = typeInference(mImp.getBlock());

        // 4. give back the result.
        ResultInference res = new ResultInference();

        Contract co = _df.newContractEmpty();
        for (TypingEnvironment te : resBlock.getEnvironment())
            co = _df.newContractUnion(mImp, co, _df.newContract(te.unsync(mImp)));

        res.add(_cd.getName(), mImp.getMethodSig().getName(), _df.newMethodContract(mi, resBlock.getContract(), co));
        res.add(resBlock.getConstraint());

        _env.clearFutures();
        _env.clearVariables();

        _log.endIndent();
        _log.logDebug("Inference for Method Block Finished");
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
        return m;
    }

    public ResultInferenceStmt typeInference(Decl d) {
        Method m = getInferenceMethod(d.getClass(), Decl.class);

        if (m != null)
            try {
                return (ResultInferenceStmt) m.invoke(this, d);
            } catch (IllegalAccessException e) {
                e.printStackTrace();
            } catch (IllegalArgumentException e) {
                e.printStackTrace();
            } catch (InvocationTargetException e) {
                e.printStackTrace();
            }
        _log.logWarning("WARNING: Contract inference not implemented for declaration \"" + d.getClass().getName()
                + "\". Assumed empty contract and constraint");
        return new ResultInferenceStmt(_df.newContractEmpty(), _df.newConstraint(), _env);
    }

    public ResultInferenceStmt typeInference(Stmt s) {
        Method m = getInferenceMethod(s.getClass(), Stmt.class);

        if (m != null)
            try {
                return (ResultInferenceStmt) m.invoke(this, s);
            } catch (IllegalAccessException e) {
                e.printStackTrace();
            } catch (IllegalArgumentException e) {
                e.printStackTrace();
            } catch (InvocationTargetException e) {
                e.printStackTrace();
            }
        _log.logWarning("WARNING: Contract inference not implemented for statement \"" + s.getClass().getName()
                + "\". Assumed empty contract and constraint");
        return new ResultInferenceStmt(_df.newContractEmpty(), _df.newConstraint(), _env);
    }

    public ResultInferenceEffExp typeInference(Exp e) {
        Method m = getInferenceMethod(e.getClass(), Exp.class);

        if (m != null)
            try {
                return (ResultInferenceEffExp) m.invoke(this, e);
            } catch (IllegalAccessException e1) {
                e1.printStackTrace();
            } catch (IllegalArgumentException e1) {
                e1.printStackTrace();
            } catch (InvocationTargetException e1) {
                e1.printStackTrace();
            }
        _log.logWarning("WARNING: Contract inference not implemented for Expression \"" + e.getClass().getName()
                + "\". Assumed Unit Type");
        return new ResultInferenceEffExp(_df.newRecordVariable(), _df.newContractEmpty(), _df.newConstraint(), _env);
    }

    public ResultInferencePureExp typeInferenceAsPure(PureExp exp) {
        Method m = getInferenceMethod("typeInferenceAsPure", exp.getClass(), PureExp.class);

        if (m != null)
            try {
                return (ResultInferencePureExp) m.invoke(this, exp);
            } catch (IllegalAccessException e) {
                e.printStackTrace();
            } catch (IllegalArgumentException e) {
                e.printStackTrace();
            } catch (InvocationTargetException e) {
                e.printStackTrace();
            }
        _log.logWarning("WARNING: Contract inference not implemented for Expression \"" + exp.getClass().getName()
                + "\". Assumed Unit Type");
        // return new ResultInferencePureExp(_df,
        // _df.newRecordDataType(env.getUnitType(), new LinkedList<IRecord>()));
        return new ResultInferencePureExp(_df, _df.dummyDataType());
    }

    public ResultInferenceEffExp typeInference(PureExp pexp) {
        Method m = getInferenceMethod(pexp.getClass(), PureExp.class);

        if (m != null)
            try {
                return (ResultInferenceEffExp) m.invoke(this, pexp);
            } catch (IllegalAccessException e) {
                e.printStackTrace();
            } catch (IllegalArgumentException e) {
                e.printStackTrace();
            } catch (InvocationTargetException e) {
                e.printStackTrace();
            }
        ResultInferencePureExp resPure = typeInferenceAsPure(pexp);
        return new ResultInferenceEffExp(resPure.getVariableType(), _df.newContractEmpty(), _df.newConstraint(), _env);
    }

    public ResultInferencePureExp typeInference(Guard g) {
        Method m = getInferenceMethod(g.getClass(), Guard.class);

        if (m != null)
            try {
                return (ResultInferencePureExp) m.invoke(this, g);
            } catch (IllegalAccessException e) {
                e.printStackTrace();
            } catch (IllegalArgumentException e) {
                e.printStackTrace();
            } catch (InvocationTargetException e) {
                e.printStackTrace();
            }
        _log.logWarning("WARNING: Contract inference not implemented for Guard \"" + g.getClass().getName()
                + "\". Assumed Boolean Type");
        // return new ResultInferencePureExp(_df,
        // _df.newRecordDataType(env.getBoolType(), new LinkedList<IRecord>()));
        return new ResultInferencePureExp(_df, _df.dummyDataType());
    }

    // /////////////////////////////////////////////////////////////////////////////
    // 2.2. Declarations

    public ResultInferenceStmt typeInference(VarDecl vd) {
        _log.logDebug("Contract Inference for the VarDecl \"" + vd.getName() + "\"");
        _log.beginIndent();

        Contract contract;
        Constraint c;
        ITypingEnvironmentVariableType X;

        if (vd.hasInitExp()) {
            Exp exp = vd.getInitExp();
            if (exp instanceof PureExp) {
                ResultInferencePureExp resInitExp = typeInferenceAsPure((PureExp) exp);
                contract = _df.newContractEmpty();
                c = _df.newConstraint();
                X = resInitExp.getVariableType();
            } else {
                ResultInferenceEffExp resInitExp = typeInference((EffExp) exp);
                contract = resInitExp.getContract();
                c = resInitExp.getConstraint();
                X = resInitExp.getRecord();
            }
        } else {
            contract = _df.newContractEmpty();
            c = _df.newConstraint();
            X = _df.newRecordVariable();
        }

        _env.putVariable(vd.getName(), X);
        return new ResultInferenceStmt(contract, c, _env);
    }

    // ////////////////////////////////////////////////////////////////////////////
    // 2.3. Statments

    public ResultInferenceStmt typeInference(AssignStmt astmt) {
        _log.logDebug("Contract Inference for the AssignStmt");
        _log.beginIndent();

        String name = astmt.getVar().getName();

        Contract contract;
        Constraint c;
        ITypingEnvironmentVariableType X;

        Exp exp = astmt.getValue();
        if (exp instanceof PureExp) {
            ResultInferencePureExp resValue = typeInferenceAsPure((PureExp) exp);
            contract = _df.newContractEmpty();
            c = _df.newConstraint();
            X = resValue.getVariableType();
        } else {
            ResultInferenceEffExp resValue = typeInference((EffExp) exp);
            contract = resValue.getContract();
            c = resValue.getConstraint();
            X = resValue.getRecord();
        }

        _log.endIndent();
        _log.logDebug("AssignStmt Sub-Expression Finished");
        if (_env.isField(name)) {
            c.addEquation(new ASTNodeInformation(astmt), _env.getVariableRecord(name), (IRecord) X);
        } else {
            _env.putVariable(name, X);
        }
        return new ResultInferenceStmt(contract, c, _env);
    }

    public ResultInferenceStmt typeInference(AwaitStmt astmt) {
        _log.logDebug("Contract Inference for the AwaitStmt");
        _log.beginIndent();

        Contract contract;
        Constraint c = _df.newConstraint();
        GroupName aprime = _df.newGroupName();

        // 1. First look if there is some annotation defined
        Iterator<Annotation> it = astmt.getAnnotations().iterator();
        if (it.hasNext()) {
            PureExp dep = it.next().getValue();
            ResultInferencePureExp resAnn = typeInferenceAsPure(dep);
            contract = _df.newContractAwait(astmt, aprime, _a);

            _log.endIndent();
            _log.logDebug("AwaitStmt Annotation Finished");
            c.addEquation(new ASTNodeInformation(astmt), (IRecord) resAnn.getVariableType(),
                    createInstance(dep.getType(), _cd, aprime));
            return new ResultInferenceStmt(contract, c, _env);
        } else {
            ResultInferencePureExp resGuard = typeInference(astmt.getGuard());
            _log.endIndent();
            _log.logDebug("AwaitStmt Sub-Expression Finished");

            if (resGuard.getVariableType() instanceof TypingEnvironmentVariableTypeFuture) {
                ITypingEnvironmentFutureType z = _env
                        .getFuture((TypingEnvironmentVariableTypeFuture) resGuard.getVariableType());
                if (z instanceof TypingEnvironmentFutureTypeUntick) {
                    _env.putFuture((TypingEnvironmentVariableTypeFuture) resGuard.getVariableType(),
                            new TypingEnvironmentFutureTypeTick(z.getRecord()));
                    c.addEquation(new ASTNodeInformation(astmt), z.getRecord(),
                            _df.newRecordFuture(aprime, _df.newRecordVariable()));
                    contract = _df.newContract(_df.newContractElementParallel(
                            _df.newContractInvkA(astmt, ((TypingEnvironmentFutureTypeUntick) z).getContract(),
                                    new ContractElementAwait(astmt, _a, aprime)),
                            _env.unsync(astmt)));
                } else {
                    try {
                        c.addEquation(new ASTNodeInformation(astmt), z.getRecord(),
                                _df.newRecordFuture(aprime, _df.newRecordVariable()));
                    } catch (Exception e) {
                        e.getMessage();
                    }
                    contract = _df.newContractEmpty();
                }
            } else {
                // the guard in the await is not a future.
                // maybe a boolean or something else, in any case, we cannot
                // manage it for now
                _log.logWarning("WARNING: the guard of the await statement is not a future. Assumed an empty contract");
                contract = _df.newContractEmpty();
            }
            return new ResultInferenceStmt(contract, c, _env);
        }
    }

    public ResultInferenceStmt typeInference(SkipStmt skip) {
        return new ResultInferenceStmt(_df.newContractEmpty(), _df.newConstraint(), _env);
    }

    public ResultInferenceStmt typeInference(SuspendStmt susp) {
        return new ResultInferenceStmt(_df.newContractEmpty(), _df.newConstraint(), _env);
    }

    public ResultInferenceStmt typeInference(DurationStmt dration) {
        return new ResultInferenceStmt(_df.newContractEmpty(), _df.newConstraint(), _env);
    }

    public ResultInferenceStmt typeInference(ReturnStmt res) {
        _log.logDebug("Contract Inference for the ReturnStmt");
        _log.beginIndent();
        ResultInferenceEffExp resRetExp = typeInference(res.getRetExp());
        _log.endIndent();
        _log.logDebug("ReturnStmt Sub-Expression Finished");

        Contract contract = resRetExp.getContract();
        Constraint c = resRetExp.getConstraint();
        c.addEquation(new ASTNodeInformation(res), _env.getVariableRecord(_destinyName),
                _env.getRecord(resRetExp.getRecord()));

        return new ResultInferenceStmt(contract, c, resRetExp.getEnvironment());
    }

    public ResultInferenceStmt typeInference(ExpressionStmt exp) {
        _log.logDebug("Contract Inference for the ExpressionStmt");
        _log.beginIndent();
        ResultInferenceEffExp resExp = typeInference(exp.getExp());
        _log.endIndent();
        _log.logDebug("ExpressionStmt Sub-Expression Finished");
        return new ResultInferenceStmt(resExp.getContract(), resExp.getConstraint(), resExp.getEnvironment());
    }

    public ResultInferenceStmt typeInference(AssertStmt ass) {
        _log.logDebug("Contract Inference for the AssertStmt");
        return new ResultInferenceStmt(_df.newContractEmpty(), _df.newConstraint(), _env);
    }

    public ResultInferenceStmt typeInference(VarDeclStmt vd) {
        _log.logDebug("Contract Inference for the VarDeclStmt");
        _log.beginIndent();
        ResultInferenceStmt resVarDecl = typeInference(vd.getVarDecl());
        _log.endIndent();
        _log.logDebug("VarDeclStmt Sub-Expression Finished");
        return resVarDecl;
    }

    public ResultInferenceStmt typeInference(IfStmt ifstmt) {
        _log.logDebug("Contract inference of a Conditional Statement");
        _log.beginIndent();
        TypingEnvironment tmp = _env.copy();
        ResultInferenceStmt resThen = typeInference(ifstmt.getThen());
        List<TypingEnvironment> resultEnvs = new LinkedList<>();
        resultEnvs.addAll(resThen.getEnvironment());

        Contract contract;
        Constraint c = resThen.getConstraint();

        if (ifstmt.hasElse()) {
            this._env = tmp;
            ResultInferenceStmt resElse = typeInference(ifstmt.getElse());
            c.add(resElse.getConstraint());
            resultEnvs.addAll(resElse.getEnvironment());
            contract = _df.newContractUnion(ifstmt, resThen.getContract(), resElse.getContract());
        } else {
            resultEnvs.add(tmp);
            contract = _df.newContractUnion(ifstmt, resThen.getContract(), _df.newContractEmpty());
        }
        _log.endIndent();
        _log.logDebug("IfStmt Sub-Statments Finished");

        return new ResultInferenceStmt(contract, c, resultEnvs);
    }

    public ResultInferenceStmt typeInference(WhileStmt whilestmt) {
        _log.logDebug("Contract Inference for the While (Approximated with one iteration)");
        _log.beginIndent();
        ResultInferenceStmt resStmt = typeInference(whilestmt.getBody());
        _log.endIndent();
        _log.logDebug("While Sub-Statments Finished");
        return resStmt;
    }

    public ResultInferenceStmt typeInference(Block b) {
        _log.logDebug("Contract Inference for the Block");
        _log.beginIndent();
        _env.newScope();

        // accumulate contracts and constraint.Constraints
        // in the resulting output
        ResultInferenceStmt resStmt = null;
        Constraint c = _df.newConstraint();
        Contract contract = _df.newContractEmpty();
        List<TypingEnvironment> envs = new LinkedList<TypingEnvironment>();
        envs.add(_env);

        for (Stmt s : b.getStmts()) {
            List<TypingEnvironment> cumul = new LinkedList<TypingEnvironment>();
            Contract current = null;
            for (TypingEnvironment tmpEnv : envs) {
                _env = tmpEnv;
                resStmt = typeInference(s);
                c.add(resStmt.getConstraint());
                cumul.addAll(resStmt.getEnvironment());
                current = (current == null) ? resStmt.getContract()
                        : _df.newContractUnion(b, current, resStmt.getContract());
            }
            envs = cumul;
            contract.add(current);
        }
        _log.endIndent();
        _log.logDebug("Block Sub-Expression Finished");
        for (TypingEnvironment env : envs) {
            env.popScope();
        }
        return new ResultInferenceStmt(contract, c, envs);
    }

    // ////////////////////////////////////////////////////////////////////////////
    // 2.4. Expressions

    // 2.4.1. Pure Expressions

    public ResultInferencePureExp typeInferenceAsPure(NullExp exp) {
        _log.logDebug("Contract Inference for the 'null' PureExp");
        return new ResultInferencePureExp(_df, _df.newRecordVariable());
    }

    public ResultInferencePureExp typeInferenceAsPure(ThisExp exp) {
        _log.logDebug("Contract Inference for the 'this' PureExp");
        return new ResultInferencePureExp(_df, _env.getVariableRecord(_this));
    }

    public ResultInferencePureExp typeInferenceAsPure(VarOrFieldUse var) {
        _log.logDebug("Contract Inference for the VarOrFieldUse");
        return new ResultInferencePureExp(_df, _env.getVariable(var.getName()));
    }

    // DATATYPES YEAH
    /*
     * //reviewed// public ResultInferencePureExp
     * typeInferenceAsPure(StringLiteral lit, String ident, TypingEnvironment
     * env, GroupName a, Map<InterfaceDecl, ClassDecl> intertoclass, Factory df,
     * ClassDecl cl) { if (ident != null) { System.out.println(ident +
     * "Contract inference of a Literal "); }
     * 
     * return new ResultInferencePureExp(df,
     * df.newRecordDataType(env.getStringType(), new LinkedList<IRecord>())); }
     * 
     * //reviewed// public ResultInferencePureExp typeInferenceAsPure(IntLiteral
     * lit, String ident, TypingEnvironment env, GroupName a, Map<InterfaceDecl,
     * ClassDecl> intertoclass, Factory df, ClassDecl cl) { if (ident != null) {
     * System.out.println(ident + "Contract inference of a Literal "); } return
     * new ResultInferencePureExp(df, df.newRecordDataType(env.getIntType(), new
     * LinkedList<IRecord>())); }
     * 
     * //reviewed// public ResultInferencePureExp
     * typeInferenceAsPure(DataConstructorExp exp, String ident,
     * TypingEnvironment env, GroupName a, Map<InterfaceDecl, ClassDecl>
     * intertoclass, Factory df, ClassDecl cl) { // DATATYPES String nident =
     * null; if (ident != null) { System.out.println(ident +
     * "Contract inference of the Data type Constructor \"" +
     * exp.getConstructor() + "\""); nident = " " + ident; } FunctionInterface
     * fi = env.getFunction(exp.getConstructor()); Constraint c =
     * df.newConstraint(); LinkedList<Term> params = new LinkedList<Term>();
     * for(PureExp e : exp.getParamList()) { ResultInferencePureExp tmp =
     * this.typeInferenceAsPure(e, ident, env, a,intertoclass, df, cl);
     * c.add(tmp.getConstraint());
     * params.add(env.getRecord(tmp.getVariableType())); }
     * 
     * RecordVariable r = df.newRecordVariable(); FunctionInterface fiapp = new
     * FunctionInterface(params, r); c.addSemiEquation(new
     * ASTNodeInformation(exp), fiapp, fi); //for (PureExp param :
     * exp.getParams()) { // res.add(typeInferenceAsPure(param, nident, env, a,
     * intertoclass, df, cl)); //} return new ResultInferencePureExp(df, r, c);
     * }
     * 
     * //reviewed// public ResultInferencePureExp typeInferenceAsPure(LetExp
     * exp, String ident, TypingEnvironment env, GroupName a, Map<InterfaceDecl,
     * ClassDecl> intertoclass, Factory df, ClassDecl cl) { String nident =
     * null; if (ident != null) { System.out.println(ident +
     * "Contract inference of a Let Expression "); nident = " " + ident; }
     * ResultInferencePureExp res = typeInferenceAsPure(exp.getVal(), nident,
     * env, a, intertoclass, df, cl); ITypingEnvironmentVariableType X =
     * res.getVariableType(); TypingEnvironment envExp = env.clone();
     * envExp.putVariable(exp.getVar().getName(), X);
     * 
     * ResultInferencePureExp resExp = typeInferenceAsPure(exp.getExp(), nident,
     * envExp, a, intertoclass, df, cl); return resExp; }
     * 
     * // reviewed // public ResultInferencePureExp typeInferenceAsPure(FnApp
     * fn, String ident, TypingEnvironment env, GroupName a, Map<InterfaceDecl,
     * ClassDecl> intertoclass, Factory df, ClassDecl cl) { // DATATYPES String
     * nident = null; if (ident != null) { System.out.println(ident +
     * "Contract inference of the function application \"" + fn.getName() +
     * "\""); nident = " " + ident; } FunctionInterface fi =
     * env.getFunction(fn.getName()); Constraint c = df.newConstraint();
     * LinkedList<Term> params = new LinkedList<Term>(); for(PureExp e :
     * fn.getParamList()) { ResultInferencePureExp tmp =
     * this.typeInferenceAsPure(e, ident, env, a,intertoclass, df, cl);
     * c.add(tmp.getConstraint());
     * params.add(env.getRecord(tmp.getVariableType())); }
     * 
     * RecordVariable r = df.newRecordVariable(); FunctionInterface fiapp = new
     * FunctionInterface(params, r); c.addSemiEquation(new
     * ASTNodeInformation(fn), fiapp, fi); //for (PureExp param :
     * exp.getParams()) { // res.add(typeInferenceAsPure(param, nident, env, a,
     * intertoclass, df, cl)); //} return new ResultInferencePureExp(df, r, c);
     * }
     * 
     * 
     * //reviewed// public ResultInferencePureExp typeInferenceAsPure(IfExp
     * ifExp, String ident, TypingEnvironment env, GroupName a,
     * Map<InterfaceDecl, ClassDecl> intertoclass, Factory df, ClassDecl cl) {
     * String nident = null; if (ident != null) { System.out.println(ident +
     * "Contract inference of an Exp Conditional Expression"); nident = " " +
     * ident; } ResultInferencePureExp resL =
     * typeInferenceAsPure(ifExp.getThenExp(), nident, env, a, intertoclass, df,
     * cl); ResultInferencePureExp resR =
     * typeInferenceAsPure(ifExp.getThenExp(), nident, env, a, intertoclass, df,
     * cl); Constraint c = df.newConstraint(); c.addEquation(new
     * ASTNodeInformation(ifExp), env.getRecord(resL.getVariableType()),
     * env.getRecord(resR.getVariableType()));
     * 
     * return new ResultInferencePureExp(df,
     * env.getRecord(resL.getVariableType()), c); }
     * 
     * 
     * 
     * 
     * // reviewed // public ResultInferencePureExp typeInferenceAsPure(BoolExp
     * bin, String ident, TypingEnvironment env, GroupName a, Map<InterfaceDecl,
     * ClassDecl> intertoclass, Factory df, ClassDecl cl) { // DATATYPES String
     * nident = null; if (ident != null) { System.out.println(ident +
     * "Contract Inference for a Bool Binary Pure Expression -> no contract and no record "
     * ); nident = " " + ident; }
     * 
     * return new ResultInferencePureExp(df, new
     * RecordDataType(env.getBoolType(), new LinkedList<IRecord>())); }
     * 
     * // reviewed // public ResultInferencePureExp
     * typeInferenceAsPure(RelationalExpr bin, String ident, TypingEnvironment
     * env, GroupName a, Map<InterfaceDecl, ClassDecl> intertoclass, Factory df,
     * ClassDecl cl) { // DATATYPES String nident = null; if (ident != null) {
     * System.out.println(ident +
     * "Contract Inference for a Relational Binary Pure Expression -> no contract and no record "
     * ); nident = " " + ident; }
     * 
     * return new ResultInferencePureExp(df, new
     * RecordDataType(env.getBoolType(), new LinkedList<IRecord>())); }
     * 
     * // reviewed // public ResultInferencePureExp
     * typeInferenceAsPure(ArithmeticExp bin, String ident, TypingEnvironment
     * env, GroupName a, Map<InterfaceDecl, ClassDecl> intertoclass, Factory df,
     * ClassDecl cl) { // DATATYPES String nident = null; if (ident != null) {
     * System.out.println(ident +
     * "Contract Inference for an Arithmetic Binary Pure Expression -> no contract and no record "
     * ); nident = " " + ident; }
     * 
     * return new ResultInferencePureExp(df, new
     * RecordDataType(env.getIntType(), new LinkedList<IRecord>())); }
     * 
     * // reviewed // public ResultInferencePureExp typeInferenceAsPure(NegExp
     * un, String ident, TypingEnvironment env, GroupName a, Map<InterfaceDecl,
     * ClassDecl> intertoclass, Factory df, ClassDecl cl) { // DATATYPES String
     * nident = null; if (ident != null) { System.out.println(ident +
     * "Contract Inference for a Pure Negative Boolean Expression -> no contract and no record "
     * ); nident = " " + ident; }
     * 
     * return new ResultInferencePureExp(df, new
     * RecordDataType(env.getBoolType(), new LinkedList<IRecord>())); }
     * 
     * // reviewed // public ResultInferencePureExp typeInferenceAsPure(MinusExp
     * un, String ident, TypingEnvironment env, GroupName a, Map<InterfaceDecl,
     * ClassDecl> intertoclass, Factory df, ClassDecl cl) { // DATATYPES String
     * nident = null; if (ident != null) { System.out.println(ident +
     * "Contract Inference for an Unary Minus Pure Expression -> no contract and no record "
     * ); nident = " " + ident; }
     * 
     * return new ResultInferencePureExp(df, new
     * RecordDataType(env.getIntType(), new LinkedList<IRecord>())); }
     * 
     */

    /***********************************************************/
    // INIT TEMPORARY VERSION OF DATATYPE MANAGEMENT
    public ResultInferencePureExp typeInferenceAsPure(StringLiteral lit) {
        _log.logDebug("Contract Inference for a String Literal");
        return new ResultInferencePureExp(_df, _df.dummyDataType());
    }

    public ResultInferencePureExp typeInferenceAsPure(IntLiteral lit) {
        _log.logDebug("Contract Inference for an Int Literal");
        return new ResultInferencePureExp(_df, _df.dummyDataType());
    }

    public ResultInferencePureExp typeInferenceAsPure(DataConstructorExp exp) {
        _log.logDebug("Contract Inference for a DataType Constructor");
        return new ResultInferencePureExp(_df, _df.dummyDataType());
    }

    public ResultInferencePureExp typeInferenceAsPure(LetExp exp) {
        _log.logDebug("Contract Inference for a Let Expression");
        return new ResultInferencePureExp(_df, _df.dummyDataType());
    }

    public ResultInferencePureExp typeInferenceAsPure(FnApp fn) {
        _log.logDebug("Contract Inference for a Function Application");
        return new ResultInferencePureExp(_df, _df.dummyDataType());
    }

    public ResultInferencePureExp typeInferenceAsPure(IfExp ifExp) {
        _log.logDebug("Contract Inference for an if Expression");
        return new ResultInferencePureExp(_df, _df.dummyDataType());
    }

    public ResultInferencePureExp typeInferenceAsPure(BoolExp bin) {
        _log.logDebug("Contract Inference for a Boolean Expression");
        return new ResultInferencePureExp(_df, _df.dummyDataType());
    }

    public ResultInferencePureExp typeInferenceAsPure(RelationalExpr bin) {
        _log.logDebug("Contract Inference for a Relational Expression");
        return new ResultInferencePureExp(_df, _df.dummyDataType());
    }

    public ResultInferencePureExp typeInferenceAsPure(ArithmeticExp bin) {
        _log.logDebug("Contract Inference for an Arithmetic Expression");
        return new ResultInferencePureExp(_df, _df.dummyDataType());
    }

    // reviewed //
    public ResultInferencePureExp typeInferenceAsPure(NegExp un) {
        _log.logDebug("Contract Inference for a Not Expression");
        return new ResultInferencePureExp(_df, _df.dummyDataType());
    }

    // reviewed //
    public ResultInferencePureExp typeInferenceAsPure(MinusExp un) {
        _log.logDebug("Contract Inference for a Minus Expression");
        return new ResultInferencePureExp(_df, _df.dummyDataType());
    }

    /*********************************************************/

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

    public ResultInferenceEffExp typeInference(NewExp newExp) {
        _log.logDebug("Contract Inference for the NewExp");
        _log.beginIndent();

        // 1. Create the record for the new object, and collect contract and
        // constraints from arguments
        ClassDecl cl = (ClassDecl) (newExp.lookup(new KindedName(Kind.CLASS, newExp.getClassName())));
        LinkedList<RecordField> fields = new LinkedList<RecordField>();
        int i = 0;
        Contract contract = _df.newContractEmpty();
        Constraint c = _df.newConstraint();
        // 1.1. Params
        _env.newScope();
        ResultInferencePureExp resParam;
        for (PureExp p : newExp.getParams()) {
            resParam = typeInferenceAsPure(p);
            ITypingEnvironmentVariableType x = _env.getRecord(resParam.getVariableType());
            IRecord r = _env.getRecord(x);
            fields.add(_df.newRecordField(cl.getParam(i).getName(), r));
            _env.putVariable(cl.getParam(i).getName(), r);
            c.add(resParam.getConstraint());
            i++;
        }
        // 1.2. Fields
        for (FieldDecl f : cl.getFields()) {
            if (f.hasInitExp()) {
                resParam = typeInferenceAsPure(f.getInitExp());
                RecordField newRecordField = _df.newRecordField(f.getName(), (IRecord) resParam.getVariableType());
                fields.add(newRecordField);
                _env.putVariable(f.getName(), (IRecord) resParam.getVariableType());
                c.add(resParam.getConstraint());

            } else {
                fields.add(_df.newRecordField(f.getName(), _df.newRecordVariable()));
            }
        }
        // 1.3. Group Name
        GroupName aprime; // depends if the new object is in the same cog or not
        if (!newExp.hasLocal()) {
            aprime = _df.newGroupName();
        } else {
            aprime = _a;
        }
        IRecord r = _df.newRecordPresent(aprime, fields);

        // 1.4. Calling the init of r
        MethodInterface miinit = _df.newMethodInterface(r, new LinkedList<IRecord>(), _df.newRecordVariable());
        c.addSemiEquation(new ASTNodeInformation(newExp), _env.getMethod(cl.getName(), _initName), miinit);
        contract.add(_df.newContractInvk(newExp, cl.getName(), _initName, miinit));
        _env.popScope();
        _log.endIndent();
        _log.logDebug("End Contract Inference for the NewExp");
        return new ResultInferenceEffExp(r, contract, c, _env);
    }

    public ResultInferenceEffExp typeInference(Call call) {
        _log.logDebug("Contract Inference for a " + ((call instanceof SyncCall) ? "Synchronous" : "Asynchronous")
                + "  method Call");
        _log.beginIndent();
        ITypingEnvironmentVariableType r;
        GroupName aprime = _df.newGroupName();
        // 1. Get the method interface
        Type t = call.getCallee().getType();
        ClassDecl cl;
        if (t.isInterfaceType()) {
            cl = _intertoclass.get(((InterfaceType) t).getDecl());
        } else {
            cl = _cd;
        }

        String nameOfClass;
        String nameOfMethod;

        LinkedList<IRecord> s = new LinkedList<IRecord>();

        if (cl == _emptyDecl) {
            // we are in presence of an non implemented interface
            // in that case, we don't know how the method would behave, and
            // simply put a null contract.
            _log.endIndent();
            _log.logError("Class retrival failed!!!");

            nameOfClass = _dummyClass;
            nameOfMethod = _dummyMethod;
            // return new ResultInferenceEffExp( _df.newRecordVariable(),
            // _df.newContractEmpty(), _df.newConstraint(), _env);
        } else {
            nameOfClass = cl.getName();
            nameOfMethod = call.getMethod();

            ResultInferencePureExp resParam;
            for (PureExp p : call.getParams()) {
                resParam = typeInferenceAsPure(p);
                s.add(_env.getRecord(resParam.getVariableType()));
            }
        }
        // else
        {
            // 2. Collect contracts and
            // deadlock.constraints.constraint.Constraints from the call
            ResultInferencePureExp resCallee = typeInferenceAsPure(call.getCallee());
            Contract contract = _df.newContractEmpty();
            Constraint c = _df.newConstraint();

            // cast to IRecord as the callee cannot be a future
            IRecord callee = (IRecord) resCallee.getVariableType();

            // 3. Construct the record for the return value
            IRecord Y = _df.newRecordVariable();

            // 4. pack up the result
            MethodInterface mi = _df.newMethodInterface(callee, s, Y);

            c.addSemiEquation(new ASTNodeInformation(call), _env.getMethod(nameOfClass, nameOfMethod), mi);

            if (call instanceof SyncCall) {
                r = Y;
                contract.add(_df.newContractSyncInvk(call, nameOfClass, nameOfMethod, mi));
            } else if (call instanceof AsyncCall) {

                IRecord calleeShape = createInstance(cl, aprime);
                c.addEquation(new ASTNodeInformation(call), callee, calleeShape);
                r = new TypingEnvironmentVariableTypeFuture();
                _env.putFuture((TypingEnvironmentVariableTypeFuture) r, new TypingEnvironmentFutureTypeUntick(
                        _df.newRecordFuture(aprime, Y), new ContractElementInvk(call, nameOfClass, nameOfMethod, mi)));
            } else if (call instanceof AwaitAsyncCall) {
                // same stuff that for sync calls
                // AwaitAsyncCall are actually rewritten in the AST if the flag
                // public static boolean Model.doAACrewrite is set to true;
                // objects of this kind are rewritten at the ast level as
                // AsyncCall + await
                // you can switch this switch in file
                // frontend/src/abs/backend/coreabs/GenerateCoreAbs.jadd line 38
                r = Y;
                contract.add(_df.newContractSyncInvk(call, nameOfClass, nameOfMethod, mi));
            } else {
                // should not happen
                r = null;
                _log.logError("Unexpected call type");
            }
            _log.endIndent();
            _log.logDebug("End Contract Inference for the Call");
            return new ResultInferenceEffExp(r, contract, c, _env);
        }
    }

    public ResultInferenceEffExp typeInference(GetExp exp) {
        _log.logDebug("Contract Inference for a GetExp");
        _log.beginIndent();

        // 1. first is calculate the inference of the expression 'e' of 'e.get'
        ResultInferencePureExp resPureExp = typeInferenceAsPure(exp.getPureExp());
        _log.endIndent();
        _log.logDebug("GetExp Sub-Expression Finished");

        Contract contract;
        Constraint c = _df.newConstraint();

        // 2. record for the result
        GroupName aprime = _df.newGroupName();
        IRecord X = _df.newRecordVariable();

        // 3. check if future is tick
        ITypingEnvironmentVariableType variableType = resPureExp.getVariableType();
        if (variableType instanceof TypingEnvironmentVariableTypeFuture) {
            ITypingEnvironmentFutureType fType = _env.getFuture((TypingEnvironmentVariableTypeFuture) variableType);
            if (fType instanceof TypingEnvironmentFutureTypeUntick) {
                _env.putFuture((TypingEnvironmentVariableTypeFuture) variableType,
                        new TypingEnvironmentFutureTypeTick(fType.getRecord()));
                contract = _df.newContract(_df.newContractElementParallel(
                        _df.newContractInvkG(exp, ((TypingEnvironmentFutureTypeUntick) fType).getContract(),
                                new ContractElementGet(exp, _a, aprime)),
                        _env.unsync(exp)));
            } else {
                contract = _df.newContractEmpty();
            }
            try {
                c.addEquation(new ASTNodeInformation(exp), _df.newRecordFuture(aprime, X), fType.getRecord());
            } catch (Exception e) {
                e.getMessage();
            }
        } else {
            contract = _df.newContractEmpty();
        }

        

        return new ResultInferenceEffExp(X, contract, c, _env);
    }

    // ////////////////////////////////////////////////////////////////////////////
    // 2.5. Guards

    public ResultInferencePureExp typeInference(ClaimGuard cg) {
        _log.logDebug("Contract Inference for a ClaimGuard");
        _log.beginIndent();
        ResultInferencePureExp res = typeInferenceAsPure(cg.getVar());
        _log.endIndent();
        return res;
    }

    // TODO; AndGuard, ExpGuard, DurationGuard

}
