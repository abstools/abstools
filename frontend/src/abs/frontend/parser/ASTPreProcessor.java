/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.parser;

import abs.frontend.ast.*;
import java.util.Map;
import java.util.HashMap;
import java.util.LinkedList;

/**
 * Preprocesses the AST directly after it has been parsed, before any name and
 * type analysis.  Typically, syntactic sugar is eliminated in this phase.
 *
 * Note that we might not have a full model yet - consider using NoTransform()
 * for accessors.
 *
 * Currently the following things are done:
 *
 *  - selector names of constructors are transformed to functions
 *
 * @author Jan Schäfer
 *
 */
public class ASTPreProcessor {

    public final static String FUNCTIONSELECTOR = "selector";

    public CompilationUnit preprocess(CompilationUnit unit) {
        for (ModuleDecl d : unit.getModuleDecls()) {
            preprocess(d);
        }
        return unit;
    }

    private void preprocess(ModuleDecl moduleDecl) {
        for (Decl decl : moduleDecl.getDecls()) {
            if (decl.isDataType()) {
                DataTypeDecl dtd = (DataTypeDecl) decl;
                for (FunctionDecl fd : createSelectorFunctions(dtd, false)) {
                    moduleDecl.addDeclNoTransform(fd);
                }
            }
        }
    }

    public LinkedList<FunctionDecl> createSelectorFunctionsForDeltaApplication(DataTypeDecl dtd) {
        return createSelectorFunctions(dtd, true);
    }

    private List<Pattern> makePatternList(int constructorArgs, int numArg) {
        List<Pattern> patternList = new List<>();
        for (int i = 0; i < constructorArgs; i++) {
            if (i == numArg)
                patternList.add(new PatternVar(new PatternVarDecl("res")));
            else
                patternList.add(new UnderscorePattern());
        }
        return patternList;
    }

    private FunctionDef makeAccessorFunction(LinkedList<DataDeclarationArg> args) {
        List<CaseBranch> branches = new List<>();

        for (DataDeclarationArg d : args) {
            String constructorName = d.getConstructor().getName();
            List<Pattern> pattern = makePatternList(d.getConstructor().getNumConstructorArg(), d.getPosition());
            branches.add(new CaseBranch(new ConstructorPattern(constructorName, pattern), new VarUse("res")));
        }

        return new ExpFunctionDef(new CaseExp(new VarUse("data"), branches));
    }

    /**
     * Given a data decleration, map each argument name to the corresponding
     * DataDeclarationArg, containing the data constructor, the argument and
     * the position of the argument.
     */
    private Map<String, LinkedList<DataDeclarationArg>> makeConstructorMap(DataTypeDecl dtd) {
        Map<String, LinkedList<DataDeclarationArg>> constructors = new HashMap<>();

        for (DataConstructor c : dtd.getDataConstructors()) {
            int argpos = 0;
            for (ConstructorArg ca : c.getConstructorArgs()) {
                if (ca.hasSelectorName()) {
                    String name = ca.getSelectorName().getName();
                    LinkedList<DataDeclarationArg> tmp = constructors.get(name);
                    if (tmp == null) {
                        tmp = new LinkedList<>();
                        constructors.put(name, tmp);
                    }
                    tmp.add(new DataDeclarationArg(c, ca, argpos));
                }
                argpos++;
            }
        }

        return constructors;
    }

    /**
     * Given a data decleration, generate selector functions, e.g.
     *
     * <pre>
     * data Foo = Bar(Bool isTrue, String name) | Baz(String name);
     * </pre>
     *
     * creates:
     * <pre>
     * def Bool isTrue(Foo data) =
     *     case data {
     *     Bar(res,_) => res;
     *     };
     *
     * def String name(Foo data) =
     *     case data {
     *     Bar(_,res) => res;
     *     Baz(res) => res;
     *     };
     * <pre>
     */
    private LinkedList<FunctionDecl> createSelectorFunctions(DataTypeDecl dtd, boolean delta) {
        // We need to know what data constructor and what argument inside that
        // data constructors a name corresponds to.
        Map<String, LinkedList<DataDeclarationArg>> constructors = makeConstructorMap(dtd);

        LinkedList<FunctionDecl> fds = new LinkedList<>();

        // Make an accessor function for every argument name in the declaration
        for (String name : constructors.keySet()) {
            // Make the function body, containing a single case statement with
            // a branch for each constructor
            FunctionDef funDef = makeAccessorFunction(constructors.get(name));

            // the type parameters of the function
            List<TypeParameterDecl> typeParams;
            // the type of the parameter of the function
            TypeUse paramType;

            if (dtd instanceof ParametricDataTypeDecl) {
                ParametricDataTypeDecl pdtd = (ParametricDataTypeDecl) dtd;
                typeParams = delta ? pdtd.getTypeParameterList().treeCopyNoTransform() : pdtd.getTypeParameterList();
                List<TypeUse> typeParams2 = new List<>();
                for (TypeParameterDecl p : typeParams) {
                    // was p.getType().toUse() but we're not type-checked yet
                    typeParams2.add(new TypeParameterUse(p.getName(), new List<>()));
                }
                paramType = new ParametricDataTypeUse(pdtd.getName(), new List<>(), typeParams2);
            } else {
                typeParams = new List<>();
                // was dtd.getType().toUse() but we're not type-checked yet
                paramType = new DataTypeUse(dtd.getName(), new List<>());
            }

            List<ParamDecl> parameters = new List<ParamDecl>()
                .add(new ParamDecl("data",paramType, new List<>()));

            // Get an arbitrary constructor argument, which is used to decide
            // the return type of the function (if there are mismatching types,
            // then let the type checker catch it).
            ConstructorArg ca = constructors.get(name).element().getArg();

            // the complete function definition
            FunctionDecl fd =
                new ParametricFunctionDecl(
                                           name, // function name
                                           (TypeUse)ca.getTypeUse().copy(), // type
                                           parameters, // parameters
                                           funDef,
                    new List<>(), // annotations
                                           typeParams
                                           );

            // annotate that this function is a selector function
            // such that backends will know about it
            fd.addAnnotation(new Annotation(new StringLiteral(FUNCTIONSELECTOR)));

            for (DataDeclarationArg d : constructors.get(name)) {
                setAllPositionsFromNode(fd, d.getArg());
            }

            fds.add(fd);
        }
        return fds;
    }

    /**
     * A utility class for storing an constructor argument, along with the
     * enclosing constructor and the position.
     */
    private class DataDeclarationArg {
        private final DataConstructor constructor;
        private final ConstructorArg arg;
        private final int position;

        public DataDeclarationArg(DataConstructor constructor, ConstructorArg arg, int position) {
            this.constructor = constructor;
            this.arg = arg;
            this.position = position;
        }

        public DataConstructor getConstructor(){ return constructor; }
        public ConstructorArg getArg(){ return arg; }
        public int getPosition(){ return position; }
    }

    /**
     * recursively set the position of this ast node and its childs
     */
    private void setAllPositionsFromNode(ASTNode<?> node, ASTNode<?> fromNode) {
        node.setPositionFromNode(fromNode);
        for (int i=0; i < node.getNumChildNoTransform(); i++) {
            ASTNode<?> child = node.getChildNoTransform(i);
            setAllPositionsFromNode(child, fromNode);
        }
    }
}
