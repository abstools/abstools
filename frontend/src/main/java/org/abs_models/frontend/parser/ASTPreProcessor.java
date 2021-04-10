/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.parser;

import org.abs_models.common.Constants;
import org.abs_models.frontend.ast.*;

import java.util.Map;
import java.util.HashMap;
import java.util.LinkedList;

/**
 * This class contains code to preprocess the AST directly after it has been
 * parsed, before any name and type analysis.  Typically, syntactic sugar is
 * eliminated in this phase.
 *
 * Note that we might not have a full model yet - consider using NoTransform()
 * for accessors.
 *
 * @author Jan Schäfer
 * @author Rudi Schlatte
 *
 */
public class ASTPreProcessor {

    public final static String FUNCTIONSELECTOR = "selector";

    /**
     * Preprocess string literals coming from the parser.
     *
     * Remove leading and trailing " and replace character escape sequences
     * with the designated character.
     *
     * @argument tokenText The string token produced by the parser
     * @return a fresh string suitable as the content of a StringLiteral
     */
    public final static String preprocessStringLiteral(String tokenText) {
        StringBuffer s = new StringBuffer(tokenText.length() - 2);
        // i = 1..len-1 to skip beginning and ending \" of the stringliteral
        for (int i = 1; i < tokenText.length() - 1; i++) {
            char c = tokenText.charAt(i);
            if (c == '\\') {
                i++;
                c = tokenText.charAt(i);
                switch (c) {
                case 'n' : s.append('\n'); break;
                case 'r' : s.append('\r'); break;
                case 't' : s.append('\t'); break;
                default : s.append(c); break;
                }
            } else {
                s.append(c);
            }
        }
        return s.toString();
    }

    /**
     * Preprocess template string literals coming from the parser.
     *
     * Remove leading and trailing ` / $ characters and replace \` and \$
     * sequences with single ` / $.  All other literal \ characters are kept.
     *
     * @argument tokenText The string token produced by the parser
     * @return a fresh string suitable as the content of a StringLiteral
     */
    public final static String preprocessTemplateStringLiteral(String tokenText) {
        StringBuffer s = new StringBuffer(tokenText.length() - 2);
        // i = 1..len-1 to skip beginning and ending ` or $ of the stringliteral
        for (int i = 1; i < tokenText.length() - 1; i++) {
            char c = tokenText.charAt(i);
            if (c == '\\') {
                i++;
                char c1 = tokenText.charAt(i); // safe since we only iterate to len-1
                switch (c1) {
                case '`' : s.append('`'); break;   // escaped end (\`)
                case '$' : s.append('$'); break;   // escaped interpolation delimiter (\$)
                // do not drop backslash if it doesn't escape any of the above:
                default : s.append('\\'); s.append(c1); break;
                }
            } else {
                s.append(c);
            }
        }
        return s.toString();
    }

    /**
     * Perform various preprocessing steps after creating an AST.  This
     * routine is typically called after parsing but before type-checking.
     * Currently the following things are done:
     *
     * - Transform selector names of constructors to functions
     * - Import the standard library in modules that don’t mention it
     * - Add "implements Object" for classes that don’t implement any interface
     * - Add "extends Object" for interfaces that don’t extend any interface
     * - Add default "throw PatternMatchFailException" behavior to case
     *   statements
     *
     * @argument unit - the CompilationUnit to preprocess
     * @return the same object as passed in the argument unit
     */
    public CompilationUnit preprocess(CompilationUnit unit) {
        for (ModuleDecl d : unit.getModuleDecls()) {
            preprocess(d);
        }
        return unit;
    }

    private void preprocess(ModuleDecl moduleDecl) {
        // Add import clauses if module does not import anything from standard
        // library
        if (!Constants.STDLIB_NAME.equals(moduleDecl.getName())) {
            boolean needsImport = true;
            for (Import i : moduleDecl.getImports()) {
                if (i instanceof StarImport
                    && ((StarImport)i).getModuleName().equals(Constants.STDLIB_NAME))
                    needsImport = false;
                else if (i instanceof FromImport
                         && ((FromImport)i).getModuleName().equals(Constants.STDLIB_NAME))
                    needsImport = false;
            }
            if (needsImport) {
                moduleDecl.getImports().add(new StarImport(Constants.STDLIB_NAME) {public int getEndLine() { return -1; }});
            }
        }
        if (moduleDecl.hasBlock()) {
            createCaseStmtDefaultBranchesForChildren(moduleDecl.getBlockOptNoTransform());
        }
        for (Decl decl : moduleDecl.getDecls()) {
            if (decl.isDataType()) {
                // Add selector functions, e.g., functions fst and snd for
                // datatype data Pair<A, B> = Pair(A fst, B snd);
                DataTypeDecl dtd = (DataTypeDecl) decl;
                for (FunctionDecl fd : createSelectorFunctions(dtd, false)) {
                    moduleDecl.addDeclNoTransform(fd);
                }
            } else if (decl.isClass()) {
                createCaseStmtDefaultBranchesForChildren(decl);
                // If decl implements no interface: implement Object
                ClassDecl c = (ClassDecl)decl;
                if (!c.hasImplementedInterfaceUse()) {
                    InterfaceTypeUse s = new InterfaceTypeUse("ABS.StdLib.Object", new List<Annotation>());
                    // set position such that SourcePosition.findPosition()
                    // does not find this node
                    s.setPosition(-1, 0, -1, 0);
                    c.addImplementedInterfaceUseNoTransform(s);
                }
            } else if (decl.isInterface()) {
                // If decl extends no interface: extend Object
                InterfaceDecl i = (InterfaceDecl)decl;
                if (!i.hasExtendedInterfaceUse() && !i.getName().equals("Object")) {
                    // KLUDGE: this breaks if we manage to define an interface
                    // called "Object" in some other package.
                    InterfaceTypeUse s = new InterfaceTypeUse("ABS.StdLib.Object", new List<Annotation>());
                    // set position such that SourcePosition.findPosition()
                    // does not find this node
                    s.setPosition(-1, 0, -1, 0);
                    i.addExtendedInterfaceUseNoTransform(s);
                }
            }
        }
    }

    private void createCaseStmtDefaultBranchesForChildren(ASTNode<?> n) {
        for (CaseStmt cs : n.findChildren(CaseStmt.class)) {
            List<CaseBranchStmt> branches = cs.getBranchListNoTransform();
            CaseBranchStmt lastbranch = null;
            if (branches.getNumChildNoTransform() > 0) {
                lastbranch = branches.getChildNoTransform(branches.getNumChildNoTransform() - 1);
            }
            if (lastbranch == null || !(lastbranch.getLeftNoTransform() instanceof UnderscorePattern)) {
                // Add default branch that throws PatternMatchFailException.
                // See "Behavior of non-exhaustive case statement: no branch
                // match = skip or error?" on abs-dev on Jan 25-26, 2017
                Block block = new Block(new List<>(), new List<>());
                block.addStmt(new ThrowStmt(new List<>(),
                                            new DataConstructorExp("PatternMatchFailException",
                                                                   new List<>())));
                cs.addBranchNoTransform(new CaseBranchStmt(new UnderscorePattern(), block));
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
     * recursively set the position of this ast node and its children
     */
    private void setAllPositionsFromNode(ASTNode<?> node, ASTNode<?> fromNode) {
        node.setPositionFromNode(fromNode);
        for (int i=0; i < node.getNumChildNoTransform(); i++) {
            ASTNode<?> child = node.getChildNoTransform(i);
            setAllPositionsFromNode(child, fromNode);
        }
    }
}
