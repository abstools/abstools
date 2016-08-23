/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.parser;

import abs.frontend.ast.*;

/**
 * Preprocesses the AST directly after it has been parsed, before any name and type analysis.
 * Typically, syntactic sugar is eliminated in this phase
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
                for (DataConstructor c : dtd.getDataConstructors()) {
                    int i = 0;
                    for (ConstructorArg ca : c.getConstructorArgs()) {
                        if (ca.hasSelectorName()) {
                            moduleDecl.addDeclNoTransform(createSelectorFunction(dtd, c, ca, i, false));
                        }
                        i++;
                    }
                }
            }
        }
    }

    public FunctionDecl createSelectorFunctionForDeltaApplication(DataTypeDecl dtd, DataConstructor c, ConstructorArg ca, int numArg) {
        return createSelectorFunction(dtd, c, ca, numArg, true);
    }

    /**
     * Creates for a selector a corresponding function, e.g.
     *
     * <pre>
     * data Foo = Bar(String baz, Bool isTrue, String name);
     * </pre>
     *
     * creates:
     * <pre>
     * def Bool isTrue(Foo data) =
     *     case data {
     *     Bar(_,res,_) => res;
     *     };
     * <pre>
     */
    private FunctionDecl createSelectorFunction(DataTypeDecl dtd, DataConstructor c, ConstructorArg ca, int numArg, boolean delta) {
        String selName = ca.getSelectorName().getName();

        // the list of patterns, e.g. _,res,_
        List<Pattern> patternList = new List<Pattern>();
        for (int i = 0; i < c.getNumConstructorArg(); i++) {
            if (i == numArg)
                patternList.add(new PatternVar(new PatternVarDecl("res")));
            else
                patternList.add(new UnderscorePattern());
        }

        // the case expression
        FunctionDef funDef =
            new ExpFunctionDef(
                new CaseExp(
                    new VarUse("data"),
                    new List<CaseBranch>().add(
                        new CaseBranch(
                            new ConstructorPattern(c.getName(), patternList),
                            new VarUse("res")))));

        // the type parameters of the function
        List<TypeParameterDecl> typeParams;
        // the type of the parameter of the function
        TypeUse paramType;
        if (dtd instanceof ParametricDataTypeDecl) {
            ParametricDataTypeDecl pdtd = (ParametricDataTypeDecl) dtd;
            typeParams = (delta) ? pdtd.getTypeParameterList().treeCopyNoTransform() : pdtd.getTypeParameterList();
            List<TypeUse> typeParams2 = new List<TypeUse>();
            for (TypeParameterDecl p : typeParams) {
                typeParams2.add(p.getType().toUse());
            }
            paramType = new ParametricDataTypeUse(dtd.getName(), new List<Annotation>(), typeParams2);
        } else {
            typeParams = new List<TypeParameterDecl>();
            paramType = dtd.getType().toUse();
        }



        List<ParamDecl> parameters = new List<ParamDecl>()
                .add(new ParamDecl("data",paramType,new List<Annotation>()));
        // the complete function definition
        FunctionDecl fd =
            new ParametricFunctionDecl(
                    selName,    // function name
                    (TypeUse)ca.getTypeUse().copy(), // type
                    parameters, // parameters
                    funDef,
                    new List<Annotation>(), // annotations
                    typeParams
                    );

        // annotate that this function is a selector function
        // such that backends will know about it
        fd.addAnnotation(new Annotation(new StringLiteral(FUNCTIONSELECTOR)));

        setAllPositionsFromNode(fd, ca);
        return fd;
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
