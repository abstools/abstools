/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.parser;

import abs.frontend.ast.CaseBranch;
import abs.frontend.ast.CaseExp;
import abs.frontend.ast.CompilationUnit;
import abs.frontend.ast.ConstructorArg;
import abs.frontend.ast.ConstructorPattern;
import abs.frontend.ast.DataConstructor;
import abs.frontend.ast.DataTypeDecl;
import abs.frontend.ast.DataTypeUse;
import abs.frontend.ast.Decl;
import abs.frontend.ast.ExpFunctionDef;
import abs.frontend.ast.FunctionDecl;
import abs.frontend.ast.FunctionDef;
import abs.frontend.ast.List;
import abs.frontend.ast.ModuleDecl;
import abs.frontend.ast.ParamDecl;
import abs.frontend.ast.ParametricDataTypeDecl;
import abs.frontend.ast.ParametricFunctionDecl;
import abs.frontend.ast.Pattern;
import abs.frontend.ast.PatternVar;
import abs.frontend.ast.PatternVarDecl;
import abs.frontend.ast.TypeParameterDecl;
import abs.frontend.ast.TypeUse;
import abs.frontend.ast.UnderscorePattern;
import abs.frontend.ast.VarUse;

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
                            moduleDecl.addDeclNoTransform(createSelectorFunction(dtd, c, ca, i));
                        }
                        i++;
                    }
                }
            }
        }
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
    private FunctionDecl createSelectorFunction(DataTypeDecl dtd, DataConstructor c, ConstructorArg ca, int numArg) {
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
        
        List<TypeParameterDecl> typeParams = new List<TypeParameterDecl>();
        if (dtd instanceof ParametricDataTypeDecl) {
            typeParams = ((ParametricDataTypeDecl) dtd).getTypeParameterList();
        }
        
        // the complete function definition
        FunctionDecl fd = 
            new ParametricFunctionDecl(
                    selName,    // function name
                    ca.getDataTypeUse().copy(), // type
                    new List().add(new ParamDecl("data",new DataTypeUse(dtd.getName(), new List()),new List())), // parameters
                    funDef,
                    new List(), // annotations
                    typeParams
                    );
        return fd;
    }
}
