/**
 * Copyright (c) 2014, Rudolf Schlatte. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.antlr.parser;

import org.abs_models.frontend.ast.*;
import org.abs_models.frontend.parser.Main;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeProperty;

import org.abs_models.frontend.antlr.parser.ABSParser.MethodsigContext;
import org.abs_models.frontend.antlr.parser.ABSParser.TraitApplyFragmentContext;
import org.abs_models.frontend.antlr.parser.ABSParser.TraitNameFragmentContext;
import org.abs_models.frontend.antlr.parser.ABSParser.TraitSetFragmentContext;

/**
 * This class creates the JastAdd AST from an Antlr parse tree.
 *
 * @author Rudi Schlatte
 */
public class CreateJastAddASTListener extends ABSBaseListener {

    String filename = Main.UNKNOWN_FILENAME;

    /** maps antlr nodes to JastAdd nodes - see antlr book Sec.7.5 */
    ParseTreeProperty<ASTNode<?>> values = new ParseTreeProperty<>();
    UnresolvedTypeUse t = null;
    CompilationUnit result = null;

    public CreateJastAddASTListener(java.io.File filename) {
        if (filename != null) this.filename = filename.getPath();
    }

    private <T extends ASTNode<?>> T setASTNodePosition(ParserRuleContext node, T value) {
        assert node != null;
        Token start = node.getStart();
        Token stop = node.getStop();
        // for a completely empty file, CompilationUnit.stop will be null
        if (stop == null) stop = start;
        int startline = start.getLine();
        int startcol = start.getCharPositionInLine();
        int endline = stop.getLine();
        int endcol = stop.getCharPositionInLine() + stop.getText().length();
        value.setPosition(startline, startcol, endline, endcol);
        value.setFileName(this.filename);
        return value;
    }

    private <T extends ASTNode<?>> T setASTNodePosition(Token node, T value) {
        assert node != null;
        int startline = node.getLine();
        int startcol = node.getCharPositionInLine();
        // for a completely empty file, CompilationUnit.stop will be null
        int endline = startline;
        int endcol = startcol + node.getText().length();
        value.setPosition(startline, startcol, endline, endcol);
        value.setFileName(this.filename);
        return value;
    }

    /**
     * Associates JastAdd value with antlr node such that v(node) will
     * return value.  Also sets filename and position of the JastAdd
     * value.  Returns the passed-in JastAdd value.
     */
    private <T extends ASTNode<?>> T setV(ParserRuleContext node, T value) {
        setASTNodePosition(node, value);
        values.put(node, value);
        return value;
    }

    /**
     * Returns the AstNode for the given antlr parse node.  The result
     * is guaranteed to be non-null.
     */
    @SuppressWarnings("unchecked")
    private <T extends ASTNode<?>> T v(ParseTree node) {
        ASTNode<?> result = values.get(node);
        if (result == null) throw new NullPointerException();
        return (T) result;
    }

    /**
     * Returns a fresh Opt<ASTNode> filled with the result of v(node) if
     * node is non-null, empty otherwise.
     */
    private <T extends ASTNode<?>> Opt<T> o(ParseTree node) {
        if (node == null) return new Opt<>();
        else return new Opt<>(v(node));
    }

    /**
     * Returns a list of ASTNodes given a list of antlr parse nodes.
     * The result list elements are found via 'v' and are guaranteed
     * non-null.
     */
    private <T extends ASTNode<?>> List<T> l(java.util.List<? extends ParseTree> l) {
        List<T> result = new List<>();
        for (ParseTree n : l) {
            result.add(v(n));
        }
        return result;
    }

    public CompilationUnit getCompilationUnit() {
        return result;
    }

    private StringLiteral makeStringLiteral(String tokenText) {
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
        return new StringLiteral(s.toString());
    }

    private PureExp makeTemplateStringLiteral(String tokenText) {
        StringBuffer s = new StringBuffer(tokenText.length() - 2);
        // i = 1..len-1 to skip beginning and ending \` of the stringliteral
        for (int i = 1; i < tokenText.length() - 1; i++) {
            char c = tokenText.charAt(i);
            if (c == '\\') {
                i++;
                c = tokenText.charAt(i);
                switch (c) {
                    // only handling ` and \ here
                default : s.append(c); break;
                }
            } else {
                s.append(c);
            }
        }
        return new StringLiteral(s.toString());
    }

    @Override public void enterCompilation_unit(ABSParser.Compilation_unitContext ctx) {
        this.result = setV(ctx, new CompilationUnit(this.filename,
            new List<>(),
            new List<>(), new List<>(),
            new Opt<>(), new List<>(),
            new List<>(), new List<>()));
    }

    @Override public void exitCompilation_unit(ABSParser.Compilation_unitContext ctx) {
        result.setModuleDeclList(l(ctx.module_decl()));
        result.setDeltaDeclList(l(ctx.delta_decl()));
        result.setProductLineOpt(o(ctx.productline_decl()));
        result.setProductDeclList(l(ctx.product_decl()));
        result.setFeatureDeclList(l(ctx.feature_decl()));
        result.setFExtList(l(ctx.fextension()));
    }


    // Traits


    @Override public void exitDeltaTraitFragment(ABSParser.DeltaTraitFragmentContext ctx) {
        setV(ctx,new DeltaTraitModifier(v(ctx.trait_oper())));
    }

    @Override public void exitTraitAddFragment(ABSParser.TraitAddFragmentContext ctx) {
        setV(ctx,new AddMethodModifier((TraitExpr) v(ctx.trait_expr())));
    }
    @Override public void exitTraitModifyFragment(ABSParser.TraitModifyFragmentContext ctx) {
        setV(ctx,new ModifyMethodModifier((TraitExpr) v(ctx.trait_expr())));
     }
    @Override public void exitTraitRemoveFragment(ABSParser.TraitRemoveFragmentContext ctx) {
        List<MethodSig> l = new List<>();
        for (MethodsigContext methodSig : ctx.methodsig()) {
            l.add(v(methodSig));
        }
        setV(ctx,new RemoveMethodModifier(l));
     }


         @Override
        public void exitTraitApplyFragment(TraitApplyFragmentContext ctx) {
             setV(ctx, new TraitModifyExpr(v(ctx.trait_expr()), v(ctx.trait_oper())));
        }
         @Override
        public void exitTraitNameFragment(TraitNameFragmentContext ctx) {
             setV(ctx, new TraitNameExpr(ctx.TYPE_IDENTIFIER().getText()));
        }
         @Override
        public void exitTraitSetFragment(TraitSetFragmentContext ctx) {
             setV(ctx, new TraitSetExpr(l(ctx.method())));
        }
    @Override public void exitTrait_usage( ABSParser.Trait_usageContext ctx) {
        //setV(ctx, new TraitUse(ctx.TYPE_IDENTIFIER().toString(), new List()));
        setV(ctx, new TraitUse(v(ctx.trait_expr())));
    }
    @Override public void exitTrait_decl( ABSParser.Trait_declContext ctx) {
        setV(ctx, new TraitDecl(ctx.TYPE_IDENTIFIER().getText(), v(ctx.trait_expr())));
    }

    // Declarations
    @Override public void exitDecl(ABSParser.DeclContext ctx) {
        setV(ctx, v(ctx.getChild(0))); // relies on decl having one token
    }

    @Override public void exitModule_decl(ABSParser.Module_declContext ctx) {
        setV(ctx, new ModuleDecl(ctx.qualified_type_identifier().getText(), l(ctx.exports), l(ctx.imports), l(ctx.decl()), o(ctx.main_block())));
    }

    @Override public void exitModule_export(ABSParser.Module_exportContext ctx) {
        if (ctx.f == null) {
            if (ctx.s == null || ctx.s.isEmpty()) setV(ctx, new StarExport());
            else setV(ctx, new NamedExport(l(ctx.s)));
        } else {
            if (ctx.s == null || ctx.s.isEmpty()) setV(ctx, new StarExport(o(ctx.f)));
            else setV(ctx, new FromExport(l(ctx.s), ctx.f.getText()));
        }
    }

    @Override public void exitModule_import(ABSParser.Module_importContext ctx) {
        if (ctx.s == null || ctx.s.isEmpty()) setV(ctx, new StarImport(ctx.f.getText()));
        else  if (ctx.f == null) setV(ctx, new NamedImport(l(ctx.s)));
        else setV(ctx, new FromImport(l(ctx.s), ctx.f.getText()));
    }

    @Override public void exitDatatype_decl(ABSParser.Datatype_declContext ctx) {
        ParametricDataTypeDecl d = setV(ctx, new ParametricDataTypeDecl(ctx.n.getText(), l(ctx.c), v(ctx.annotations()),
            new List<>()));
        for (Token t : ctx.p) {
            TypeParameterDecl tpd = new TypeParameterDecl(t.getText());
            setASTNodePosition(t, tpd);
            d.addTypeParameter(tpd);
        }
    }

    @Override public void exitData_constructor(ABSParser.Data_constructorContext ctx) {
        DataConstructor d
            = setV(ctx, new DataConstructor(ctx.n.getText(), new List<>()));
        // KLUDGE: copied into exitException_decl
        for (ABSParser.Data_constructor_argContext a : ctx.a) {
            final TypeUse vt = v(a.type_use());
            final DataTypeUse vtresolved;
            if (vt instanceof DataTypeUse) {
                vtresolved = (DataTypeUse) vt;
            } else {
                // See below, we may be facing an UnresolvedTypeUse.
                assert vt instanceof UnresolvedTypeUse : vt.getClass().getName();
                vtresolved = new DataTypeUse(vt.getName(), vt.getAnnotations());
                vtresolved.setPositionFromNode(vt);
            }
            ConstructorArg ca = new ConstructorArg(vtresolved, a.IDENTIFIER() != null ? new Opt<>(new Name(a.IDENTIFIER().getText())) : new Opt<>());
            setASTNodePosition(a, ca);
            d.addConstructorArg(ca);
        }
    }

    @Override public void exitFunction_decl(ABSParser.Function_declContext ctx) {
        // TODO: datatypes do not distinguish between DataTypeDecl and
        // ParametricDataTypeDecl; it would be nice to do this for
        // functions as well.
        FunctionDef d = ctx.e == null ? new BuiltinFunctionDef() : new ExpFunctionDef(v(ctx.e));
        List<ParamDecl> p = v(ctx.paramlist());
        TypeUse t = v(ctx.type_use());
        if (ctx.p != null && !ctx.p.isEmpty()) {
            ParametricFunctionDecl dp
                = setV(ctx, new ParametricFunctionDecl(ctx.n.getText(), t, p, d, v(ctx.annotations()),
                new List<>()));
            for (Token tp : ctx.p) {
                TypeParameterDecl tpd = new TypeParameterDecl(tp.getText());
                setASTNodePosition(tp, tpd);
                dp.addTypeParameter(tpd);
            }
        } else {
            setV(ctx, new FunctionDecl(ctx.n.getText(), v(ctx.annotations()), t, p, d));
        }
    }

    @Override
    public void exitPar_function_decl(ABSParser.Par_function_declContext ctx) {
        PartialFunctionDef d = new PartialFunctionDef(v(ctx.e));
        List<ParamDecl> params = v(ctx.params);
        List<FunctionParamDecl> funcParams = v(ctx.functions);
        TypeUse t = v(ctx.type_use());
        if(ctx.p != null && !ctx.p.isEmpty()) {
            ParametricPartialFunctionDecl fd =
                setV(ctx, new ParametricPartialFunctionDecl(ctx.n.getText(), l(ctx.annotation()),
                    new List<>(), t, params, funcParams, d));
            for (Token tp : ctx.p) {
                TypeParameterDecl tpd = new TypeParameterDecl(tp.getText());
                setASTNodePosition(tp, tpd);
                fd.addTypeParameter(tpd);
            }
        } else {
            setV(ctx, new PartialFunctionDecl(ctx.n.getText(), l(ctx.annotation()), t, params, funcParams, d));
        }
    }

    @Override public void exitTypesyn_decl(ABSParser.Typesyn_declContext ctx) {

        setV(ctx, new TypeSynDecl(ctx.TYPE_IDENTIFIER().getText(), v(ctx.annotations()), v(ctx.type_use())));
    }

    @Override public void exitException_decl(ABSParser.Exception_declContext ctx) {
        ExceptionConstructor d
            = new ExceptionConstructor(ctx.n.getText(), new List<>());
        // KLUDGE: copy of exitData_constructor
        for (ABSParser.Data_constructor_argContext a : ctx.a) {
            final TypeUse vt = v(a.type_use());
            final DataTypeUse vtresolved;
            if (vt instanceof DataTypeUse) {
                vtresolved = (DataTypeUse) vt;
            } else {
                // See below, we may be facing an UnresolvedTypeUse.
                assert vt instanceof UnresolvedTypeUse : vt.getClass().getName();
                vtresolved = new DataTypeUse(vt.getName(), vt.getAnnotations());
                vtresolved.setPositionFromNode(vt);
            }
            ConstructorArg ca = new ConstructorArg(vtresolved, a.IDENTIFIER() != null ? new Opt<>(new Name(a.IDENTIFIER().getText())) : new Opt<>());
            setASTNodePosition(a, ca);
            d.addConstructorArg(ca);
        }
        List<DataConstructor> l = new List<>();
        l.add(d);
        setV(ctx, new ExceptionDecl(ctx.n.getText(), v(ctx.annotations()), l));
    }

    @Override public void exitMain_block(ABSParser.Main_blockContext ctx) {
        setV(ctx, new MainBlock(v(ctx.annotations()), l(ctx.stmt())));
    }

    // Interfaces
    @Override public void exitInterface_decl(ABSParser.Interface_declContext ctx) {
        setV(ctx, new InterfaceDecl(ctx.TYPE_IDENTIFIER().getText(), v(ctx.annotations()), l(ctx.e), l(ctx.methodsig())));
    }

    @Override public void exitMethodsig(ABSParser.MethodsigContext ctx) {
        setV(ctx, new MethodSig(ctx.IDENTIFIER().getText(), v(ctx.annotations()), v(ctx.type_use()), v(ctx.paramlist())));
    }

    // Classes
    @Override public void exitClass_decl(ABSParser.Class_declContext ctx) {
        ClassDecl c = setV(ctx, new ClassDecl(ctx.TYPE_IDENTIFIER().getText(), v(ctx.annotations()),
            new List<>(), l(ctx.interface_name()),
                                                         l(ctx.trait_usage()), new Opt<>(), l(ctx.casestmtbranch()), l(ctx.field_decl()), l(ctx.method())));
        if (ctx.paramlist() != null) {
            c.setParamList(v(ctx.paramlist()));
        }
        if (ctx.stmt() != null && !ctx.stmt().isEmpty()) {
            InitBlock b = new InitBlock(new List<>(), new List<>());
            for (ABSParser.StmtContext s : ctx.stmt()) {
                b.addStmt(v(s));
            }
            c.setInitBlock(b);
        }
    }

    @Override public void exitField_decl(ABSParser.Field_declContext ctx) {
        // FIXME: 'port' missing (for component model)
        FieldDecl f = setV(ctx, new FieldDecl(ctx.IDENTIFIER().getText(), v(ctx.type_use()), o(ctx.pure_exp()),
            v(ctx.annotations()), false));
    }

    @Override public void exitMethod(ABSParser.MethodContext ctx) {
        MethodSig ms = new MethodSig(ctx.IDENTIFIER().getText(), v(ctx.annotations()), v(ctx.type_use()),
            v(ctx.paramlist()));
        ms.setPosition(ctx.IDENTIFIER().getSymbol().getLine(), ctx.IDENTIFIER().getSymbol().getCharPositionInLine(),
                       ctx.paramlist().getStop().getLine(), ctx.paramlist().getStop().getCharPositionInLine() + ctx.paramlist().getStop().getText().length());
        Block b = new Block(new List<>(), new List<>());
        for (ABSParser.StmtContext s : ctx.stmt()) {
            b.addStmt(v(s));
        }
        // FIXME: 'critical' missing (for component model)
        setV(ctx, new MethodImpl(ms, b, false));
    }

    // Statements
    @Override public void exitVardeclStmt(ABSParser.VardeclStmtContext ctx) {
        VarDecl v = new VarDecl(ctx.IDENTIFIER().getText(), v(ctx.type_exp()), new Opt<>());
        setASTNodePosition(ctx, v);
        if (ctx.exp() != null) {
            v.setInitExp(v(ctx.exp()));
        }
        setV(ctx, new VarDeclStmt(v(ctx.annotations()), v));
    }

    @Override public void exitAssignStmt(ABSParser.AssignStmtContext ctx) {
        setV(ctx, new AssignStmt(v(ctx.annotations()), v(ctx.var_or_field_ref()), v(ctx.exp())));
    }
    @Override public void exitSkipStmt(ABSParser.SkipStmtContext ctx) {
        setV(ctx, new SkipStmt(v(ctx.annotations())));
    }
    @Override public void exitReturnStmt(ABSParser.ReturnStmtContext ctx) {
        setV(ctx, new ReturnStmt(v(ctx.annotations()), v(ctx.exp())));
    }
    @Override public void exitAssertStmt(ABSParser.AssertStmtContext ctx) {
        setV(ctx, new AssertStmt(v(ctx.annotations()), v(ctx.exp())));
    }
    @Override public void exitBlockStmt(ABSParser.BlockStmtContext ctx) {
        setV(ctx, new Block(v(ctx.annotations()), l(ctx.stmt())));
    }
    @Override public void exitIfStmt(ABSParser.IfStmtContext ctx) {
        Stmt l = v(ctx.l);
        if (!(l instanceof Block)) {
            setV(ctx.l, new Block(new List<>(), new List<>(l)));
        }
        if (ctx.r != null) {
            Stmt r = v(ctx.r);
            if (!(r instanceof Block)) {
                setV(ctx.r, new Block(new List<>(), new List<>(r)));
            }
        }
        setV(ctx, new IfStmt(v(ctx.annotations()), v(ctx.c),
            v(ctx.l), o(ctx.r)));
    }
    @Override public void exitWhileStmt(ABSParser.WhileStmtContext ctx) {
        Stmt body = v(ctx.stmt());
        if (!(body instanceof Block)) {
            setV(ctx.stmt(), new Block(new List<>(), new List<>(body)));
        }
        setV(ctx, new WhileStmt(v(ctx.annotations()), v(ctx.c), v(ctx.stmt())));
    }
    @Override public void exitForeachStmt(ABSParser.ForeachStmtContext ctx) {
        Stmt body = v(ctx.stmt());
        if (!(body instanceof Block)) {
            setV(ctx.stmt(), new Block(new List<>(), new List<>(body)));
        }
        setV(ctx, new ForeachStmt(v(ctx.annotations()), new LoopVarDecl(ctx.i.getText()), v(ctx.l), v(ctx.stmt())));
    }
    @Override public void exitTryCatchFinallyStmt(ABSParser.TryCatchFinallyStmtContext ctx) {
        Stmt body = v(ctx.b);
        if (!(body instanceof Block)) {
            setV(ctx.b, new Block(new List<>(), new List<>(body)));
        }
        if (ctx.f != null) {
            Stmt finall = v(ctx.f);
            if (!(finall instanceof Block)) {
                setV(ctx.f, new Block(new List<>(), new List<>(finall)));
            }
        }
        setV(ctx, new TryCatchFinallyStmt(v(ctx.annotations()), v(ctx.b),
                                          l(ctx.casestmtbranch()), o(ctx.f)));
    }
    @Override public void exitAwaitStmt(ABSParser.AwaitStmtContext ctx) {
        setV(ctx, new AwaitStmt(v(ctx.annotations()), v(ctx.guard())));
    }
    @Override public void exitClaimGuard(ABSParser.ClaimGuardContext ctx) {
        setV(ctx, new ClaimGuard(v(ctx.var_or_field_ref())));
    }
    @Override public void exitDurationGuard(ABSParser.DurationGuardContext ctx) {
        setV(ctx, new DurationGuard(v(ctx.min), v(ctx.max)));
    }
    @Override public void exitExpGuard(ABSParser.ExpGuardContext ctx) {
        setV(ctx, new ExpGuard(v(ctx.e)));
    }
    @Override public void exitAndGuard(ABSParser.AndGuardContext ctx) {
        setV(ctx, new AndGuard(v(ctx.l), v(ctx.r)));
    }
    @Override public void exitSuspendStmt(ABSParser.SuspendStmtContext ctx) {
        setV(ctx, new SuspendStmt(v(ctx.annotations())));
    }
    @Override public void exitDurationStmt(ABSParser.DurationStmtContext ctx) {
        setV(ctx, new DurationStmt(v(ctx.annotations()), v(ctx.f),
            v(ctx.t)));
    }
    @Override public void exitThrowStmt(ABSParser.ThrowStmtContext ctx) {
        setV(ctx, new ThrowStmt(v(ctx.annotations()), v(ctx.pure_exp())));
    }
    @Override public void exitDieStmt(ABSParser.DieStmtContext ctx) {
        setV(ctx, new DieStmt(v(ctx.annotations()), v(ctx.pure_exp())));
    }
    @Override public void exitMoveCogToStmt(ABSParser.MoveCogToStmtContext ctx) {
        setV(ctx, new MoveCogToStmt(v(ctx.annotations()), v(ctx.pure_exp())));
    }
    @Override public void exitExpStmt(ABSParser.ExpStmtContext ctx) {
        setV(ctx, new ExpressionStmt(v(ctx.annotations()), v(ctx.exp())));
    }
    @Override public void exitCaseStmt(ABSParser.CaseStmtContext ctx) {
        List<CaseBranchStmt> branches = l(ctx.casestmtbranch());
        // Add default branch that throws PatternMatchFailException.  See
        // "Behavior of non-exhaustive case statement: no branch match = skip
        // or error?" on abs-dev on Jan 25-26, 2017
        Block block = new Block(new List<>(), new List<>());
        block.addStmt(new ThrowStmt(new List<>(),
                                    new DataConstructorExp("PatternMatchFailException",
                                                           new List<>())));
        CaseBranchStmt defaultBranch = new CaseBranchStmt(new UnderscorePattern(), block);
        setASTNodePosition(ctx, defaultBranch);
        branches.add(defaultBranch);
        setV(ctx, new CaseStmt(v(ctx.annotations()), v(ctx.c), branches));
    }
    @Override public void exitCasestmtbranch(ABSParser.CasestmtbranchContext ctx) {
        Stmt body = v(ctx.stmt());
        if (!(body instanceof Block)) {
            setV(ctx.stmt(), new Block(new List<>(), new List<>(body)));
        }
        setV(ctx, new CaseBranchStmt(v(ctx.pattern()), v(ctx.stmt())));
    }

    // Annotations
    @Override public void exitAnnotation(ABSParser.AnnotationContext ctx) {
        if (ctx.l == null) setV(ctx, new Annotation(v(ctx.r)));
        else setV(ctx, new TypedAnnotation(v(ctx.r), v(ctx.l)));
    }

    @Override public void exitAnnotations(ABSParser.AnnotationsContext ctx) {
        setV(ctx, l(ctx.al));
    }

    // Expressions
    @Override public void exitPureExp(ABSParser.PureExpContext ctx) {
        setV(ctx, v(ctx.pure_exp()));
    }
    @Override public void exitEffExp(ABSParser.EffExpContext ctx) {
        setV(ctx, v(ctx.eff_exp()));
    }

    // Side-effectful expressions
    @Override public void exitGetExp(ABSParser.GetExpContext ctx) {
        setV(ctx, new GetExp(v(ctx.pure_exp())));
    }
    @Override public void exitNewExp(ABSParser.NewExpContext ctx) {
        NewExp n = setV(ctx, new NewExp(ctx.c.getText(), v(ctx.pure_exp_list()), new Opt<>()));
        if (ctx.l != null) { n.setLocal(new Local()); }
    }
    @Override public void exitAsyncCallExp(ABSParser.AsyncCallExpContext ctx) {
        if (ctx.a != null) {
            setV(ctx, new AwaitAsyncCall(v(ctx.o), ctx.m.getText(), v(ctx.pure_exp_list())));
        } else {
            setV(ctx, new AsyncCall(v(ctx.o), ctx.m.getText(), v(ctx.pure_exp_list())));
        }
    }
    @Override public void exitSyncCallExp(ABSParser.SyncCallExpContext ctx) {
        setV(ctx, new SyncCall(v(ctx.o), ctx.m.getText(), v(ctx.pure_exp_list())));
    }
    @Override public void exitOriginalCallExp(ABSParser.OriginalCallExpContext ctx) {
        List<PureExp> l = ctx.pure_exp_list() == null
            ? new List<>()
            : v(ctx.pure_exp_list());
        if (ctx.c != null) {
            setV(ctx, new TargetedOriginalCall(new DeltaID("core"), l));
        } else if (ctx.d != null) {
            setV(ctx, new TargetedOriginalCall(v(ctx.d), l));
        } else {
            setV(ctx, new OriginalCall(l));
        }
    }

    @Override
    public void exitFunction_list(ABSParser.Function_listContext ctx) {
        List<ParFnAppParam> list = ctx.function_param() == null
            ? new List<>()
            : l(ctx.function_param());
        setV(ctx, list);
    }

    @Override
    public void exitFunction_param(ABSParser.Function_paramContext ctx) {
        if(ctx.anon_function_decl() != null) {
            setV(ctx, v(ctx.anon_function_decl()));
        } else if(ctx.function_name_param_decl() != null) {
            setV(ctx, v(ctx.function_name_param_decl()));
        }
    }

    @Override public void exitFunction_name_param_decl(ABSParser.Function_name_param_declContext ctx) {
        setV(ctx, new NamedParFnAppParam(ctx.IDENTIFIER().getText()));
    }

    @Override
    public void exitAnon_function_decl(ABSParser.Anon_function_declContext ctx) {
        List<ParamDecl> params = v(ctx.params);
        PureExp pureExp = v(ctx.pure_exp());
        setV(ctx, new AnonymousFunctionDecl(params, pureExp));
    }

    // Pure expressions
    @Override public void exitFunctionExp(ABSParser.FunctionExpContext ctx) {
        List<PureExp> l = ctx.pure_exp_list() == null
            ? new List<>()
            : v(ctx.pure_exp_list());
        setV(ctx, new FnApp(ctx.qualified_identifier().getText(), l));
    }
    @Override public void exitPartialFunctionExp(ABSParser.PartialFunctionExpContext ctx) {
        List<PureExp> params = ctx.pure_exp_list() == null
            ? new List<>()
            : v(ctx.pure_exp_list());
        List<ParFnAppParam> functionParams = ctx.function_list() == null
            ? new List<>()
            : v(ctx.function_list());

        setV(ctx, new ParFnApp(ctx.qualified_identifier().getText(), params, functionParams));
    }
    @Override public void exitVariadicFunctionExp(ABSParser.VariadicFunctionExpContext ctx) {
        List<PureExp> l = v(ctx.pure_exp_list());
        PureExp arglist = null;
        if (l.getNumChildNoTransform() == 0) {
            arglist = new DataConstructorExp("Nil", new List<>());
        } else {
            arglist = new ListLiteral(l);
        }
        setASTNodePosition(ctx.pure_exp_list(), arglist);
        List<PureExp> llist = new List<>();
        llist.add(arglist);
        setV(ctx, new FnApp(ctx.qualified_identifier().getText(), llist));
    }
    @Override public void exitConstructorExp(ABSParser.ConstructorExpContext ctx) {
        List<PureExp> l = ctx.pure_exp_list() == null
            ? new List<>()
            : v(ctx.pure_exp_list());
        setV(ctx,
                 new DataConstructorExp(ctx.qualified_type_identifier().getText(),
                                        l));
    }
    @Override public void exitUnaryExp(ABSParser.UnaryExpContext ctx) {
        switch (ctx.op.getType()) {
        case ABSParser.NEGATION :
        case ABSParser.NEGATION_CREOL :
            setV(ctx, new NegExp(v(ctx.pure_exp())));
            break;
        case ABSParser.MINUS :
            setV(ctx, new MinusExp(v(ctx.pure_exp())));
            break;
        }
    }
    @Override public void exitMultExp(ABSParser.MultExpContext ctx) {
        switch (ctx.op.getType()) {
        case ABSParser.MULT :
            setV(ctx, new MultMultExp(v(ctx.l), v(ctx.r)));
            break;
        case ABSParser.DIV :
            setV(ctx, new DivMultExp(v(ctx.l), v(ctx.r)));
            break;
        case ABSParser.MOD :
            setV(ctx, new ModMultExp(v(ctx.l), v(ctx.r)));
            break;
        }
    }
    @Override public void exitAddExp(ABSParser.AddExpContext ctx) {
        switch (ctx.op.getType()) {
        case ABSParser.PLUS :
            setV(ctx, new AddAddExp(v(ctx.l), v(ctx.r)));
            break;
        case ABSParser.MINUS :
            setV(ctx, new SubAddExp(v(ctx.l), v(ctx.r)));
            break;
        }
    }
    @Override public void exitGreaterExp(ABSParser.GreaterExpContext ctx) {
        switch (ctx.op.getType()) {
        case ABSParser.LT :
            setV(ctx, new LTExp(v(ctx.l), v(ctx.r)));
            break;
        case ABSParser.GT :
            setV(ctx, new GTExp(v(ctx.l), v(ctx.r)));
            break;
        case ABSParser.LTEQ :
            setV(ctx, new LTEQExp(v(ctx.l), v(ctx.r)));
            break;
        case ABSParser.GTEQ :
            setV(ctx, new GTEQExp(v(ctx.l), v(ctx.r)));
            break;
        }
    }
    @Override public void exitEqualExp(ABSParser.EqualExpContext ctx) {
        switch (ctx.op.getType()) {
        case ABSParser.EQEQ :
            setV(ctx, new EqExp(v(ctx.l), v(ctx.r)));
            break;
        case ABSParser.NOTEQ :
            setV(ctx, new NotEqExp(v(ctx.l), v(ctx.r)));
            break;
        }
    }
    @Override public void exitAndExp(ABSParser.AndExpContext ctx) {
        setV(ctx, new AndBoolExp(v(ctx.l), v(ctx.r)));
    }
    @Override public void exitOrExp(ABSParser.OrExpContext ctx) {
        setV(ctx, new OrBoolExp(v(ctx.l), v(ctx.r)));
    }
    @Override public void exitVarOrFieldExp(ABSParser.VarOrFieldExpContext ctx) {
        setV(ctx, v(ctx.var_or_field_ref()));
    }
    @Override public void exitIntExp(ABSParser.IntExpContext ctx) {
        setV(ctx, new IntLiteral(ctx.INTLITERAL().getText()));
    }
    @Override public void exitFloatExp(ABSParser.FloatExpContext ctx) {
        setV(ctx, new FloatLiteral(ctx.FLOATLITERAL().getText()));
    }
    @Override public void exitStringExp(ABSParser.StringExpContext ctx) {
        setV(ctx, makeStringLiteral(ctx.STRINGLITERAL().getText()));
    }
    @Override public void exitTemplateStringExp(ABSParser.TemplateStringExpContext ctx) {
        setV(ctx, makeTemplateStringLiteral(ctx.TEMPLATESTRINGLITERAL().getText()));
    }
    @Override public void exitThisExp(ABSParser.ThisExpContext ctx) {
        setV(ctx, new ThisExp());
    }
    @Override public void exitNullExp(ABSParser.NullExpContext ctx) {
        setV(ctx, new NullExp());
    }
    @Override public void exitIfExp(ABSParser.IfExpContext ctx) {
        setV(ctx, new IfExp(v(ctx.c),
            v(ctx.l),
            v(ctx.r)));
    }
    @Override public void exitCaseExp(ABSParser.CaseExpContext ctx) {
        List<CaseBranch> l = new List<>();
        for (ABSParser.CasebranchContext b : ctx.casebranch()) {
            l.add(v(b));
        }
        setV(ctx, new CaseExp(v(ctx.c), l));
    }
    @Override public void exitLetExp(ABSParser.LetExpContext ctx) {
        ParamDecl pd = new ParamDecl(ctx.IDENTIFIER().getText(),
            v(ctx.type_use()),
            new List<>());
        setASTNodePosition(ctx.IDENTIFIER().getSymbol(), pd);
        setV(ctx, new LetExp(pd, v(ctx.e), v(ctx.b)));
    }
    @Override public void exitImplementsExp(ABSParser.ImplementsExpContext ctx) {
        setV(ctx, new ImplementsExp(v(ctx.e), v(ctx.i)));
    }
    @Override public void exitAsExp(ABSParser.AsExpContext ctx) {
        setV(ctx, new AsExp(v(ctx.e), v(ctx.i)));
    }
    @Override public void exitParenExp(ABSParser.ParenExpContext ctx) {
        setV(ctx, v(ctx.pure_exp()));
    }

    @Override public void exitCasebranch(ABSParser.CasebranchContext ctx) {
        setV(ctx, new CaseBranch(v(ctx.pattern()),
            v(ctx.pure_exp())));
    }

    @Override public void exitUnderscorePattern(ABSParser.UnderscorePatternContext ctx) {
        setV(ctx, new UnderscorePattern());
    }
    @Override public void exitIntPattern(ABSParser.IntPatternContext ctx) {
        setV(ctx, new LiteralPattern(new IntLiteral(ctx.INTLITERAL().getText())));
    }
    @Override public void exitStringPattern(ABSParser.StringPatternContext ctx) {
        setV(ctx, new LiteralPattern(makeStringLiteral(ctx.STRINGLITERAL().getText())));
    }
    @Override public void exitVarPattern(ABSParser.VarPatternContext ctx) {
        setV(ctx, new PatternVarUse(ctx.IDENTIFIER().getText()));
    }
    @Override public void exitConstructorPattern(ABSParser.ConstructorPatternContext ctx) {
        setV(ctx, new ConstructorPattern(ctx.qualified_type_identifier().getText(), l(ctx.pattern())));
    }

    @Override public void exitParamlist(ABSParser.ParamlistContext ctx) {
        setV(ctx, l(ctx.param_decl()));
    }

    @Override public void exitParam_decl(ABSParser.Param_declContext ctx) {
        setV(ctx, new ParamDecl(ctx.IDENTIFIER().getText(), v(ctx.type_exp()), v(ctx.annotations())));
    }

    @Override public void exitFunction_name_list(ABSParser.Function_name_listContext ctx) {
        setV(ctx, l(ctx.function_name_decl()));
    }

    @Override public void exitFunction_name_decl(ABSParser.Function_name_declContext ctx) {
        setV(ctx, new FunctionParamDecl(ctx.IDENTIFIER().getText()));
    }

    @Override public void exitInterface_name(ABSParser.Interface_nameContext ctx) {
        setV(ctx, new InterfaceTypeUse(ctx.qualified_type_identifier().getText(), new List<>()));
    }

    @Override public void exitPure_exp_list(ABSParser.Pure_exp_listContext ctx) {
        List<PureExp> l = setV(ctx, new List<PureExp>());
        for (ABSParser.Pure_expContext a : ctx.pure_exp()) {
            l.add(v(a));
        }
    }

    @Override public void exitType_use(ABSParser.Type_useContext ctx) {
        /* As we could be looking at an interface type, first keep symbol
         * unresolved and have rewrite-rules patch it up.
         * However, this means that in the parser the DataConstructor could
         * be seeing. But there we know what it must be and "rewrite" it ourselves.
         */
        if (ctx.p.isEmpty()) {
            // normal type use
            setV(ctx, new UnresolvedTypeUse(ctx.n.getText(), v(ctx.annotations())));
        } else {
            // parametric type use
            ParametricDataTypeUse p
                = setV(ctx, new ParametricDataTypeUse(ctx.n.getText(), v(ctx.annotations()),
                new List<>()));
            for (ABSParser.Type_useContext c : ctx.type_use()) {
                p.addParam(v(c));
            }
        }
    }

    @Override
    public void exitType_use_paramlist(ABSParser.Type_use_paramlistContext ctx) {
        List<TypeUse> list = setV(ctx, new List<TypeUse>());
        for (ABSParser.Type_useContext typeUseContext : ctx.type_use()) {
            list.add(v(typeUseContext));
        }
    }

    @Override public void exitType_exp(ABSParser.Type_expContext ctx) {
        if (ctx.p.isEmpty()) {
            // normal type use
            setV(ctx, new UnresolvedTypeUse(ctx.n.getText(), new List<>()));
        } else {
            // parametric type use
            ParametricDataTypeUse p
                = setV(ctx, new ParametricDataTypeUse(ctx.n.getText(), new List<>(),
                new List<>()));
            for (ABSParser.Type_useContext c : ctx.type_use()) {
                p.addParam(v(c));
            }
        }
    }

    @Override public void exitVar_or_field_ref(ABSParser.Var_or_field_refContext ctx) {
        if (ctx.getChildCount() == 1) { // id
            setV(ctx, new VarUse(ctx.IDENTIFIER().getText()));
        } else {                // this.id
            setV(ctx, new FieldUse(ctx.IDENTIFIER().getText()));
        }
    }

    @Override public void exitQualified_type_identifier(ABSParser.Qualified_type_identifierContext ctx) {
        setV(ctx, new Name(ctx.getText()));
    }

    @Override public void exitQualified_identifier(ABSParser.Qualified_identifierContext ctx) {
        setV(ctx, new Name(ctx.getText()));
    }

    @Override public void exitAny_identifier(ABSParser.Any_identifierContext ctx) {
        setV(ctx, v(ctx.getChild(0))); // relies on any_identifier
                                       // having one token
    }

    // Deltas
    @Override public void exitDelta_decl(ABSParser.Delta_declContext ctx) {
        setV(ctx, new DeltaDecl(ctx.TYPE_IDENTIFIER().getText(),
                                l(ctx.p), l(ctx.delta_access()),
                                l(ctx.module_modifier())));
    }

    @Override public void exitDeltaFieldParam(ABSParser.DeltaFieldParamContext ctx) {
        setV(ctx, new DeltaFieldParam(v(ctx.param_decl())));
    }

    @Override public void exitDeltaClassParam(ABSParser.DeltaClassParamContext ctx) {
        setV(ctx, new DeltaClassParam(ctx.qualified_type_identifier().getText(),
            v(ctx.has_condition())));
    }

    @Override public void exitDeltaHasFieldCondition(ABSParser.DeltaHasFieldConditionContext ctx) {
        setV(ctx, new HasField(v(ctx.f)));
    }

    @Override public void exitDeltaHasMethodCondition(ABSParser.DeltaHasMethodConditionContext ctx) {
        setV(ctx, new HasMethod(v(ctx.m)));
    }

    @Override public void exitDeltaHasInterfaceCondition(ABSParser.DeltaHasInterfaceConditionContext ctx) {
        setV(ctx, new HasInterface(v(ctx.i)));
    }

    @Override public void exitDelta_access(ABSParser.Delta_accessContext ctx) {
        setV(ctx, new DeltaAccess(ctx.qualified_type_identifier().getText()));
    }

    @Override public void exitModule_modifier(ABSParser.Module_modifierContext ctx) {
        setV(ctx, v(ctx.getChild(0))); // relies on node having one token
    }

    @Override public void exitDeltaAddFunctionModifier(ABSParser.DeltaAddFunctionModifierContext ctx) {
        setV(ctx, new AddFunctionModifier(v(ctx.function_decl())));
    }

    @Override public void exitDeltaAddDataTypeModifier(ABSParser.DeltaAddDataTypeModifierContext ctx) {
        setV(ctx, new AddDataTypeModifier(v(ctx.datatype_decl())));
    }

    @Override public void exitDeltaAddTypeSynModifier(ABSParser.DeltaAddTypeSynModifierContext ctx) {
        setV(ctx, new AddTypeSynModifier(v(ctx.typesyn_decl())));
    }

    @Override public void exitDeltaModifyTypeSynModifier(ABSParser.DeltaModifyTypeSynModifierContext ctx) {
        setV(ctx, new ModifyTypeSynModifier(v(ctx.typesyn_decl())));
    }

    @Override public void exitDeltaModifyDataTypeModifier(ABSParser.DeltaModifyDataTypeModifierContext ctx) {
        setV(ctx, new ModifyDataTypeModifier(v(ctx.datatype_decl())));
    }

    @Override public void exitDeltaAddClassModifier(ABSParser.DeltaAddClassModifierContext ctx) {
        setV(ctx, new AddClassModifier(v(ctx.class_decl())));
    }

    @Override public void exitDeltaRemoveClassModifier(ABSParser.DeltaRemoveClassModifierContext ctx) {
        setV(ctx, new RemoveClassModifier(ctx.qualified_type_identifier().getText()));
    }

    @Override public void exitDeltaModifyClassModifier(ABSParser.DeltaModifyClassModifierContext ctx) {
        setV(ctx, new ModifyClassModifier(ctx.n.getText(), l(ctx.ia), l(ctx.ir),
                                          l(ctx.class_modifier_fragment())));
    }

    @Override public void exitDeltaAddInterfaceModifier(ABSParser.DeltaAddInterfaceModifierContext ctx) {
        setV(ctx, new AddInterfaceModifier(v(ctx.interface_decl())));
    }

    @Override public void exitDeltaRemoveInterfaceModifier(ABSParser.DeltaRemoveInterfaceModifierContext ctx) {
        setV(ctx, new RemoveInterfaceModifier(ctx.qualified_type_identifier().getText()));
    }

    @Override public void exitDeltaModifyInterfaceModifier(ABSParser.DeltaModifyInterfaceModifierContext ctx) {
        setV(ctx, new ModifyInterfaceModifier(ctx.qualified_type_identifier().getText(),
                                              l(ctx.interface_modifier_fragment())));
    }

    @Override public void exitDeltaAddFieldFragment(ABSParser.DeltaAddFieldFragmentContext ctx) {
        setV(ctx, new AddFieldModifier(v(ctx.field_decl())));
    }
    @Override public void exitDeltaRemoveFieldFragment(ABSParser.DeltaRemoveFieldFragmentContext ctx) {
        setV(ctx, new RemoveFieldModifier(v(ctx.field_decl())));
    }

    @Override public void exitDeltaAddMethodsigFragment(ABSParser.DeltaAddMethodsigFragmentContext ctx) {
        setV(ctx, new AddMethodSigModifier(v(ctx.methodsig())));
    }
    @Override public void exitDeltaRemoveMethodsigFragment(ABSParser.DeltaRemoveMethodsigFragmentContext ctx) {
        setV(ctx, new RemoveMethodSigModifier(v(ctx.methodsig())));
    }

    @Override public void exitDeltaAddModuleImportFragment(ABSParser.DeltaAddModuleImportFragmentContext ctx) {
        setV(ctx, new AddImportModifier(v(ctx.module_import())));
    }
    @Override public void exitDeltaAddModuleExportFragment(ABSParser.DeltaAddModuleExportFragmentContext ctx) {
        setV(ctx, new AddExportModifier(v(ctx.module_export())));
    }

    // Updates (?)
    @Override public void exitUpdateDecl(ABSParser.UpdateDeclContext ctx) {
        setV(ctx, new UpdateDecl(ctx.TYPE_IDENTIFIER().getText(), l(ctx.object_update())));
    }

    @Override public void exitObjectUpdateDecl(ABSParser.ObjectUpdateDeclContext ctx) {
        setV(ctx, new ObjectUpdate(ctx.qualified_type_identifier().getText(),
                                   new AwaitStmt(new List<>(), v(ctx.guard())),
                                   new UpdatePreamble(l(ctx.update_preamble_decl())),
                                   l(ctx.pre), l(ctx.post)));
    }

    @Override public void exitObjectUpdateAssignStmt(ABSParser.ObjectUpdateAssignStmtContext ctx) {
        setV(ctx, new AssignStmt(new List<>(), v(ctx.var_or_field_ref()), v(ctx.exp())));
    }

    @Override public void exitUpdatePreambleDecl(ABSParser.UpdatePreambleDeclContext ctx) {
        setV(ctx, new VarDeclStmt(new List<>(), new VarDecl(ctx.IDENTIFIER().getText(), v(ctx.type_exp()), new Opt<>())));
    }


    // Productline
    @Override public void exitProductline_decl(ABSParser.Productline_declContext ctx) {
        setV(ctx, new ProductLine(ctx.TYPE_IDENTIFIER().getText(),
                                  l(ctx.feature()), l(ctx.delta_clause())));
    }

    @Override public void exitFeature(ABSParser.FeatureContext ctx) {
        setV(ctx, new Feature((ctx.p == null ? "" : "$") + ctx.TYPE_IDENTIFIER().getText(),
                              l(ctx.attr_assignment())));
    }

    @Override public void exitAttr_assignment(ABSParser.Attr_assignmentContext ctx) {
        Value val = null;
        String id = ctx.IDENTIFIER().getText();
        if (ctx.i != null) val = new IntVal(Integer.parseInt(ctx.i.getText()));
        else if(ctx.s != null) val = new StringVal(ctx.s.getText());
        else val = new UnknownVal(ctx.b.getText());
        setV(ctx, new AttrAssignment(id, val));
    }

    @Override public void exitDelta_clause(ABSParser.Delta_clauseContext ctx) {
        setV(ctx, new DeltaClause(v(ctx.deltaspec()),
                                  ctx.after_condition() == null ? new List()
                                  : v(ctx.after_condition()),
                                  o(ctx.from_condition()), o(ctx.when_condition())));
    }

    @Override public void exitDeltaspec(ABSParser.DeltaspecContext ctx) {
        setV(ctx, new Deltaspec(ctx.TYPE_IDENTIFIER().getText(), l(ctx.deltaspec_param())));
    }

    @Override public void exitBoolOrIDDeltaspecParam(ABSParser.BoolOrIDDeltaspecParamContext ctx) {
        String id = ctx.TYPE_IDENTIFIER().getText();
        setV(ctx, id.equals("True")
             ? new Const(new BoolVal(true))
             : (id.equals("False") ? new Const(new BoolVal(false)) : new FID(id)));
    }
    @Override public void exitIntDeltaspecParam(ABSParser.IntDeltaspecParamContext ctx) {
        setV(ctx, new Const(new IntVal(Integer.parseInt(ctx.INTLITERAL().getText()))));
    }
    @Override public void exitFIDAIDDeltaspecParam(ABSParser.FIDAIDDeltaspecParamContext ctx) {
        setV(ctx, new FIDAID(ctx.TYPE_IDENTIFIER().getText(), ctx.IDENTIFIER().getText()));
    }

    @Override public void exitAfter_condition(ABSParser.After_conditionContext ctx) {
        setV(ctx, l(ctx.delta_id()));
    }

    @Override public void exitDelta_id(ABSParser.Delta_idContext ctx) {
        setV(ctx, new DeltaID(ctx.TYPE_IDENTIFIER().getText()));
    }

    @Override public void exitFrom_condition(ABSParser.From_conditionContext ctx) {
        setV(ctx, v(ctx.application_condition()));
    }

    @Override public void exitWhen_condition(ABSParser.When_conditionContext ctx) {
        setV(ctx, v(ctx.application_condition()));
    }

    @Override public void exitNotApplicationCondition(ABSParser.NotApplicationConditionContext ctx) {
        setV(ctx, new AppCondNot(v(ctx.application_condition())));
    }

    @Override public void exitAndApplicationCondition(ABSParser.AndApplicationConditionContext ctx) {
        setV(ctx, new AppCondAnd(v(ctx.l), v(ctx.r)));
    }

    @Override public void exitOrApplicationCondition(ABSParser.OrApplicationConditionContext ctx) {
        setV(ctx, new AppCondOr(v(ctx.l), v(ctx.r)));
    }

    @Override public void exitParenApplicationCondition(ABSParser.ParenApplicationConditionContext ctx) {
        setV(ctx, v(ctx.application_condition()));
    }

    @Override public void exitFeatureApplicationCondition(ABSParser.FeatureApplicationConditionContext ctx) {
        setV(ctx, new AppCondFeature(((Feature)v(ctx.feature())).getName()));
    }

    // Products
    @Override public void exitProduct_decl(ABSParser.Product_declContext ctx) {
        if(ctx.product_expr() == null) {
            // old syntax: a product is declared as a set of features
            setV(ctx, new ProductDecl(ctx.TYPE_IDENTIFIER().getText(), new ProductFeatureSet(l(ctx.feature())), l(ctx.product_reconfiguration())));
        } else {
            // new syntax: using product expressions
            setV(ctx, new ProductDecl(ctx.TYPE_IDENTIFIER().getText(), v(ctx.product_expr()), l(ctx.product_reconfiguration())));
        }
    }

    @Override public void exitProduct_reconfiguration(ABSParser.Product_reconfigurationContext ctx) {
        setV(ctx, new Reconfiguration(ctx.product.getText(), l(ctx.delta_id()), ctx.update.getText()));
    }

    // Product Expression
    @Override public void exitProductFeatureSet(ABSParser.ProductFeatureSetContext ctx) {
        setV(ctx, new ProductFeatureSet(l(ctx.feature())));
    }

    @Override public void exitProductIntersect(ABSParser.ProductIntersectContext ctx) {
        setV(ctx, new ProductIntersect(v(ctx.l), v(ctx.r)));
    }

    @Override public void exitProductUnion(ABSParser.ProductUnionContext ctx) {
        setV(ctx, new ProductUnion(v(ctx.l), v(ctx.r)));
    }

    @Override public void exitProductDifference(ABSParser.ProductDifferenceContext ctx) {
        setV(ctx, new ProductDifference(v(ctx.l), v(ctx.r)));
    }

    @Override public void exitProductName(ABSParser.ProductNameContext ctx) {
        setV(ctx, new ProductName(ctx.TYPE_IDENTIFIER().getText()));
    }

    @Override public void exitProductParen(ABSParser.ProductParenContext ctx) {
        setV(ctx, v(ctx.product_expr()));
    }

    //  mTVL
	@Override public void exitFextension(ABSParser.FextensionContext ctx) {
        setV(ctx, new FExt(ctx.TYPE_IDENTIFIER().getText(),
                           o(ctx.feature_decl_group()),
                           new AttrConstraints(l(ctx.feature_decl_attribute()),
                                                      l(ctx.feature_decl_constraint()))));
    }

    @Override public void exitFeature_decl(ABSParser.Feature_declContext ctx) {
        setV(ctx, new FeatureDecl(ctx.TYPE_IDENTIFIER().getText(),
                                  o(ctx.feature_decl_group()),
                                  new AttrConstraints(l(ctx.feature_decl_attribute()),
                                                      l(ctx.feature_decl_constraint()))));
    }

    @Override public void exitFeature_decl_group(ABSParser.Feature_decl_groupContext ctx) {
        Cardinality c = null;
        if (ctx.o != null) c = new CRange(1,1);
        else if (ctx.a != null) c = new AllOf();
        else if (ctx.s != null) c = new Minim(Integer.parseInt(ctx.l.getText()));
        else c = new CRange(Integer.parseInt(ctx.l.getText()),
                            Integer.parseInt(ctx.u.getText()));
        setV(ctx, new Group(c, l(ctx.fnode())));
    }

    @Override public void exitFnode(ABSParser.FnodeContext ctx) {
        if (ctx.o == null) setV(ctx, new MandFeat(v(ctx.feature_decl())));
        else setV(ctx, new OptFeat(v(ctx.feature_decl())));
    }


	@Override public void exitFeatureDeclConstraintIfIn(ABSParser.FeatureDeclConstraintIfInContext ctx) {
        setV(ctx, new IfIn(v(ctx.mexp())));
    }
	@Override public void exitFeatureDeclConstraintIfOut(ABSParser.FeatureDeclConstraintIfOutContext ctx) {
        setV(ctx, new IfOut(v(ctx.mexp())));
    }
	@Override public void exitFeatureDeclConstraintExclude(ABSParser.FeatureDeclConstraintExcludeContext ctx) {
        setV(ctx, new Exclude(new FeatVar(ctx.TYPE_IDENTIFIER().getText())));
    }
	@Override public void exitFeatureDeclConstraintRequire(ABSParser.FeatureDeclConstraintRequireContext ctx) {
        setV(ctx, new Require(new FeatVar(ctx.TYPE_IDENTIFIER().getText())));
    }

	@Override public void exitMexp(ABSParser.MexpContext ctx) {
        if (ctx.op != null) {
            switch (ctx.op.getType()) {
            case ABSParser.OROR :
                setV(ctx, new MOrBoolExp(v(ctx.l), v(ctx.r)));
                return;
            case ABSParser.ANDAND :
                setV(ctx, new MAndBoolExp(v(ctx.l), v(ctx.r)));
                return;
            case ABSParser.IMPLIES :
                setV(ctx, new MImpliesExp(v(ctx.l), v(ctx.r)));
                return;
            case ABSParser.EQUIV :
                setV(ctx, new MEquivExp(v(ctx.l), v(ctx.r)));
                return;
            case ABSParser.EQEQ :
                setV(ctx, new MEqExp(v(ctx.l), v(ctx.r)));
                return;
            case ABSParser.NOTEQ :
                setV(ctx, new MNotEqExp(v(ctx.l), v(ctx.r)));
                return;
            case ABSParser.LT :
                setV(ctx, new MLTExp(v(ctx.l), v(ctx.r)));
                return;
            case ABSParser.GT :
                setV(ctx, new MGTExp(v(ctx.l), v(ctx.r)));
                return;
            case ABSParser.LTEQ :
                setV(ctx, new MLTEQExp(v(ctx.l), v(ctx.r)));
                return;
            case ABSParser.GTEQ :
                setV(ctx, new MGTEQExp(v(ctx.l), v(ctx.r)));
                return;
            case ABSParser.PLUS :
                setV(ctx, new MAddAddExp(v(ctx.l), v(ctx.r)));
                return;
            case ABSParser.MINUS :
                if (ctx.l != null) setV(ctx, new MSubAddExp(v(ctx.l), v(ctx.r)));
                else setV(ctx, new MMinusExp(v(ctx.a)));
                return;
            case ABSParser.MULT :
                setV(ctx, new MMultMultExp(v(ctx.l), v(ctx.r)));
                return;
            case ABSParser.DIV :
                setV(ctx, new MDivMultExp(v(ctx.l), v(ctx.r)));
                return;
            case ABSParser.MOD :
                setV(ctx, new MModMultExp(v(ctx.l), v(ctx.r)));
                return;
            case ABSParser.NEGATION :
                setV(ctx, new MNegExp(v(ctx.a)));
                return;
            }
        } else if (ctx.a != null)
            setV(ctx, v(ctx.a));
        else if (ctx.INTLITERAL() != null)
            setV(ctx, new MValue(new IntVal(Integer.parseInt(ctx.INTLITERAL().getText()))));
        else if (ctx.IDENTIFIER() != null) {
            if (ctx.TYPE_IDENTIFIER() != null)
                setV(ctx, new FAVar(ctx.TYPE_IDENTIFIER().getText(),
                                    ctx.IDENTIFIER().getText()));
            else
                setV(ctx, new AttVar(ctx.IDENTIFIER().getText()));
        } else {
            // TYPE_IDENTIFIER is not null
            String id = ctx.TYPE_IDENTIFIER().getText();
            if (id.equals("True")) setV(ctx, new MValue(new BoolVal(true)));
            else if (id.equals("False")) setV(ctx, new MValue(new BoolVal(false)));
            else setV(ctx, new FeatVar(id));
        }
    }


    @Override public void exitFeature_decl_attribute(ABSParser.Feature_decl_attributeContext ctx) {
        String t = ctx.TYPE_IDENTIFIER().getText();
        if (ctx.l != null) {
            setV(ctx, new Attribute(ctx.IDENTIFIER().getText(), new IntMType(t, v(ctx.l), v(ctx.u))));
        } else if (ctx.is != null && !ctx.is.isEmpty()) {
            setV(ctx, new Attribute(ctx.IDENTIFIER().getText(), new IntListMType(t, l(ctx.is))));
        } else if (t.equals("Int")) {
            setV(ctx, new Attribute(ctx.IDENTIFIER().getText(), new IntMType(t, new Limit(), new Limit())));
        } else if (t.equals("String")) {
            setV(ctx, new Attribute(ctx.IDENTIFIER().getText(), new StringMType(t)));
        } else if (t.equals("Bool")) {
            setV(ctx, new Attribute(ctx.IDENTIFIER().getText(), new BoolMType(t)));
        } else {
            setV(ctx, new Attribute(ctx.IDENTIFIER().getText(), new UnresolvedMType(t)));
        }
    }

	@Override public void exitBoundary_int(ABSParser.Boundary_intContext ctx) {
        if (ctx.star != null) setV(ctx, new Limit());
        else setV(ctx, v(ctx.boundary_val()));
    }
	@Override public void exitBoundary_val(ABSParser.Boundary_valContext ctx) {
        setV(ctx, new BoundaryVal((ctx.m == null ? +1 : -1) * Integer.parseInt(ctx.INTLITERAL().getText())));
    }

}

