/**
 * Copyright (c) 2014, Rudolf Schlatte. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.antlr.parser;

import abs.frontend.ast.*;

import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.*;

/**
 * This class creates the JastAdd AST from an Antlr parse tree.
 *
 * @author Rudi Schlatte
 */
public class CreateJastAddASTListener extends ABSBaseListener {

    String filename = "<unknown file>";

    /** maps antlr nodes to JastAdd nodes - see antlr book Sec.7.5 */
    ParseTreeProperty<ASTNode<?>> values = new ParseTreeProperty<ASTNode<?>>();
    UnresolvedTypeUse t = null;
    CompilationUnit result = null;

    public CreateJastAddASTListener(java.io.File filename) {
        if (filename != null) this.filename = filename.getPath();
    }

    /**
     * Associates JastAdd value with antlr node such that v(node) will
     * return value.  Also sets filename and position of the JastAdd
     * value.  Returns the passed-in JastAdd value.
     */
    private ASTNode<?> setV(ParserRuleContext node, ASTNode<?> value) {
        assert node != null;
        int startline = node.start.getLine();
        int startcol = node.start.getCharPositionInLine();
        // for a completely empty file, CompilationUnit.stop will be null
        int endline = (node.stop == null ? node.start : node.stop).getLine();
        int endcol = (node.stop == null ? node.start : node.stop).getCharPositionInLine();
        value.setPosition(beaver.Symbol.makePosition(startline, startcol),
                          beaver.Symbol.makePosition(endline, endcol));
        value.setFileName(this.filename);
        values.put(node, value);
        return value;
    }

    /**
     * Returns the AstNode for the given antlr parse node.  The result
     * is guaranteed to be non-null.
     */
    private ASTNode<?> v(ParseTree node) {
        ASTNode<?> result = values.get(node);
        if (result == null) throw new NullPointerException();
        return result;
    }

    /**
     * Returns a fresh Opt<ASTNode> filled with the result of v(node) if
     * node is non-null, empty otherwise.
     */
    @SuppressWarnings({ "rawtypes", "unchecked" })
    private Opt o(ParseTree node) {
        if (node == null) return new Opt();
        else return new Opt(v(node));
    }

    /**
     * Returns a list of ASTNodes given a list of antlr parse nodes.
     * The result list elements are found via 'v' and are guaranteed
     * non-null.
     */
    @SuppressWarnings({ "rawtypes", "unchecked" })
    private List l(java.util.List<? extends ParseTree> l) {
        List result = new List();
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

    @Override public void enterCompilation_unit(ABSParser.Compilation_unitContext ctx) {
        this.result = (CompilationUnit)setV(ctx, new CompilationUnit(this.filename,
new List<ModuleDecl>(),
                              new List<DeltaDecl>(),new List<UpdateDecl>(),
                              new Opt<ProductLine>(),new List<Product>(),
                              new List<FeatureDecl>(),new List<FExt>()));
    }

    @Override public void exitCompilation_unit(ABSParser.Compilation_unitContext ctx) {
        result.setModuleDeclList(l(ctx.module_decl()));
        result.setDeltaDeclList(l(ctx.delta_decl()));
        result.setProductLineOpt(o(ctx.productline_decl()));
        result.setProductList(l(ctx.product_decl()));
        result.setFeatureDeclList(l(ctx.feature_decl()));
        result.setFExtList(l(ctx.fextension()));
    }

    // Declarations
    @Override public void exitDecl(ABSParser.DeclContext ctx) {
        setV(ctx, v(ctx.getChild(0))); // relies on decl having one token
    }

    @Override public void exitModule_decl(ABSParser.Module_declContext ctx) {
        setV(ctx, new ModuleDecl(ctx.qualified_type_identifier().getText(), l(ctx.module_export()), l(ctx.module_import()), l(ctx.decl()), o(ctx.main_block())));
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
        ParametricDataTypeDecl d = (ParametricDataTypeDecl)setV(ctx, new ParametricDataTypeDecl(ctx.n.getText(), l(ctx.c), l(ctx.annotation()), new List<TypeParameterDecl>()));
        for (Token t : ctx.p) {
            d.addTypeParameter(new TypeParameterDecl(t.getText()));
        }
    }

    @Override public void exitData_constructor(ABSParser.Data_constructorContext ctx) {
        DataConstructor d
            = (DataConstructor)setV(ctx, new DataConstructor(ctx.n.getText(), new List<ConstructorArg>()));
        for (ABSParser.Data_constructor_argContext a : ctx.a) {
            final TypeUse vt = (TypeUse) v(a.type_use());
            final DataTypeUse vtresolved;
            if (vt instanceof DataTypeUse) {
                vtresolved = (DataTypeUse) vt;
            } else {
                // See below, we may be facing an UnresolvedTypeUse.
                assert vt instanceof UnresolvedTypeUse : vt.getClass().getName();
                vtresolved = new DataTypeUse(vt.getName(), vt.getAnnotations());
            }
            d.addConstructorArg(new ConstructorArg(vtresolved, a.IDENTIFIER() != null ? new Opt(new Name(a.IDENTIFIER().getText())) : new Opt()));
        }
    }

    @Override public void exitFunction_decl(ABSParser.Function_declContext ctx) {
        // TODO: datatypes do not distinguish between DataTypeDecl and
        // ParametricDataTypeDecl; it would be nice to do this for
        // functions as well.
        FunctionDef d = ctx.e == null ? new BuiltinFunctionDef() : new ExpFunctionDef((PureExp)v(ctx.e));
        List<ParamDecl> p = (List<ParamDecl>)v(ctx.paramlist());
        TypeUse t = (TypeUse)v(ctx.type_use());
        if (ctx.p != null && !ctx.p.isEmpty()) {
            ParametricFunctionDecl dp
                = (ParametricFunctionDecl)setV(ctx, new ParametricFunctionDecl(ctx.n.getText(), t, p, d, l(ctx.annotation()), new List<TypeParameterDecl>()));
            for (Token tp : ctx.p) {
                dp.addTypeParameter(new TypeParameterDecl(tp.getText()));
            }
        } else {
            setV(ctx, new FunctionDecl(ctx.n.getText(), l(ctx.annotation()), t, p, d));
        }
    }

    @Override public void exitTypesyn_decl(ABSParser.Typesyn_declContext ctx) {

        setV(ctx, new TypeSynDecl(ctx.TYPE_IDENTIFIER().getText(), l(ctx.annotation()), (TypeUse)v(ctx.type_use())));
    }

    @Override public void exitException_decl(ABSParser.Exception_declContext ctx) {
        ExceptionDecl e = (ExceptionDecl)setV(ctx, new ExceptionDecl(ctx.n.getText(), l(ctx.annotation()), new List<ConstructorArg>()));
        for (ABSParser.Data_constructor_argContext a : ctx.a) {
            e.addConstructorArg(new ConstructorArg((DataTypeUse)v(a.type_use()), a.IDENTIFIER() != null ? new Opt(new Name(a.IDENTIFIER().getText())) : new Opt()));
        }
    }

    @Override public void exitMain_block(ABSParser.Main_blockContext ctx) {
        setV(ctx, new MainBlock(l(ctx.annotation()), l(ctx.stmt())));
    }

    // Interfaces
    @Override public void exitInterface_decl(ABSParser.Interface_declContext ctx) {
        setV(ctx, new InterfaceDecl(ctx.qualified_type_identifier().getText(), l(ctx.annotation()), l(ctx.e), l(ctx.methodsig())));
    }

    @Override public void exitMethodsig(ABSParser.MethodsigContext ctx) {
        setV(ctx, new MethodSig(ctx.IDENTIFIER().getText(), l(ctx.annotation()), (Access)v(ctx.type_use()), (List<ParamDecl>)v(ctx.paramlist())));
    }

    // Classes
    @Override public void exitClass_decl(ABSParser.Class_declContext ctx) {
        ClassDecl c = (ClassDecl)setV(ctx, new ClassDecl(ctx.qualified_type_identifier().getText(), l(ctx.annotation()), new List<ParamDecl>(), l(ctx.interface_name()), new Opt<InitBlock>(), l(ctx.field_decl()), l(ctx.method())));
        if (ctx.paramlist() != null) {
            c.setParamList((List<ParamDecl>)v(ctx.paramlist()));
        }
        if (ctx.stmt() != null && !ctx.stmt().isEmpty()) {
            InitBlock b = new InitBlock(new List<Annotation>(), new List<Stmt>());
            for (ABSParser.StmtContext s : ctx.stmt()) {
                b.addStmt((Stmt)v(s));
            }
            c.setInitBlock(b);
        }
    }

    @Override public void exitField_decl(ABSParser.Field_declContext ctx) {
        // FIXME: 'port' missing (for component model)
        FieldDecl f = (FieldDecl)setV(ctx, new FieldDecl(ctx.IDENTIFIER().getText(), (Access)v(ctx.type_use()), o(ctx.pure_exp()), l(ctx.annotation()), false));
    }

    @Override public void exitMethod(ABSParser.MethodContext ctx) {
        MethodSig ms = new MethodSig(ctx.IDENTIFIER().getText(), l(ctx.annotation()), (Access)v(ctx.type_use()), (List<ParamDecl>)v(ctx.paramlist()));
        Block b = new Block(new List<Annotation>(), new List<Stmt>());
        for (ABSParser.StmtContext s : ctx.stmt()) {
            b.addStmt((Stmt)v(s));
        }
        // FIXME: 'critical' missing (for component model)
        setV(ctx, new MethodImpl(ms, b, false));
    }

    // Statements
    @Override public void exitVardeclStmt(ABSParser.VardeclStmtContext ctx) {
        VarDecl v = new VarDecl(ctx.IDENTIFIER().getText(), (Access)v(ctx.type_exp()), new Opt<Exp>());
        if (ctx.exp() != null) {
            v.setInitExp((Exp)v(ctx.exp()));
        }
        setV(ctx, new VarDeclStmt(l(ctx.annotation()), v));
    }

    @Override public void exitAssignStmt(ABSParser.AssignStmtContext ctx) {
        setV(ctx, new AssignStmt(l(ctx.annotation()), (VarOrFieldUse)v(ctx.var_or_field_ref()), (Exp)v(ctx.exp())));
    }
    @Override public void exitSkipStmt(ABSParser.SkipStmtContext ctx) {
        setV(ctx, new SkipStmt(l(ctx.annotation())));
    }
    @Override public void exitReturnStmt(ABSParser.ReturnStmtContext ctx) {
        setV(ctx, new ReturnStmt(l(ctx.annotation()), (Exp)v(ctx.exp())));
    }
    @Override public void exitAssertStmt(ABSParser.AssertStmtContext ctx) {
        setV(ctx, new AssertStmt(l(ctx.annotation()), (PureExp)v(ctx.exp())));
    }
    @Override public void exitBlockStmt(ABSParser.BlockStmtContext ctx) {
        setV(ctx, new Block(l(ctx.annotation()), l(ctx.stmt())));
    }
    @Override public void exitIfStmt(ABSParser.IfStmtContext ctx) {
        setV(ctx, new IfStmt(l(ctx.annotation()), (PureExp)v(ctx.c),
                             (Stmt)v(ctx.l), o(ctx.r)));
    }
    @Override public void exitWhileStmt(ABSParser.WhileStmtContext ctx) {
        setV(ctx, new WhileStmt(l(ctx.annotation()), (PureExp)v(ctx.c), (Stmt)v(ctx.stmt())));
    }
	@Override public void exitTryCatchFinallyStmt(ABSParser.TryCatchFinallyStmtContext ctx) {
        setV(ctx, new TryCatchFinallyStmt(l(ctx.annotation()), (Stmt)v(ctx.b),
                                          l(ctx.casestmtbranch()), o(ctx.f)));
    }
    @Override public void exitAwaitStmt(ABSParser.AwaitStmtContext ctx) {
        setV(ctx, new AwaitStmt(l(ctx.annotation()), (Guard)v(ctx.guard())));
    }
    @Override public void exitClaimGuard(ABSParser.ClaimGuardContext ctx) {
        setV(ctx, new ClaimGuard((VarOrFieldUse)v(ctx.var_or_field_ref())));
    }
    @Override public void exitDurationGuard(ABSParser.DurationGuardContext ctx) {
        setV(ctx, new DurationGuard((PureExp)v(ctx.min), (PureExp)v(ctx.max)));
    }
    @Override public void exitExpGuard(ABSParser.ExpGuardContext ctx) {
        setV(ctx, new ExpGuard((PureExp)v(ctx.e)));
    }
    @Override public void exitAndGuard(ABSParser.AndGuardContext ctx) {
        setV(ctx, new AndGuard((Guard)v(ctx.l), (Guard)v(ctx.r)));
    }
    @Override public void exitSuspendStmt(ABSParser.SuspendStmtContext ctx) {
        setV(ctx, new SuspendStmt(l(ctx.annotation())));
    }
    @Override public void exitDurationStmt(ABSParser.DurationStmtContext ctx) {
        setV(ctx, new DurationStmt(l(ctx.annotation()), (PureExp)v(ctx.f),
                                   (PureExp)v(ctx.t)));
    }
    @Override public void exitThrowStmt(ABSParser.ThrowStmtContext ctx) {
        setV(ctx, new ThrowStmt(l(ctx.annotation()), (PureExp)v(ctx.pure_exp())));
    }
    @Override public void exitDieStmt(ABSParser.DieStmtContext ctx) {
        setV(ctx, new DieStmt(l(ctx.annotation()), (PureExp)v(ctx.pure_exp())));
    }
    @Override public void exitMoveCogToStmt(ABSParser.MoveCogToStmtContext ctx) {
        setV(ctx, new MoveCogToStmt(l(ctx.annotation()), (PureExp)v(ctx.pure_exp())));
    }
    @Override public void exitExpStmt(ABSParser.ExpStmtContext ctx) {
        setV(ctx, new ExpressionStmt(l(ctx.annotation()), (Exp)v(ctx.exp())));
    }
    @Override public void exitCaseStmt(ABSParser.CaseStmtContext ctx) {
        setV(ctx, new CaseStmt(l(ctx.annotation()), (PureExp)v(ctx.c), l(ctx.casestmtbranch())));
    }
    @Override public void exitCasestmtbranch(ABSParser.CasestmtbranchContext ctx) {
        setV(ctx, new CaseBranchStmt((Pattern)v(ctx.pattern()), (Stmt)v(ctx.stmt())));
    }

    // Annotations
    @Override public void exitAnnotation(ABSParser.AnnotationContext ctx) {
        if (ctx.l == null) setV(ctx, new Annotation((PureExp)v(ctx.r)));
        else setV(ctx, new TypedAnnotation((PureExp)v(ctx.r), (Access)v(ctx.l)));
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
        setV(ctx, new GetExp((PureExp)v(ctx.pure_exp())));
    }
    @Override public void exitNewExp(ABSParser.NewExpContext ctx) {
        NewExp n = (NewExp)setV(ctx, new NewExp(ctx.c.getText(), (List<PureExp>)v(ctx.pure_exp_list()), new Opt<Local>()));
        if (ctx.l != null) { n.setLocal(new Local()); }
    }
    @Override public void exitAsyncCallExp(ABSParser.AsyncCallExpContext ctx) {
        if (ctx.a != null) {
            setV(ctx, new AwaitAsyncCall((PureExp)v(ctx.o), ctx.m.getText(), (List<PureExp>)v(ctx.pure_exp_list())));
        } else {
            setV(ctx, new AsyncCall((PureExp)v(ctx.o), ctx.m.getText(), (List<PureExp>)v(ctx.pure_exp_list())));
        }
    }
    @Override public void exitSyncCallExp(ABSParser.SyncCallExpContext ctx) {
        setV(ctx, new SyncCall((PureExp)v(ctx.o), ctx.m.getText(), (List<PureExp>)v(ctx.pure_exp_list())));
    }
    @Override public void exitOriginalCallExp(ABSParser.OriginalCallExpContext ctx) {
        List<PureExp> l = ctx.pure_exp_list() == null
            ? new List<PureExp>()
            : (List<PureExp>)v(ctx.pure_exp_list());
        if (ctx.c != null) {
            setV(ctx, new TargetedOriginalCall(new DeltaID("core"), l));
        } else if (ctx.d != null) {
            setV(ctx, new TargetedOriginalCall((DeltaID)v(ctx.d), l));
        } else {
            setV(ctx, new OriginalCall(l));
        }
    }

    // Pure expressions
    @Override public void exitFunctionExp(ABSParser.FunctionExpContext ctx) {
        List<PureExp> l = ctx.pure_exp_list() == null
            ? new List<PureExp>()
            : (List<PureExp>)v(ctx.pure_exp_list());
        setV(ctx, new FnApp(ctx.qualified_identifier().getText(), l));
    }
    @Override public void exitVariadicFunctionExp(ABSParser.VariadicFunctionExpContext ctx) {
        List<PureExp> l = (List<PureExp>)v(ctx.pure_exp_list());
        // Construct a list literal from all given arguments
        DataConstructorExp arglist
            = new DataConstructorExp("Cons", new List<PureExp>());
        DataConstructorExp current = arglist;
        /* DO NOT use the iterator here -- it interferes with rewriting [stolz] */
        if (l.getNumChildNoTransform() > 0) {
            PureExp last = (PureExp)l.getChildNoTransform(l.getNumChildNoTransform()-1);
            for (int i = 0; i < l.getNumChildNoTransform(); i++) {
                PureExp e = (PureExp) l.getChildNoTransform(i);
                DataConstructorExp next = new DataConstructorExp("Cons", new List<PureExp>());
                next.setPosition(e.getStartPos(), last.getEndPos());
                current.addParamNoTransform(e);            
                current.addParamNoTransform(next);
                current = next;
            }
        }
        current.setConstructor("Nil");
        List<PureExp> llist = new List<PureExp>();
        llist.add(arglist);
        setV(ctx, new FnApp(ctx.qualified_identifier().getText(), llist));
    }
    @Override public void exitConstructorExp(ABSParser.ConstructorExpContext ctx) {
        List<PureExp> l = ctx.pure_exp_list() == null
            ? new List<PureExp>()
            : (List<PureExp>)v(ctx.pure_exp_list());
        setV(ctx,
                 new DataConstructorExp(ctx.qualified_type_identifier().getText(),
                                        l));
    }
    @Override public void exitUnaryExp(ABSParser.UnaryExpContext ctx) {
        switch (ctx.op.getType()) {
        case ABSParser.NEGATION :
        case ABSParser.NEGATION_CREOL :
            setV(ctx, new NegExp((PureExp)v(ctx.pure_exp())));
            break;
        case ABSParser.MINUS :
            setV(ctx, new MinusExp((PureExp)v(ctx.pure_exp())));
            break;
        }
    }
    @Override public void exitMultExp(ABSParser.MultExpContext ctx) {
        switch (ctx.op.getType()) {
        case ABSParser.MULT :
            setV(ctx,
                     new MultMultExp((PureExp)v(ctx.l), (PureExp)v(ctx.r)));
            break;
        case ABSParser.DIV :
            setV(ctx, new DivMultExp((PureExp)v(ctx.l), (PureExp)v(ctx.r)));
            break;
        case ABSParser.MOD :
            setV(ctx, new ModMultExp((PureExp)v(ctx.l), (PureExp)v(ctx.r)));
            break;
        }
    }
    @Override public void exitAddExp(ABSParser.AddExpContext ctx) {
        switch (ctx.op.getType()) {
        case ABSParser.PLUS :
            setV(ctx, new AddAddExp((PureExp)v(ctx.l), (PureExp)v(ctx.r)));
            break;
        case ABSParser.MINUS :
            setV(ctx, new SubAddExp((PureExp)v(ctx.l), (PureExp)v(ctx.r)));
            break;
        }
    }
    @Override public void exitGreaterExp(ABSParser.GreaterExpContext ctx) {
        switch (ctx.op.getType()) {
        case ABSParser.LT :
            setV(ctx, new LTExp((PureExp)v(ctx.l), (PureExp)v(ctx.r)));
            break;
        case ABSParser.GT :
            setV(ctx, new GTExp((PureExp)v(ctx.l), (PureExp)v(ctx.r)));
            break;
        case ABSParser.LTEQ :
            setV(ctx, new LTEQExp((PureExp)v(ctx.l), (PureExp)v(ctx.r)));
            break;
        case ABSParser.GTEQ :
            setV(ctx, new GTEQExp((PureExp)v(ctx.l), (PureExp)v(ctx.r)));
            break;
        }
    }
    @Override public void exitEqualExp(ABSParser.EqualExpContext ctx) {
        switch (ctx.op.getType()) {
        case ABSParser.EQEQ :
            setV(ctx, new EqExp((PureExp)v(ctx.l), (PureExp)v(ctx.r)));
            break;
        case ABSParser.NOTEQ :
            setV(ctx, new NotEqExp((PureExp)v(ctx.l), (PureExp)v(ctx.r)));
            break;
        }
    }
    @Override public void exitAndExp(ABSParser.AndExpContext ctx) {
        setV(ctx, new AndBoolExp((PureExp)v(ctx.l), (PureExp)v(ctx.r)));
    }
    @Override public void exitOrExp(ABSParser.OrExpContext ctx) {
        setV(ctx, new OrBoolExp((PureExp)v(ctx.l), (PureExp)v(ctx.r)));
    }
    @Override public void exitVarOrFieldExp(ABSParser.VarOrFieldExpContext ctx) {
        setV(ctx, (VarOrFieldUse)v(ctx.var_or_field_ref()));
    }
    @Override public void exitIntExp(ABSParser.IntExpContext ctx) {
        setV(ctx, new IntLiteral(ctx.INTLITERAL().getText()));
    }
    @Override public void exitStringExp(ABSParser.StringExpContext ctx) {
        setV(ctx, makeStringLiteral(ctx.STRINGLITERAL().getText()));
    }
    @Override public void exitThisExp(ABSParser.ThisExpContext ctx) {
        setV(ctx, new ThisExp());
    }
    @Override public void exitNullExp(ABSParser.NullExpContext ctx) {
        setV(ctx, new NullExp());
    }
    @Override public void exitIfExp(ABSParser.IfExpContext ctx) {
        setV(ctx, new IfExp((PureExp)v(ctx.c),
                                (PureExp)v(ctx.l),
                                (PureExp)v(ctx.r)));
    }
    @Override public void exitCaseExp(ABSParser.CaseExpContext ctx) {
        List<CaseBranch> l = new List<CaseBranch>();
        for (ABSParser.CasebranchContext b : ctx.casebranch()) {
            l.add((CaseBranch)v(b));
        }
        setV(ctx, new CaseExp((PureExp)v(ctx.c), l));
    }
    @Override public void exitLetExp(ABSParser.LetExpContext ctx) {
        setV(ctx, new LetExp(new ParamDecl(ctx.IDENTIFIER().getText(),
                                           (TypeUse)v(ctx.type_use()),
                                           new List<Annotation>()),
                             (PureExp)v(ctx.i),
                             (PureExp)v(ctx.b)));
    }
    @Override public void exitParenExp(ABSParser.ParenExpContext ctx) {
        setV(ctx, (PureExp)v(ctx.pure_exp()));
    }

    @Override public void exitCasebranch(ABSParser.CasebranchContext ctx) {
        setV(ctx, new CaseBranch((Pattern)v(ctx.pattern()),
                                     (PureExp)v(ctx.pure_exp())));
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
        setV(ctx, new ParamDecl(ctx.IDENTIFIER().getText(), (Access)v(ctx.type_exp()), l(ctx.annotation())));
    }

    @Override public void exitInterface_name(ABSParser.Interface_nameContext ctx) {
        setV(ctx, new InterfaceTypeUse(ctx.qualified_type_identifier().getText(),
                                       new List()));
    }

    @Override public void exitPure_exp_list(ABSParser.Pure_exp_listContext ctx) {
        List<PureExp> l = (List<PureExp>)setV(ctx, new List<PureExp>());
        for (ABSParser.Pure_expContext a : ctx.pure_exp()) {
            l.add((PureExp)v(a));
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
            setV(ctx, new UnresolvedTypeUse(ctx.n.getText(), l(ctx.annotation())));
        } else {
            // parametric type use
            ParametricDataTypeUse p
                = (ParametricDataTypeUse)setV(ctx, new ParametricDataTypeUse(ctx.n.getText(), l(ctx.annotation()), new List<TypeUse>()));
            for (ABSParser.Type_useContext c : ctx.type_use()) {
                p.addParam((TypeUse)v(c));
            }
        }
    }

    @Override public void exitType_exp(ABSParser.Type_expContext ctx) {
        if (ctx.p.isEmpty()) {
            // normal type use
            setV(ctx, new UnresolvedTypeUse(ctx.n.getText(), new List<Annotation>()));
        } else {
            // parametric type use
            ParametricDataTypeUse p
                = (ParametricDataTypeUse)setV(ctx, new ParametricDataTypeUse(ctx.n.getText(), new List<Annotation>(), new List<TypeUse>()));
            for (ABSParser.Type_useContext c : ctx.type_use()) {
                p.addParam((TypeUse)v(c));
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
        setV(ctx, new DeltaFieldParam((ParamDecl)v(ctx.param_decl())));
    }

    @Override public void exitDeltaClassParam(ABSParser.DeltaClassParamContext ctx) {
        setV(ctx, new DeltaClassParam(ctx.qualified_type_identifier().getText(),
                                      (HasCondition)v(ctx.has_condition())));
    }

    @Override public void exitDeltaHasFieldCondition(ABSParser.DeltaHasFieldConditionContext ctx) {
        setV(ctx, new HasField((FieldDecl)v(ctx.f)));
    }

    @Override public void exitDeltaHasMethodCondition(ABSParser.DeltaHasMethodConditionContext ctx) {
        setV(ctx, new HasMethod((MethodSig)v(ctx.m)));
    }

    @Override public void exitDeltaHasInterfaceCondition(ABSParser.DeltaHasInterfaceConditionContext ctx) {
        setV(ctx, new HasInterface((InterfaceTypeUse)v(ctx.i)));
    }

    @Override public void exitDelta_access(ABSParser.Delta_accessContext ctx) {
        setV(ctx, new DeltaAccess(ctx.qualified_type_identifier().getText()));
    }

    @Override public void exitModule_modifier(ABSParser.Module_modifierContext ctx) {
        setV(ctx, v(ctx.getChild(0))); // relies on node having one token
    }

    @Override public void exitDeltaAddFunctionModifier(ABSParser.DeltaAddFunctionModifierContext ctx) {
        setV(ctx, new AddFunctionModifier((FunctionDecl)v(ctx.function_decl())));
    }

    @Override public void exitDeltaAddDataTypeModifier(ABSParser.DeltaAddDataTypeModifierContext ctx) {
        setV(ctx, new AddDataTypeModifier((DataTypeDecl)v(ctx.datatype_decl())));
    }
    
    @Override public void exitDeltaAddTypeSynModifier(ABSParser.DeltaAddTypeSynModifierContext ctx) {
        setV(ctx, new AddTypeSynModifier((TypeSynDecl)v(ctx.typesyn_decl())));
    }

    @Override public void exitDeltaModifyTypeSynModifier(ABSParser.DeltaModifyTypeSynModifierContext ctx) {
        setV(ctx, new ModifyTypeSynModifier((TypeSynDecl)v(ctx.typesyn_decl())));
    }

    @Override public void exitDeltaModifyDataTypeModifier(ABSParser.DeltaModifyDataTypeModifierContext ctx) {
        setV(ctx, new ModifyDataTypeModifier((DataTypeDecl)v(ctx.datatype_decl())));
    }

    @Override public void exitDeltaAddClassModifier(ABSParser.DeltaAddClassModifierContext ctx) {
        setV(ctx, new AddClassModifier((ClassDecl)v(ctx.class_decl())));
    }

    @Override public void exitDeltaRemoveClassModifier(ABSParser.DeltaRemoveClassModifierContext ctx) {
        setV(ctx, new RemoveClassModifier(ctx.qualified_type_identifier().getText()));
    }

    @Override public void exitDeltaModifyClassModifier(ABSParser.DeltaModifyClassModifierContext ctx) {
        setV(ctx, new ModifyClassModifier(ctx.n.getText(), l(ctx.ia), l(ctx.ir),
                                          l(ctx.class_modifier_fragment())));
    }

    @Override public void exitDeltaAddInterfaceModifier(ABSParser.DeltaAddInterfaceModifierContext ctx) {
        setV(ctx, new AddInterfaceModifier((InterfaceDecl)v(ctx.interface_decl())));
    }

    @Override public void exitDeltaRemoveInterfaceModifier(ABSParser.DeltaRemoveInterfaceModifierContext ctx) {
        setV(ctx, new RemoveInterfaceModifier(ctx.qualified_type_identifier().getText()));
    }
    
    @Override public void exitDeltaModifyInterfaceModifier(ABSParser.DeltaModifyInterfaceModifierContext ctx) {
        setV(ctx, new ModifyInterfaceModifier(ctx.qualified_type_identifier().getText(),
                                              l(ctx.interface_modifier_fragment())));
    }

    @Override public void exitDeltaAddFieldFragment(ABSParser.DeltaAddFieldFragmentContext ctx) {
        setV(ctx, new AddFieldModifier((FieldDecl)v(ctx.field_decl())));
    }
    @Override public void exitDeltaRemoveFieldFragment(ABSParser.DeltaRemoveFieldFragmentContext ctx) {
        setV(ctx, new RemoveFieldModifier((FieldDecl)v(ctx.field_decl())));
    }
    @Override public void exitDeltaAddMethodFragment(ABSParser.DeltaAddMethodFragmentContext ctx) {
        setV(ctx, new AddMethodModifier((MethodImpl)v(ctx.method())));
    }
    @Override public void exitDeltaModifyMethodFragment(ABSParser.DeltaModifyMethodFragmentContext ctx) {
        setV(ctx, new ModifyMethodModifier((MethodImpl)v(ctx.method())));
    }
    @Override public void exitDeltaRemoveMethodFragment(ABSParser.DeltaRemoveMethodFragmentContext ctx) {
        setV(ctx, new RemoveMethodModifier((MethodSig)v(ctx.methodsig())));
    }
    
    @Override public void exitDeltaAddMethodsigFragment(ABSParser.DeltaAddMethodsigFragmentContext ctx) {
        setV(ctx, new AddMethodSigModifier((MethodSig)v(ctx.methodsig())));
    }
    @Override public void exitDeltaRemoveMethodsigFragment(ABSParser.DeltaRemoveMethodsigFragmentContext ctx) {
        setV(ctx, new RemoveMethodSigModifier((MethodSig)v(ctx.methodsig())));
    }

    @Override public void exitDeltaAddModuleImportFragment(ABSParser.DeltaAddModuleImportFragmentContext ctx) {
        setV(ctx, new AddImportModifier((Import)v(ctx.module_import())));
    }
    @Override public void exitDeltaAddModuleExportFragment(ABSParser.DeltaAddModuleExportFragmentContext ctx) {
        setV(ctx, new AddExportModifier((Export)v(ctx.module_export())));
    }

    // Updates (?)
    @Override public void exitUpdateDecl(ABSParser.UpdateDeclContext ctx) {
        setV(ctx, new UpdateDecl(ctx.TYPE_IDENTIFIER().getText(), l(ctx.object_update())));
    }
    
    @Override public void exitObjectUpdateDecl(ABSParser.ObjectUpdateDeclContext ctx) {
        setV(ctx, new ObjectUpdate(ctx.qualified_type_identifier().getText(),
                                   new AwaitStmt(new List(), (Guard)v(ctx.guard())),
                                   new UpdatePreamble(l(ctx.update_preamble_decl())),
                                   l(ctx.pre), l(ctx.post)));
    }
    
    @Override public void exitObjectUpdateAssignStmt(ABSParser.ObjectUpdateAssignStmtContext ctx) {
        setV(ctx, new AssignStmt(new List(), (VarOrFieldUse)v(ctx.var_or_field_ref()), (Exp)v(ctx.exp())));
    }

    @Override public void exitUpdatePreambleDecl(ABSParser.UpdatePreambleDeclContext ctx) {
        setV(ctx, new VarDeclStmt(new List(), new VarDecl(ctx.IDENTIFIER().getText(), (Access)v(ctx.type_exp()), new Opt())));
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
        setV(ctx, new DeltaClause((Deltaspec)v(ctx.deltaspec()),
                                  ctx.after_condition() == null ? new List()
                                  : (List)v(ctx.after_condition()),
                                  o(ctx.from_condition()), o(ctx.when_condition())));
    }

    @Override public void exitDeltaspec(ABSParser.DeltaspecContext ctx) {
        setV(ctx, new Deltaspec(ctx.TYPE_IDENTIFIER().getText(),
                                l(ctx.deltaspec_param())));
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
        setV(ctx, (AppCond)v(ctx.application_condition()));
    }

    @Override public void exitWhen_condition(ABSParser.When_conditionContext ctx) {
        setV(ctx, (AppCond)v(ctx.application_condition()));
    }

    @Override public void exitNotApplicationCondition(ABSParser.NotApplicationConditionContext ctx) {
        setV(ctx, new AppCondNot((AppCond)v(ctx.application_condition())));
    }

    @Override public void exitAndApplicationCondition(ABSParser.AndApplicationConditionContext ctx) {
        setV(ctx, new AppCondAnd((AppCond)v(ctx.l), (AppCond)v(ctx.r)));
    }

    @Override public void exitOrApplicationCondition(ABSParser.OrApplicationConditionContext ctx) {
        setV(ctx, new AppCondOr((AppCond)v(ctx.l), (AppCond)v(ctx.r)));
    }

    @Override public void exitParenApplicationCondition(ABSParser.ParenApplicationConditionContext ctx) {
        setV(ctx, (AppCond)v(ctx.application_condition()));
    }

    @Override public void exitFeatureApplicationCondition(ABSParser.FeatureApplicationConditionContext ctx) {
        setV(ctx, new AppCondFeature(((Feature)v(ctx.feature())).getName()));
    }

    // Products

    @Override public void exitProduct_decl(ABSParser.Product_declContext ctx) {
        setV(ctx, new Product(ctx.TYPE_IDENTIFIER().getText(),
                              l(ctx.feature()), l(ctx.product_reconfiguration())));
    }

    @Override public void exitProduct_reconfiguration(ABSParser.Product_reconfigurationContext ctx) {
        setV(ctx, new Reconfiguration(ctx.product.getText(),
                                      l(ctx.delta_id()), ctx.update.getText()));
    }

    // mTVL
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
        if (ctx.o == null) setV(ctx, new MandFeat((FeatureDecl)v(ctx.feature_decl())));
        else setV(ctx, new OptFeat((FeatureDecl)v(ctx.feature_decl())));
    }


	@Override public void exitFeatureDeclConstraintIfIn(ABSParser.FeatureDeclConstraintIfInContext ctx) {
        setV(ctx, new IfIn((MExp)v(ctx.mexp())));
    }
	@Override public void exitFeatureDeclConstraintIfOut(ABSParser.FeatureDeclConstraintIfOutContext ctx) {
        setV(ctx, new IfOut((MExp)v(ctx.mexp())));
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
                setV(ctx, new MOrBoolExp((MExp)v(ctx.l), (MExp)v(ctx.r)));
                return;
            case ABSParser.ANDAND :
                setV(ctx, new MAndBoolExp((MExp)v(ctx.l), (MExp)v(ctx.r)));
                return;
            case ABSParser.IMPLIES :
                setV(ctx, new MImpliesExp((MExp)v(ctx.l), (MExp)v(ctx.r)));
                return;
            case ABSParser.EQUIV :
                setV(ctx, new MEquivExp((MExp)v(ctx.l), (MExp)v(ctx.r)));
                return;
            case ABSParser.EQEQ :
                setV(ctx, new MEqExp((MExp)v(ctx.l), (MExp)v(ctx.r)));
                return;
            case ABSParser.NOTEQ :
                setV(ctx, new MNotEqExp((MExp)v(ctx.l), (MExp)v(ctx.r)));
                return;
            case ABSParser.LT :
                setV(ctx, new MLTExp((MExp)v(ctx.l), (MExp)v(ctx.r)));
                return;
            case ABSParser.GT :
                setV(ctx, new MGTExp((MExp)v(ctx.l), (MExp)v(ctx.r)));
                return;
            case ABSParser.LTEQ :
                setV(ctx, new MLTEQExp((MExp)v(ctx.l), (MExp)v(ctx.r)));
                return;
            case ABSParser.GTEQ :
                setV(ctx, new MGTEQExp((MExp)v(ctx.l), (MExp)v(ctx.r)));
                return;
            case ABSParser.PLUS :
                setV(ctx, new MAddAddExp((MExp)v(ctx.l), (MExp)v(ctx.r)));
                return;
            case ABSParser.MINUS :
                if (ctx.l != null) setV(ctx, new MSubAddExp((MExp)v(ctx.l), (MExp)v(ctx.r)));
                else setV(ctx, new MMinusExp((MExp)v(ctx.a)));
                return;
            case ABSParser.MULT :
                setV(ctx, new MMultMultExp((MExp)v(ctx.l), (MExp)v(ctx.r)));
                return;
            case ABSParser.DIV :
                setV(ctx, new MDivMultExp((MExp)v(ctx.l), (MExp)v(ctx.r)));
                return;
            case ABSParser.MOD :
                setV(ctx, new MModMultExp((MExp)v(ctx.l), (MExp)v(ctx.r)));
                return;
            case ABSParser.NEGATION :
                setV(ctx, new MNegExp((MExp)v(ctx.a)));
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
            setV(ctx, new Attribute(ctx.IDENTIFIER().getText(),
                                    new IntMType(t, (BoundaryInt)v(ctx.l),
                                                 (BoundaryInt)v(ctx.u))));
        } else if (ctx.is != null && !ctx.is.isEmpty()) {
            setV(ctx, new Attribute(ctx.IDENTIFIER().getText(),
                                    new IntListMType(t, l(ctx.is))));
        } else if (t.equals("Int")) {
            setV(ctx, new Attribute(ctx.IDENTIFIER().getText(),
                                    new IntMType(t, new Limit(), new Limit())));
        } else if (t.equals("String")) {
            setV(ctx, new Attribute(ctx.IDENTIFIER().getText(),
                                    new StringMType(t)));
        } else if (t.equals("Bool")) {
            setV(ctx, new Attribute(ctx.IDENTIFIER().getText(),
                                    new BoolMType(t)));
        } else {
            setV(ctx, new Attribute(ctx.IDENTIFIER().getText(),
                                    new UnresolvedMType(t)));
        }
    }

	@Override public void exitBoundary_int(ABSParser.Boundary_intContext ctx) {
        if (ctx.star != null) setV(ctx, new Limit());
        else setV(ctx, (BoundaryVal)v(ctx.boundary_val()));
    }
	@Override public void exitBoundary_val(ABSParser.Boundary_valContext ctx) {
        setV(ctx, new BoundaryVal((ctx.m == null
                                   ? +1 : -1)
                                  * Integer.parseInt(ctx.INTLITERAL().getText())));
    }
    
}

