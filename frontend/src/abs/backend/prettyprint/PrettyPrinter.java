/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.prettyprint;

import java.io.PrintWriter;

import abs.frontend.ast.*;
import abs.frontend.parser.Main;
import abs.frontend.tests.ABSFormatter;

public class PrettyPrinter {

    public static void main(String... args) throws Exception {
        Main main = new Main();
        Model m = main.parse(args);
        PrintWriter writer = new PrintWriter(System.out,true);
        ABSFormatter formatter = new DefaultABSFormatter();
        formatter.setPrintWriter(writer);
        for (ModuleDecl d : m.getModuleDecls()) {
            if (!d.getName().equals("ABS.StdLib") && !d.getName().equals("ABS.Scheduler")
                    && !d.getName().equals("ABS.FLI") && !d.getName().equals("ABS.DC")) {
                prettyPrint(d, writer, formatter);
            }
        }
        System.exit(0);
    }

    //Code below this line is to be removed!
    
    public static void prettyPrint(ModuleDecl m, PrintWriter stream, ABSFormatter formatter) {
        stream.println("module " + m.getName() + ";");
        formatter.afterStmt();

        for (Export e : m.getExportList()) {
            prettyPrint(e, stream, formatter);
            stream.println();
            formatter.afterStmt();
        }

        for (Import i : m.getImportList()) {
            prettyPrint(i, stream, formatter);
            stream.println();
            formatter.afterStmt();
        }

        for (Decl decl : m.getDecls()) {
            prettyPrint(decl, stream, formatter);
            stream.println();
            stream.println();
            formatter.afterStmt();
        }

        if (m.hasProductLine()) {
            prettyPrint(m.getProductLine(), stream, formatter);
            stream.println();
            formatter.afterStmt();
        }

        for (Product p : m.getProductList()) {
            prettyPrint(p, stream, formatter);
            stream.println();
            formatter.afterStmt();
        }

        if (m.hasBlock()) {
            prettyPrint(m.getBlock(), stream, formatter);
        }
        
        stream.flush();
    }

    public static void prettyPrint(Product p, PrintWriter stream, ABSFormatter formatter) {
        stream.print("product ");
        stream.print(p.getName());
        stream.print("(");
        addFeatures(p.getFeatureList(), stream, formatter);
        stream.print(");");
    }

    public static void prettyPrint(Feature child, PrintWriter stream, ABSFormatter formatter) {
        stream.print(child.getName());
        if (child.getNumAttrAssignment() > 0) {
            stream.print("{");
            addAttrAssignments(child.getAttrAssignmentList(), stream, formatter);
            stream.print("}");
        }
    }

    public static void prettyPrint(AttrAssignment child, PrintWriter stream, ABSFormatter formatter) {
        stream.print(child.getName());
        stream.print("=");
        Value v = child.getValue();
        if (v instanceof IntVal) {
            stream.print(((IntVal) v).getValue());
        } else if (v instanceof BoolVal) {
            stream.print(((BoolVal) v).getValue() ? "True" : "False");
        }
    }

    public static void prettyPrint(ProductLine productLine, PrintWriter stream, ABSFormatter formatter) {
        stream.print("productline ");
        stream.print(productLine.getName());
        formatter.beforeOpenBrace();
        stream.print(" {");
        formatter.afterOpenBrace();
        if (productLine.getNumCoreFeature() > 0) {
            addFeatures(productLine.getCoreFeatureList(), stream, formatter);
            if (productLine.getNumOptionalFeature() > 0) {
                stream.print(", ");
            }
        }
        addFeatures(productLine.getOptionalFeatureList(), stream, formatter);
        stream.println(";");
        formatter.afterStmt();

        for (DeltaClause dc : productLine.getDeltaClauseList()) {
            prettyPrint(dc, stream, formatter);
            stream.println(";");
            formatter.afterStmt();
        }

        formatter.beforeCloseBrace();
        stream.print("}");
        formatter.afterCloseBrace();
    }

    public static void prettyPrint(DeltaClause dc, PrintWriter stream, ABSFormatter formatter) {
        stream.print("delta ");

        prettyPrint(dc.getDeltaspec(), stream, formatter);

        stream.print(" after ");
        addDeltaID(dc.getDeltaIDList(), stream, formatter);

        stream.print(" when ");
        addFeatures(dc.getFeatureList(), stream, formatter);
    }

    public static void prettyPrint(Deltaspec deltaspec, PrintWriter stream, ABSFormatter formatter) {
        stream.print(deltaspec.getName());
        if (deltaspec.getNumDeltaparam() > 0) {
            stream.print("(");
            addDeltaParam(deltaspec.getDeltaparamList(), stream, formatter);
            stream.print(")");
        }
    }

    public static void prettyPrint(Deltaparam child, PrintWriter stream, ABSFormatter formatter) {
        stream.print(child.getName());
    }

    public static void prettyPrint(DeltaID child, PrintWriter stream, ABSFormatter formatter) {
        stream.print(child.getName());
    }

    public static void prettyPrint(Stmt value, PrintWriter stream, ABSFormatter formatter) {
        if (value instanceof AssertStmt) {
            prettyPrint((AssertStmt) value, stream, formatter);
        } else if (value instanceof AssignStmt) {
            prettyPrint((AssignStmt) value, stream, formatter);
        } else if (value instanceof AwaitStmt) {
            prettyPrint((AwaitStmt) value, stream, formatter);
        } else if (value instanceof Block) {
            prettyPrint((Block) value, stream, formatter);
        } else if (value instanceof DurationStmt) {
            prettyPrint((DurationStmt) value, stream, formatter);
        } else if (value instanceof ExpressionStmt) {
            prettyPrint((ExpressionStmt) value, stream, formatter);
        } else if (value instanceof IfStmt) {
            prettyPrint((IfStmt) value, stream, formatter);
        } else if (value instanceof InitStmt) {
            prettyPrint((InitStmt) value, stream, formatter);
        } else if (value instanceof ReturnStmt) {
            prettyPrint((ReturnStmt) value, stream, formatter);
        } else if (value instanceof SkipStmt) {
            prettyPrint((SkipStmt) value, stream, formatter);
        } else if (value instanceof SuspendStmt) {
            prettyPrint((SuspendStmt) value, stream, formatter);
        } else if (value instanceof VarDeclStmt) {
            prettyPrint((VarDeclStmt) value, stream, formatter);
        } else if (value instanceof WhileStmt) {
            prettyPrint((WhileStmt) value, stream, formatter);
        }
    }

    public static void prettyPrint(Block value, PrintWriter stream, ABSFormatter formatter) {
        insertAnnotationln(value.getAnnotationList(), stream, formatter);
        formatter.beforeOpenBrace();
        stream.println("{");
        formatter.afterOpenBrace();
        formatter.afterStmt();
        for (Stmt s : value.getStmtList()) {
            prettyPrint(s, stream, formatter);
            stream.println();
            formatter.afterStmt();
        }
        formatter.beforeCloseBrace();
        stream.print("}");
        formatter.afterCloseBrace();
    }

    public static void prettyPrint(AssertStmt value, PrintWriter stream, ABSFormatter formatter) {
        insertAnnotationln(value.getAnnotationList(), stream, formatter);
        stream.print("assert ");
        prettyPrint(value.getCondition(), stream, formatter);
        stream.print(";");
    }

    public static void prettyPrint(AssignStmt value, PrintWriter stream, ABSFormatter formatter) {
        insertAnnotationln(value.getAnnotationList(), stream, formatter);
        prettyPrint(value.getVar(), stream, formatter);
        stream.print(" = ");
        prettyPrint(value.getValue(), stream, formatter);
        stream.print(";");
    }

    public static void prettyPrint(Exp value, PrintWriter stream, ABSFormatter formatter) {
        if (value instanceof EffExp) {
            prettyPrint((EffExp) value, stream, formatter);
        } else if (value instanceof PureExp) {
            prettyPrint((PureExp) value, stream, formatter);
        }
    }

    public static void prettyPrint(EffExp value, PrintWriter stream, ABSFormatter formatter) {
        if (value instanceof Call) {
            prettyPrint((Call) value, stream, formatter);
        } else if (value instanceof GetExp) {
            prettyPrint((GetExp) value, stream, formatter);
        } else if (value instanceof NewExp) {
            prettyPrint((NewExp) value, stream, formatter);
        } else if (value instanceof OriginalCall) {
            prettyPrint((OriginalCall) value, stream, formatter);
        }
    }

    public static void prettyPrint(Call value, PrintWriter stream, ABSFormatter formatter) {
        prettyPrint(value.getCallee(), stream, formatter);
        if (value instanceof AsyncCall) {
            prettyPrint((AsyncCall) value, stream, formatter);
        } else if (value instanceof SyncCall) {
            prettyPrint((SyncCall) value, stream, formatter);
        }
        stream.print(value.getMethod());
        stream.print("(");
        if (value.getNumParam() > 0) {
            prettyPrint(value.getParam(0), stream, formatter);
            for (int i = 1; i < value.getNumParam(); i++) {
                stream.print(", ");
                prettyPrint(value.getParam(i), stream, formatter);
            }
        }
        stream.print(")");
    }

    public static void prettyPrint(AsyncCall value, PrintWriter stream, ABSFormatter formatter) {
        stream.print("!");
    }

    public static void prettyPrint(SyncCall value, PrintWriter stream, ABSFormatter formatter) {
        stream.print(".");
    }

    public static void prettyPrint(GetExp value, PrintWriter stream, ABSFormatter formatter) {
        prettyPrint(value.getPureExp(), stream, formatter);
        stream.print(".get;");
    }

    public static void prettyPrint(NewExp value, PrintWriter stream, ABSFormatter formatter) {
        stream.print("new ");
        if (value.hasCog()) {
            stream.print("cog ");
        }
        stream.print(value.getClassName());
        stream.print("(");
        addPureExp(value.getParamList(), stream, formatter);
        stream.print(")");
    }

    public static void prettyPrint(OriginalCall value, PrintWriter stream, ABSFormatter formatter) {
        stream.print("original ");
        stream.print("(");
        addPureExp(value.getParamList(), stream, formatter);
        stream.print(")");
    }

    public static void prettyPrint(AwaitStmt value, PrintWriter stream, ABSFormatter formatter) {
        insertAnnotationln(value.getAnnotationList(), stream, formatter);
        stream.print("await ");
        prettyPrint(value.getGuard(), stream, formatter);
        stream.print(";");
    }

    public static void prettyPrint(Guard guard, PrintWriter stream, ABSFormatter formatter) {
        if (guard instanceof AndGuard) {
            prettyPrint((AndGuard) guard, stream, formatter);
        } else if (guard instanceof ClaimGuard) {
            prettyPrint((ClaimGuard) guard, stream, formatter);
        } else if (guard instanceof DurationGuard) {
            prettyPrint((DurationGuard) guard, stream, formatter);
        } else if (guard instanceof ExpGuard) {
            prettyPrint((ExpGuard) guard, stream, formatter);
        }
    }

    public static void prettyPrint(AndGuard guard, PrintWriter stream, ABSFormatter formatter) {
        prettyPrint(guard.getLeft(), stream, formatter);
        stream.print(" & ");
        prettyPrint(guard.getRight(), stream, formatter);
    }

    public static void prettyPrint(ClaimGuard guard, PrintWriter stream, ABSFormatter formatter) {
        prettyPrint(guard.getVar(), stream, formatter);
        stream.print("?");
    }

    public static void prettyPrint(DurationGuard guard, PrintWriter stream, ABSFormatter formatter) {
        stream.print("duration(");
        prettyPrint(guard.getMin(), stream, formatter);
        stream.print(", ");
        prettyPrint(guard.getMax(), stream, formatter);
        stream.print(")");
    }

    public static void prettyPrint(ExpGuard guard, PrintWriter stream, ABSFormatter formatter) {
        prettyPrint(guard.getPureExp(), stream, formatter);
    }

    public static void prettyPrint(DurationStmt value, PrintWriter stream, ABSFormatter formatter) {
        insertAnnotationln(value.getAnnotationList(), stream, formatter);
        stream.print("duration(");
        prettyPrint(value.getMin(), stream, formatter);
        stream.print(", ");
        prettyPrint(value.getMax(), stream, formatter);
        stream.print(")");
        stream.print(";");
    }

    public static void prettyPrint(ExpressionStmt value, PrintWriter stream, ABSFormatter formatter) {
        insertAnnotationln(value.getAnnotationList(), stream, formatter);
        prettyPrint(value.getExp(), stream, formatter);
        stream.print(";");
    }

    public static void prettyPrint(IfStmt value, PrintWriter stream, ABSFormatter formatter) {
        stream.print("if ");
        prettyPrint(value.getCondition(), stream, formatter);
        prettyPrint(value.getThen(), stream, formatter);
        if (value.hasElse()) {
            stream.print(" else ");
            prettyPrint(value.getElse(), stream, formatter);
        }
    }

    public static void prettyPrint(InitStmt value, PrintWriter stream, ABSFormatter formatter) {
        // Nothing for now
    }

    public static void prettyPrint(ReturnStmt value, PrintWriter stream, ABSFormatter formatter) {
        stream.print("return ");
        prettyPrint(value.getRetExp(), stream, formatter);
        stream.print(";");
    }

    public static void prettyPrint(SkipStmt value, PrintWriter stream, ABSFormatter formatter) {
        stream.print("skip;");
    }

    public static void prettyPrint(SuspendStmt value, PrintWriter stream, ABSFormatter formatter) {
        stream.print("suspend;");
    }

    public static void prettyPrint(VarDeclStmt value, PrintWriter stream, ABSFormatter formatter) {
        insertAnnotationln(value.getAnnotationList(), stream, formatter);
        prettyPrint(value.getVarDecl(), stream, formatter);
        stream.print(";");
    }

    public static void prettyPrint(VarDecl value, PrintWriter stream, ABSFormatter formatter) {
        prettyPrint(value.getAccess(), stream, formatter);
        stream.print(" ");
        stream.print(value.getName());
        if (value.hasInitExp()) {
            stream.print(" = ");
            prettyPrint(value.getInitExp(), stream, formatter);
        }
    }

    public static void prettyPrint(WhileStmt value, PrintWriter stream, ABSFormatter formatter) {
        stream.print("while ");
        prettyPrint(value.getCondition(), stream, formatter);
        prettyPrint(value.getBody(), stream, formatter);
    }

    public static void prettyPrint(Export ip, PrintWriter stream, ABSFormatter formatter) {
        if (ip instanceof FromExport) {
            prettyPrint((FromExport) ip, stream, formatter);
        } else if (ip instanceof NamedExport) {
            prettyPrint((NamedExport) ip, stream, formatter);
        } else if (ip instanceof StarExport) {
            prettyPrint((StarExport) ip, stream, formatter);
        }
    }

    public static void prettyPrint(NamedExport ip, PrintWriter stream, ABSFormatter formatter) {
        stream.print("export ");
        Name n = ip.getName(0);
        stream.print(n.getName());
        for (int i = 1; i < ip.getNumName(); i++) {
            n = ip.getName(i);
            stream.print(", " + n.getName());
        }
        stream.print(";");
    }

    public static void prettyPrint(StarExport ip, PrintWriter stream, ABSFormatter formatter) {
        stream.print("export *");
        if (ip.hasModuleName()) {
            stream.print(" from ");
            stream.print(ip.getModuleName());
        }
        stream.print(";");
    }

    public static void prettyPrint(FromExport ip, PrintWriter stream, ABSFormatter formatter) {
        stream.print("export ");
        Name n = ip.getName(0);
        stream.print(n.getName());
        for (int i = 1; i < ip.getNumName(); i++) {
            n = ip.getName(i);
            stream.print(", " + n.getName());
        }
        stream.print("from ");
        stream.print(ip.getModuleName());
        stream.print(";");
    }

    public static void prettyPrint(Import ip, PrintWriter stream, ABSFormatter formatter) {
        if (ip instanceof FromImport) {
            prettyPrint((FromImport) ip, stream, formatter);
        } else if (ip instanceof NamedImport) {
            prettyPrint((NamedImport) ip, stream, formatter);
        } else if (ip instanceof StarImport) {
            prettyPrint((StarImport) ip, stream, formatter);
        }
    }

    public static void prettyPrint(NamedImport ip, PrintWriter stream, ABSFormatter formatter) {
        stream.print("import ");
        Name n = ip.getName(0);
        stream.print(n.getName());
        for (int i = 1; i < ip.getNumName(); i++) {
            n = ip.getName(i);
            stream.print(", " + n.getName());
        }
        stream.print(";");

    }

    public static void prettyPrint(StarImport ip, PrintWriter stream, ABSFormatter formatter) {
        stream.print("import * from ");
        stream.print(ip.getModuleName());
        stream.print(";");
    }

    public static void prettyPrint(FromImport ip, PrintWriter stream, ABSFormatter formatter) {
        stream.print("import ");
        Name n = ip.getName(0);
        stream.print(n.getName());
        for (int i = 1; i < ip.getNumName(); i++) {
            n = ip.getName(i);
            stream.print(", " + n.getName());
        }
        stream.print("from ");
        stream.print(ip.getModuleName());
        stream.print(";");
    }

    public static void prettyPrint(Decl node, PrintWriter stream, ABSFormatter formatter) {
        if (node instanceof BehaviorDecl) {
            prettyPrint((BehaviorDecl) node, stream, formatter);
        } else if (node instanceof DeltaDecl) {
            prettyPrint((DeltaDecl) node, stream, formatter);
        } else if (node instanceof TypeDecl) {
            prettyPrint((TypeDecl) node, stream, formatter);
        }
    }

    public static void prettyPrint(BehaviorDecl node, PrintWriter stream, ABSFormatter formatter) {
        if (node instanceof DataConstructor) {
            prettyPrint((DataConstructor) node, stream, formatter);
        } else if (node instanceof ClassDecl) {
            prettyPrint((ClassDecl) node, stream, formatter);
        } else if (node instanceof FunctionDecl) {
            prettyPrint((FunctionDecl) node, stream, formatter);
        }
    }

    public static void prettyPrint(FunctionDecl fun, PrintWriter stream, ABSFormatter formatter) {
        insertAnnotationln(fun.getAnnotationList(), stream, formatter);
        stream.print("def ");
        prettyPrint(fun.getTypeUse(), stream, formatter);
        stream.print(" ");
        stream.print(fun.getName());
        if (fun instanceof ParametricFunctionDecl) {
            if (((ParametricFunctionDecl) fun).getNumTypeParameter() > 0) {
                stream.print("<");
                addTypeParamDecl(((ParametricFunctionDecl) fun).getTypeParameterList(), stream, formatter);
                stream.print(">");
            }
        }
        stream.print("(");
        addParams(fun.getParamList(), stream, formatter);
        stream.print(") = ");
        if (fun.getFunctionDef() instanceof BuiltinFunctionDef) {
            stream.print("builtin;");
        } else {
            stream.println();
            formatter.afterOpenBrace();
            formatter.afterStmt();
            prettyPrint((ExpFunctionDef) fun.getFunctionDef(), stream, formatter);
            stream.print(";");
            formatter.beforeCloseBrace();
        }
        formatter.afterStmt();
    }

    public static void prettyPrint(ExpFunctionDef fun, PrintWriter stream, ABSFormatter formatter) {
        prettyPrint(fun.getRhs(), stream, formatter);
    }

    public static void prettyPrint(DeltaDecl delta, PrintWriter stream, ABSFormatter formatter) {
        insertAnnotationln(delta.getAnnotationList(), stream, formatter);
        stream.print("delta ");
        stream.print(delta.getName());
        stream.print("(");
        addDeltaParams(delta.getParamList(), stream, formatter);
        stream.print(")");
        formatter.beforeOpenBrace();
        stream.println("{");
        formatter.afterOpenBrace();

        for (ClassOrIfaceModifier m : delta.getClassOrIfaceModifierList()) {
            if (m instanceof AddClassModifier) {
                prettyPrint((AddClassModifier) m, stream, formatter);
            } else if (m instanceof AddInterfaceModifier) {
                prettyPrint((AddInterfaceModifier) m, stream, formatter);
            } else if (m instanceof ModifyClassModifier) {
                prettyPrint((ModifyClassModifier) m, stream, formatter);
            } else if (m instanceof RemoveClassModifier) {
                prettyPrint((RemoveClassModifier) m, stream, formatter);
            }
            stream.println();
            formatter.afterStmt();
        }

        formatter.beforeCloseBrace();
        stream.println("}");
        formatter.afterCloseBrace();
        formatter.afterStmt();
    }

    public static void prettyPrint(RemoveClassModifier m, PrintWriter stream, ABSFormatter formatter) {
        stream.print("removes class ");
        stream.print(m.getName());
        stream.print(";");
    }

    public static void prettyPrint(ModifyClassModifier m, PrintWriter stream, ABSFormatter formatter) {
        stream.print("modifies class ");
        stream.print(m.getName());

        if (m.getNumImplementedInterfaceUse() > 0) {
            stream.print(" adds ");
            addInterfaceTypeUses(m.getImplementedInterfaceUseList(), stream, formatter);
        }

        formatter.beforeOpenBrace();
        stream.println("{");
        formatter.afterOpenBrace();

        for (Modifier d : m.getModifierList()) {
            if (d instanceof AddFieldModifier) {
                prettyPrint((AddFieldModifier) d, stream, formatter);
            } else if (d instanceof AddMethodModifier) {
                prettyPrint((AddMethodModifier) d, stream, formatter);
            } else if (d instanceof ModifyMethodModifier) {
                prettyPrint((ModifyMethodModifier) d, stream, formatter);
            } else if (d instanceof RemoveFieldModifier) {
                prettyPrint((RemoveFieldModifier) d, stream, formatter);
            } else if (d instanceof RemoveMethodModifier) {
                prettyPrint((RemoveMethodModifier) d, stream, formatter);
            }
            stream.println();
            formatter.afterStmt();
        }

        formatter.beforeCloseBrace();
        stream.print("}");
        formatter.afterCloseBrace();
    }

    public static void prettyPrint(AddFieldModifier d, PrintWriter stream, ABSFormatter formatter) {
        stream.print("adds ");
        prettyPrint(d.getFieldDecl(), stream, formatter);
    }

    public static void prettyPrint(AddMethodModifier d, PrintWriter stream, ABSFormatter formatter) {
        stream.print("adds ");
        prettyPrint(d.getMethodImpl(), stream, formatter);
    }

    public static void prettyPrint(ModifyMethodModifier d, PrintWriter stream, ABSFormatter formatter) {
        stream.print("modifies ");
        prettyPrint(d.getMethodImpl(), stream, formatter);
    }

    public static void prettyPrint(RemoveFieldModifier d, PrintWriter stream, ABSFormatter formatter) {
        stream.print("removes ");
        prettyPrint(d.getFieldDecl(), stream, formatter);
    }

    public static void prettyPrint(RemoveMethodModifier d, PrintWriter stream, ABSFormatter formatter) {
        stream.print("removes ");
        prettyPrint(d.getMethodSig(), stream, formatter);
        stream.print(";");
    }

    public static void prettyPrint(AddInterfaceModifier m, PrintWriter stream, ABSFormatter formatter) {
        stream.print("adds ");
        prettyPrint(m.getInterfaceDecl(), stream, formatter);
    }

    public static void prettyPrint(AddClassModifier m, PrintWriter stream, ABSFormatter formatter) {
        stream.print("adds ");
        prettyPrint(m.getClassDecl(), stream, formatter);
    }

    public static void prettyPrint(DeltaParamDecl child, PrintWriter stream, ABSFormatter formatter) {
        if (child instanceof DeltaClassParam) {
            prettyPrint((DeltaClassParam) child, stream, formatter);
        } else if (child instanceof DeltaFieldParam) {
            prettyPrint((DeltaFieldParam) child, stream, formatter);
        }
    }

    public static void prettyPrint(DeltaClassParam child, PrintWriter stream, ABSFormatter formatter) {
        stream.print(child.getName());
        stream.print(" ");
        HasCondition cond = child.getHasCondition();
        if (cond instanceof HasField) {
            stream.print("hasField");
            prettyPrint(((HasField) cond).getFieldDecl(), stream, formatter);
        } else if (cond instanceof HasInterface) {
            stream.print("hasInterface");
            prettyPrint(((HasInterface) cond).getInterfaceTypeUse(), stream, formatter);
        } else if (cond instanceof HasMethod) {
            stream.print("hasMethod");
            prettyPrint(((HasMethod) cond).getMethodSig(), stream, formatter);
        }
    }

    public static void prettyPrint(DeltaFieldParam child, PrintWriter stream, ABSFormatter formatter) {
        prettyPrint(child.getParamDecl(), stream, formatter);
    }

    public static void prettyPrint(TypeDecl node, PrintWriter stream, ABSFormatter formatter) {
        if (node instanceof DataTypeDecl) {
            prettyPrint((DataTypeDecl) node, stream, formatter);
        } else if (node instanceof InterfaceDecl) {
            prettyPrint((InterfaceDecl) node, stream, formatter);
        } else if (node instanceof TypeParameterDecl) {
            prettyPrint((TypeParameterDecl) node, stream, formatter);
        } else if (node instanceof TypeSynDecl) {
            prettyPrint((TypeSynDecl) node, stream, formatter);
        }
    }

    public static void prettyPrint(TypeSynDecl data, PrintWriter stream, ABSFormatter formatter) {
        insertAnnotationln(data.getAnnotationList(), stream, formatter);
        stream.print("type ");
        stream.print(data.getName());
        stream.print(" = ");
        prettyPrint(data.getValue(), stream, formatter);
        stream.print(";");
    }

    public static void prettyPrint(InterfaceDecl data, PrintWriter stream, ABSFormatter formatter) {
        insertAnnotationln(data.getAnnotationList(), stream, formatter);
        stream.print("interface ");
        stream.print(data.getName());
        if (data.getNumExtendedInterfaceUse() > 0) {
            stream.print(" extends ");
            addInterfaceTypeUses(data.getExtendedInterfaceUseList(), stream, formatter);
        }
        formatter.beforeOpenBrace();
        stream.println(" {");
        formatter.afterOpenBrace();
        formatter.afterStmt();
        
        for (MethodSig m : data.getAllMethodSigs()) {
            prettyPrint(m, stream, formatter);
            stream.println(";");
            formatter.afterStmt();
        }

        formatter.beforeCloseBrace();
        stream.print("}");
        formatter.afterCloseBrace();
    }

    public static void prettyPrint(MethodSig methodSig, PrintWriter stream, ABSFormatter formatter) {
        prettyPrint(methodSig.getReturnType(), stream, formatter);
        stream.print(" ");
        stream.print(methodSig.getName());
        stream.print("(");
        addParams(methodSig.getParams(), stream, formatter);
        stream.print(")");
    }

    public static void prettyPrint(ClassDecl data, PrintWriter stream, ABSFormatter formatter) {
        insertAnnotationln(data.getAnnotationList(), stream, formatter);
        stream.print("class ");
        stream.print(data.getName());

        if (data.getNumParam() > 0) {
            stream.print("(");
            addParams(data.getParams(), stream, formatter);
            stream.print(")");
        }

        if (data.getNumImplementedInterfaceUse() > 0) {
            stream.print(" implements ");
            addInterfaceTypeUses(data.getImplementedInterfaceUseList(), stream, formatter);
        }
        formatter.beforeOpenBrace();
        stream.println(" {");
        formatter.afterOpenBrace();
        formatter.afterStmt();

        for (FieldDecl m : data.getFieldList()) {
            prettyPrint(m, stream, formatter);
            stream.println();
            formatter.afterStmt();
        }

        stream.println();
        formatter.afterStmt();

        if (data.hasInitBlock()) {
            prettyPrint(data.getInitBlock(), stream, formatter);
            stream.println();
            formatter.afterStmt();
        }

        stream.println();
        formatter.afterStmt();
        
        for (MethodImpl m : data.getMethodList()) {
            prettyPrint(m, stream, formatter);
            stream.println();
            stream.println();
            formatter.afterStmt();
        }

        formatter.beforeCloseBrace();
        stream.print("}");
        formatter.afterCloseBrace();
    }

    public static void prettyPrint(FieldDecl fieldDecl, PrintWriter stream, ABSFormatter formatter) {
        insertAnnotationln(fieldDecl.getAnnotationList(), stream, formatter);
        prettyPrint(fieldDecl.getAccess(), stream, formatter);
        stream.print(" ");
        stream.print(fieldDecl.getName());
        if (fieldDecl.hasInitExp()) {
            stream.print(" = ");
            prettyPrint(fieldDecl.getInitExp(), stream, formatter);
        }
        stream.print(";");
    }

    public static void prettyPrint(MethodImpl methodImpl, PrintWriter stream, ABSFormatter formatter) {
        prettyPrint(methodImpl.getMethodSig(), stream, formatter);
        prettyPrint(methodImpl.getBlock(), stream, formatter);
    }

    public static void prettyPrint(DataTypeDecl data, PrintWriter stream, ABSFormatter formatter) {
        stream.print("data ");
        stream.print(data.getName());
        if (data instanceof ParametricDataTypeDecl) {
            if (((ParametricDataTypeDecl) data).getNumTypeParameter() > 0) {
                stream.print("<");
                addTypeParamDecl(((ParametricDataTypeDecl) data).getTypeParameterList(), stream, formatter);
                stream.print(">");
            }
        }
        if (data.getNumDataConstructor() > 0) {
            stream.print(" = ");
            addDataConstructors(data.getDataConstructorList(), stream, formatter);
        }
        stream.print(";");
    }

    public static void prettyPrint(DataConstructor dc, PrintWriter stream, ABSFormatter formatter) {
        stream.print(dc.getName());
        if (dc.getNumConstructorArg() > 0) {
            stream.print("(");
            prettyPrint(dc.getConstructorArg(0), stream, formatter);
            for (int i = 1; i < dc.getNumConstructorArg(); i++) {
                stream.print(", ");
                prettyPrint(dc.getConstructorArg(i), stream, formatter);
            }
            stream.print(")");
        }
    }

    public static void prettyPrint(ConstructorArg c, PrintWriter stream, ABSFormatter formatter) {
        prettyPrint(c.getDataTypeUse(), stream, formatter); // constructor
        if (c.hasSelectorName()) {
            stream.print(c.getSelectorName());
        }
    }

    public static void prettyPrint(DataTypeUse dataTypeUse, PrintWriter stream, ABSFormatter formatter) {
        insertAnnotation(dataTypeUse.getAnnotationList(), stream, formatter);
        stream.print(dataTypeUse.getName());
        if (dataTypeUse instanceof ParametricDataTypeUse) {
            if (((ParametricDataTypeUse) dataTypeUse).getNumParam() > 0) {
                stream.print("<");
                addDataTypeUse(((ParametricDataTypeUse) dataTypeUse).getParamList(), stream, formatter);
                stream.print(">");
            }
        }
    }

    public static void prettyPrint(TypedAnnotation annotation, PrintWriter stream, ABSFormatter formatter) {
        stream.print("[");
        prettyPrint(annotation.getAccess(), stream, formatter);
        stream.print(" : ");
        prettyPrint(annotation.getValue(), stream, formatter);
        stream.print("]");
    }

    public static void prettyPrint(Annotation annotation, PrintWriter stream, ABSFormatter formatter) {
        if (annotation instanceof TypedAnnotation) {
            prettyPrint((TypedAnnotation) annotation, stream, formatter);
        } else {
            stream.print("[");
            prettyPrint(annotation.getValue(), stream, formatter);
            stream.print("]");
        }
    }

    public static void prettyPrint(PureExp value, PrintWriter stream, ABSFormatter formatter) {
        if (value instanceof Access) {
            prettyPrint((Access) value, stream, formatter);
        } else if (value instanceof Binary) {
            prettyPrint((Binary) value, stream, formatter);
        } else if (value instanceof CaseExp) {
            prettyPrint((CaseExp) value, stream, formatter);
        } else if (value instanceof DataConstructorExp) {
            prettyPrint((DataConstructorExp) value, stream, formatter);
        } else if (value instanceof FnApp) {
            prettyPrint((FnApp) value, stream, formatter);
        } else if (value instanceof IfExp) {
            prettyPrint((IfExp) value, stream, formatter);
        } else if (value instanceof LetExp) {
            prettyPrint((LetExp) value, stream, formatter);
        } else if (value instanceof LiteralExp) {
            prettyPrint((LiteralExp) value, stream, formatter);
        } else if (value instanceof NullExp) {
            prettyPrint((NullExp) value, stream, formatter);
        } else if (value instanceof ThisExp) {
            prettyPrint((ThisExp) value, stream, formatter);
        } else if (value instanceof Unary) {
            prettyPrint((Unary) value, stream, formatter);
        }
    }

    public static void prettyPrint(Access t, PrintWriter stream, ABSFormatter formatter) {
        if (t instanceof IdUse) {
            prettyPrint((IdUse) t, stream, formatter);
        } else if (t instanceof VarOrFieldUse) {
            prettyPrint((VarOrFieldUse) t, stream, formatter);
        }
    }

    public static void prettyPrint(IdUse t, PrintWriter stream, ABSFormatter formatter) {
        if (t instanceof TypeUse) {
            prettyPrint((TypeUse) t, stream, formatter);
        }
    }

    public static void prettyPrint(VarOrFieldUse t, PrintWriter stream, ABSFormatter formatter) {
        if (t instanceof FieldUse) {
            prettyPrint((FieldUse) t, stream, formatter);
        } else if (t instanceof VarUse) {
            prettyPrint((VarUse) t, stream, formatter);
        }
    }

    public static void prettyPrint(TypeUse t, PrintWriter stream, ABSFormatter formatter) {
        if (t instanceof DataTypeUse) {
            prettyPrint((DataTypeUse) t, stream, formatter);
        } else if (t instanceof TypeParameterUse) {
            prettyPrint((TypeParameterUse) t, stream, formatter);
        } else if (t instanceof InterfaceTypeUse) {
            prettyPrint((InterfaceTypeUse) t, stream, formatter);
        }
    }

    // XXX not sure what this one does
    public static void prettyPrint(TypeParameterUse t, PrintWriter stream, ABSFormatter formatter) {
        stream.print(t.getName());
    }

    // XXX not sure what this one does
    public static void prettyPrint(InterfaceTypeUse t, PrintWriter stream, ABSFormatter formatter) {
        stream.print(t.getName());
    }

    // XXX not sure what this one does
    public static void prettyPrint(FieldUse t, PrintWriter stream, ABSFormatter formatter) {
        stream.print(t.getName());
    }

    // XXX not sure what this one does
    public static void prettyPrint(VarUse t, PrintWriter stream, ABSFormatter formatter) {
        stream.print(t.getName());
    }

    public static void prettyPrint(Binary t, PrintWriter stream, ABSFormatter formatter) {
        formatter.beforeOpenBrace();
        stream.print("( ");
        formatter.afterOpenBrace();
        prettyPrint(t.getLeft(), stream, formatter);

        if (t instanceof ArithmeticExp) {
            prettyPrint((ArithmeticExp) t, stream, formatter);
        } else if (t instanceof BoolExp) {
            prettyPrint((BoolExp) t, stream, formatter);
        } else if (t instanceof RelationalExpr) {
            prettyPrint((RelationalExpr) t, stream, formatter);
        }

        prettyPrint(t.getRight(), stream, formatter);
        formatter.beforeCloseBrace();
        stream.print(" )");
        formatter.afterCloseBrace();
    }

    public static void prettyPrint(ArithmeticExp t, PrintWriter stream, ABSFormatter formatter) {
        if (t instanceof AddExp) {
            prettyPrint((AddExp) t, stream, formatter);
        } else if (t instanceof MultExp) {
            prettyPrint((MultExp) t, stream, formatter);
        }
    }

    public static void prettyPrint(AddExp t, PrintWriter stream, ABSFormatter formatter) {
        if (t instanceof AddAddExp) {
            prettyPrint((AddAddExp) t, stream, formatter);
        } else if (t instanceof SubAddExp) {
            prettyPrint((SubAddExp) t, stream, formatter);
        }
    }

    public static void prettyPrint(MultExp t, PrintWriter stream, ABSFormatter formatter) {
        if (t instanceof DivMultExp) {
            prettyPrint((DivMultExp) t, stream, formatter);
        } else if (t instanceof ModMultExp) {
            prettyPrint((ModMultExp) t, stream, formatter);
        } else if (t instanceof MultMultExp) {
            prettyPrint((MultMultExp) t, stream, formatter);
        }
    }

    public static void prettyPrint(AddAddExp t, PrintWriter stream, ABSFormatter formatter) {
        stream.print(" + ");
    }

    public static void prettyPrint(SubAddExp t, PrintWriter stream, ABSFormatter formatter) {
        stream.print(" - ");
    }

    public static void prettyPrint(DivMultExp t, PrintWriter stream, ABSFormatter formatter) {
        stream.print(" / ");
    }

    public static void prettyPrint(ModMultExp t, PrintWriter stream, ABSFormatter formatter) {
        stream.print(" % ");
    }

    public static void prettyPrint(MultMultExp t, PrintWriter stream, ABSFormatter formatter) {
        stream.print(" * ");
    }

    public static void prettyPrint(BoolExp t, PrintWriter stream, ABSFormatter formatter) {
        if (t instanceof AndBoolExp) {
            prettyPrint((AndBoolExp) t, stream, formatter);
        } else if (t instanceof OrBoolExp) {
            prettyPrint((OrBoolExp) t, stream, formatter);
        }
    }

    public static void prettyPrint(AndBoolExp t, PrintWriter stream, ABSFormatter formatter) {
        stream.print(" && ");
    }

    public static void prettyPrint(OrBoolExp t, PrintWriter stream, ABSFormatter formatter) {
        stream.print(" || ");
    }

    public static void prettyPrint(RelationalExpr t, PrintWriter stream, ABSFormatter formatter) {
        if (t instanceof EqualityExpr) {
            prettyPrint((EqualityExpr) t, stream, formatter);
        } else if (t instanceof GTEQExp) {
            prettyPrint((GTEQExp) t, stream, formatter);
        } else if (t instanceof GTExp) {
            prettyPrint((GTExp) t, stream, formatter);
        } else if (t instanceof LTEQExp) {
            prettyPrint((LTEQExp) t, stream, formatter);
        } else if (t instanceof LTExp) {
            prettyPrint((LTExp) t, stream, formatter);
        }
    }

    public static void prettyPrint(EqualityExpr t, PrintWriter stream, ABSFormatter formatter) {
        if (t instanceof EqExp) {
            prettyPrint((EqExp) t, stream, formatter);
        } else if (t instanceof NotEqExp) {
            prettyPrint((NotEqExp) t, stream, formatter);
        }
    }

    public static void prettyPrint(EqExp t, PrintWriter stream, ABSFormatter formatter) {
        stream.print(" == ");
    }

    public static void prettyPrint(NotEqExp t, PrintWriter stream, ABSFormatter formatter) {
        stream.print(" != ");
    }

    public static void prettyPrint(GTEQExp t, PrintWriter stream, ABSFormatter formatter) {
        stream.print(" >= ");
    }

    public static void prettyPrint(GTExp t, PrintWriter stream, ABSFormatter formatter) {
        stream.print(" > ");
    }

    public static void prettyPrint(LTEQExp t, PrintWriter stream, ABSFormatter formatter) {
        stream.print(" <= ");
    }

    public static void prettyPrint(LTExp t, PrintWriter stream, ABSFormatter formatter) {
        stream.print(" < ");
    }

    public static void prettyPrint(CaseExp t, PrintWriter stream, ABSFormatter formatter) {
        stream.print("case ");
        prettyPrint(t.getExpr(), stream, formatter);
        formatter.beforeOpenBrace();
        stream.println(" {");
        formatter.afterOpenBrace();
        for (CaseBranch b : t.getBranchList()) {
            formatter.afterStmt();
            prettyPrint(b.getLeft(), stream, formatter);
            stream.print(" => ");
            prettyPrint(b.getRight(), stream, formatter);
            stream.println(";");
        }
        formatter.beforeCloseBrace();
        formatter.afterStmt();
        stream.print("}");
        formatter.afterCloseBrace();
    }

    public static void prettyPrint(Pattern t, PrintWriter stream, ABSFormatter formatter) {
        if (t instanceof ConstructorPattern) {
            prettyPrint((ConstructorPattern) t, stream, formatter);
        } else if (t instanceof LiteralPattern) {
            prettyPrint((LiteralPattern) t, stream, formatter);
        } else if (t instanceof PatternVar) {
            prettyPrint((PatternVar) t, stream, formatter);
        } else if (t instanceof PatternVarUse) {
            prettyPrint((PatternVarUse) t, stream, formatter);
        } else if (t instanceof UnderscorePattern) {
            prettyPrint((UnderscorePattern) t, stream, formatter);
        }
    }

    public static void prettyPrint(ConstructorPattern t, PrintWriter stream, ABSFormatter formatter) {
        stream.print(t.getConstructor());
        if (t.getNumParam() > 0) {
            stream.print("( ");
            prettyPrint(t.getParam(0), stream, formatter);
            for (int i = 1; i < t.getNumParam(); i++) {
                stream.print(", ");
                prettyPrint(t.getParam(i), stream, formatter);
            }
            stream.print(" )");
        }
    }

    public static void prettyPrint(LiteralPattern t, PrintWriter stream, ABSFormatter formatter) {
        prettyPrint(t.getLiteral(), stream, formatter);
    }

    public static void prettyPrint(PatternVar t, PrintWriter stream, ABSFormatter formatter) {
        prettyPrint(t.getVar(), stream, formatter);
    }

    // XXX not sure what this one does
    public static void prettyPrint(PatternVarDecl var, PrintWriter stream, ABSFormatter formatter) {
        stream.print(var.getName());
    }

    // XXX not sure what this one does
    public static void prettyPrint(PatternVarUse t, PrintWriter stream, ABSFormatter formatter) {
        stream.print(t.getName());
    }

    public static void prettyPrint(UnderscorePattern t, PrintWriter stream, ABSFormatter formatter) {
        stream.print("_");
    }

    public static void prettyPrint(DataConstructorExp t, PrintWriter stream, ABSFormatter formatter) {
        stream.print(t.getConstructor());
        if (t.getNumParam() > 0) {
            stream.print("( ");
            addPureExp(t.getParamList(), stream, formatter);
            stream.print(" )");
        }
    }

    public static void prettyPrint(FnApp t, PrintWriter stream, ABSFormatter formatter) {
        stream.print(t.getName());
        stream.print("(");
        addPureExp(t.getParamList(), stream, formatter);
        stream.print(")");
    }

    public static void prettyPrint(IfExp t, PrintWriter stream, ABSFormatter formatter) {
        stream.print("if ");
        prettyPrint(t.getCondExp(), stream, formatter);
        formatter.beforeOpenBrace();
        stream.println("{");
        formatter.afterOpenBrace();
        prettyPrint(t.getThenExp(), stream, formatter);
        formatter.beforeCloseBrace();
        stream.print("}");
        formatter.afterCloseBrace();
        stream.print(" else ");
        formatter.beforeOpenBrace();
        stream.println("{");
        formatter.afterOpenBrace();
        prettyPrint(t.getElseExp(), stream, formatter);
        formatter.beforeCloseBrace();
        stream.print("}");
        formatter.afterCloseBrace();
    }

    public static void prettyPrint(LetExp t, PrintWriter stream, ABSFormatter formatter) {
        stream.print("let ( ");
        prettyPrint(t.getVar(), stream, formatter);
        stream.print(" ) = ");
        prettyPrint(t.getVal(), stream, formatter);
        stream.print(" in ");
        prettyPrint(t.getExp(), stream, formatter);
    }

    public static void prettyPrint(ParamDecl var, PrintWriter stream, ABSFormatter formatter) {
        insertAnnotation(var.getAnnotationList(), stream, formatter);
        prettyPrint(var.getAccess(), stream, formatter);
        stream.print(" ");
        stream.print(var.getName());
    }

    public static void prettyPrint(LiteralExp t, PrintWriter stream, ABSFormatter formatter) {
        if (t instanceof IntLiteral) {
            prettyPrint((IntLiteral) t, stream, formatter);
        } else if (t instanceof StringLiteral) {
            prettyPrint((StringLiteral) t, stream, formatter);
        }
    }

    public static void prettyPrint(IntLiteral t, PrintWriter stream, ABSFormatter formatter) {
        stream.print(t.getContent());
    }

    public static void prettyPrint(StringLiteral t, PrintWriter stream, ABSFormatter formatter) {
        stream.print("\"");
        stream.print(t.getContent());
        stream.print("\"");
    }

    public static void prettyPrint(NullExp t, PrintWriter stream, ABSFormatter formatter) {
        stream.print("null");
    }

    public static void prettyPrint(ThisExp t, PrintWriter stream, ABSFormatter formatter) {
        stream.print("this");
    }

    public static void prettyPrint(Unary t, PrintWriter stream, ABSFormatter formatter) {
        if (t instanceof MinusExp) {
            prettyPrint((MinusExp) t, stream, formatter);
        } else if (t instanceof NegExp) {
            prettyPrint((NegExp) t, stream, formatter);
        }
    }

    public static void prettyPrint(MinusExp t, PrintWriter stream, ABSFormatter formatter) {
        stream.print("- ");
        prettyPrint(t.getOperand(), stream, formatter);
    }

    public static void prettyPrint(NegExp t, PrintWriter stream, ABSFormatter formatter) {
        stream.print("~ ");
        prettyPrint(t.getOperand(), stream, formatter);
    }

    public static void insertAnnotationln(List<Annotation> value, PrintWriter stream, ABSFormatter formatter) {
        if (value.getNumChild() > 0) {
            insertAnnotation(value, stream, formatter);
            stream.println();
            formatter.afterStmt();
        }
    }

    public static void insertAnnotation(List<Annotation> value, PrintWriter stream, ABSFormatter formatter) {
        if (value.getNumChild() > 0) {
            prettyPrint(value.getChild(0), stream, formatter);
            for (int i = 1; i < value.getNumChild(); i++) {
                stream.print(" ");
                prettyPrint(value.getChild(i), stream, formatter);
            }
            stream.print(" ");
        }
    }

    public static void addDataConstructors(List<DataConstructor> ps, PrintWriter stream, ABSFormatter formatter) {
        if (ps.getNumChild() > 0) {
            prettyPrint(ps.getChild(0), stream, formatter);
            for (int i = 1; i < ps.getNumChild(); i++) {
                stream.print(" | ");
                prettyPrint(ps.getChild(i), stream, formatter);
            }
        }
    }

    public static void addParams(List<ParamDecl> ps, PrintWriter stream, ABSFormatter formatter) {
        if (ps.getNumChild() > 0) {
            prettyPrint(ps.getChild(0), stream, formatter);
            for (int i = 1; i < ps.getNumChild(); i++) {
                stream.print(", ");
                prettyPrint(ps.getChild(i), stream, formatter);
            }
        }
    }

    public static void addPureExp(List<PureExp> ps, PrintWriter stream, ABSFormatter formatter) {
        if (ps.getNumChild() > 0) {
            prettyPrint(ps.getChild(0), stream, formatter);
            for (int i = 1; i < ps.getNumChild(); i++) {
                stream.print(", ");
                prettyPrint(ps.getChild(i), stream, formatter);
            }
        }
    }

    public static void addTypeParamDecl(List<TypeParameterDecl> ps, PrintWriter stream, ABSFormatter formatter) {
        if (ps.getNumChild() > 0) {
            prettyPrint(ps.getChild(0), stream, formatter);
            for (int i = 1; i < ps.getNumChild(); i++) {
                stream.print(", ");
                prettyPrint(ps.getChild(i), stream, formatter);
            }
        }
    }

    public static void addDataTypeUse(List<DataTypeUse> ps, PrintWriter stream, ABSFormatter formatter) {
        if (ps.getNumChild() > 0) {
            prettyPrint(ps.getChild(0), stream, formatter);
            for (int i = 1; i < ps.getNumChild(); i++) {
                stream.print(", ");
                prettyPrint(ps.getChild(i), stream, formatter);
            }
        }
    }

    public static void addDeltaParams(List<DeltaParamDecl> ps, PrintWriter stream, ABSFormatter formatter) {
        if (ps.getNumChild() > 0) {
            prettyPrint(ps.getChild(0), stream, formatter);
            for (int i = 1; i < ps.getNumChild(); i++) {
                stream.print(", ");
                prettyPrint(ps.getChild(i), stream, formatter);
            }
        }
    }

    public static void addInterfaceTypeUses(List<InterfaceTypeUse> ps, PrintWriter stream, ABSFormatter formatter) {
        if (ps.getNumChild() > 0) {
            prettyPrint(ps.getChild(0), stream, formatter);
            for (int i = 1; i < ps.getNumChild(); i++) {
                stream.print(", ");
                prettyPrint(ps.getChild(i), stream, formatter);
            }
        }
    }

    public static void addFeatures(List<Feature> ps, PrintWriter stream, ABSFormatter formatter) {
        if (ps.getNumChild() > 0) {
            prettyPrint(ps.getChild(0), stream, formatter);
            for (int i = 1; i < ps.getNumChild(); i++) {
                stream.print(", ");
                prettyPrint(ps.getChild(i), stream, formatter);
            }
        }
    }

    public static void addDeltaID(List<DeltaID> ps, PrintWriter stream, ABSFormatter formatter) {
        if (ps.getNumChild() > 0) {
            prettyPrint(ps.getChild(0), stream, formatter);
            for (int i = 1; i < ps.getNumChild(); i++) {
                stream.print(", ");
                prettyPrint(ps.getChild(i), stream, formatter);
            }
        }
    }

    public static void addDeltaParam(List<Deltaparam> deltaparamList, PrintWriter stream, ABSFormatter formatter) {
        if (deltaparamList.getNumChild() > 0) {
            prettyPrint(deltaparamList.getChild(0), stream, formatter);
            for (int i = 1; i < deltaparamList.getNumChild(); i++) {
                stream.print(", ");
                prettyPrint(deltaparamList.getChild(i), stream, formatter);
            }
        }
    }

    public static void addAttrAssignments(List<AttrAssignment> attrAssignmentList, PrintWriter stream,
            ABSFormatter formatter) {
        if (attrAssignmentList.getNumChild() > 0) {
            prettyPrint(attrAssignmentList.getChild(0), stream, formatter);
            for (int i = 1; i < attrAssignmentList.getNumChild(); i++) {
                stream.print(", ");
                prettyPrint(attrAssignmentList.getChild(i), stream, formatter);
            }
        }

    }

}
