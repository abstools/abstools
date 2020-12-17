package org.abs_models.backend.rvsdg.builder;

import org.abs_models.backend.rvsdg.abs.*;
import org.abs_models.backend.rvsdg.abs.Variable;
import org.abs_models.backend.rvsdg.core.*;
import org.abs_models.common.NotImplementedYetException;
import org.abs_models.frontend.ast.*;

import java.util.ArrayList;
import java.util.List;

/**
 * Builder is the main class for building the IR based on the AST.
 */
public class Builder {
    final Region region;
    final Scope scope;
    final Model model;

    public Builder(Region region, Scope scope, Model model) {
        this.region = region;
        this.scope = scope;
        this.model = model;
    }

    static class BuildResult {
        final Output result;
        final Output newState;

        public BuildResult(Output result, Output newState) {
            this.result = result;
            this.newState = newState;
        }
    }

    public Output process(Output currentState, Block block) {
        for (Stmt stmt : block.getStmts()) {
            if (stmt instanceof VarDeclStmt) {
                VarDecl varDecl = ((VarDeclStmt) stmt).getVarDecl();
                String name = varDecl.getName();
                Exp initExp = varDecl.getInitExp();
                Variable var = scope.declareVar(name, initExp.getType());
                BuildResult initialResult = process(currentState, initExp);
                SetVarNode node = new SetVarNode(region, initialResult.newState, var, initialResult.result);
                currentState = node.getNewState();
            } else if (stmt instanceof AssignStmt) {
                AssignStmt assignStmt = (AssignStmt)stmt;
                Variable var = scope.lookupVar(assignStmt.getVar().getName());
                BuildResult valueResult = process(currentState, assignStmt.getValue());
                SetVarNode node = new SetVarNode(region, valueResult.newState, var, valueResult.result);
                currentState = node.getNewState();
            } else if (stmt instanceof IfStmt) {
                IfStmt ifStmt = (IfStmt) stmt;
                BuildResult predicateResult = process(currentState, ifStmt.getCondition());
                currentState = predicateResult.newState;
                Output predicate = predicateResult.result;
                GammaNode gamma = new GammaNode(region, 2, predicate);

                // Transfer the state variable through the regions:
                GammaInput innerState = gamma.transfer(currentState);
                Output elseState = innerState.get(0);
                Output thenState = innerState.get(1);

                // Process child blocks:
                Block elseBlock = ifStmt.getElse();
                if (elseBlock != null) {
                    Builder elseBuilder = new Builder(gamma.branchRegions.get(0), scope.createChild(), model);
                    elseState = elseBuilder.process(elseState, elseBlock);
                }

                Block thenBlock = ifStmt.getThen();
                Builder thenBuilder = new Builder(gamma.branchRegions.get(1), scope.createChild(), model);
                thenState = thenBuilder.process(thenState, thenBlock);

                currentState = gamma.createOutput(currentState.type, elseState, thenState);
            } else if (stmt instanceof ExpressionStmt) {
                ExpressionStmt exprStmp = (ExpressionStmt) stmt;
                currentState = process(currentState, exprStmp.getExp()).newState;
            } else {
                throw new NotImplementedYetException(stmt);
            }
        }

        return currentState;
    }

    BuildResult process(Output currentState, Exp exp) {
        if (exp instanceof FnApp) {
            FnApp fnApp = (FnApp) exp;
            FunctionDecl decl = (FunctionDecl) fnApp.getDecl();
            List<Output> args = new ArrayList<>();
            for (PureExp param : fnApp.getParams()) {
                BuildResult result = process(currentState, param);
                currentState = result.newState;
                args.add(result.result);
            }

            if (decl.getFunctionDef() instanceof BuiltinFunctionDef) {
                return callBuiltin(currentState, decl, args);
            }

            throw new NotImplementedYetException(exp);
        } else if (exp instanceof DataConstructorExp) {
            DataConstructorExp cexp = (DataConstructorExp) exp;
            DataConstructNode node = new DataConstructNode(region, cexp.getDataConstructor());

            for (PureExp param : cexp.getParams()) {
                BuildResult result = process(currentState, param);
                currentState = result.newState;
                node.addParam(result.result);
            }

            return new BuildResult(node.getResult(), currentState);
        } else if (exp instanceof AddExp) {
            AddExp addExp = (AddExp) exp;
            BuildResult leftBR = process(currentState, addExp.getLeft());
            currentState = leftBR.newState;
            BuildResult rightBR = process(currentState, addExp.getRight());
            currentState = rightBR.newState;

            if (leftBR.result.type.isStringType()) {
                StringConcatNode node = new StringConcatNode(region, leftBR.result, rightBR.result);
                return new BuildResult(node.getResult(), currentState);
            }

            throw new NotImplementedYetException(exp);
        } else if (exp instanceof StringLiteral) {
            String content = ((StringLiteral) exp).getContent();
            StringLiteralNode node = new StringLiteralNode(region, exp.getType(), content);
            return new BuildResult(node.getResult(), currentState);
        } else if (exp instanceof VarUse) {
            VarUse varUse = (VarUse)exp;
            Variable var = scope.lookupVar(varUse.getName());
            GetVarNode node = new GetVarNode(region, currentState, var);
            return new BuildResult(node.getResult(), node.getNewState());
        } else {
            throw new NotImplementedYetException(exp);
        }
    }

    BuildResult callBuiltin(Output currentState, FunctionDecl decl, List<Output> args) {
        switch (decl.getName()) {
            case "print": {
                PrintNode node = new PrintNode(region, currentState, args.get(0), false);
                return new BuildResult(null, node.getNewState());
            }
            case "println": {
                PrintNode node = new PrintNode(region, currentState, args.get(0), true);
                return new BuildResult(null, node.getNewState());
            }
            case "toString": {
                ToStringNode node = new ToStringNode(region, model.getStringType(), args.get(0));
                return new BuildResult(node.getResult(), currentState);
            }
            default:
                throw new NotImplementedYetException(decl);
        }
    }
}
