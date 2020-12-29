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

    public Output process(Output currentState, Block block) {
        CurrentState state = new CurrentState(currentState);
        process(state, block);
        return state.current;
    }

    /**
     * Represents the current state. This is threaded through out the building and is modified.
     */
    static class CurrentState {
        public Output current;

        public CurrentState(Output initial) {
            this.current = initial;
        }
    }

    public void process(CurrentState state, Block block) {
        for (Stmt stmt : block.getStmts()) {
            if (stmt instanceof VarDeclStmt) {
                VarDecl varDecl = ((VarDeclStmt) stmt).getVarDecl();
                String name = varDecl.getName();
                Exp initExp = varDecl.getInitExp();
                Variable var = scope.declareVar(name, initExp.getType());
                Output initialResult = process(state, initExp);
                SetVarNode node = new SetVarNode(region, state.current, var, initialResult);
                state.current = node.getNewState();
            } else if (stmt instanceof AssignStmt) {
                AssignStmt assignStmt = (AssignStmt)stmt;
                Variable var = scope.lookupVar(assignStmt.getVar().getName());
                Output valueResult = process(state, assignStmt.getValue());
                SetVarNode node = new SetVarNode(region, state.current, var, valueResult);
                state.current = node.getNewState();
            } else if (stmt instanceof IfStmt) {
                IfStmt ifStmt = (IfStmt) stmt;
                Output predicate = process(state, ifStmt.getCondition());
                GammaNode gamma = new GammaNode(region, 2, predicate);

                // Transfer the state variable through the regions:
                GammaInput innerState = gamma.transfer(state.current);
                CurrentState elseState = new CurrentState(innerState.get(0));
                CurrentState thenState = new CurrentState(innerState.get(1));

                // Process child blocks:
                Block elseBlock = ifStmt.getElse();
                if (elseBlock != null) {
                    Builder elseBuilder = new Builder(gamma.branchRegions.get(0), scope.createChild(), model);
                    elseBuilder.process(elseState, elseBlock);
                }

                Block thenBlock = ifStmt.getThen();
                Builder thenBuilder = new Builder(gamma.branchRegions.get(1), scope.createChild(), model);
                thenBuilder.process(thenState, thenBlock);

                state.current = gamma.createOutput(state.current.type, elseState.current, thenState.current);
            } else if (stmt instanceof ExpressionStmt) {
                ExpressionStmt exprStmp = (ExpressionStmt) stmt;
                process(state, exprStmp.getExp());
            } else {
                throw new NotImplementedYetException(stmt);
            }
        }
    }

    Output process(CurrentState state, Exp exp) {
        if (exp instanceof FnApp) {
            FnApp fnApp = (FnApp) exp;
            FunctionDecl decl = (FunctionDecl) fnApp.getDecl();
            List<Output> args = new ArrayList<>();
            for (PureExp param : fnApp.getParams()) {
                Output result = process(state, param);
                args.add(result);
            }

            if (decl.getFunctionDef() instanceof BuiltinFunctionDef) {
                return callBuiltin(state, decl, args);
            }

            throw new NotImplementedYetException(exp);
        } else if (exp instanceof DataConstructorExp) {
            DataConstructorExp cexp = (DataConstructorExp) exp;
            DataConstructNode node = new DataConstructNode(region, cexp.getDataConstructor());

            for (PureExp param : cexp.getParams()) {
                Output result = process(state, param);
                node.addParam(result);
            }

            return node.getResult();
        } else if (exp instanceof AddAddExp) {
            AddAddExp addExp = (AddAddExp) exp;
            Output leftResult = process(state, addExp.getLeft());
            Output rightResult = process(state, addExp.getRight());

            if (leftResult.type.isStringType()) {
                StringConcatNode node = new StringConcatNode(region, leftResult, rightResult);
                return node.getResult();
            }

            if (leftResult.type.isIntType() && rightResult.type.isIntType()) {
                BinaryArithmeticNode node = new BinaryArithmeticNode(region, BinaryArithmeticNode.Operator.Add, leftResult, rightResult);
                return node.getResult();
            }

            throw new NotImplementedYetException(exp);
        } else if (exp instanceof SubAddExp) {
            SubAddExp subExp = (SubAddExp) exp;
            Output leftResult = process(state, subExp.getLeft());
            Output rightResult = process(state, subExp.getRight());
            BinaryArithmeticNode node = new BinaryArithmeticNode(region, BinaryArithmeticNode.Operator.Sub, leftResult, rightResult);
            return node.getResult();
        } else if (exp instanceof MultMultExp) {
            MultMultExp multExp = (MultMultExp) exp;
            Output leftResult = process(state, multExp.getLeft());
            Output rightResult = process(state, multExp.getRight());
            BinaryArithmeticNode node = new BinaryArithmeticNode(region, BinaryArithmeticNode.Operator.Mul, leftResult, rightResult);
            return node.getResult();
        }  else if (exp instanceof ModMultExp) {
            ModMultExp modExp = (ModMultExp) exp;
            Output leftResult = process(state, modExp.getLeft());
            Output rightResult = process(state, modExp.getRight());
            BinaryArithmeticNode node = new BinaryArithmeticNode(region, BinaryArithmeticNode.Operator.Mod, leftResult, rightResult);
            return node.getResult();
        } else if (exp instanceof StringLiteral) {
            String content = ((StringLiteral) exp).getContent();
            StringLiteralNode node = new StringLiteralNode(region, exp.getType(), content);
            return node.getResult();
        } else if (exp instanceof IntLiteral) {
            String content = ((IntLiteral) exp).getContent();
            IntLiteralNode node = new IntLiteralNode(region, exp.getType(), content);
            return node.getResult();
        } else if (exp instanceof VarUse) {
            VarUse varUse = (VarUse) exp;
            Variable var = scope.lookupVar(varUse.getName());
            GetVarNode node = new GetVarNode(region, state.current, var);
            state.current = node.getNewState();
            return node.getResult();
        } else if (exp instanceof EqExp) {
            EqExp eqExp = (EqExp) exp;
            Output left = process(state, eqExp.getLeft());
            Output right = process(state, eqExp.getRight());
            ComparisonNode node = new ComparisonNode(region, ComparisonNode.Operator.Eq, left, right, eqExp.getType());
            return node.getResult();
        }  else if (exp instanceof NotEqExp) {
            NotEqExp cmpExp = (NotEqExp) exp;
            Output left = process(state, cmpExp.getLeft());
            Output right = process(state, cmpExp.getRight());
            ComparisonNode node = new ComparisonNode(region, ComparisonNode.Operator.NotEq, left, right, cmpExp.getType());
            return node.getResult();
        } else if (exp instanceof GTExp) {
            GTExp cmpExp = (GTExp) exp;
            Output left = process(state, cmpExp.getLeft());
            Output right = process(state, cmpExp.getRight());
            ComparisonNode node = new ComparisonNode(region, ComparisonNode.Operator.Gt, left, right, cmpExp.getType());
            return node.getResult();
        } else if (exp instanceof GTEQExp) {
            GTEQExp cmpExp = (GTEQExp) exp;
            Output left = process(state, cmpExp.getLeft());
            Output right = process(state, cmpExp.getRight());
            ComparisonNode node = new ComparisonNode(region, ComparisonNode.Operator.Gte, left, right, cmpExp.getType());
            return node.getResult();
        } else if (exp instanceof LTExp) {
            LTExp cmpExp = (LTExp) exp;
            Output left = process(state, cmpExp.getLeft());
            Output right = process(state, cmpExp.getRight());
            ComparisonNode node = new ComparisonNode(region, ComparisonNode.Operator.Lt, left, right, cmpExp.getType());
            return node.getResult();
        } else if (exp instanceof LTEQExp) {
            LTEQExp cmpExp = (LTEQExp) exp;
            Output left = process(state, cmpExp.getLeft());
            Output right = process(state, cmpExp.getRight());
            ComparisonNode node = new ComparisonNode(region, ComparisonNode.Operator.Lte, left, right, cmpExp.getType());
            return node.getResult();
        } else {
            throw new NotImplementedYetException(exp);
        }
    }

    Output callBuiltin(CurrentState state, FunctionDecl decl, List<Output> args) {
        switch (decl.getName()) {
            case "print": {
                PrintNode node = new PrintNode(region, state.current, args.get(0), false);
                state.current = node.getNewState();
                return null;
            }
            case "println": {
                PrintNode node = new PrintNode(region, state.current, args.get(0), true);
                state.current = node.getNewState();
                return null;
            }
            case "toString": {
                ToStringNode node = new ToStringNode(region, model.getStringType(), args.get(0));
                return node.getResult();
            }
            default:
                throw new NotImplementedYetException(decl);
        }
    }
}
