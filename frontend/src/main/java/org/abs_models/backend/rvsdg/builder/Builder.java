package org.abs_models.backend.rvsdg.builder;

import org.abs_models.backend.rvsdg.abs.DataConstructNode;
import org.abs_models.backend.rvsdg.abs.SetVarNode;
import org.abs_models.backend.rvsdg.abs.Variable;
import org.abs_models.backend.rvsdg.core.*;
import org.abs_models.common.NotImplementedYetException;
import org.abs_models.frontend.ast.*;
import java.util.List;

/**
 * Builder is the main class for building the IR based on the AST.
 */
public class Builder {
    final Region region;
    final Scope scope;

    public Builder(Region region, Scope scope) {
        this.region = region;
        this.scope = scope;
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
                Output predicate = process(ifStmt.getCondition());
                GammaNode gamma = new GammaNode(region, 2, predicate);

                // Transfer the state variable through the regions:
                GammaInput innerState = gamma.transfer(currentState);
                Output elseState = innerState.get(0);
                Output thenState = innerState.get(1);

                // Process child blocks:
                Block elseBlock = ifStmt.getElse();
                if (elseBlock != null)  {
                    Builder elseBuilder = new Builder(gamma.branchRegions.get(0), scope.createChild());
                    elseState = elseBuilder.process(elseState, elseBlock);
                }

                Block thenBlock = ifStmt.getThen();
                Builder thenBuilder = new Builder(gamma.branchRegions.get(1), scope.createChild());
                thenState = thenBuilder.process(thenState, thenBlock);

                currentState = gamma.createOutput(currentState.type, elseState, thenState);
            } else {
                throw new NotImplementedYetException(stmt);
            }
        }

        return currentState;
    }

    Output process(PureExp exp) {
        if (exp instanceof DataConstructorExp) {
            DataConstructorExp cexp = (DataConstructorExp) exp;
            DataConstructNode node = new DataConstructNode(region, cexp.getDataConstructor());

            for (PureExp param : cexp.getParams()) {
                node.addParam(process(param));
            }

            return node.getResult();
        } else {
            throw new NotImplementedYetException(exp);
        }
    }

    BuildResult process(Output currentState, Exp exp) {
        if (exp instanceof PureExp) {
            return new BuildResult(process((PureExp) exp), currentState);
        } else {
            throw new NotImplementedYetException(exp);
        }
    }
}
