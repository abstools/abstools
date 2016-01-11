/**
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.erlang;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import abs.backend.common.CodeStream;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.ModuleDecl;
import abs.frontend.ast.ParamDecl;
import abs.frontend.ast.PureExp;
import abs.frontend.typechecker.Type;

/**
 * Utility functions to mostly generate headers or parameter lists.
 * 
 * @author Georg Göri
 * 
 */
public class ErlUtil {

    public enum Mask {
        none,
        all,
        not_first
    };

    public static final void functionHeaderParamsAsList(CodeStream ecs, String funName, String firstParameter,
                                                        abs.frontend.ast.List<ParamDecl> args, Mask mask) {
        StringBuilder b = new StringBuilder("[");
        boolean first = true;
        for (ParamDecl p : args) {
            if (!first)
                b.append(',');
            else
                first = false;
            b.append("P_" + p.getName());
        }

        if (args.hasChildren()) {
            b.append(',');
        }

        b.append("Stack]");
        functionHeader(ecs, funName, mask, firstParameter, b.toString());
    }

    // Not used when garbage collector pause points are added to functions
    // Consider removing
    public static final void functionHeader(CodeStream ecs, String funName, abs.frontend.ast.List<ParamDecl> args) {
        List<String> a = new ArrayList<String>(args.getNumChild());
        for (ParamDecl p : args)
            a.add(p.getName());
        functionHeader(ecs, funName, a, Mask.all);
    }

    public static final void functionHeader(CodeStream ecs, String funName, String firstParameter,
            abs.frontend.ast.List<ParamDecl> args) {
        List<String> a = new ArrayList<String>(args.getNumChild());
        a.add(firstParameter);
        for (ParamDecl p : args)
            a.add(p.getName());
        functionHeader(ecs, funName, a, Mask.not_first);
    }

    public static final void functionHeader(CodeStream ecs, String funName, String... args) {
        functionHeader(ecs, funName, Mask.all, args);
    }

    public static final void functionHeader(CodeStream ecs, String funName, Mask mask, String... args) {
        functionHeader(ecs, funName, Arrays.asList(args), mask);
    }

    public static final void functionHeader(CodeStream ecs, String funName, List<String> args, Mask mask) {
        ecs.format("'%s'(", funName);
        boolean first = true;
        for (String a : args) {
            if (!first)
                ecs.print(',');
            if (mask == Mask.all || mask == Mask.not_first && !first) {
                ecs.print(Vars.PREFIX);
                ecs.print(a + "_0");
            } else
                ecs.print(a);
            if (first)
                first = false;
        }

        // Consider using a parameter for this
        // Any function/method callable in ABS, should take a representation of the execution stack
        // except the built-in functions.
        if (funName.startsWith("m_") || funName.startsWith("f_")) {
            if (!first) {
                ecs.print(',');
            }
            ecs.print("Stack");
        }

        ecs.println(")->");
        ecs.incIndent();

    }

    public static final String getName(ClassDecl cd) {
        return "class_" + cd.getModuleDecl().getName().replace('.', '_') + "_" + cd.getName();
    }

    public static final String getName(ModuleDecl cd) {
        return getModuleName(cd.getName());
    }

    public static final String getModuleName(String name) {
        return "m_" + name.replace('.', '_');
    }

    public static void buildParams(CodeStream ecs, abs.frontend.ast.List<PureExp> params, List<Type> paramTypes, Vars vars, boolean emptyStack) {
        ecs.print("[");
        buildParamsWithOutBrackets(ecs, params, paramTypes, vars);

        if (params.hasChildren()) {
            ecs.print(',');
        }
        
        if (emptyStack) {
            ecs.print("[]");
        } else {
            ecs.print(vars.toStack());
        }

        ecs.print("]");
    }

    public static void buildParamsWithOutBrackets(CodeStream ecs, abs.frontend.ast.List<PureExp> params, List<Type> paramTypes, Vars vars) {
        boolean first = true;
        int num = paramTypes.size();
        for (int i = 0; i < num; i++) {
            PureExp param = params.getChild(i);
            Type paramType = paramTypes.get(i);
            boolean needTrunc=paramType.isIntType()
                && param.getType().isRatType();
            if (!first)
                ecs.print(',');
            else
                first = false;

            if (needTrunc) ecs.print("rationals:trunc(");
            param.generateErlangCode(ecs, vars);
            if (needTrunc) ecs.print(")");
        }
    }

    public static void argumentList(CodeStream ecs, PureExp callee, boolean builtin, boolean imperativeContext, abs.frontend.ast.List<PureExp> params, List<Type> paramTypes, Vars vars) {
        ecs.print("(");
        if (callee != null) {
            callee.generateErlangCode(ecs, vars);
            if (params.hasChildren()) {
                ecs.print(",");
            }

        } else {
            ecs.print("Cog");
            if (params.hasChildren()) {
                ecs.print(",");
            }
        }

        buildParamsWithOutBrackets(ecs, params, paramTypes, vars);

        if (!builtin) {
            ecs.print(',');

            if (imperativeContext) {
                ecs.print(vars.toStack());
            } else {
                ecs.print("Stack");
            }
        }

        ecs.print(")");
    }

    public static void stopWorldPrelude(CodeStream ecs, Vars vars, boolean functional) {
        ecs.println("receive");
        ecs.incIndent();
        ecs.println("{stop_world, CogRef} ->");
        ecs.incIndent();
        ecs.println("task:block_without_time_advance(Cog),");
        ecs.print("task:acquire_token(Cog, ");
        if (functional) {
            ecs.print("Stack");
        } else {
            ecs.print(vars.toStack());
        }
        ecs.println(");");
        ecs.decIndent().println("die_prematurely ->");
        ecs.incIndent().println("exit(killed_by_the_clock)");
        ecs.decIndent();
        ecs.decIndent();
        ecs.println("after 0 -> ok end,");
    }
}
