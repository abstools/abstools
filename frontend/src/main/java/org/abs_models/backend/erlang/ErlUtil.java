/**
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.erlang;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.abs_models.backend.common.CodeStream;
import org.abs_models.frontend.ast.ClassDecl;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.ast.ModuleDecl;
import org.abs_models.frontend.ast.ParamDecl;
import org.abs_models.frontend.ast.PureExp;

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
                                                        org.abs_models.frontend.ast.List<ParamDecl> args, Mask mask) {
        StringBuilder b = new StringBuilder("[");
        boolean first = true;
        for (ParamDecl p : args) {
            if (!first)
                b.append(',');
            else
                first = false;
            b.append("P_" + p.getName());
        }

        if (args.getNumChild() > 0) {
            b.append(',');
        }

        b.append("Stack]");
        functionHeader(ecs, funName, mask, firstParameter, b.toString());
    }

    // Not used when garbage collector pause points are added to functions
    // Consider removing
    public static final void functionHeader(CodeStream ecs, String funName, org.abs_models.frontend.ast.List<ParamDecl> args) {
        List<String> a = new ArrayList<>(args.getNumChild());
        for (ParamDecl p : args)
            a.add(p.getName());
        functionHeader(ecs, funName, a, Mask.all);
    }

    public static final void functionHeader(CodeStream ecs, String funName, String firstParameter,
            org.abs_models.frontend.ast.List<ParamDecl> args) {
        List<String> a = new ArrayList<>(args.getNumChild());
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

    public static void buildParams(CodeStream ecs, org.abs_models.frontend.ast.List<PureExp> params, Vars vars, boolean emptyStack) {
        ecs.print("[");
        buildParamsWithOutBrackets(ecs, params, vars);

        if (params.getNumChild() > 0) {
            ecs.print(',');
        }

        if (emptyStack) {
            ecs.print("[]");
        } else {
            ecs.print(vars.toStack());
        }

        ecs.print("]");
    }

    public static void buildParamsWithOutBrackets(CodeStream ecs, org.abs_models.frontend.ast.List<PureExp> params, Vars vars) {
        boolean first = true;
        for (PureExp a : params) {
            if (!first)
                ecs.print(',');
            else
                first = false;
            a.generateErlangCode(ecs, vars);
        }
    }

    public static void argumentList(CodeStream ecs, PureExp callee, boolean builtin, boolean imperativeContext, org.abs_models.frontend.ast.List<PureExp> params, Vars vars, boolean includeLocalVarsInStack) {
        ecs.print("(");
        if (callee != null) {
            callee.generateErlangCode(ecs, vars);
            if (params.getNumChild() > 0) {
                ecs.print(",");
            }

        } else {
            ecs.print("Cog");
            if (params.getNumChild() > 0) {
                ecs.print(",");
            }
        }

        buildParamsWithOutBrackets(ecs, params, vars);

        if (!builtin) {
            ecs.print(',');

            if (imperativeContext) {
                ecs.print(vars.toStack(includeLocalVarsInStack));
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
        ecs.println("cog:process_is_blocked_for_gc(Cog, self()),");
        ecs.println("cog:process_is_runnable(Cog,self()),");
        ecs.print("task:wait_for_token(Cog,");
        if (functional) {
            ecs.print("Stack");
        } else {
            ecs.print(vars.toStack());
        }
        ecs.println(")");
        ecs.decIndent();
        ecs.println("after 0 -> ok");
        ecs.decIndent();
        ecs.println("end,");
    }

    public static void emitLocationInformation(CodeStream ecs, Model m, String filename, int start, int end) {
        if (m.generate_erlang_coverage) {
            ecs.pf("coverage:register(\"%s\", %s, %s),", new java.io.File(filename).getName(), start, end);
        } else {
            ecs.pf(" %%%% %s:%s--%s", filename, start, end);
        }
    }

    public static String absParamDeclToErlVarName(ParamDecl p) {
        return Vars.PREFIX + p.getName() + "_0";
    }
}
