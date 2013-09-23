/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.erlang;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.ModuleDecl;
import abs.frontend.ast.ParamDecl;
import abs.frontend.ast.PureExp;

public class ErlUtil {

    public enum Mask {
        none,
        all,
        not_first
    };

    public static final void functionHeaderParamsAsList(ErlangCodeStream ecs, String funName, String firstParameter,
            abs.frontend.ast.List<ParamDecl> args, Mask mask) {
        List<String> a = new ArrayList<String>(args.getNumChild());
        String b = "[";
        boolean first = true;
        for (ParamDecl p : args) {
            if (!first)
                b = b + (',');
            else
                first = false;
            b = b + "P_" + p.getName();
        }
        b = b + "]";

        functionHeader(ecs, funName, mask, firstParameter, b);
    }

    public static final void functionHeader(ErlangCodeStream ecs, String funName, abs.frontend.ast.List<ParamDecl> args) {
        List<String> a = new ArrayList<String>(args.getNumChild());
        for (ParamDecl p : args)
            a.add(p.getName());
        functionHeader(ecs, funName, a, Mask.all);
    }

    public static final void functionHeader(ErlangCodeStream ecs, String funName, String firstParameter,
            abs.frontend.ast.List<ParamDecl> args) {
        List<String> a = new ArrayList<String>(args.getNumChild());
        a.add(firstParameter);
        for (ParamDecl p : args)
            a.add(p.getName());
        functionHeader(ecs, funName, a, Mask.not_first);
    }

    public static final void functionHeader(ErlangCodeStream ecs, String funName, String... args) {
        functionHeader(ecs, funName, Mask.all, args);
    }

    public static final void functionHeader(ErlangCodeStream ecs, String funName, Mask mask, String... args) {
        functionHeader(ecs, funName, Arrays.asList(args), mask);
    }

    public static final void functionHeader(ErlangCodeStream ecs, String funName, List<String> args, Mask mask) {
        ecs.format("%s(", funName);
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

    public static void buildParams(ErlangCodeStream ecs, abs.frontend.ast.List<PureExp> params, Vars vars) {
        ecs.print("[");
        buildParamsWithOutBrackets(ecs, params, vars);
        ecs.print("]");
    }

    public static void buildParamsWithOutBrackets(ErlangCodeStream ecs, abs.frontend.ast.List<PureExp> params, Vars vars) {
        boolean first = true;
        for (PureExp a : params) {
            if (!first)
                ecs.print(',');
            else
                first = false;
            a.generateErlangCode(ecs, vars);
        }
    }

    public static void argumentList(ErlangCodeStream ecs, PureExp callee, abs.frontend.ast.List<PureExp> params,
            Vars vars) {
        ecs.print("(");
        if (callee != null) {
            callee.generateErlangCode(ecs, vars);
            if (params.hasChildren())
                ecs.print(",");
        }
        buildParamsWithOutBrackets(ecs, params, vars);
        ecs.print(")");

    }
}
