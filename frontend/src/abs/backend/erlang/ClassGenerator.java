/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.erlang;

import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

import abs.backend.erlang.ErlUtil.Mask;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.FieldDecl;
import abs.frontend.ast.InterfaceDecl;
import abs.frontend.ast.InterfaceTypeUse;
import abs.frontend.ast.MethodImpl;
import abs.frontend.ast.MethodSig;
import abs.frontend.ast.ParamDecl;
import abs.frontend.ast.TypedVarOrFieldDecl;

import com.google.common.collect.Iterables;

public class ClassGenerator {
    private final ErlangCodeStream ecs;
    private final ClassDecl classDecl;
    private final String modName;
    private final boolean hasFields;

    public ClassGenerator(ErlApp ea, ClassDecl classDecl) throws IOException {
        this.classDecl = classDecl;
        modName = ErlUtil.getName(classDecl);
        ecs = ea.createFile(modName);
        hasFields = classDecl.getParams().hasChildren() || classDecl.getFields().hasChildren();
        try {
            generateHeader();
            generateExports();
            generateConstructor();
            generateMethods();
            generateDataAccess();
        } finally {
            ecs.close();
        }
    }

    private void generateHeader() {
        ecs.pf("-module(%s).", modName);
        ecs.println("-include_lib(\"abs_types.hrl\").");
        if (hasFields)
            ecs.println("-behaviour(object).");
    }

    private void generateMethods() {
        // Group methods of same arity
        // Multimap<String, MethodSig> map = LinkedListMultimap.create();
        for (MethodImpl m : classDecl.getMethodList()) {
            // String n = ms.getName() + "/" + (ms.getParamList().getNumChild()
            // + 1);
            // map.put(n, ms);
            MethodSig ms = m.getMethodSig();
            ErlUtil.functionHeader(ecs, "m_" + ms.getName(), generatorClassMatcher(), ms.getParamList());
            m.getBlock().generateErlangCode(ecs, Vars.n(ms.getParamList()));
            ecs.println(".");
            ecs.decIndent();
            ecs.println();
        }

    }

    private void generateConstructor() {
        ErlUtil.functionHeaderParamsAsList(ecs, "init", generatorClassMatcher(), classDecl.getParamList(), Mask.none);
        for (ParamDecl p : classDecl.getParamList()) {
            ecs.pf("set(O,%s,%s),", p.getName(), "P_" + p.getName());
        }
        for (FieldDecl p : classDecl.getFields()) {
            if (p.hasInitExp()) {
                ecs.format("set(O,%s,", p.getName());
                p.getInitExp().generateErlangCode(ecs, Vars.n());
                ecs.println("),");
            }
        }
        if (classDecl.getInitBlock() != null) {
            classDecl.getInitBlock().generateErlangCode(ecs, Vars.n());
            ecs.println(",");
        }
        ecs.println("O.");
        ecs.decIndent();
    }

    private String generatorClassMatcher() {
        return String.format("O=#object{class=%s=C,ref=Ref,cog=Cog}", modName);
    }

    private void generateDataAccess() {
        ErlUtil.functionHeader(ecs, "set", Mask.none, generatorClassMatcher(), "Var", "Val");
        ecs.println("Ref!{O,Var,Val,self()},");
        ecs.println("receive {reply,O} -> ok end.");
        ecs.decIndent();
        ecs.println();
        ErlUtil.functionHeader(ecs, "get", Mask.none, generatorClassMatcher(), "Var");
        ecs.println("Ref!{O,Var,self()},");
        ecs.println("receive {reply,O,Val} -> Val end.");
        ecs.decIndent();
        ecs.println();
        ecs.print("-record(state,{");
        boolean first = true;
        for (TypedVarOrFieldDecl f : Iterables.concat(classDecl.getParams(), classDecl.getFields())) {
            if (!first)
                ecs.print(",");
            first = false;
            ecs.format("%s=null", f.getName());
        }
        ecs.println("}).");
        ErlUtil.functionHeader(ecs, "init_internal");
        ecs.println("#state{}.");
        ecs.decIndent();
        ecs.println();
        if (hasFields) {
            first = true;
            for (TypedVarOrFieldDecl f : Iterables.concat(classDecl.getParams(), classDecl.getFields())) {
                if (!first) {
                    ecs.println(";");
                    ecs.decIndent();
                }
                first = false;
                ErlUtil.functionHeader(ecs, "get_val_internal", Mask.none,
                        String.format("S=#state{%s=G}", f.getName()), f.getName());
                ecs.print("{S,G}");
            }
            ecs.println(".");
            ecs.decIndent();
            ecs.println();
            first = true;
            for (TypedVarOrFieldDecl f : Iterables.concat(classDecl.getParams(), classDecl.getFields())) {
                if (!first) {
                    ecs.println(";");
                    ecs.decIndent();
                }
                first = false;
                ErlUtil.functionHeader(ecs, "set_val_internal", Mask.none, "S", f.getName(), "V");
                ecs.format("S#state{%s=V}", f.getName());
            }
            ecs.println(".");
            ecs.decIndent();
            ecs.println();
        } else
        // Generate failing Dummies
        {
            ErlUtil.functionHeader(ecs, "set_val_internal", Mask.none, "S", "S", "S");
            ecs.println("throw(badarg).");
            ecs.decIndent();
            ErlUtil.functionHeader(ecs, "get_val_internal", Mask.none, "S", "S");
            ecs.println("throw(badarg).");
            ecs.decIndent();
        }
    }

    private void generateExports() {
        ecs.println("-export([init/2]).");

        ecs.println("-export([get_val_internal/2,set_val_internal/3,init_internal/0]).");
        for (InterfaceTypeUse i : classDecl.getImplementedInterfaceUses()) {
            InterfaceDecl id = (InterfaceDecl) i.getDecl();
            ecs.pf("%% Interface: %s", id.getName());
            ecs.print("-export([");
            Set<String> s = new HashSet<String>();
            Boolean first = true;
            for (MethodSig ms : id.getBodys()) {
                String n = "m_" + ms.getName() + "/" + (ms.getParamList().getNumChild() + 1);
                if (s.add(n)) {
                    if (!first)
                        ecs.print(",");
                    first = false;
                    ecs.print(n);
                }
            }
            ecs.println("]).");
        }
        ecs.println();
    }
}
