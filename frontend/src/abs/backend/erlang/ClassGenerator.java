/**
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.erlang;

import java.io.IOException;

import abs.backend.erlang.ErlUtil.Mask;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.FieldDecl;
import abs.frontend.ast.MethodImpl;
import abs.frontend.ast.MethodSig;
import abs.frontend.ast.ParamDecl;
import abs.frontend.ast.TypedVarOrFieldDecl;

import com.google.common.collect.Iterables;

/**
 * Genereates the Erlang module for one class
 * 
 * @author Georg Göri
 * 
 */
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
        for (MethodImpl m : classDecl.getMethodList()) {
            MethodSig ms = m.getMethodSig();
            ErlUtil.functionHeader(ecs, "m_" + ms.getName(), generatorClassMatcher(), ms.getParamList());
            ecs.println("try");
            ecs.incIndent();
            m.getBlock().generateErlangCode(ecs, Vars.n(ms.getParamList()));
            ecs.println();
            ecs.decIndent().println("catch");
            ecs.incIndent();
            ecs.println("exit:Error -> object:die(O, Error);");
            ecs.println("throw:Error -> exit(Error)");
            ecs.decIndent().println("end.");
            ecs.decIndent();
        }

    }

    private void generateConstructor() {
        ErlUtil.functionHeaderParamsAsList(ecs, "init", generatorClassMatcher(), classDecl.getParamList(), Mask.none);
        Vars vars = Vars.n();
        for (ParamDecl p : classDecl.getParamList()) {
            ecs.pf("set(O,%s,%s),", p.getName(), "P_" + p.getName());
        }
        for (FieldDecl p : classDecl.getFields()) {
            if (p.hasInitExp()) {
                ecs.format("set(O,%s,", p.getName());
                p.getInitExp().generateErlangCode(ecs, vars);
                ecs.println("),");
            }
        }
        if (classDecl.getInitBlock() != null) {
            classDecl.getInitBlock().generateErlangCode(ecs, vars);
            ecs.println(",");
        }
        if (classDecl.isActiveClass())
            ecs.println("cog:add(Cog,active_object_task,O),");
        ecs.println("O.");
        ecs.decIndent();
    }

    private String generatorClassMatcher() {
        return String.format("O=#object{class=%s=C,ref=Ref,cog=Cog=#cog{ref=CogRef}}", modName);
    }

    private void generateDataAccess() {
        ErlUtil.functionHeader(ecs, "set", Mask.none,
                String.format("O=#object{class=%s=C,ref=Ref,cog=Cog=#cog{tracker=Tracker}}", modName), "Var", "Val");
        ecs.println("object_tracker:dirty(Tracker,O),");
        ecs.println("gen_fsm:send_event(Ref,{O,set,Var,Val}).");
        ecs.decIndent();
        ecs.println();
        ErlUtil.functionHeader(ecs, "get", Mask.none, generatorClassMatcher(), "Var");
        ecs.println("gen_fsm:sync_send_event(Ref,{O,get,Var}).");
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
                ErlUtil.functionHeader(ecs, "get_val_internal", Mask.none, String.format("#state{%s=G}", f.getName()),
                        f.getName());
                ecs.print("G");
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
        ecs.println("-export([get_val_internal/2,set_val_internal/3,init_internal/0]).");
        ecs.println("-compile(export_all).");
    }
}
