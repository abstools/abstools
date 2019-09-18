/**
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.erlang;

import java.io.IOException;
import java.io.StringWriter;
import java.nio.charset.Charset;
import java.util.HashSet;
import java.util.Iterator;

import com.google.common.collect.Iterables;

import org.abs_models.backend.common.CodeStream;
import org.abs_models.backend.erlang.ErlUtil.Mask;
import org.abs_models.frontend.ast.CaseBranchStmt;
import org.abs_models.frontend.ast.ClassDecl;
import org.abs_models.frontend.ast.FieldDecl;
import org.abs_models.frontend.ast.InterfaceDecl;
import org.abs_models.frontend.ast.InterfaceTypeUse;
import org.abs_models.frontend.ast.MethodImpl;
import org.abs_models.frontend.ast.MethodSig;
import org.abs_models.frontend.ast.ParamDecl;
import org.abs_models.frontend.ast.TypedVarOrFieldDecl;
import org.abs_models.frontend.typechecker.DataTypeType;
import org.abs_models.frontend.typechecker.Type;
import org.apache.commons.io.output.WriterOutputStream;

/**
 * Generates the Erlang module for one class
 *
 * @author Georg Göri
 *
 */
public class ClassGenerator {
    private final CodeStream ecs;
    private final ClassDecl classDecl;
    private final String modName;
    private final boolean hasFields;

    public ClassGenerator(ErlApp ea, ClassDecl classDecl) throws IOException {
        this.classDecl = classDecl;
        modName = ErlUtil.getName(classDecl);
        ecs = ea.createSourceFile(modName);
        hasFields = classDecl.getParams().getNumChild() > 0 || classDecl.getFields().getNumChild() > 0;
        try {
            generateHeader();
            generateExports();
            generateDataAccess();
            generateConstructor();
            generateRecoverHandler();
            generateMethods();
        } finally {
            ecs.close();
        }
    }

    private void generateHeader() {
        ecs.pf("-module(%s).", modName);
        ecs.println("-include_lib(\"../include/abs_types.hrl\").");
        if (hasFields) {
            ecs.println("-behaviour(object).");
        }
    }

    private void generateMethods() {
        ecs.println("%% --- Methods\n");
        for (MethodImpl m : classDecl.getMethodList()) {
            ecs.pf(" %%%% %s:%s", m.getFileName(), m.getStartLine());
            MethodSig ms = m.getMethodSig();
            ecs.pf(" %%%% %s:%s", m.getFileName(), m.getStartLine());
            ErlUtil.functionHeader(ecs, "m_" + ms.getName(), generatorClassMatcher(), ms.getParamList());
            ecs.println("C=(get(this))#state.class,");
            ecs.print("put(vars, #{ 'this' => O");
            for (ParamDecl p : ms.getParamList()) {
                // Same name construction as
                // ErlUtil.functionHeader(CodeStream, String, List<String>, Mask)
                ecs.format(",%n '%s' => %s", p.getName(), ErlUtil.absParamDeclToErlVarName(p));
            }
            ecs.println(" }),");
            ecs.println("try");
            ecs.incIndent();
            Vars vars = new Vars();
            m.getBlock().generateErlangCode(ecs, vars);
            if (!m.hasReturnStmt()) {
                ecs.println(",");
                ecs.println("dataUnit");
            }
            ecs.println();
            ecs.decIndent().println("catch");
            ecs.incIndent();
            ecs.println("_:Exception ->");
            if (classDecl.hasRecoverBranch()) {
                ecs.incIndent();
                ecs.println("Recovered = try 'recover'(O, Exception) catch _:RecoverError -> io:format(standard_error, \"Recovery block for ~s in class " + classDecl.getQualifiedName() + " failed with exception ~s~n\", [builtin:toString(Cog, Exception), builtin:toString(Cog, RecoverError)]), false end,");
                ecs.println("case Recovered of");
                ecs.incIndent().println("true -> exit(Exception);");
                ecs.println("false ->");
                ecs.incIndent();
                ecs.println("io:format(standard_error, \"Uncaught ~s in method " + ms.getName() + " not handled successfully by recovery block, killing object ~s~n\", [builtin:toString(Cog, Exception), builtin:toString(Cog, O)]),");
                ecs.println("io:format(standard_error, \"stacktrace: ~tp~n\", [erlang:get_stacktrace()]),");
                ecs.println("object:die(O, Exception), exit(Exception)");
                ecs.decIndent().println("end");
                ecs.decIndent();
            } else {
                ecs.incIndent();
                ecs.println("io:format(standard_error, \"Uncaught ~s in method " + ms.getName() + " and no recovery block in class definition, killing object ~s~n\", [builtin:toString(Cog, Exception), builtin:toString(Cog, O)]),");
                ecs.println("io:format(standard_error, \"stacktrace: ~tp~n\", [erlang:get_stacktrace()]),");
                ecs.println("object:die(O, Exception), exit(Exception)");
                ecs.decIndent();
            }
            ecs.decIndent().println("end.");
            ecs.decIndent();
        }

    }

    private void generateConstructor() {
        ecs.println("%% --- Constructor: field initializers and init block\n");
        ErlUtil.functionHeaderParamsAsList(ecs, "init", generatorClassMatcher(), classDecl.getParamList(), Mask.none);
        ecs.println("C=(get(this))#state.class,");
        ecs.println("put(vars, #{}),");
        Vars vars = Vars.n();
        for (ParamDecl p : classDecl.getParamList()) {
            ecs.pf("put(this, C:set_val_internal(get(this),'%s',%s)),", p.getName(), "P_" + p.getName());
        }
        for (FieldDecl p : classDecl.getFields()) {
            ErlUtil.emitLocationInformation(ecs, p.getModel(), p.getFileName(),
                                            p.getStartLine(), p.getEndLine());
            if (p.hasInitExp()) {
                ecs.format("put(this, C:set_val_internal(get(this),'%s',", p.getName());
                p.getInitExp().generateErlangCode(ecs, vars);
                ecs.println(")),");
            }
        }
        if (classDecl.getInitBlock() != null) {
            classDecl.getInitBlock().generateErlangCode(ecs, vars);
            ecs.println(",");
        }
        if (classDecl.isActiveClass()) {
            ecs.println("cog:process_is_blocked_for_gc(Cog, self(), get(process_info), get(this)),");
            ecs.print("cog:add_task(Cog,active_object_task,null,O,[],#process_info{event=#event{type=schedule, local_id=run}, method= <<\"run\"/utf8>>,this=O,destiny=null},");
            ecs.print(vars.toStack());
            ecs.println("),");
            ecs.println("cog:process_is_runnable(Cog,self()),");
            ecs.print("task:wait_for_token(Cog,");
            ecs.print(vars.toStack());
            ecs.println("),");
        }
        ecs.println("O.");
        ecs.decIndent();
    }

    private void generateRecoverHandler() {
        if (classDecl.hasRecoverBranch()) {
            ecs.println("%% --- Recovery block\n");
            Vars vars = new Vars();
            Vars safe = vars.pass();
            // Build var scopes and statmemnts for each branch
            java.util.List<Vars> branches_vars = new java.util.LinkedList<>();
            java.util.List<String> branches = new java.util.LinkedList<>();
            for (CaseBranchStmt b : classDecl.getRecoverBranchs()) {
                Vars v = vars.pass();
                StringWriter sw = new StringWriter();
                CodeStream buffer = new CodeStream(new WriterOutputStream(sw, Charset.forName("UTF-8")),"");
                b.getLeft().generateErlangCode(ecs, buffer, v);
                buffer.setIndent(ecs.getIndent());
                buffer.println("->");
                buffer.incIndent();
                b.getRight().generateErlangCode(buffer, v);
                buffer.println(",");
                buffer.print("true");
                buffer.decIndent();
                buffer.close();
                branches_vars.add(v);
                branches.add(sw.toString());
                vars.updateTemp(v);
            }
            ErlUtil.functionHeader(ecs, "recover", ErlUtil.Mask.none, generatorClassMatcher(), "Exception");
            ecs.println("C=(get(this))#state.class,");
            ecs.println("Stack = [],");
            ecs.println("Result=case Exception of ");
            ecs.incIndent();
            // Now print statments and mergelines for each branch.
            java.util.List<String> mergeLines = vars.merge(branches_vars);
            Iterator<String> ib = branches.iterator();
            Iterator<String> im = mergeLines.iterator();
            while (ib.hasNext()) {
                ecs.print(ib.next());
                ecs.incIndent();
                ecs.print(im.next());
                ecs.println(";");
                ecs.decIndent();
            }
            ecs.println("_ -> false");
            ecs.decIndent();
            ecs.print("end");
            ecs.println(".");
            ecs.decIndent();
        } else {
            ecs.println("%% --- Class has no recovery block\n");
        }
    }

    private String generatorClassMatcher() {
        return String.format("O=#object{ref=Ref,cog=Cog=#cog{ref=CogRef,dc=DC}}", modName);
    }

    private void generateDataAccess() {
        ecs.println("%% --- Internal state and low-level accessors\n");
        ecs.format("-record(state,{'class'=%s", modName);
        for (TypedVarOrFieldDecl f : Iterables.concat(classDecl.getParams(), classDecl.getFields())) {
            ecs.print(",");
            ecs.format("'%s'=null", f.getName());
        }
        ecs.println("}).");
        ErlUtil.functionHeader(ecs, "init_internal");
        ecs.println("#state{}.");
        ecs.decIndent();
        ecs.println();
        for (TypedVarOrFieldDecl f : Iterables.concat(classDecl.getParams(), classDecl.getFields())) {
            ecs.pf(" %%%% %s:%s", f.getFileName(), f.getStartLine());
            ErlUtil.functionHeader(ecs, "get_val_internal", Mask.none, String.format("#state{'%s'=G}", f.getName()),
                                   "'" + f.getName() + "'");
            ecs.println("object:register_read('" + f.getName() + "'),");
            ecs.println("G;");
            ecs.decIndent();
        }
        ErlUtil.functionHeader(ecs, "get_val_internal", Mask.none, "_", "_");
        ecs.println("%% Invalid return value; handled by HTTP API when querying for non-existant field.");
        ecs.println("%% Will never occur in generated code.");
        ecs.println("none.");
        ecs.decIndent();
        ecs.println();
        if (hasFields) {
            boolean first = true;
            for (TypedVarOrFieldDecl f : Iterables.concat(classDecl.getParams(), classDecl.getFields())) {
                if (!first) {
                    ecs.println(";");
                    ecs.decIndent();
                }
                first = false;
                ecs.pf(" %%%% %s:%s", f.getFileName(), f.getStartLine());
                ErlUtil.functionHeader(ecs, "set_val_internal", Mask.none, "S", "'" + f.getName() + "'", "V");
                ecs.println("object:register_write('" + f.getName() + "'),");
                ecs.format("S#state{'%s'=V}", f.getName());
            }
            ecs.println(".");
            ecs.decIndent();
            ecs.println();
        } else {
            // Generate failing Dummy
            ErlUtil.functionHeader(ecs, "set_val_internal", Mask.none, "S", "S", "S");
            ecs.println("throw(badarg).");
            ecs.decIndent();
        }
        ErlUtil.functionHeader(ecs, "get_state_for_modelapi", Mask.none, "S");
        ecs.println("[");
        ecs.incIndent();
        boolean first = true;
        for (TypedVarOrFieldDecl f : Iterables.concat(classDecl.getParams(), classDecl.getFields())) {
            if (!first) ecs.print(", ");
            first = false;
            ecs.pf("{ '%s', S#state.'%s' }",
                   f.getName(), f.getName());
        }
        ecs.decIndent();
        ecs.println("].");
        ecs.decIndent();
    }

    private void generateParameterDescription(Type paramtype) {
        ecs.print("<<\"" + paramtype.getQualifiedName() + "\">>,");
        ecs.print(" {");
        if (paramtype.isDataType()) {
            DataTypeType paramdatatype = (DataTypeType)paramtype;
            if (paramdatatype.hasTypeArgs()) {
                String interp = "";
                for (Type typearg : paramdatatype.getTypeArgs()) {
                    ecs.print(interp);
                    interp = ", ";
                    generateParameterDescription(typearg);
                }
            }
        }
        ecs.print(" }");
    }

    private void generateExports() {
        ecs.println("-export([get_val_internal/2,set_val_internal/3,init_internal/0,get_state_for_modelapi/1,implemented_interfaces/0,exported/0]).");
        ecs.println("-compile(export_all).");
        ecs.println();

        HashSet<MethodSig> callable_sigs = new HashSet<>();
        HashSet<InterfaceDecl> visited = new HashSet<>();
        for (InterfaceTypeUse i : classDecl.getImplementedInterfaceUseList()) {
            visited.add((InterfaceDecl)i.getDecl());
        }

        while (!visited.isEmpty()) {
            InterfaceDecl id = visited.iterator().next();
            visited.remove(id);
            for (MethodSig ms : id.getBodyList()) {
                if (ms.isHTTPCallable()) {
                    callable_sigs.add(ms);
                }
            }
            for (InterfaceTypeUse i : id.getExtendedInterfaceUseList()) {
                visited.add((InterfaceDecl)i.getDecl());
            }
        }

        ecs.print("implemented_interfaces() -> [ ");
        String separator = "";
        for (InterfaceDecl i : classDecl.getSuperTypes()) {
            ecs.format("%s<<\"%s\">>", separator, i.getName());
            separator = ", ";
        }
        ecs.println(" ].");
        ecs.println();


        ecs.print("exported() -> #{ ");
        boolean first = true;
        for (MethodSig ms : callable_sigs) {
            if (ms.isHTTPCallable()) {
                if (!first) ecs.print(", ");
                first = false;
                ecs.print("<<\"" + ms.getName() + "\">> => { ");
                ecs.print("'m_" + ms.getName() + "'");
                ecs.print(", ");
                ecs.print("<<\"" + ms.getReturnType().getType().toString() + "\">>");
                ecs.print(", ");
                ecs.print("[ ");
                boolean innerfirst = true;
                for (ParamDecl p : ms.getParamList()) {
                    // For each parameter, we need name, human-readable type,
                    // ABS type, and ABS type arguments (if present)
                    if (!innerfirst) ecs.print(", ");
                    innerfirst = false;
                    ecs.print("{ ");
                    ecs.print("<<\"" + p.getName() + "\">>, ");
                    ecs.print("<<\"" + p.getType().toString() + "\">>, ");
                    generateParameterDescription(p.getType());
                    ecs.print(" }");
                }
                ecs.print("] ");
                ecs.print("}");
            }
        }
        ecs.println(" }.");
        ecs.println();
    }
}
