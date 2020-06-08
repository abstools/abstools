package org.abs_models.backend.scala;

import javax.lang.model.element.Modifier;
import java.io.IOException;
import java.io.Writer;
import java.util.*;
import java.util.regex.Matcher;

import static javax.lang.model.element.Modifier.ABSTRACT;

//import com.squareup.javawriter.StringLiteral;

//import jabsc.JavaWriter.Scope;

/* XXX
 * 
 * This class is an extension over the extension of Square's original JavaWriter
 * in order to generate Scala code and support pattern matching.
 * due to our limitations of parsing and lexing of ABS code.
 * 
 */

/** A utility class which aids in generating Scala source files. */
public class ScalaWriter extends JavaWriter {

    /**
     * @param out
     *            the stream to which Java source will be written. This should
     *            be a buffered stream.
     */

    HashMap<String, String> methodParameters = new HashMap<>();
    LinkedList<HashMap<String, String>> duplicateReplacements = new LinkedList<>();
    boolean voidMethod = false;
    boolean avoiddc = false;

    public ScalaWriter(Writer out, boolean isAux) {
        super(out, isAux);

        duplicateReplacements.push(new HashMap<>());
    }

    public ScalaWriter(Writer out) {
        super(out);
        duplicateReplacements.push(new HashMap<>());
    }

    public ScalaWriter(boolean isAux, Writer out, boolean checkAwaits) {
        super(isAux, out, checkAwaits);

        duplicateReplacements.push(new HashMap<>());
    }

    public ScalaWriter(Writer out, boolean isAux, boolean avoidDuplicates) {
        super(out, isAux, avoidDuplicates);

        duplicateReplacements.push(new HashMap<>());
    }

    /**
     * Emit a package declaration and empty line. Exactly the same as in Java
     */
    @Override
    public ScalaWriter emitPackage(String packageName) throws IOException {
        if (this.packagePrefix != null) {
            throw new IllegalStateException();
        }
        if (packageName.isEmpty()) {
            this.packagePrefix = "";
        } else {
            out.write("package ");
            out.write(packageName);
            out.write(";\n\n");
            this.packagePrefix = packageName + ".";
        }
        return this;
    }

    /**
     * Emit an import for each {@code type} provided. For the duration of the
     * file, all references to these classes will be automatically shortened.
     */
    @Override
    public ScalaWriter emitImports(String... types) throws IOException {
        return emitImports(Arrays.asList(types));
    }

    /**
     * Emit an import for each {@code type} provided. For the duration of the
     * file, all references to these classes will be automatically shortened.
     */
    @Override
    public ScalaWriter emitImports(Class<?>... types) throws IOException {
        List<String> classNames = new ArrayList<String>(types.length);
        for (Class<?> classToImport : types) {
            classNames.add(classToImport.getCanonicalName());
        }
        return emitImports(classNames);
    }

    /**
     * Emit an import for each {@code type} in the provided {@code Collection}.
     * For the duration of the file, all references to these classes will be
     * automatically shortened.
     */
    @Override
    public ScalaWriter emitImports(Collection<String> types) throws IOException {
        for (String type : new TreeSet<String>(types)) {

            out.write("import ");
            out.write(type);
            out.write(";\n");
        }
        return this;
    }

    /**
     * Emit a static import for each {@code type} provided. For the duration of
     * the file, all references to these classes will be automatically
     * shortened.
     */
    @Override
    public ScalaWriter emitStaticImports(String... types) throws IOException {
        return emitStaticImports(Arrays.asList(types));
    }

    /**
     * Emit a static import for each {@code type} in the provided
     * {@code Collection}. For the duration of the file, all references to these
     * classes will be automatically shortened. In Scala just omit the keyword
     * "static".
     */
    @Override
    public ScalaWriter emitStaticImports(Collection<String> types) throws IOException {
        for (String type : new TreeSet<String>(types)) {
            Matcher matcher = TYPE_PATTERN.matcher(type);
            if (!matcher.matches()) {
                throw new IllegalArgumentException(type);
            }
            if (importedTypes.put(type, matcher.group(1)) != null) {
                throw new IllegalArgumentException(type);
            }
            out.write("import ");
            out.write(type);
            out.write(";\n");
        }
        return this;
    }

    /**
     * Emits an initializer declaration.
     *
     * @param isStatic
     *            true if it should be an static initializer, false for an
     *            instance initializer.
     */
    @Override
    public ScalaWriter beginInitializer(boolean isStatic) throws IOException {
        indent();

        out.write("{\n");
        scopes.push(Scope.INITIALIZER);
        return this;
    }

    /** Ends the current initializer declaration. */
    public ScalaWriter endInitializer() throws IOException {
        popScope(Scope.INITIALIZER);
        indent();
        out.write("}\n");
        return this;
    }

    /**
     * Emits a type declaration.
     *
     * @param kind
     *            such as "class", "interface" or "enum".
     */
    @Override
    public ScalaWriter beginType(String type, String kind) throws IOException {
        return beginType(type, kind, EnumSet.noneOf(Modifier.class), null);
    }

    /**
     * Emits a type declaration.
     *
     * @param kind
     *            such as "class", "interface" or "enum".
     */
    @Override
    public ScalaWriter beginType(String type, String kind, Set<Modifier> modifiers) throws IOException {
        return beginType(type, kind, modifiers, null);
    }

    /**
     * Emits a type declaration.
     *
     * @param kind
     *            such as "class", "interface" or "enum".
     * 
     */
    @Override
    public ScalaWriter beginType(String type, String kind, Set<Modifier> modifiers, String extendsType,
                                 String... implementsTypes) throws IOException {
        return beginType(type, kind, modifiers, extendsType, null, implementsTypes);
    }

    public ScalaWriter beginType(String type, String kind, Set<Modifier> modifiers, String extendsType,
                                 List<String> parameters, String... implementsTypes) throws IOException {
        indent();
        emitModifiers(modifiers);
        out.write(kind.equals("interface") ? "trait" : kind);
        out.write(" ");
        emitCompressedType(type);
        if (parameters != null) {
            if (parameters.size() == 0) {
                out.write("()");
            } else {
                out.write(String.format("(var %s : %s", parameters.get(1), parameters.get(0)));

                for (int i = 2; i < parameters.size(); i += 2) {
                    out.write(String.format(",var %s : %s", parameters.get(i + 1), parameters.get(i)));
                }

                out.write(")");
            }
        }

        if (extendsType != null) {
            out.write(" extends ");
            emitCompressedType(extendsType);
        }

        if (implementsTypes.length > 0) {
            indent();
            if (extendsType == null)
                out.write(" extends ");
            else
                out.write(" with ");
            emitCompressedType(implementsTypes[0]);
            for (int i = 1; i < implementsTypes.length; i++) {
                if (i != 0) {
                    out.write(" with ");
                }
                emitCompressedType(implementsTypes[i]);
            }
        }
        out.write(" {\n");
        scopes.push("interface".equals(kind) ? Scope.INTERFACE_DECLARATION : Scope.TYPE_DECLARATION);
        types.push(type);
        return this;
    }

    /** Emits a field declaration. */
    @Override
    public ScalaWriter emitField(String type, String name) throws IOException {
        return emitField(type, name, EnumSet.noneOf(Modifier.class), null);
    }

    /** Emits a field declaration. */
    @Override
    public ScalaWriter emitField(String type, String name, Set<Modifier> modifiers) throws IOException {
        return emitField(type, name, modifiers, null);
    }

    /** Emits a field declaration. */
    @Override
    public ScalaWriter emitField(String type, String name, Set<Modifier> modifiers, String initialValue)
            throws IOException {
        indent();
        emitModifiers(modifiers);

        out.write("var ");
        out.write(name);

        if (type != null) {
            out.write(" : " + type);
        }

        if (initialValue != null) {
            out.write(" = ");
            if (!initialValue.startsWith("\n")) {
                out.write(" ");
            }

            String[] lines = initialValue.split("\n", -1);
            out.write(lines[0]);
            for (int i = 1; i < lines.length; i++) {
                out.write("\n");
                hangingIndent();
                out.write(lines[i]);
            }
        }
        out.write(";\n");
        return this;
    }

    /**
     * Emit a method declaration.
     *
     * <p>
     * A {@code null} return type may be used to indicate a constructor, but
     * {@link #beginConstructor(Set, String...)} should be preferred. This
     * behavior may be removed in a future release.
     *
     * @param returnType
     *            the method's return type, or null for constructors
     * @param name
     *            the method name, or the fully qualified class name for
     *            constructors.
     * @param modifiers
     *            the set of modifiers to be applied to the method
     * @param parameters
     *            alternating parameter types and names.
     */
    @Override
    public ScalaWriter beginMethod(String returnType, String name, Set<Modifier> modifiers, String... parameters)
            throws IOException {
        return beginMethod(returnType, name, modifiers, Arrays.asList(parameters), null);
    }

    /**
     * Emit a method declaration.
     *
     * <p>
     * A {@code null} return type may be used to indicate a constructor, but
     * {@link #beginConstructor(Set, List, List)} should be preferred. This
     * behavior may be removed in a future release.
     *
     * @param returnType
     *            the method's return type, or null for constructors.
     * @param name
     *            the method name, or the fully qualified class name for
     *            constructors.
     * @param modifiers
     *            the set of modifiers to be applied to the method
     * @param parameters
     *            alternating parameter types and names.
     * @param throwsTypes
     *            the classes to throw, or null for no throws clause.
     */
    @Override
    public ScalaWriter beginMethod(String returnType, String name, Set<Modifier> modifiers, List<String> parameters,
                                   List<String> throwsTypes) throws IOException {
        indent();
        // emitModifiers(modifiers);
        out.write("def");
        if (returnType != null) {
            out.write(" ");
            out.write(name);
        } else {
            emitCompressedType(" " + name);
        }
        out.write("(");
        if (parameters != null && !parameters.isEmpty()) {
            out.write(String.format(" %s : %s", parameters.get(1), parameters.get(0)));

            if (continuationLevel >= -1) {
                methodParameters.put(parameters.get(1), parameters.get(0));
            }

            for (int i = 2; i < parameters.size(); i += 2) {
                out.write(String.format(",  %s : %s", parameters.get(i + 1), parameters.get(i)));
                if (continuationLevel >= -1) {
                    methodParameters.put(parameters.get(i + 1), parameters.get(i));
                }
            }

        }
        out.write(")");

        if (returnType != null) {
            out.write(": ");
            emitCompressedType(returnType);
        }
         else
             out.write("");


        if (returnType!=null && returnType.equals("ABSFuture[Void]")
                && (!(modifiers.contains(ABSTRACT) || Scope.INTERFACE_DECLARATION.equals(scopes.peek())))) {
            voidMethod = true;
        }

        if (throwsTypes != null && throwsTypes.size() > 0) {
            out.write("\n");
            indent();
            out.write("    throws ");
            for (int i = 0; i < throwsTypes.size(); i++) {
                if (i != 0) {
                    out.write(", ");
                }
                emitCompressedType(throwsTypes.get(i));
            }
        }
        if (modifiers.contains(ABSTRACT) || Scope.INTERFACE_DECLARATION.equals(scopes.peek())) {
            out.write(";\n");
            scopes.push(Scope.ABSTRACT_METHOD);
        } else {
            out.write("= {\n");
            scopes.push(returnType == null ? Scope.CONSTRUCTOR : Scope.NON_ABSTRACT_METHOD);
        }

        return this;
    }

    @Override
    public ScalaWriter beginConstructor(Set<Modifier> modifiers, String... parameters) throws IOException {
        indent();
        // emitModifiers(modifiers);
        out.write("def this");
        out.write("(");
        if (parameters != null && !(parameters.length == 0)) {
            out.write(String.format(" %s : %s", parameters[1], parameters[0]));

            for (int i = 2; i < parameters.length; i += 2) {
                out.write(String.format(",  %s : %s", parameters[i + 1], parameters[i]));

            }

        }
        out.write("){\n");
        out.write("this(");
        if (parameters != null && parameters.length > 2) {
            out.write(parameters[3]);

            for (int i = 4; i < parameters.length; i += 2) {
                out.write(String.format(",  %s", parameters[i + 1]));
            }

        }
        out.write(")\n");
        out.write(String.format("moveToCOG(%s)\n", parameters[1]));
        scopes.push(Scope.CONSTRUCTOR);
        return this;
    }

    @Override
    public ScalaWriter beginConstructor(Set<Modifier> modifiers, List<String> parameters, List<String> throwsTypes)
            throws IOException {
        indent();
        // emitModifiers(modifiers);
        out.write("def this");
        out.write("(");
        if (parameters != null && !(parameters.isEmpty())) {
            out.write(String.format(" %s : %s", parameters.get(1), parameters.get(0)));

            for (int i = 2; i < parameters.size(); i += 2) {
                out.write(String.format(",  %s : %s", parameters.get(i + 1), parameters.get(i)));

            }

        }
        out.write("){\n");
        out.write("this(");
        if (parameters != null && parameters.size() > 2) {
            out.write(parameters.get(3));

            for (int i = 4; i < parameters.size(); i += 2) {
                out.write(String.format(",  %s", parameters.get(i + 1)));
            }

        }
        out.write(")\n");
        out.write(String.format("moveToCOG(%s)\n", parameters.get(1)));

        scopes.push(Scope.CONSTRUCTOR);

        return this;
    }

    public ScalaWriter beginConstructor() throws IOException {
        out.write("{\n");
        scopes.push(Scope.CONSTRUCTOR);

        return this;
    }

    @Override
    public JavaWriter endMethod() throws IOException {
        if (voidMethod) {
            voidMethod = false;
            this.emitStatement("return %s.done()", ScalaVisitor.ABSFUTURE_CLASS);
        }
        return super.endMethod();
    }
    
   
    
    

}
