package abs.backend.java;

import AST.*;

class JavaCompiler extends Frontend {
  public static void main(String... args) {
    if(!compile(args))
      System.exit(1);
  }

  public static boolean compile(String... args) {
    return new JavaCompiler().process(
        args,
        new BytecodeParser(),
        new JavaParser() {
          public CompilationUnit parse(java.io.InputStream is, String fileName) throws java.io.IOException, beaver.Parser.Exception {
            return new parser.JavaParser().parse(is, fileName);
          }
        }
    );
  }
  protected void processNoErrors(CompilationUnit unit) {
    unit.transformation();
    unit.generateClassfile();
  }

  protected String name() { return "Java5Compiler"; }
  protected String version() { return "R20071015"; }
}
