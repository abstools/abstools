package abs.frontend.tests;

import java.io.PrintWriter;

public interface ABSFormatter {
    void setPrintWriter(PrintWriter w);
    
    void beforeOpenBrace();
    void afterOpenBrace();
    void afterStmt();
    void beforeCloseBrace();
    void afterCloseBrace();
    
}
