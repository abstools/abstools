package abs.frontend.tests;

import java.io.PrintWriter;

public class EmptyFormatter implements ABSFormatter {

    @Override
    public void setPrintWriter(PrintWriter w) {
    }

    @Override
    public void beforeOpenBrace() {
    }

    @Override
    public void afterOpenBrace() {
    }

    @Override
    public void afterStmt() {

    }

    @Override
    public void beforeCloseBrace() {
    }

    @Override
    public void afterCloseBrace() {
    }

}
