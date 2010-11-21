package abs.backend.java.lib.runtime;

import java.util.logging.ConsoleHandler;
import java.util.logging.Formatter;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

public class Logging {
    public static final Level LOGLEVEL = Level.parse(System.getProperty("abs.loglevel", "warning").toUpperCase());
    public static final boolean DEBUG = !LOGLEVEL.equals(Level.WARNING);

    public static Logger getLogger(String name) {
        Logger logger = Logger.getLogger(name);
        Level level = LOGLEVEL;
        if (DEBUG) {
            level = Level.ALL;
        }

        Handler h = new ConsoleHandler();
        h.setLevel(level);
        logger.addHandler(h);
        logger.setLevel(level);
        return logger;
    }

    static class MyFormatter extends Formatter {

        @Override
        public String format(LogRecord record) {
            return record.getLoggerName() + " [" + record.getThreadID() + "]:" + record.getMessage();
        }

    }
}
