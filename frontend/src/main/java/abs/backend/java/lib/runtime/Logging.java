/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

import java.util.logging.ConsoleHandler;
import java.util.logging.Formatter;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

public class Logging {
    // Default log level is WARNING
    public static Level LOGLEVEL = Level.parse(System.getProperty("abs.loglevel", "warning").toUpperCase());

    public static void setLogLevel(String levelName) {
        LOGLEVEL = Level.parse(levelName.toUpperCase());
    }

    public static Logger getLogger(String name) {
        Logger logger = Logger.getLogger(name);
        Level level = LOGLEVEL;

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
