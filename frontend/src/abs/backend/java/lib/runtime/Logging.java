package abs.backend.java.lib.runtime;

import java.util.logging.ConsoleHandler;
import java.util.logging.Formatter;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

public class Logging {
    public static final boolean DEBUG = Boolean.parseBoolean(System.getProperty("abs.debug", "false"));

    public static Logger getLogger(String name) {
        Logger logger = Logger.getLogger(name);
        Handler h = new ConsoleHandler();
        Level level = Level.WARNING;
        if (DEBUG) {
            level = Level.ALL;
        }
        
        h.setLevel(level);
        logger.setLevel(level);
        logger.addHandler(h);
        return logger;
    }
    
    static class MyFormatter extends Formatter {

		@Override
      public String format(LogRecord record) {
	      return record.getLoggerName()+" ["+record.getThreadID()+"]:"+record.getMessage();
      }
   	 
    }
}
