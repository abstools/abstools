package abs.backend.java.lib.runtime;

import java.util.logging.ConsoleHandler;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;

public class Logging {

    public static Logger getLogger(String name) {
        Logger logger = Logger.getLogger(name);
        Handler h = new ConsoleHandler();
        h.setLevel(Level.ALL);
        logger.setLevel(Level.ALL);
        logger.addHandler(h);
        return logger;
    }
}
