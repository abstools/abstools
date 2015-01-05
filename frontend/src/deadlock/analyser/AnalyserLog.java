package deadlock.analyser;


public class AnalyserLog {

  public static enum Level { ALL, PANIC, ERROR, WARNING, DEBUG, NORMAL, ESSENCIAL, NONE; }; 


  private Level _level = Level.ALL;
  private int _indent = 0;

  public void setLogLevel(Level level) { this._level = level; }

  public void log(Level level, String message) {
    if(level.ordinal() <= _level.ordinal()) {
      for(int i = 0; i < _indent; i++) { System.out.print("  "); }
      System.out.println(message);
    }
  }

  public void beginIndent() { _indent++; }
  public void endIndent() { if (_indent > 0) _indent--; }

  // Util
  public void logError(String message)   { this.log(Level.ERROR, message); }
  public void logWarning(String message) { this.log(Level.WARNING, message); }
  public void logDebug(String message) { this.log(Level.DEBUG, message); }
  public void logNormal(String message)  { this.log(Level.NORMAL, message); }
  public void logAll(String message)     { this.log(Level.ALL, message); }

  public void verbose() { this._level = Level.NONE; } 
}
