package abs.backend.java.debugging;

public interface DebugModelListener {
    void taskInfoChanged(TaskInfo line);

    void taskInfoAdded(TaskInfo line);

    void taskInfoRemoved(TaskInfo line);

    void cogCreated(COGInfo info);

    void cogChanged(COGInfo info);
}