package abs.backend.java.debugging;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.EventObject;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Semaphore;

import javax.swing.AbstractCellEditor;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.event.CellEditorListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;
import javax.swing.text.BadLocationException;

import abs.backend.java.observing.COGView;
import abs.backend.java.observing.FutView;
import abs.backend.java.observing.GuardView;
import abs.backend.java.observing.ObjectView;
import abs.backend.java.observing.SystemObserver;
import abs.backend.java.observing.TaskObserver;
import abs.backend.java.observing.TaskView;

public class GraphicalDebugger implements SystemObserver {
    final DebugWindow window;
    final DebugModel model;
    
    public GraphicalDebugger() {
        model = new DebugModel();
        window = new DebugWindow(model);
    }
    
    // only for ad-hoc testing
    public static void main(String[] args) {
        new GraphicalDebugger();
    }


    @Override
    public void systemStarted() {
    }


    @Override
    public void newCOGCreated(COGView cog, ObjectView initialObject) {
        model.cogCreated(cog,initialObject);
    }


}

interface DebugModelListener {
    void taskInfoChanged(TaskInfo line);
    void taskInfoAdded(TaskInfo line);
    void taskInfoRemoved(TaskInfo line);
    void cogCreated(COGInfo info);
    void cogChanged(COGInfo info);
}

class COGInfo {
    final COGView cog;
    final ObjectView initialObject;
    final List<TaskInfo> tasks = new ArrayList<TaskInfo>();
    
    COGInfo(COGView cog, ObjectView o) {
        this.cog = cog;
        this.initialObject = o;
    }

    public void addTask(TaskInfo task) {
        tasks.add(task);
    }
    
    
}

class DebugModel implements TaskObserver {
    final Map<TaskView, TaskInfo> taskToLineMap = new HashMap<TaskView, TaskInfo>();
    final Map<COGView, COGInfo> cogInfo = new HashMap<COGView, COGInfo>();
    final ArrayList<DebugModelListener> listener = new ArrayList<DebugModelListener>();

    public COGInfo getCOGInfo(COGView view) {
        return cogInfo.get(view);
    }
    
    public TaskInfo getTaskInfo(TaskView task) {
        return taskToLineMap.get(task);
    }
    
    public void cogCreated(COGView cog, ObjectView initialObject) {
        cog.getScheduler().registerTaskObserver(this);
        ArrayList<DebugModelListener> localList;
        COGInfo info = new COGInfo(cog,initialObject);
        synchronized (this) {
            cogInfo.put(cog, info);
            localList = new ArrayList<DebugModelListener>(listener);
        }
        
        for (DebugModelListener l : localList) {
            l.cogCreated(info);
        }
    }

    public synchronized TaskInfo addInfoLine(TaskView task) {
        TaskInfo line = new TaskInfo(task);
        taskToLineMap.put(task,line);
        for (DebugModelListener l : listener) {
            l.taskInfoAdded(line);
        }
        return line;
    }
    
    public void updateInfoLine(TaskView task, TaskInfo line) {
        ArrayList<DebugModelListener> localList;
        synchronized (this) {
            taskToLineMap.put(task,line);
            localList = new ArrayList<DebugModelListener>(listener);
        }
        
        for (DebugModelListener l : localList) {
            l.taskInfoChanged(line);
        }
    }
    
    public synchronized void removeInfoLine(TaskView task) {
        TaskInfo line = taskToLineMap.remove(task);
        for (DebugModelListener l : listener) {
            l.taskInfoRemoved(line);
        }
    }
    
    public synchronized void registerListener(DebugModelListener l) {
        listener.add(l);
    }
    
    @Override
    public synchronized void taskCreated(TaskView task) {
        
        task.registerTaskListener(this);
        TaskInfo info = addInfoLine(task);

        COGInfo cinfo = cogInfo.get(task.getCOG());
        cinfo.addTask(info);
        cogInfoChanged(cinfo);
    }
    
    


    private synchronized void cogInfoChanged(COGInfo info) {
        for (DebugModelListener l : listener) {
            l.cogChanged(info);
        }
    }

    @Override
    public synchronized void taskSuspended(TaskView task, GuardView guard) {
        TaskInfo info = getTaskInfo(task);
        info.state = TaskState.SUSPENDED;
        updateInfoLine(task, info);
        
    }


    @Override
    public void taskStarted(TaskView task) {
        // TODO Auto-generated method stub
        
    }


    @Override
    public synchronized void taskFinished(TaskView task) {
        TaskInfo info = getTaskInfo(task);
        info.state = TaskState.FINISHED;
        updateInfoLine(task,info);
    }


    @Override
    public void taskBlockedOnFuture(TaskView task, FutView fut) {
        // TODO Auto-generated method stub
        
    }


    @Override
    public void taskRunningAfterWaiting(TaskView view, FutView fut) {
        // TODO Auto-generated method stub
        
    }


    @Override
    public void taskResumed(TaskView runningTask, GuardView view) {
        // TODO Auto-generated method stub
        
    }


    @Override
    public synchronized void taskStep(TaskView task, String fileName, int line) {
        TaskInfo info = getTaskInfo(task);
        info.updateLine(line);
        info.updateFile(fileName);
        info.state = TaskState.RUNNING;
        updateInfoLine(task,info);
    }
    
}

class Line {
    int startPos;
    int endPos;
}


enum TaskState {
    CREATED, SUSPENDED, RUNNING, FINISHED;
}

class TaskInfo {
    TaskView task;
    int currentLine;
    int previousLine;
    String previousFile;
    String currentFile;
    TaskState state = TaskState.CREATED;
    public TaskInfo(TaskView task) {
        this.task = task;
    }
    
    public String toString() {
        String res = "Task "+task.getID();
        if (state == TaskState.SUSPENDED) {
            res = "("+res+")";
        }
        return res;
    }
    
    public void updateLine(int newLine) {
        previousLine = currentLine;
        currentLine = newLine;
    }
    
    public void updateFile(String fileName) {
        previousFile = currentFile;
        currentFile = fileName;
    }
}


class SourceView extends JPanel implements DebugModelListener {
    private static final long serialVersionUID = 1L;
    private final DebugModel model;
    
    JTextArea textArea;
    JTextArea lineArea;
    JTextArea taskArea;
    
    File file;
    
    SourceView(DebugModel model, File file) {
        this.model = model;
        JPanel content = new JPanel();
        this.setLayout(new BorderLayout());
        this.add(new JScrollPane(content),BorderLayout.CENTER);
        
        content.setLayout(new BorderLayout());
        
        JPanel leftContent = new JPanel();
        leftContent.setLayout(new BorderLayout());
        content.add(leftContent,BorderLayout.WEST);
        
        textArea = new JTextArea();
        lineArea = new JTextArea();
        taskArea = new JTextArea();
        lineArea.setBorder(BorderFactory.createLineBorder(Color.gray));
        textArea.setBorder(BorderFactory.createLineBorder(Color.gray));
        taskArea.setBorder(BorderFactory.createLineBorder(Color.gray));
        textArea.setSelectionColor(Color.LIGHT_GRAY);
        
        
        content.add(textArea,BorderLayout.CENTER);
        leftContent.add(lineArea,BorderLayout.EAST);
        leftContent.add(taskArea,BorderLayout.WEST);
        this.file = file;
        
        fillArea();
        
        model.registerListener(this);
    }
    
    private void fillArea() {
        try {
            int lineNo = 1;
            BufferedReader reader = new BufferedReader(new FileReader(file));
            while (reader.ready()) {
                String line = reader.readLine();
                if (line == null)
                    break;
                textArea.append(line+"\n");
                lineArea.append(lineNo+" \n");
                taskArea.append("       \n");
                taskLineInfo.add(new ArrayList<TaskInfo>(0));
                lineNo++;
            }
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
        
        textArea.setCaretPosition(0);
        textArea.moveCaretPosition(1);
        textArea.setEditable(false);
    }

    List<List<TaskInfo>> taskLineInfo = new ArrayList<List<TaskInfo>>();
    
    public void updateTaskLine(int line) {
        String string = "";
        boolean first = true;
        List<TaskInfo> tasks = taskLineInfo.get(line-1);
        for (TaskInfo info : tasks) {
            if (first) first = false;
            else string+=", ";
            string+= info.toString();
        }
        if (tasks.isEmpty())
            string = "        ";
        else
            string += " ->";
        setTaskLine(string,line);
    }
    
    private void highlightLine(TaskInfo line) {
        try {
            int start = textArea.getLineStartOffset(line.currentLine-1);
            int end = textArea.getLineEndOffset(line.currentLine-1);
            textArea.select(start,end);
        } catch (BadLocationException e) {
            e.printStackTrace();
        }
    }

    private void setTaskLine(String string, int line) {
        try {
            int start = taskArea.getLineStartOffset(line-1);
            int end = taskArea.getLineEndOffset(line-1);
            taskArea.replaceRange(string+" \n", start, end);
        } catch (BadLocationException e) {
            e.printStackTrace();
        }   
    }

    @Override
    public void taskInfoChanged(TaskInfo infoLine) {
        if (infoLine.previousLine > 0) {
            taskLineInfo.get(infoLine.previousLine-1).remove(infoLine);
            updateTaskLine(infoLine.previousLine);
        }
        
        if (infoLine.state == TaskState.FINISHED) {
            taskInfoRemoved(infoLine);
            textArea.select(0, 0);
            return;
        }
        
        if (!taskLineInfo.get(infoLine.currentLine-1).contains(infoLine)){
            taskLineInfo.get(infoLine.currentLine-1).add(infoLine);
        }

        updateTaskLine(infoLine.currentLine);
        highlightLine(infoLine);
    }

    @Override
    public void taskInfoAdded(TaskInfo line) {
    }

    @Override
    public void taskInfoRemoved(TaskInfo line) {
        taskLineInfo.get(line.currentLine-1).remove(line);
        updateTaskLine(line.currentLine);
    }

    @Override
    public void cogCreated(COGInfo cog) {
    }

    @Override
    public void cogChanged(COGInfo info) {
    }
}

class TaskControls extends JPanel  {
    private static final long serialVersionUID = 1L;

    final JTable table;
    final TableModel tableModel;
    final DebugModel debugModel;

    private JScrollPane scrollPane;
    
    TaskControls(DebugModel debugModel) {
        tableModel = new TableModel();
        this.debugModel = debugModel;
        setLayout(new BorderLayout());
        table = new JTable(tableModel);
        
        for (int c = 0; c < tableModel.getColumnCount()-1; c++) {
            table.getColumnModel().getColumn(c).setCellRenderer(new StringRenderer());
        }
        table.getColumnModel().getColumn(3).setCellEditor(new StepBtnCellEditor());
        table.getColumnModel().getColumn(3).setCellRenderer(new DefaultTableCellRenderer());
        
        scrollPane = new JScrollPane(table);
        add(scrollPane, BorderLayout.CENTER);
        setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5), "Tasks"));
        setPreferredSize(new Dimension(200,Integer.MAX_VALUE));
        debugModel.registerListener(tableModel);
    }
    
    class StringRenderer extends JLabel implements TableCellRenderer {
        
        public StringRenderer() {
            setOpaque(true);
        }
        
        @Override
        public Component getTableCellRendererComponent(
                JTable table, Object value,
                boolean isSelected, boolean hasFocus,
                int row, int column) {
            TaskInfo info = tableModel.rows.get(row);
            
            if (info.state == TaskState.FINISHED) {
                setBackground(Color.gray);
            } else if (info.state == TaskState.RUNNING){
                setBackground(Color.GREEN);
            } else {
                setBackground(Color.YELLOW);
            }
            setText((String)value);
            return this;
        }
    }

    class StepBtnCellEditor extends AbstractCellEditor implements TableCellEditor, ActionListener {
        
        public StepBtnCellEditor() {
        }
        
        
        @Override
        public void actionPerformed(ActionEvent arg0) {
            System.out.println("Hello World");
        }

        @Override
        public Object getCellEditorValue() {
            return "TRUE";
        }
        
        @Override
        public Component getTableCellEditorComponent(JTable table,
                Object value, boolean isSelected, int row, int column) {
            JButton btn = new JButton("step");
            btn.addActionListener(this);
            btn.setVisible(true);
            TaskInfo task = tableModel.rows.get(row);
            btn.setEnabled(task.state == TaskState.RUNNING); 
            return btn;
        }

    }
    
    class TableModel extends AbstractTableModel implements DebugModelListener {
        private static final long serialVersionUID = 1L;
        
        final List<TaskInfo> rows = new ArrayList<TaskInfo>();
        protected final String[] columnNames = new String[]{ "Task ID", "State", "COG", "Action"};
        protected final Class<?>[] columnClasses = new Class<?>[]{ String.class, String.class, String.class, String.class };
        
        @Override
        public String getColumnName(int column) {
            return columnNames[column];
        }
        
        @Override
        public int getColumnCount() {
            return columnNames.length;
        }

        @Override
        public int getRowCount() {
            return rows.size();
        }

        @Override
        public Class<?> getColumnClass(int columnIndex) {
            return columnClasses[columnIndex];
        }
        
        @Override
        public Object getValueAt(int row, int col) {
            TaskInfo line = rows.get(row);
            switch (col) {
            case 0: return ""+line.task.getID();
            case 1: return line.state.toString();
            case 2: return ""+line.task.getCOG().getID();
            case 3: return "";
            }
            return "ERROR";
        }
        
        

        @Override
        public void taskInfoChanged(TaskInfo task) {
            int row = rows.indexOf(task);
            System.out.println("changed");
            
            
            fireTableRowsUpdated(row, row);
            fireTableDataChanged();
            repaint();
        }

        @Override
        public void taskInfoAdded(TaskInfo task) {
            rows.add(task);
            fireTableRowsInserted(rows.size()-1, rows.size()-1);
            fireTableDataChanged();

        }

        @Override
        public void taskInfoRemoved(TaskInfo task) {
            int row = rows.indexOf(task);
            rows.remove(task);
            fireTableRowsDeleted(row,row);
        }

        @Override
        public void cogCreated(COGInfo cog) {
        }

        @Override
        public void cogChanged(COGInfo info) {
        }
        
    }
}

class COGTable extends JPanel  {
    private final DebugModel model;
    private final TableModel tableModel;
    private JTable table;
    private JScrollPane scrollPane;
    
    COGTable(DebugModel model) {
        this.model = model;
        this.tableModel = new TableModel();
        setLayout(new BorderLayout());
        table = new JTable(tableModel);
        scrollPane = new JScrollPane(table);
        add(scrollPane, BorderLayout.CENTER);
        setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5), "Concurrent Object Groups (COGs)"));
        setPreferredSize(new Dimension(300,400));
        
        model.registerListener(tableModel);
        
    }
    
    class TableModel extends AbstractTableModel implements DebugModelListener {

        final List<COGInfo> rows = new ArrayList<COGInfo>();
        protected final String[] columnNames = new String[]{ "COG ID", "Class", "Tasks"};
        protected final Class<?>[] columnClasses = new Class<?>[]{ String.class, String.class, String.class };
        
        @Override
        public String getColumnName(int column) {
            return columnNames[column];
        }
        
        @Override
        public int getColumnCount() {
            return columnNames.length;
        }

        @Override
        public int getRowCount() {
            return rows.size();
        }

        @Override
        public Class<?> getColumnClass(int columnIndex) {
            return columnClasses[columnIndex];
        }
        
        @Override
        public Object getValueAt(int row, int col) {
            COGInfo line = rows.get(row);
            switch (col) {
            case 0: return line.cog.getID();
            case 1: return line.initialObject.getClassName();
            case 2: {
                boolean first = true;
                StringBuilder sb = new StringBuilder();
                for (TaskInfo t : line.tasks) {
                    if (first) first = false;
                    else sb.append(", ");
                    sb.append(t.task.getID());
                }
                return sb.toString();
            }
            }
            return "ERROR";
        }

        @Override
        public void taskInfoChanged(TaskInfo line) {
        }

        @Override
        public void taskInfoAdded(TaskInfo line) {
        }

        @Override
        public void taskInfoRemoved(TaskInfo line) {
        }

        @Override
        public void cogCreated(COGInfo info) {
            rows.add(info);
            fireTableRowsInserted(rows.size()-1, rows.size()-1);
        }

        @Override
        public void cogChanged(COGInfo info) {
            int row = rows.indexOf(info);
            fireTableRowsUpdated(row, row);
        }
        
    }

}

class DebugWindow implements DebugModelListener  {
    final JFrame frame;
    final JTabbedPane tabs;
    final JButton nextStepBtn;
    final Semaphore sema = new Semaphore(1);
    final TaskControls controls;
    final DebugModel model;
    
    final Map<String, SourceView> windows = new HashMap<String, SourceView>();
    private COGTable cogTable;
    
    DebugWindow(DebugModel model) {
        this.model = model;
        frame = new JFrame("ABS Graphical Debugger");
        frame.setLayout(new BorderLayout());
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        tabs = new JTabbedPane();
        frame.add(tabs, BorderLayout.CENTER);
        
        nextStepBtn = new JButton("Next Step");
        nextStepBtn.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent arg0) {
                sema.release();
            }
        });
        
        frame.add(nextStepBtn,BorderLayout.NORTH);
        
        JPanel leftSide = new JPanel();
        leftSide.setLayout(new BorderLayout());
        frame.add(leftSide, BorderLayout.WEST);
        
        cogTable = new COGTable(model);
        leftSide.add(cogTable,BorderLayout.NORTH);

        controls = new TaskControls(model);
        leftSide.add(controls,BorderLayout.CENTER);
        
        frame.setBounds(400,100,800,800);
        frame.setVisible(true);
        model.registerListener(this);
    }

    private void waitForClick() {
        try {
            sema.acquire();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    private SourceView getSourceView(String fileName) {
        SourceView c = windows.get(fileName);
        if (c == null) {
            c = new SourceView(model,new File(fileName));
            windows.put(fileName, c);
            tabs.addTab(new File(fileName).getName(), c);
        }
        return c;
    }

    @Override
    public void taskInfoChanged(TaskInfo line) {
        waitForClick();
        SourceView view = getSourceView(line.currentFile);
        tabs.setSelectedComponent(view);
    }

    @Override
    public void taskInfoAdded(TaskInfo line) {
    }

    @Override
    public void taskInfoRemoved(TaskInfo line) {
    }

    @Override
    public void cogCreated(COGInfo cog) {
    }

    @Override
    public void cogChanged(COGInfo info) {
    }
}