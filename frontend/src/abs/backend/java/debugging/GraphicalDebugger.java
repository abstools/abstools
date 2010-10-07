package abs.backend.java.debugging;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Semaphore;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.PlainDocument;

import abs.backend.java.observing.TaskView;

public class GraphicalDebugger implements Debugger {
    DebugWindow window;
    
    public GraphicalDebugger() {
        window = new DebugWindow();
    }
    
    
    @Override
    public void nextStep(TaskView taskView, String fileName, int line) {
        window.activateLine(fileName,line);
    }

    // only for ad-hoc testing
    public static void main(String[] args) {
        new GraphicalDebugger();
    }
}

class Line {
    int startPos;
    int endPos;
}

class SourceView extends JPanel {
    private static final long serialVersionUID = 1L;
    
    JTextArea textArea;
    File file;
    
    SourceView(File file) {
        textArea = new JTextArea();
        
        this.setLayout(new BorderLayout());
        this.add(new JScrollPane(textArea),BorderLayout.CENTER);
        this.file = file;
        
        fillArea();
    }
    
    private void fillArea() {
        try {
            int lineNo = 1;
            BufferedReader reader = new BufferedReader(new FileReader(file));
            while (reader.ready()) {
                String line = reader.readLine();
                if (line == null)
                    break;
                textArea.append(lineNo+": "+line+"\n");
                lineNo++;
            }
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
        
        textArea.setCaretPosition(0);
        
    }

    public void highlightLine(int line) {
        try {
            int start = textArea.getLineStartOffset(line-1);
            int end = textArea.getLineEndOffset(line-1);
            textArea.setSelectionStart(start);
            textArea.setSelectionEnd(end);
            textArea.setSelectionColor(Color.gray);
            textArea.repaint();
            System.out.println("start : "+start+" end: "+end);
        } catch (BadLocationException e) {
            e.printStackTrace();
        }
    }
}

class DebugWindow  {
    JFrame frame;
    JTabbedPane tabs;
    JButton nextStepBtn;
    Semaphore sema = new Semaphore(1);

    
    final Map<String, SourceView> windows = new HashMap<String, SourceView>();
    
    DebugWindow() {
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
        
        frame.setBounds(400,100,600,800);
        frame.setVisible(true);
    }


    public void activateLine(String fileName, int line) {
        try {
            sema.acquire();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        SourceView view = getSourceView(fileName);
        tabs.setSelectedComponent(view);
        view.highlightLine(line);
    }


    private SourceView getSourceView(String fileName) {
        SourceView c = windows.get(fileName);
        if (c == null) {
            c = new SourceView(new File(fileName));
            windows.put(fileName, c);
            tabs.addTab(new File(fileName).getName(), c);
        }
        return c;
    }
}