package abs.backend.java.scheduling;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;

import abs.backend.java.lib.runtime.COG;
import abs.backend.java.scheduling.SchedulerGUI.ChooseBtn;
import abs.backend.java.scheduling.SimpleTaskScheduler.TaskInfo;



public class InteractiveScheduler implements TotalSchedulingStrategy {
    private final boolean DEBUG = false;
    private SchedulerGUI gui;
    private SchedulerGUISwing guiSwingWrapper;
    private List<HistoryItem> history = new ArrayList<HistoryItem>(0);
    
    
    public InteractiveScheduler() {
        gui = new SchedulerGUI(this);
        guiSwingWrapper = SwingWrapperProxy.newInstance(gui, SchedulerGUISwing.class);
    }
    
    ScheduleOptions options;
    @Override
    public ScheduleAction choose(ScheduleOptions options) {
        debug("showing options...");
        synchronized (this) {
      	  this.options = options;
      	  if (!history.isEmpty()) {
      		  ScheduleAction a = getOptionByHistory();
      		  if (a == null) {
      			  guiSwingWrapper.illegalHistory();
      			  return null;
      		  } else {
      			  guiSwingWrapper.choosedAction(a);
      			  return a;
      		  }
      	  }
        }
        guiSwingWrapper.showOptions(options);
        debug("awaiting GUI action...");
        ScheduleAction a  = awaitGUIAction();
		  guiSwingWrapper.choosedAction(a);
		  return a;
    }

    
    
    private synchronized ScheduleAction getOptionByHistory() {
   	 HistoryItem item = history.remove(0);
   	 for (ScheduleAction a : options.allOptions()) {
   		 if (item.matches(a)) {
   			 return a;
   		 }
   	 }
	   return null;
   }



	private void debug(String string) {
        if (DEBUG)
            System.out.println("InteractiveScheduler: "+string);
    }

    private ScheduleAction nextAction;
    public synchronized ScheduleAction awaitGUIAction() {
        
        while (nextAction == null) {
            try {
                wait();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
        ScheduleAction res = nextAction;
        System.out.println("Chosen action "+nextAction);
        nextAction = null;
        return res;
    }
    
    public synchronized void setNextAction(ScheduleAction a) {
        nextAction = a;
        notify();
    }

    @Override
    public TaskInfo schedule(TaskScheduler scheduler,
            List<TaskInfo> scheduableTasks) {
        System.out.println("Scheduling TASKS");
        if (!history.isEmpty()) {
      	  try {
      		  HistoryItem at = history.remove(0);
      		  for (TaskInfo i : scheduableTasks) {
      			  if (i.task.getID() == at.taskid &&
      					 i.task.getCOG().getID() == at.cogId) {
      		        guiSwingWrapper.activatedTask(i);
      				  return i;
      			  }
      		  }
      		  throw new IllegalStateException();
      	  } catch (Exception e) {
      		  guiSwingWrapper.illegalHistory();
      	  }
        }
        
        TaskInfo task = null;
        if (scheduableTasks.size() == 1) {
            task = scheduableTasks.get(0);
        } else {
            guiSwingWrapper.chooseTask(scheduler,scheduableTasks);
            task = awaitGUITaskChoose();
        }
        guiSwingWrapper.activatedTask(task);
        return task;
    }

    public synchronized void setNextTask(TaskInfo i) {
   	  if (i == null)
   		  throw new IllegalArgumentException("i is null");
        choosenTask = i;
        notify();
    }
    
    private TaskInfo choosenTask;
    private synchronized TaskInfo awaitGUITaskChoose() {
        while (choosenTask == null) {
            try {
                wait();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
        TaskInfo t = choosenTask;
        choosenTask = null;
        return t;
    }

	public synchronized void playHistory(List<HistoryItem> historyItems) {
		history = historyItems;
		nextAction = getOptionByHistory();
		notify();
	}
    
}


interface SchedulerGUISwing {

    void showOptions(ScheduleOptions options);

    void choosedAction(ScheduleAction a);

	void illegalHistory();

	void activatedTask(TaskInfo task);

	void chooseTask(TaskScheduler scheduler, List<TaskInfo> scheduableTasks);
    
}

enum HistoryAction {
	 SCHEDULE, EXECUTE, ACTIVATE;
}

class HistoryItem {
	  int cogId;
	  int taskid;
	  HistoryAction action;

	  HistoryItem(String s) {
		  String[] strings = s.trim().split(",");
		  cogId = Integer.parseInt(strings[0]);
		  String a = strings[1];
		  if (a.equals("S"))
			  action = HistoryAction.SCHEDULE;
		  else if (a.equals("E"))
			  action = HistoryAction.EXECUTE;
		  else if (a.equals("A"))
			  action = HistoryAction.ACTIVATE;
		  
		  if (action != HistoryAction.SCHEDULE) {
			  taskid = Integer.parseInt(strings[2]);
		  }
	  }
	  
	  boolean matches(ScheduleAction a) {
		  boolean res = false;
		  if (cogId != a.getCOG().getID())
			  return false;
		  if ( (a instanceof StepTask && action != HistoryAction.EXECUTE) ||
				 (a instanceof ScheduleTask && action != HistoryAction.SCHEDULE) ||
				 (a instanceof ActivateTask && action != HistoryAction.ACTIVATE))
			  return false;

		  if (action != HistoryAction.SCHEDULE)
			  return a.getTask().getID() == taskid;
		  else 	
			  return true;
	  }
	  
	  
	  
}


class SchedulerGUI implements SchedulerGUISwing {
    final JFrame frame;
    final InteractiveScheduler scheduler;
    JPanel btnPnl;
    JPanel historyPnl;
    JTextArea historyArea;
    List<ScheduleAction> history = new ArrayList<ScheduleAction>();
    JButton saveHistory;
    JButton loadHistory;
    
    SchedulerGUI(InteractiveScheduler s) {
        scheduler = s;
        frame = new JFrame("ABS Interactive Scheduler");
        frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        frame.setLayout(new BorderLayout());
        createBtnPnl();
        createHistoryPnl();
        frame.setBounds(50, 100, 100, 50);
        frame.setVisible(true);
        frame.pack();
    }

    private void createHistoryPnl() {
   	 historyPnl = new JPanel();
   	 historyPnl.setLayout(new BorderLayout());
   	 historyArea = new JTextArea();
   	 historyPnl.add(new JScrollPane(historyArea), BorderLayout.CENTER);
   	 historyPnl.setPreferredSize(new Dimension(300,400));
       historyPnl.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5), "Scheduling History"));
   
       Box btnBox = new Box(BoxLayout.LINE_AXIS);
   	 historyPnl.add(btnBox, BorderLayout.SOUTH);

   	 loadHistory = new JButton("Load History");
   	 loadHistory.addActionListener(new ActionListener() {
         public void actionPerformed(ActionEvent e) {
         	loadHistoryBtnPressed();
         }});
   	 btnBox.add(loadHistory);
   	 btnBox.add(Box.createHorizontalGlue());
   	 saveHistory = new JButton("Save History");
   	 saveHistory.addActionListener(new ActionListener() {
         public void actionPerformed(ActionEvent e) {
         	saveHistoryBtnPressed();
         }});
   	 btnBox.add(saveHistory);
       
   	 frame.add(historyPnl, BorderLayout.SOUTH);
    }
    

	 private void loadHistoryBtnPressed() {
   	 final JFileChooser fc = new JFileChooser(new File(System.getProperty("user.dir")));
   	 if (fc.showOpenDialog(frame) == JFileChooser.APPROVE_OPTION) {
   		 File f = fc.getSelectedFile();
  			 loadHistory(f);
   	 }
   	 
	 }       
    
	 
   private void loadHistory(File f) {
   	try {
	      BufferedReader reader = new BufferedReader(new FileReader(f));
	      List<HistoryItem> historyItems = new ArrayList<HistoryItem>();
	         while (reader.ready()) {
	         	String s = reader.readLine();
	         	if (s == null)
	         		break;

	         	historyItems.add(new HistoryItem(s));
	         }
	      
	      ChooseBtn btn = btnLines.values().iterator().next().btn;
	      btn.updateAction(null);
	      scheduler.playHistory(historyItems);
      } catch (FileNotFoundException e) {
			 JOptionPane.showMessageDialog(frame, "File "+f+" does not exist");
      } catch (Exception e) {
      	JOptionPane.showMessageDialog(frame, "Error while reading history: "+ e.getMessage());
      }
   
   }

	private void saveHistoryBtnPressed() {
   	 final JFileChooser fc = new JFileChooser(new File(System.getProperty("user.dir")));
   	 int returnVal = fc.showSaveDialog(frame);
   	 if (returnVal == JFileChooser.APPROVE_OPTION) {
   		 File f = fc.getSelectedFile();
   		 if (f.exists()) {
   			 int ans = JOptionPane.showConfirmDialog(frame, "File '"+f.getName()+"' already exists, do you want to override it?");
   			 if (ans != JOptionPane.YES_OPTION) {
   				 return;
   			 }
   		 }
			 saveHistory(f);
   	 }

    }

	private void saveHistory(File f) {
		try {
	      PrintWriter p = new PrintWriter(new FileOutputStream(f));
	      for (ScheduleAction a : history) {
	      	p.append(a.shortString());
	      	p.append("\n");
	      }
	      p.close();
	      JOptionPane.showMessageDialog(frame, "History saved");
      } catch (FileNotFoundException e) {
      	JOptionPane.showMessageDialog(frame, "Could not save history: "+e.getMessage());
      }
	}

	class BtnLine {
        final COG cog;
        final JLabel label;
        final ChooseBtn btn;
        boolean isEnabled = true;
        private JDialog taskBtnFrame;
        BtnLine(ScheduleAction a) {
            cog = a.getCOG();
            label = new JLabel("COG "+cog.getID()+" ("+cog.getInitialClass().getSimpleName()+")");
            btn = new ChooseBtn(a);
            GridBagConstraints c = new GridBagConstraints();
            c.gridx = 0;
            c.anchor = GridBagConstraints.WEST;
            c.insets = new Insets(5,5,5,5);
            btnPnl.add(label);
            btnPnl.add(btn);
            btnPnlLayout.setConstraints(label, c);
            c.gridx = 1;
            btnPnlLayout.setConstraints(btn, c);
        }
        
        
        public void setScheduleTaskBtns(List<TaskInfo> scheduableTasks) {
      	   enableBtns(false);
      	   taskBtnFrame = new TaskBtnDialog(scheduableTasks);
            taskBtnFrame.setVisible(true);
        }
        
        class TaskBtnDialog extends JDialog {
            
            public TaskBtnDialog(List<TaskInfo> scheduableTasks) {
                super(frame,"Schedule Task of COG "+cog.getID(),false);
                setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
                GridBagLayout layout = new GridBagLayout();
                setLayout(layout);
                GridBagConstraints c = new GridBagConstraints();
                c.anchor = GridBagConstraints.WEST;
                c.insets = new Insets(5,5,5,5);
                c.gridx = 0;
                
                //btn.setVisible(false);
                for (TaskInfo i : scheduableTasks) {
                    ChooseTaskBtn btn = new ChooseTaskBtn(i);
                    add(btn);
                    layout.setConstraints(btn, c);
                }
                pack();
                setLocation(frame.getX()+frame.getWidth(), frame.getY());
            }
        }

        class ChooseTaskBtn extends JButton implements ActionListener {

            private TaskInfo info;

            public ChooseTaskBtn(TaskInfo i) {
                this.info = i;
                this.addActionListener(this);
                setText("Activate Task "+i.task.getID()+" ("+i.task.methodName()+")");
            }

            @Override
            public void actionPerformed(ActionEvent arg0) {
                taskBtnFrame.dispose();
                scheduler.setNextTask(info);
           	    enableBtns(true);

            }
            
        }
    }
    
    
    final Map<COG, BtnLine> btnLines = new HashMap<COG, BtnLine>();
    private GridBagLayout btnPnlLayout;
    
    @Override
    public synchronized void showOptions(ScheduleOptions options) {
        //System.out.println("SchedulerGUI: showing options...");
        int i = 0;

        for (ScheduleAction a : options.allOptions()) {
            i++;
            COG c = a.getCOG();
            BtnLine line = btnLines.get(c);
            if (line == null) {
                line = new BtnLine(a);
                btnLines.put(c,line);
            } else {
                line.btn.updateAction(a);
            }
        }
        
       /// System.out.println("SchedulerGUI: "+i+" options showed");
        frame.pack();
    }

    void createBtnPnl() {
        btnPnl = new JPanel();
        btnPnlLayout = new GridBagLayout();
        btnPnl.setLayout(btnPnlLayout);
        btnPnl.setBorder(BorderFactory.createTitledBorder(BorderFactory.createCompoundBorder(BorderFactory.createLineBorder(Color.LIGHT_GRAY), BorderFactory.createEmptyBorder(5, 5, 5, 5)), "Scheduling Options"));
        
        frame.add(btnPnl, BorderLayout.CENTER);
    }
    
    void enableBtns(boolean b) {
   	 for (BtnLine line : btnLines.values()) {
  			 line.btn.setEnabled(b && line.btn.action != null);
   	 }
    }
    
    void chooseBtnPressed(ScheduleAction a) {
        scheduler.setNextAction(a);
    }
    
    class ChooseBtn extends JButton implements ActionListener {
        ScheduleAction action;
        
        ChooseBtn(ScheduleAction a ) {
            this.addActionListener(this);
            updateAction(a);
            setPreferredSize(new Dimension(200,getPreferredSize().height));
        }
        
        public void updateAction(ScheduleAction a) {
            action = a;
            updateBtn();
        }
        
        public void updateBtn() {
            if (action == null) {
               setEnabled(false);
               setText(" - - - - ");
            } else {
               String text = "Schedule Next Task";
            	if (action instanceof StepTask) {
            		StepTask st = (StepTask) action;
            		text = "Step Task " + st.getTask().getID() + " ("+st.getTask().methodName()+")";
            	}	 
            	setText(text);
            	setEnabled(true);
            }
        }

        @Override
        public void actionPerformed(ActionEvent arg0) {
            ScheduleAction a = action;
            action = null;
            updateBtn();
            System.out.println("Pressed Button with action "+a);
            chooseBtnPressed(a);
        }
    }

    @Override
    public void chooseTask(TaskScheduler scheduler, List<TaskInfo> scheduableTasks) {
        BtnLine l = btnLines.get(scheduler.getCOG());
        l.setScheduleTaskBtns(scheduableTasks);
    }

	@Override
   public void activatedTask(TaskInfo task) {
		historyArea.append("Activate Task "+task.task.getID()+"\n");
		history.add(new ActivateTask(task.task.getCOG(),task.task));
	}

	@Override
   public void choosedAction(ScheduleAction a) {
		historyArea.append(a.toString()+"\n");
      history.add(a);
   }

	@Override
   public void illegalHistory() {
		JOptionPane.showMessageDialog(frame, "Illegal History!");
	}
}

//needed to allow for access to package members
class SwingWrapperProxy implements InvocationHandler {
    final Object target;
    public SwingWrapperProxy(Object target) {
        this.target = target;
    }
    
    @Override
    public synchronized Object invoke(final Object proxy, final Method method, final Object[] args)
            throws Throwable {
        SwingUtilities.invokeAndWait(new Runnable() {
            @Override
            public void run() {
                try {
                    method.invoke(target, args);
                } catch (Exception e) {
                    e.printStackTrace();
                } 
            }});
        return null;
    }
    
    public static <V> V newInstance(V target, Class<?> interfce) {
        return (V) Proxy.newProxyInstance(SwingWrapperProxy.class.getClassLoader(), new Class[] { interfce}, new SwingWrapperProxy(target));
    }
}

