package abs.backend.java.scheduling;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;

import abs.backend.java.lib.runtime.COG;
import abs.backend.java.scheduling.SimpleTaskScheduler.TaskInfo;



public class InteractiveScheduler implements TotalSchedulingStrategy {
    private SchedulerGUI gui;
    private SchedulerGUISwing guiSwingWrapper;
    
    
    public InteractiveScheduler() {
        gui = new SchedulerGUI(this);
        guiSwingWrapper = SwingWrapperProxy.newInstance(gui, SchedulerGUISwing.class);
    }
    
    @Override
    public ScheduleAction choose(ScheduleOptions options) {
        debug("showing options...");
        guiSwingWrapper.showOptions(options);
        debug("awaiting GUI action...");
        return awaitGUIAction();
    }

    private void debug(String string) {
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
        if (scheduableTasks.size() == 1) {
            return scheduableTasks.get(0);
        } else {
            guiSwingWrapper.chooseTask(scheduler,scheduableTasks);
            return awaitGUITaskChoose();
        }
    }

    public synchronized void setNextTask(TaskInfo i) {
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
    
}


interface SchedulerGUISwing {

    void showOptions(ScheduleOptions options);

    void chooseTask(TaskScheduler scheduler, List<TaskInfo> scheduableTasks);
    
}

class SchedulerGUI implements SchedulerGUISwing {
    final JFrame frame;
    final InteractiveScheduler scheduler;
    JPanel btnPnl;
    
    SchedulerGUI(InteractiveScheduler s) {
        scheduler = s;
        frame = new JFrame("ABS Interactive Scheduler");
        frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        frame.setLayout(new BorderLayout());
        createBtnPnl();
        frame.setBounds(50, 100, 100, 50);
        frame.setVisible(true);
        frame.pack();
    }

    class BtnLine {
        final COG cog;
        final JLabel label;
        final ChooseBtn btn;
        private JDialog taskBtnFrame;
        BtnLine(ScheduleAction a) {
            cog = a.getCOG();
            label = new JLabel("COG "+cog.getID()+" ["+cog.getInitialClass().getSimpleName()+"]");
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
            taskBtnFrame = new TaskBtnDialog(scheduableTasks);
            taskBtnFrame.setVisible(true);
        }
        
        class TaskBtnDialog extends JDialog {
            
            public TaskBtnDialog(List<TaskInfo> scheduableTasks) {
                super(frame,"Schedule Task of COG "+cog.getID(),true);
                setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
                GridBagLayout layout = new GridBagLayout();
                setLayout(layout);
                GridBagConstraints c = new GridBagConstraints();
                c.anchor = GridBagConstraints.WEST;
                c.insets = new Insets(5,5,5,5);
                
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
                setText("Activate Task "+i.task.getID());
            }

            @Override
            public void actionPerformed(ActionEvent arg0) {
                taskBtnFrame.dispose();
                scheduler.setNextTask(info);
            }
            
        }
    }
    
    
    final Map<COG, BtnLine> btnLines = new HashMap<COG, BtnLine>();
    private GridBagLayout btnPnlLayout;
    
    @Override
    public void showOptions(ScheduleOptions options) {
        System.out.println("SchedulerGUI: showing options...");
        int i = 0;

        for (ScheduleAction a : options) {
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
        
        System.out.println("SchedulerGUI: "+i+" options showed");
        frame.pack();
    }

    void createBtnPnl() {
        btnPnl = new JPanel();
        btnPnlLayout = new GridBagLayout();
        btnPnl.setLayout(btnPnlLayout);
        frame.add(btnPnl, BorderLayout.CENTER);
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
            String text = "Schedule Next Task";
            if (a instanceof StepTask) {
                StepTask st = (StepTask) a;
                text = "Step Task " + st.getTask().getID();
            } 
            setText(text);
            setEnabled(true);
        }

        @Override
        public void actionPerformed(ActionEvent arg0) {
            setEnabled(false);
            setText(" - - - - ");
            chooseBtnPressed(action);
        }
    }

    @Override
    public void chooseTask(TaskScheduler scheduler, List<TaskInfo> scheduableTasks) {
        BtnLine l = btnLines.get(scheduler.getCOG());
        l.setScheduleTaskBtns(scheduableTasks);
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
        SwingUtilities.invokeLater(new Runnable() {
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

