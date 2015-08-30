/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.scheduling;

import java.awt.BorderLayout;
import java.awt.CardLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import abs.backend.java.lib.runtime.ABSRuntime;
import abs.backend.java.lib.runtime.COG;
import abs.backend.java.lib.runtime.Logging;
import abs.backend.java.scheduling.SimpleTaskScheduler.TaskInfo;
import abs.backend.java.utils.ColorUtils;

public class InteractiveScheduler implements TotalSchedulingStrategy {
    private static final Logger log = Logging.getLogger(ABSRuntime.class.getName());

    private final SchedulerGUI gui;
    private final SchedulerGUISwing guiSwingWrapper;
    private final SchedulingObserver obs;

    // synchronized by this
    private TotalSchedulingStrategy directScheduler;

    public InteractiveScheduler() {
        gui = new SchedulerGUI(this);
        guiSwingWrapper = SwingWrapperProxy.newBlockingInstance(gui, SchedulerGUISwing.class);
        obs = SwingWrapperProxy.newAsyncInstance(gui, SchedulingObserver.class);
    }

    public synchronized void setDirectScheduler(TotalSchedulingStrategy s) {
        directScheduler = s;
    }

    public synchronized TotalSchedulingStrategy getDirectScheduler() {
        return directScheduler;
    }

    private volatile ScheduleOptions lastOptions;

    @Override
    public ScheduleAction choose(ScheduleOptions options) {
        log.finest("showing options...");
        ScheduleAction a = null;
        lastOptions = options;
        TotalSchedulingStrategy ds = getDirectScheduler();
        if (ds != null) {
            a = ds.choose(options);
        } else {
            guiSwingWrapper.showOptions(options);
            log.finest("awaiting GUI action...");
            a = awaitGUIAction();
        }

        obs.choosedAction(a);
        return a;
    }

    public ScheduleOptions getLastOptions() {
        return lastOptions;
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
        System.out.println("Chosen action " + nextAction);
        nextAction = null;
        return res;
    }

    public synchronized void setNextAction(ScheduleAction a) {
        nextAction = a;
        notify();
    }

    @Override
    public TaskInfo schedule(TaskScheduler scheduler, List<TaskInfo> scheduableTasks) {

        TotalSchedulingStrategy ds = getDirectScheduler();
        TaskInfo task = null;

        if (ds != null) {
            task = ds.schedule(scheduler, scheduableTasks);
        } else {
            guiSwingWrapper.chooseTask(scheduler, scheduableTasks);
            task = awaitGUITaskChoose();
        }
        obs.activatedTask(task);
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

}

interface SchedulerGUISwing {

    void showOptions(ScheduleOptions options);

    void chooseTask(TaskScheduler scheduler, List<TaskInfo> scheduableTasks);

}





class InteractiveOptionPnl extends JPanel implements SchedulerGUISwing {
    private GridBagLayout layout;
    final InteractiveScheduler scheduler;
    private JFrame frame;

    public InteractiveOptionPnl(JFrame frame, InteractiveScheduler s) {
        this.frame = frame;
        scheduler = s;
        layout = new GridBagLayout();
        setLayout(layout);

    }

    class BtnLine {
        final COG cog;
        final JLabel label;
        final ChooseBtn btn;
        boolean isEnabled = true;
        private JDialog taskBtnFrame;

        BtnLine(ScheduleAction a) {
            cog = a.getCOG();
            label = new JLabel("COG " + cog.getID());
            label.setFont(label.getFont().deriveFont(Font.PLAIN));
            btn = new ChooseBtn(a);
            btn.setFont(btn.getFont().deriveFont(Font.PLAIN));

            GridBagConstraints c = new GridBagConstraints();
            c.gridx = 0;
            c.anchor = GridBagConstraints.WEST;
            c.insets = new Insets(5, 5, 5, 5);
            add(label);
            add(btn);
            layout.setConstraints(label, c);
            c.gridx = 1;
            layout.setConstraints(btn, c);
        }

        public void setScheduleTaskBtns(List<TaskInfo> scheduableTasks) {
            enableBtns(false);
            taskBtnFrame = new TaskBtnDialog(scheduableTasks);
            taskBtnFrame.setVisible(true);
        }

        class TaskBtnDialog extends JDialog {

            public TaskBtnDialog(List<TaskInfo> scheduableTasks) {
                super(frame, "Schedule Task of COG " + cog.getID(), false);
                setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
                GridBagLayout layout = new GridBagLayout();
                setLayout(layout);
                GridBagConstraints c = new GridBagConstraints();
                c.anchor = GridBagConstraints.WEST;
                c.insets = new Insets(5, 5, 5, 5);
                c.gridx = 0;

                // btn.setVisible(false);
                for (TaskInfo i : scheduableTasks) {
                    ChooseTaskBtn btn = new ChooseTaskBtn(i);
                    add(btn);
                    layout.setConstraints(btn, c);
                }
                pack();
                setLocation(frame.getX() + frame.getWidth(), frame.getY());
            }
        }

        class ChooseTaskBtn extends JButton implements ActionListener {

            private TaskInfo info;

            public ChooseTaskBtn(TaskInfo i) {
                this.info = i;
                this.addActionListener(this);
                setText("Activate Task " + i.task.getID() + " (" + i.task.getCall().methodName() + ")");
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

    @Override
    public synchronized void showOptions(ScheduleOptions options) {
        // System.out.println("SchedulerGUI: showing options...");
        int i = 0;
        for (BtnLine l : btnLines.values()) {
            l.btn.updateAction(null);
        }

        for (ScheduleAction a : options.allOptions()) {
            i++;
            COG c = a.getCOG();
            BtnLine line = btnLines.get(c);
            if (line == null) {
                line = new BtnLine(a);
                btnLines.put(c, line);
            } else {
                line.btn.updateAction(a);
            }
        }

        // / System.out.println("SchedulerGUI: "+i+" options showed");
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

        ChooseBtn(ScheduleAction a) {
            this.addActionListener(this);
            updateAction(a);
            setPreferredSize(new Dimension(200, getPreferredSize().height));
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
                    text = "Step Task " + st.getTask().getID() + " (" + st.getTask().getMethodName() + ")";
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
            chooseBtnPressed(a);
        }
    }

    @Override
    public void chooseTask(TaskScheduler scheduler, List<TaskInfo> scheduableTasks) {
        if (scheduableTasks.size() == 1) {
            this.scheduler.setNextTask(scheduableTasks.get(0));
        } else {
            BtnLine l = btnLines.get(scheduler.getCOG());
            l.setScheduleTaskBtns(scheduableTasks);
        }
    }

}

class SchedulerChoosePnl extends JPanel implements ItemListener {
    public static String INTERACTIVE = "Interactive";
    public static String REPLAY = "Replay";
    public static String RANDOM = "Random";

    private JComboBox schedulerComboBox;

    private JPanel choosePnl;
    private SchedulerGUI schedulerGUI;

    public SchedulerChoosePnl(SchedulerGUI gui) {
        schedulerGUI = gui;
        setLayout(new BorderLayout());

        createChoosePnl();
    }

    private void createChoosePnl() {
        choosePnl = new JPanel();
        choosePnl.setLayout(new BorderLayout());
        choosePnl.setBorder(BorderFactory.createTitledBorder(
                BorderFactory.createCompoundBorder(BorderFactory.createLineBorder(Color.LIGHT_GRAY),
                        BorderFactory.createEmptyBorder(5, 5, 5, 5)), "Scheduler"));
        schedulerComboBox = new JComboBox();
        choosePnl.add(schedulerComboBox, BorderLayout.CENTER);

        schedulerComboBox.addItem(INTERACTIVE);
        schedulerComboBox.addItem(REPLAY);
        schedulerComboBox.addItem(RANDOM);

        schedulerComboBox.addItemListener(this);

        add(choosePnl, BorderLayout.NORTH);
    }

    @Override
    public void itemStateChanged(ItemEvent e) {
        if (e.getItem() == RANDOM) {
            schedulerGUI.setRandomScheduler();
        } else if (e.getItem() == REPLAY) {
            schedulerGUI.setReplayScheduler();
        } else {
            schedulerGUI.setInteractiveScheduler();
        }
    }

}

class HistoryPnl extends JPanel {
    JTextArea historyArea;
    List<ScheduleAction> history = new ArrayList<ScheduleAction>();
    JButton saveHistory;
    private final SchedulerGUI schedulerGUI;

    public HistoryPnl(SchedulerGUI gui) {
        this.schedulerGUI = gui;
        setLayout(new BorderLayout());
        historyArea = new JTextArea();
        add(new JScrollPane(historyArea), BorderLayout.CENTER);
        setPreferredSize(new Dimension(300, 400));
        setBorder(BorderFactory.createTitledBorder(
                BorderFactory.createCompoundBorder(BorderFactory.createLineBorder(Color.LIGHT_GRAY),
                        BorderFactory.createEmptyBorder(5, 5, 5, 5)), "Scheduling History"));

        Box btnBox = new Box(BoxLayout.LINE_AXIS);
        add(btnBox, BorderLayout.SOUTH);

        btnBox.add(Box.createHorizontalGlue());
        saveHistory = new JButton("Save History");
        saveHistory.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                saveHistoryBtnPressed();
            }
        });
        btnBox.add(saveHistory);

    }

    void saveHistoryBtnPressed() {
        final JFileChooser fc = new JFileChooser(new File(System.getProperty("user.dir")));
        int returnVal = fc.showSaveDialog(schedulerGUI.frame);
        if (returnVal == JFileChooser.APPROVE_OPTION) {
            File f = fc.getSelectedFile();
            if (f.exists()) {
                int ans = JOptionPane.showConfirmDialog(schedulerGUI.frame, "File '" + f.getName()
                        + "' already exists, do you want to override it?");
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
            JOptionPane.showMessageDialog(schedulerGUI.frame, "History saved");
        } catch (FileNotFoundException e) {
            JOptionPane.showMessageDialog(schedulerGUI.frame, "Could not save history: " + e.getMessage());
        }
    }

    public void activatedTask(TaskInfo task) {
        historyArea.append("Activate Task " + task.task.getID() + "\n");
        history.add(new ActivateTask(task.task.getCOG(), task.task));
    }

    public void choosedAction(ScheduleAction a) {
        historyArea.append(a.toString() + "\n");
        history.add(a);
    }

}

interface SchedulingObserver {
    void choosedAction(ScheduleAction a);

    void activatedTask(TaskInfo task);

}

class ReplayOptionPnl extends JPanel implements SchedulerGUISwing, TotalSchedulingStrategy {
    JButton loadHistoryBtn;
    private InteractiveScheduler scheduler;
    private List<HistoryItem> history = new ArrayList<HistoryItem>(0);
    private ScheduleOptions options;

    public ReplayOptionPnl(InteractiveScheduler scheduler) {
        this.scheduler = scheduler;
        this.setLayout(new BorderLayout());
        loadHistoryBtn = new JButton("Replay History");
        loadHistoryBtn.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                loadHistoryBtnPressed();
            }
        });
        add(loadHistoryBtn, BorderLayout.PAGE_START);

    }

    void loadHistoryBtnPressed() {
        final JFileChooser fc = new JFileChooser(new File(System.getProperty("user.dir")));
        if (fc.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
            File f = fc.getSelectedFile();
            loadHistory(f);
        }

    }

    private void loadHistory(File f) {
        try {
            history = HistoryItem.loadHistory(f);

            scheduler.setDirectScheduler(this);
            scheduler.setNextAction(getOptionByHistory());

        } catch (FileNotFoundException e) {
            JOptionPane.showMessageDialog(this, "File " + f + " does not exist");
        } catch (IOException e) {
            JOptionPane.showMessageDialog(this, "Error while reading history: " + e.getMessage());
        }

    }

    @Override
    public synchronized void showOptions(ScheduleOptions options) {
        this.options = options;
    }

    private ScheduleAction getOptionByHistory() {
        HistoryItem item = history.remove(0);
        for (ScheduleAction a : options.allOptions()) {
            if (item.matches(a)) {
                return a;
            }
        }
        JOptionPane.showMessageDialog(this, "Illegal History!");
        return null;
    }

    @Override
    public void chooseTask(TaskScheduler s, List<TaskInfo> scheduableTasks) {
        TaskInfo i = getNextTask(scheduableTasks);
        if (i != null)
            scheduler.setNextTask(i);
    }

    private TaskInfo getNextTask(List<TaskInfo> scheduableTasks) {
        if (!history.isEmpty()) {
            try {
                HistoryItem at = history.remove(0);
                if (at.action != HistoryAction.ACTIVATE)
                    throw new IllegalStateException("Task action action expected!");

                for (TaskInfo i : scheduableTasks) {
                    if (i.task.getID() == at.taskid && i.task.getCOG().getID() == at.cogId) {

                        return i;
                    }
                }
                throw new IllegalStateException("Task " + at.taskid + " cannot be scheduled.");
            } catch (Exception e) {
                e.printStackTrace();
                JOptionPane.showMessageDialog(this, "Illegal History! " + e.getMessage());
            }
        }
        return null;
    }

    @Override
    public ScheduleAction choose(ScheduleOptions options) {
        ScheduleAction a = getOptionByHistory();
        if (history.isEmpty()) {
            scheduler.setDirectScheduler(null);
        }
        return a;
    }

    @Override
    public TaskInfo schedule(TaskScheduler taskScheduler, List<TaskInfo> scheduableTasks) {

        TaskInfo i = getNextTask(scheduableTasks);
        if (history.isEmpty()) {
            scheduler.setDirectScheduler(null);
        }
        return i;
    }

}

class RandomOptionPnl extends JPanel implements SchedulerGUISwing, TotalSchedulingStrategy {
    private final JButton nextStepBtn;
    private final JButton nextNStepBtn;
    private final JButton runBtn;
    private long seed = System.nanoTime();
    private final Random r = new Random(seed);
    private final RandomSchedulingStrategy globalStrat = new RandomSchedulingStrategy(r);
    private final RandomSchedulingStrategy taskStrat = globalStrat;
    private final InteractiveScheduler scheduler;
    private TaskInfo nextTask;
    private GridBagLayout gridBagLayout;
    private JTextField seedField;
    private boolean isRunning;

    RandomOptionPnl(InteractiveScheduler s) {
        this.scheduler = s;
        gridBagLayout = new GridBagLayout();
        setLayout(gridBagLayout);
        GridBagConstraints c = new GridBagConstraints();
        c.gridx = 0;
        c.anchor = GridBagConstraints.EAST;
        c.insets = new Insets(5, 5, 5, 5);

        JLabel lbl = new JLabel("Random Seed: ");
        lbl.setFont(lbl.getFont().deriveFont(Font.PLAIN));
        add(lbl);
        gridBagLayout.setConstraints(lbl, c);

        seedField = new JTextField();
        add(seedField);
        seedField.getDocument().addDocumentListener(new DocumentListener() {
            @Override
            public void insertUpdate(DocumentEvent e) {
                seedChanged();
            }

            @Override
            public void removeUpdate(DocumentEvent e) {
                seedChanged();
            }

            @Override
            public void changedUpdate(DocumentEvent e) {
                seedChanged();
            }
        });

        c.gridx = 1;
        c.anchor = GridBagConstraints.CENTER;
        c.fill = GridBagConstraints.HORIZONTAL;
        gridBagLayout.setConstraints(seedField, c);
        seedField.setPreferredSize(new Dimension(150, seedField.getPreferredSize().height));
        seedField.setText("" + seed);

        nextStepBtn = new JButton("Single Random Step");
        nextStepBtn.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                nextStepClicked();
            }
        });
        add(nextStepBtn);
        c.gridx = 1;
        c.gridy = 1;
        gridBagLayout.setConstraints(nextStepBtn, c);

        nextNStepBtn = new JButton("N Random Steps");
        nextNStepBtn.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                nextNStepClicked();
            }
        });
        add(nextNStepBtn);
        c.gridy++;
        gridBagLayout.setConstraints(nextNStepBtn, c);

        runBtn = new JButton("Run");
        runBtn.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                runClicked();
            }
        });
        add(runBtn);
        c.gridy++;
        gridBagLayout.setConstraints(runBtn, c);

    }

    private void runClicked() {
        isRunning = true;
        nextStepClicked();

    }

    private void seedChanged() {
        try {
            long s = Long.parseLong(seedField.getText());
            seed = s;
            r.setSeed(seed);
            System.out.println("Seed Changed to " + seedField.getText());
            seedField.setBackground(Color.WHITE);
        } catch (Exception e) {
            seedField.setBackground(ColorUtils.setSaturation(Color.RED, 0.5f));
        }
    }

    long remaindingSteps = 0;
    private ScheduleOptions options;

    private void nextNStepClicked() {
        String steps = JOptionPane.showInputDialog(this, "Number of steps");
        try {
            remaindingSteps = Long.parseLong(steps);
            nextStepClicked();
        } catch (NumberFormatException e) {
            JOptionPane.showMessageDialog(this, steps + " is not a legal number");
        }
    }

    private void nextStepClicked() {

        nextNStepBtn.setEnabled(false);
        nextStepBtn.setEnabled(false);
        runBtn.setEnabled(false);

        ScheduleAction a = nextAction();
        if (a != null) {
            scheduler.setDirectScheduler(this);
            scheduler.setNextAction(a);
        }
    }

    private synchronized ScheduleAction nextAction() {
        if (remaindingSteps > 0)
            remaindingSteps--;
        if (!options.isEmpty()) {
            return globalStrat.choose(options);
        }
        return null;
    }

    @Override
    public synchronized void showOptions(ScheduleOptions options) {
        this.options = options;
        if (isRunning || remaindingSteps > 0) {
            nextStepClicked();
        } else {
            nextStepBtn.setEnabled(true);
            nextNStepBtn.setEnabled(true);
            runBtn.setEnabled(true);
        }
    }

    @Override
    public synchronized void chooseTask(TaskScheduler s, List<TaskInfo> scheduableTasks) {
        nextTask = taskStrat.schedule(s, scheduableTasks);
        scheduler.setNextTask(nextTask);
    }

    @Override
    public synchronized ScheduleAction choose(ScheduleOptions options) {
        ScheduleAction a = nextAction();
        if (remaindingSteps == 0 && !isRunning) {
            scheduler.setDirectScheduler(null);
        }

        return a;
    }

    @Override
    public synchronized TaskInfo schedule(TaskScheduler scheduler, List<TaskInfo> scheduableTasks) {
        return taskStrat.schedule(scheduler, scheduableTasks);
    }
}

class SchedulerGUI implements SchedulerGUISwing, SchedulingObserver {
    final JFrame frame;
    final InteractiveScheduler scheduler;
    JPanel btnPnl;
    HistoryPnl historyPnl;
    private InteractiveOptionPnl interactiveOptionPnl;
    JPanel optionsPnl;

    SchedulerGUISwing currentScheduler;
    SchedulerChoosePnl schedulerChoosePnl;
    private CardLayout cardLayout;
    private RandomOptionPnl randomOptionPnl;
    private ReplayOptionPnl replayOptionPnl;

    SchedulerGUI(InteractiveScheduler s) {
        scheduler = s;
        frame = new JFrame("ABS Interactive Scheduler");
        if (ABSRuntime.runsInOwnProcess())
            frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        else
            frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        frame.setLayout(new BorderLayout());

        schedulerChoosePnl = new SchedulerChoosePnl(this);
        frame.add(schedulerChoosePnl, BorderLayout.NORTH);

        optionsPnl = new JPanel();
        cardLayout = new CardLayout();
        optionsPnl.setLayout(cardLayout);
        optionsPnl.setBorder(BorderFactory.createTitledBorder(
                BorderFactory.createCompoundBorder(BorderFactory.createLineBorder(Color.LIGHT_GRAY),
                        BorderFactory.createEmptyBorder(5, 5, 5, 5)), "Scheduling Options"));

        // optionsPnl.setPreferredSize(new Dimension(300,300));

        frame.add(optionsPnl, BorderLayout.CENTER);

        interactiveOptionPnl = new InteractiveOptionPnl(frame, scheduler);
        optionsPnl.add(interactiveOptionPnl, SchedulerChoosePnl.INTERACTIVE);

        replayOptionPnl = new ReplayOptionPnl(scheduler);
        optionsPnl.add(replayOptionPnl, SchedulerChoosePnl.REPLAY);

        randomOptionPnl = new RandomOptionPnl(scheduler);
        optionsPnl.add(randomOptionPnl, SchedulerChoosePnl.RANDOM);

        setInteractiveScheduler();

        historyPnl = new HistoryPnl(this);
        frame.add(historyPnl, BorderLayout.SOUTH);

        frame.setBounds(0, 0, 100, 50);
        frame.setVisible(true);
        frame.pack();
    }

    void setRandomScheduler() {
        setScheduler(randomOptionPnl, SchedulerChoosePnl.RANDOM);
    }

    private void setScheduler(SchedulerGUISwing swingScheduler, String name) {
        cardLayout.show(optionsPnl, name);
        currentScheduler = swingScheduler;
        ScheduleOptions lastOptions = scheduler.getLastOptions();
        if (lastOptions != null)
            currentScheduler.showOptions(lastOptions);
        frame.pack();
    }

    void setReplayScheduler() {
        setScheduler(replayOptionPnl, SchedulerChoosePnl.REPLAY);
    }

    void setInteractiveScheduler() {
        setScheduler(interactiveOptionPnl, SchedulerChoosePnl.INTERACTIVE);
    }

    @Override
    public void activatedTask(TaskInfo task) {
        historyPnl.activatedTask(task);
    }

    @Override
    public void choosedAction(ScheduleAction a) {
        historyPnl.choosedAction(a);
    }

    @Override
    public void showOptions(ScheduleOptions options) {
        currentScheduler.showOptions(options);
        frame.pack();
    }

    @Override
    public void chooseTask(TaskScheduler scheduler, List<TaskInfo> scheduableTasks) {
        currentScheduler.chooseTask(scheduler, scheduableTasks);
    }
}

// needed to allow for access to package members
class SwingWrapperProxy implements InvocationHandler {
    final Object target;
    volatile Object result;
    private boolean block;

    public SwingWrapperProxy(Object target, boolean block) {
        this.target = target;
        this.block = block;
    }

    @Override
    public synchronized Object invoke(final Object proxy, final Method method, final Object[] args) throws Throwable {
        Runnable r = new Runnable() {
            @Override
            public void run() {
                try {
                    result = method.invoke(target, args);
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        };
        if (block) {
            SwingUtilities.invokeAndWait(r);
            return result;
        } else {
            SwingUtilities.invokeLater(r);
            return null;
        }
    }

    public static <V> V newInstance(V target, Class<?> interfce, boolean block) {
        return (V) Proxy.newProxyInstance(SwingWrapperProxy.class.getClassLoader(), new Class[] { interfce },
                new SwingWrapperProxy(target, block));
    }

    public static <V> V newBlockingInstance(V target, Class<?> interfce) {
        return newInstance(target, interfce, true);
    }

    public static <V> V newAsyncInstance(V target, Class<?> interfce) {
        return newInstance(target, interfce, false);
    }
}
