/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.absunit;

import java.awt.BorderLayout;
import java.awt.CardLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.GridLayout;
import java.lang.reflect.InvocationTargetException;
import java.util.Vector;

import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.ListSelectionModel;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

public class ABSUnitGUI extends JFrame implements TestModelListener {

    /**
     * generated serial number
     */
    private static final long serialVersionUID = -5473702370941393581L;

    /** the two different views of the GUI */
    private final static String FAILURE_LIST_VIEW = "Failure List View";
    private final static String TRACE_VIEW = "Trace View";

    /* JPanel */
    private JPanel cards;
    private final TestModel model;

    private JLabel details;


    private JLabel statLabel;

    private JLabel testRunStatus;

    private Bar bar;

    private JList failureList;

    public ABSUnitGUI(TestModel model) {
        super("ABS Unit");
        setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

        this.model = model;
        model.addTestModelListener(this);

        init();
    }

    private void init() {

        cards = new JPanel(new CardLayout());

        JPanel failureCard = new JPanel();
        failureList = new JList();

        failureList.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
        failureList.setVisibleRowCount(-1);
        JScrollPane listScroller = new JScrollPane(failureList);
        listScroller.setPreferredSize(new Dimension(250, 80));

        failureCard.add(listScroller);


        JPanel traceCard = new JPanel();
        JTextArea traceView = new JTextArea();
        traceView.setEditable(false);
        traceCard.add(traceView, TRACE_VIEW);

        cards.add(failureCard, FAILURE_LIST_VIEW);
        cards.add(traceCard, TRACE_VIEW);

        final JPanel barPanel = new JPanel();
        barPanel.setLayout(new GridLayout(3, 1));
        testRunStatus = new JLabel(getTestRunStatus());
        statLabel = new JLabel(getStatistics());
        bar = new Bar();



        barPanel.add(testRunStatus);
        barPanel.add(bar);
        barPanel.add(statLabel);

        final JPanel detailsPanel = new JPanel();
        detailsPanel.setLayout(new BorderLayout());


        details = new JLabel("") {
            private static final long serialVersionUID = 1320537354431551958L;

            public Dimension getPreferredSize() {
                return new Dimension(100, 100);
            }
            public Dimension getMinimumSize() {
                return new Dimension(100, 100);
            }
        };

        details.setAutoscrolls(true);

        details.setVerticalAlignment(SwingConstants.NORTH);
        details.setHorizontalAlignment(SwingConstants.LEFT);


        JScrollPane detailsScroller = new JScrollPane(details);
        detailsScroller.setPreferredSize(new Dimension(250, 80));

        detailsPanel.add(new JLabel("Details: "), BorderLayout.NORTH);
        detailsPanel.add(new JScrollPane(detailsScroller, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED), BorderLayout.CENTER);


        getContentPane().setLayout(new BorderLayout());
        getContentPane().add(barPanel, BorderLayout.NORTH);
        getContentPane().add(cards, BorderLayout.CENTER);
        getContentPane().add(detailsPanel, BorderLayout.SOUTH);
        pack();
    }

    void updateBarPanels() {
        Runnable guiRun = new Runnable() {
            @Override
            public void run() {
                Vector<TestStatus> failedTests = new Vector<>();
                for (TestStatus status : model.getAllFinishedTests()) {
                    if (status.getStatus() != TestStatus.Status.OK) {
                        failedTests.add(status);
                    }
                }

                failureList.setListData(failedTests);
                failureList.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
                    @Override
                    public void valueChanged(ListSelectionEvent ev) {
                        TestStatus value = (TestStatus) failureList.getSelectedValue();

                        details.setText(value.displayString());
                    }
                });

                testRunStatus.setText(getTestRunStatus());
                statLabel.setText(getStatistics());
                bar.set(model.successful(), model.failed(), model.deadlocked(), model.error(), model.active());

                repaint();
            }
        };
        if (SwingUtilities.isEventDispatchThread()) {
          guiRun.run();
        } else {
            try {
                SwingUtilities.invokeAndWait(guiRun);
            } catch (InterruptedException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            } catch (InvocationTargetException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
    }

    private String getTestRunStatus() {
        return "Tests Running: " + model.active() + "\t\t Tests Finished: " + model.sizeFinished();
    }

    private String getStatistics() {
        return "Passed: " + model.successful() + "\t\t Failures: " + model.failed() + "\t\t Deadlocks: "
                + model.deadlocked() + "\t\t Errors: " + model.error();
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                ABSUnitGUI gui = new ABSUnitGUI(new TestModel());
                gui.pack();
                gui.setVisible(true);
            }
        });
    }

    static class Bar extends JPanel {

        private static final long serialVersionUID = 1555451748887382403L;

        private static Color OK = Color.GREEN;
        private static Color FAILED = Color.RED;
        private static Color ERROR = Color.ORANGE;
        private static Color DEADLOCK = Color.BLUE;
        private static Color RUNNING = Color.GRAY;

        private float all;
        private float ok;
        private float failed;
        private float error;
        private float deadlock;
        private float running;

        Bar() {
        }

        public void set(int ok, int failed, int error, int deadlock, int running) {
            this.ok = ok;
            this.failed = failed;
            this.error = error;
            this.deadlock = deadlock;
            this.running = running;
            all = ok + failed + error + deadlock + running;
        }

        public void paint(Graphics g) {
            final int width = getWidth();
            final int height = getHeight();

            g.setColor(Color.BLACK);
            g.drawRect(0, 0, width, height);
            if (all == 0) {
                return;
            }
            //
            int widthUnit = (int) (width / all);

            int okWidth = Math.round(ok * widthUnit);
            int failedWidth = Math.round(failed * widthUnit);
            int errorWidth = Math.round(error * widthUnit);
            int deadlockWidth = Math.round(deadlock * widthUnit);
            int runningWidth = Math.round(running * widthUnit);

            g.setColor(OK);
            g.fillRect(0, 0, okWidth, height);
            g.setColor(FAILED);
            g.fillRect(okWidth, 0, failedWidth, height);
            g.setColor(ERROR);
            g.fillRect(okWidth + failedWidth, 0, errorWidth, height);
            g.setColor(DEADLOCK);
            g.fillRect(okWidth + failedWidth + errorWidth, 0, deadlockWidth, height);
            g.setColor(RUNNING);
            g.fillRect(okWidth + failedWidth + errorWidth + deadlockWidth, 0, runningWidth, height);
        }

    }

    @Override
    public void testStarted(TestStatus test) {
        updateBarPanels();
    }

    @Override
    public void testFinished(TestStatus test) {
        updateBarPanels();
    }

    @Override
    public void systemFinished() {
        updateBarPanels();
    }
}
