/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.visualization;

import java.awt.BasicStroke;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.util.Arrays;
import java.util.Random;

import javax.swing.JComponent;
import javax.swing.JFrame;

import org.abs_models.backend.java.lib.runtime.ABSException;
import org.abs_models.backend.java.observing.COGView;
import org.abs_models.backend.java.observing.FutView;
import org.abs_models.backend.java.observing.GuardView;
import org.abs_models.backend.java.observing.ObjectView;
import org.abs_models.backend.java.observing.SystemObserver;
import org.abs_models.backend.java.observing.TaskObserver;
import org.abs_models.backend.java.observing.TaskSchedulerObserver;
import org.abs_models.backend.java.observing.TaskStackFrameView;
import org.abs_models.backend.java.observing.TaskView;

public class TaskStateHistoryObserver implements SystemObserver,TaskSchedulerObserver, TaskObserver {

    int[] activeTasks = new int[200];
    int[] suspendedTasks = new int[200];
    int[] readyTasks = new int[200];
    int[] deadlockedTasks = new int[200];
    int[] blockedTasks = new int[200];
    
    int nsteps = 0;
    
    
    private final TaskHistoryViewer gui = new TaskHistoryViewer(this);
    
    @Override
    public void systemStarted() {
        
    }

    @Override
    public void newCOGCreated(COGView cog, ObjectView initialObject) {
        cog.getScheduler().registerTaskSchedulerObserver(this);
    }

    @Override
    public void taskCreated(final TaskView task) {
        if (task != null)
            task.registerTaskListener(this);
       // readyTasks[nsteps] = readyTasks[nsteps]+1;
        readyTasks[nsteps] = readyTasks[nsteps]+1;
    }

    @Override
    public synchronized void taskReady(TaskView view) {
        //readyTasks[nsteps] = readyTasks[nsteps]+1;
        
    }

    @Override
    public synchronized void taskResumed(TaskView runningTask, GuardView view) {
        activeTasks[nsteps]++;
        suspendedTasks[nsteps]--;
    }

    @Override
    public synchronized void taskSuspended(TaskView task, GuardView guard) {
        activeTasks[nsteps]--;
        suspendedTasks[nsteps]++;
         
    }

    @Override
    public synchronized void taskStarted(TaskView task) {
        readyTasks[nsteps] = readyTasks[nsteps]-1;
        activeTasks[nsteps] = activeTasks[nsteps]+1;
        
        System.out.println("Task started : "+activeTasks[nsteps]);
    }

    @Override
    public synchronized void taskFinished(TaskView task) {
        activeTasks[nsteps]--;
        
    }

    @Override
    public synchronized void taskBlockedOnFuture(TaskView task, FutView fut) {
        activeTasks[nsteps]--;
        blockedTasks[nsteps]++;
        
    }

    @Override
    public synchronized void taskRunningAfterWaiting(TaskView view, FutView fut) {
        blockedTasks[nsteps]--;
        activeTasks[nsteps]++;
    }

    @Override
    public synchronized void taskStep(TaskView task, String fileName, int line) {
        nsteps++;
        if (nsteps == activeTasks.length) {
            activeTasks = Arrays.copyOf(activeTasks, nsteps*2);
            blockedTasks = Arrays.copyOf(blockedTasks, nsteps*2);
            suspendedTasks = Arrays.copyOf(suspendedTasks, nsteps*2);
            readyTasks = Arrays.copyOf(readyTasks, nsteps*2);
            deadlockedTasks = Arrays.copyOf(deadlockedTasks, nsteps*2);
        }
        
        activeTasks[nsteps] = activeTasks[nsteps-1];
        blockedTasks[nsteps] = blockedTasks[nsteps-1];
        suspendedTasks[nsteps] = suspendedTasks[nsteps-1];
        readyTasks[nsteps] = readyTasks[nsteps-1];
        deadlockedTasks[nsteps] = deadlockedTasks[nsteps-1];
        
        gui.update();
    }

    @Override
    public void taskDeadlocked(TaskView task) {
        activeTasks[nsteps]--;
        deadlockedTasks[nsteps]++;
        
    }
    
    public static void main(String[] args) {
        test();
    }

    private static void test() {
        TaskStateHistoryObserver o = new TaskStateHistoryObserver();
        Random r = new Random();
        for (int i = 0; i < 400; i++) {
            randomStep(o,r);
        }
    }

    private static void randomStep(TaskStateHistoryObserver o, Random r) {
        if (r.nextBoolean())
            o.taskStep(null, null, 0);
        
        switch (r.nextInt(10)) {
        case 0: o.taskBlockedOnFuture(null, null); break;
        case 1: o.taskCreated(null); break;
        case 2: o.taskDeadlocked(null); break;
        case 3: o.taskFinished(null); break;
        case 4: o.taskReady(null); break;
        case 5: o.taskResumed(null, null); break;
        case 6: o.taskRunningAfterWaiting(null, null); break;
        case 7: o.taskStarted(null); break;
        case 8: o.taskStep(null, null, 0); break;
        case 9: o.taskSuspended(null, null); break;
        }
    }

    @Override
    public void stackFrameCreated(TaskView task, TaskStackFrameView stackFrame) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void localVariableChanged(TaskStackFrameView stackFrame, String name, Object v) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void systemFinished() {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void systemError(ABSException e) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void stackFrameRemoved(TaskView task, TaskStackFrameView oldFrame) {
        // TODO Auto-generated method stub
        
    }

}

class TaskHistoryViewer {
    private JFrame frame;
    private TaskStateHistoryObserver state;
    private Canvas canvas;
    private int drawWidth;
    private int drawHeight;
    
    public TaskHistoryViewer(TaskStateHistoryObserver state) {
        this.state = state;
        canvas = new Canvas();
        canvas.setOpaque(true);
        frame = new JFrame("Task History");
        frame.setLayout(new BorderLayout());
        frame.add(BorderLayout.CENTER, canvas);
        frame.setLocation(400, 0);
        frame.setSize(600,300);
        frame.setVisible(true);
        
    }

    public void update() {
        canvas.repaint();
    }
    
    class Canvas extends JComponent {
        private double xfactor;

        public void updateSizes() {
            drawWidth = (this.getWidth()-2*margin);
            drawHeight = (this.getHeight()-2*margin);
            xfactor = Math.min((double)drawWidth / state.nsteps,8);
        }
        
        @Override
        public void paint(Graphics arg0) {
            updateSizes();
            Graphics2D g = (Graphics2D) arg0;
            g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            
            g.setStroke(new BasicStroke(1));
            g.setBackground(Color.white);
            
            g.setColor(Color.white);
            Rectangle r = g.getClipBounds();
            g.fillRect(r.x, r.y, r.width, r.height);
            
            g.setColor(Color.lightGray);
            g.drawRect(margin,margin, drawWidth, drawHeight);
            int xliney = margin;
            for (int x = 0; x <= state.nsteps/10; x++) {
                int xstep = (int) (x*10*xfactor);
                g.drawLine(xstep, xliney-5 , xstep, xliney+5);
            }
            
            
            g.setStroke(new BasicStroke(2));
            synchronized (state) {
                drawArray(g, state.activeTasks, Color.GREEN);
                drawArray(g, state.blockedTasks, Color.RED);
                drawArray(g, state.suspendedTasks, Color.ORANGE);
                drawArray(g, state.deadlockedTasks, Color.BLACK);
                drawArray(g, state.readyTasks, Color.YELLOW);
            }
        }

        int margin = 20;
        
        private void drawArray(Graphics2D g, int[] tasks, Color lineColor) {
            g.setColor(lineColor);
            int n = state.nsteps;
            
            int lastx = margin;
            int lasty = drawHeight-margin;
            int fac = 4;
            for (int x = 0; x < n; x++) {
                int newx = margin+(int)(xfactor*(x+1));
                int newy = drawHeight+margin-(fac*tasks[x]); 
                
                g.drawLine(lastx,lasty,newx,newy); 
                lastx = newx;
                lasty = newy;
                
            }
        }
    }
    
}
