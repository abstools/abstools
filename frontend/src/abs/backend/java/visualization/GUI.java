/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.visualization;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.concurrent.Semaphore;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;

@SuppressWarnings("serial")
public class GUI extends JFrame implements ActionListener {
    JButton stepBtn;
    private JButton runBtn;

    volatile boolean stepping = true;
    Semaphore sema;

    public GUI() {
        super("Trading System");
        sema = new Semaphore(1);
        try {
            sema.acquire();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        stepBtn = new JButton("Next Step");
        runBtn = new JButton("Run");
        JPanel panel = new JPanel();
        panel.setLayout(new BorderLayout());
        panel.add(stepBtn, BorderLayout.CENTER);
        panel.add(runBtn, BorderLayout.EAST);
        getContentPane().add(panel, BorderLayout.CENTER);
        stepBtn.addActionListener(this);
        runBtn.addActionListener(this);
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setLocation(10, 10);
        setSize(300, 300);
        pack();
        setVisible(true);

    }

    @Override
    public void actionPerformed(ActionEvent e) {
        if (e.getSource() == stepBtn) {
            sema.release();
        } else if (e.getSource() == runBtn) {
            stepping = false;
            sema.release(Integer.MAX_VALUE);
        }
    }

    public void waitForClick() {
        try {
            if (stepping)
                sema.acquire();
        } catch (InterruptedException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    public static void main(String... args) {
        javax.swing.SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                new GUI();
            }
        });

    }

}
