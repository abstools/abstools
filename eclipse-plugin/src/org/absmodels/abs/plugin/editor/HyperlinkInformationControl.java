package org.absmodels.abs.plugin.editor;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.eclipse.jface.text.AbstractInformationControl;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Composite;

import abs.frontend.ast.MethodImpl;

/**
 * This information control shows a list of different methods 
 * from which the user can select one
 */
public class HyperlinkInformationControl extends AbstractInformationControl implements SelectionListener {

    private List<MethodImpl> implementingMethods;
    private org.eclipse.swt.widgets.List list;
    private ABSEditor editor;
    private boolean firstSelect;


    public HyperlinkInformationControl(ABSEditor editor, String description, List<MethodImpl> implementingMethods) {
        super(editor.getSite().getShell(), description);
        sortByName(implementingMethods);
        this.editor = editor;
        this.implementingMethods = implementingMethods;
        create(); // calls createContent
    }

    private void sortByName(List<MethodImpl> implementingMethods) {
        Collections.sort(implementingMethods, new Comparator<MethodImpl>() {
            @Override
            public int compare(MethodImpl o1, MethodImpl o2) {
                return getDispayedString(o1).compareToIgnoreCase(getDispayedString(o2));
            }
        });
    }
    
    @Override
    protected void createContent(Composite parent) {
        list = new org.eclipse.swt.widgets.List(parent, SWT.SINGLE);
        for (MethodImpl m : implementingMethods) {
            list.add(getDispayedString(m));
        }
        firstSelect = true;
        list.addSelectionListener(this);
        list.setBackground(parent.getBackground());
        list.setForeground(parent.getForeground());
    }

    private String getDispayedString(MethodImpl m) {
        return m.getModuleDecl().getName() + "." + m.getContextDecl().getName();
    }

    @Override
    public void widgetSelected(SelectionEvent e) {
        if (firstSelect) {
            // ignore the first selection event
            firstSelect = false;
            return;
        }
        
        MethodImpl m = implementingMethods.get(list.getSelectionIndex());
        AbsHyperlinkDetector.jumpToPosition(editor, AbsHyperlinkDetector.getPosition(m));
        
        dispose();
    }


    @Override
    public void widgetDefaultSelected(SelectionEvent e) {
        // skip
    }


    @Override
    public boolean hasContents() {
        return true;
    }


    
}
