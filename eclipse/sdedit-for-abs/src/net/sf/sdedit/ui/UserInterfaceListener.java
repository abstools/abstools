package net.sf.sdedit.ui;

import java.io.File;

/**
 * An interface for receivers of call-backs from a UserInterface.
 * 
 * @author Markus Strauch
 */
public interface UserInterfaceListener
{
    /**
     * The code has changed and a new diagram must be drawn.
     * 
     * @param checkSyntaxOnly flag denoting if only syntax should be checked
     * and no diagram should be drawn yet
     */
    public void codeChanged(boolean checkSyntaxOnly);

    /**
     * The current tab shall be closed.
     */
    public void currentTabClosing();

    /**
     * A hyperlink has been clicked. The argument is a string containing a
     * colon, the part before the colon denotes the type of the hyperlink, the
     * part after the colon denotes its name.
     * <p>
     * <ul>
     * <li>example:file.sd The example file file.sd is to be loaded from the
     * examples package</li>
     * </ul>
     * 
     * @param hyperlink
     *            a string containing a colon, the part before the colon denotes
     *            the type of the hyperlink, the part after the colon denotes
     *            its name
     */
    public void hyperlinkClicked(String hyperlink);
    
    public PanelPaintDeviceListener getPanelPaintDeviceListener ();

}
