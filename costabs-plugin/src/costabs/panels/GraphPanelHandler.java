package costabs.panels;

import java.awt.CardLayout;

import javax.swing.JPanel;

import org.eclipse.core.resources.IFile;

import costabs.awt.EmbeddedSwingComposite;
import costabs.trackers.OutputManager;

public class GraphPanelHandler extends JPanel {

	public static String EMPTY = "emptypanel";

	public GraphPanelHandler(EmbeddedSwingComposite frame) {
		super();
		this.setLayout(new CardLayout());
		JPanel empty = new JPanel();
		add(empty, EMPTY);
		showEmptyPanel(EMPTY);

//		TestPanel panel = new TestPanel();
//		add(panel,EMPTY);
//		showEmptyPanel(EMPTY);
	}

	public void showPanel(IFile file) {
		String id = file.getFullPath().toOSString();
		if (OutputManager.getInstance().getOutputTracker(file) != null) {
			JPanel newpanel = new GraphPanel(file, OutputManager.getInstance()
					.getOutputTracker(file));
			add(newpanel, id);
		} else {
			id = EMPTY;
		}

		CardLayout cl = (CardLayout) (getLayout());
		cl.show(this, id);

	}

	public void showEmptyPanel(String id) {
		CardLayout cl = (CardLayout) (getLayout());
		cl.show(this, EMPTY);
	}

}
