package costabs.awt;

import java.awt.Adjustable;
import java.awt.BorderLayout;
import java.awt.Frame;
import java.awt.event.AdjustmentEvent;
import java.awt.event.AdjustmentListener;

import javax.swing.BoundedRangeModel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Slider;

/**
 * @author Henning Rogge
 */
public class ScrollableSwingComposite extends Composite {
	protected Composite embedded = null;
	protected Slider horizontalSlider = null;
	protected Slider verticalSlider = null;

	protected Frame swingFrame = null;
	protected JPanel swingMasterPanel = null;
	protected JScrollPane swingScrollPane = null;

	static int checkStyle(int style) {
		style = style & (~(SWT.V_SCROLL | SWT.H_SCROLL | SWT.EMBEDDED));
		return style;
	}

	public ScrollableSwingComposite(Composite parent, int style) {
		super(parent, checkStyle(style));
		boolean horizontal = (style & SWT.H_SCROLL) != 0;
		boolean vertical = (style & SWT.V_SCROLL) != 0;

		GridLayout layout = new GridLayout(vertical ? 2 : 1, false);
		layout.horizontalSpacing = 0;
		layout.verticalSpacing = 0;
		layout.marginWidth = 0;
		layout.marginHeight = 0;
		setLayout(layout);

		embedded = new Composite(this, SWT.EMBEDDED);
		embedded.setLayoutData(new GridData(GridData.FILL_BOTH));

		swingFrame = SWT_AWT.new_Frame(embedded);

		swingMasterPanel = new JPanel(new BorderLayout());
		swingFrame.add(swingMasterPanel);

		swingScrollPane = new JScrollPane();
		swingScrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
		swingScrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
		swingScrollPane.setDoubleBuffered(true);

		swingMasterPanel.add(swingScrollPane, BorderLayout.CENTER);

		if (vertical) {
			verticalSlider = new Slider(this, SWT.VERTICAL);
			verticalSlider.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_CENTER | GridData.FILL_VERTICAL));

			BoundedRangeModel model = swingScrollPane.getVerticalScrollBar().getModel();
			verticalSlider.setMinimum(model.getMinimum());
			verticalSlider.setMaximum(model.getMaximum());
			verticalSlider.setThumb(model.getExtent());

			verticalSlider.addSelectionListener(new VerticalSliderListener());
			swingScrollPane.getVerticalScrollBar().addAdjustmentListener(new JScrollBarListener(verticalSlider));
		}

		if (horizontal) {
			horizontalSlider = new Slider(this, SWT.HORIZONTAL);
			horizontalSlider.setLayoutData(new GridData(GridData.FILL_HORIZONTAL | GridData.VERTICAL_ALIGN_FILL));

			BoundedRangeModel model = swingScrollPane.getHorizontalScrollBar().getModel();
			horizontalSlider.setMinimum(model.getMinimum());
			horizontalSlider.setMaximum(model.getMaximum());
			horizontalSlider.setThumb(model.getExtent());

			horizontalSlider.addSelectionListener(new HorizontalSliderListener());
			swingScrollPane.getHorizontalScrollBar().addAdjustmentListener(new JScrollBarListener(horizontalSlider));
		}
	}

	public Frame getFrame() {
		return swingFrame;
	}

	public JScrollPane getJScrollPane() {
		return swingScrollPane;
	}

	private class HorizontalSliderListener extends SelectionAdapter {
		public void widgetSelected(SelectionEvent e) {
			swingScrollPane.getHorizontalScrollBar().setValue(horizontalSlider.getSelection());
			swingScrollPane.repaint();
		}
	}
	private class VerticalSliderListener extends SelectionAdapter {
		public void widgetSelected(SelectionEvent e) {
			swingScrollPane.getVerticalScrollBar().setValue(verticalSlider.getSelection());
			swingScrollPane.repaint();
		}
	}	
	private final class JScrollBarListener implements AdjustmentListener, Runnable {
		int min, max, thumb, sel;
		private Slider slider;

		private JScrollBarListener(Slider slider) {
			this.slider = slider;
		}

		public void adjustmentValueChanged(AdjustmentEvent e) {
			Adjustable adj = e.getAdjustable();
			min = adj.getMinimum();
			max = adj.getMaximum();
			thumb = adj.getVisibleAmount();
			sel = adj.getValue();

			if (slider.getDisplay() == null) {
				return;
			}
			slider.getDisplay().syncExec(this);
		}

		public void run() {
			slider.setValues(sel, min, max, thumb, slider.getIncrement(), slider.getPageIncrement());
			slider.redraw();
		}
	}
} 