package costabs.panels;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.FileReader;
import java.net.URI;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.TreeSet;

import javax.swing.JPanel;

import com.kitfox.svg.SVGCache;
import com.kitfox.svg.SVGDiagram;
import com.kitfox.svg.SVGDisplayPanel;
import com.kitfox.svg.SVGElement;
import com.kitfox.svg.SVGElementException;
import com.kitfox.svg.SVGException;
import com.kitfox.svg.SVGUniverse;
import com.kitfox.svg.animation.AnimationElement;
import com.kitfox.svg.app.beans.SVGIcon;

public class TestPanel extends JPanel implements MouseListener
{
	public static final long serialVersionUID = 0;

	final SVGIcon icon;
	SVGDiagram diagram;

	public TestPanel()
	{
		addMouseListener(this);

		FileReader reader;
		icon = new SVGIcon();
		icon.setScaleToFit(true);
		try {
			//reader = new FileReader("/Users/groman/Systems/eclipse/workspace-working/TestGraphics/src/test.svg");
			SVGUniverse universe = new SVGUniverse();
			URI uri = universe.loadSVG(new URL("file:///Users/groman/Systems/eclipse/workspace-working/TestGraphics/src/test.svg")); 
			diagram = universe.getDiagram(uri);


			icon.setSvgURI(uri);
			setPreferredSize(new Dimension((int)diagram.getWidth() , (int)diagram.getHeight()));
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public void paintComponent(Graphics gg) {

		Graphics2D g = (Graphics2D)gg;

		if (getBackground() != null)
		{
			Dimension dim = getSize();
			g.setColor(getBackground());
			g.fillRect(0, 0, dim.width, dim.height);
		}
		g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
		g.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
		if (diagram != null) 
		{
			try
			{
				diagram.render(g);
			}
			catch (SVGException e)
			{
				e.printStackTrace();
			}
		}	
	}

	//	@Override
	//	public int getWidth() {
	//		return (int)diagram.getWidth();
	//	}
	//	
	//	@Override
	//	public int getHeight() {
	//		return (int)diagram.getHeight();
	//	}

	@Override
	public void mouseClicked(MouseEvent arg0) {
		List pickedElements;

		TreeSet<String> ids = new TreeSet<String>();

		try {
			pickedElements = diagram.pick(arg0.getPoint(),null);

			for (int i = 0; i < pickedElements.size(); i++) {
				ArrayList<SVGElement> elements = (ArrayList<SVGElement>)pickedElements.get(i);
				for (SVGElement element: elements) {
					if (element.getId() != null) {
						ids.add(element.getId());
					}
				}
			}
			markNodes(ids, "red");

		} catch (SVGException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	//	@Override
	//	public void paint(Graphics g) {
	//		super(g);
	//		icon.paintIcon(this, g, 0, 0);
	//	}

	private void markNodes (TreeSet<String> ids, String color) throws SVGElementException {
		for(String id: ids) {
			SVGElement element = diagram.getElement(id);

			if (!element.hasAttribute("class", AnimationElement.AT_XML) || 
					!"node".equals(element.getPresAbsolute("class").getStringValue())){
				continue;
			}
			markNode(element, color);
		}
		repaint();

	}

	private void markNode (SVGElement element, String color) throws SVGElementException {
		for(int i=0; i < element.getNumChildren(); i ++) {

			SVGElement child = element.getChild(i);
			if (child.hasAttribute("fill", AnimationElement.AT_XML)) {
				child.setAttribute("fill", AnimationElement.AT_XML, color);
			}
			markNode(child, color);
		}
	}

	@Override
	public void mouseEntered(MouseEvent arg0) {
		// TODO Auto-generated method stub

	}

	@Override
	public void mouseExited(MouseEvent arg0) {
		// TODO Auto-generated method stub

	}

	@Override
	public void mousePressed(MouseEvent arg0) {
		// TODO Auto-generated method stub

	}

	@Override
	public void mouseReleased(MouseEvent arg0) {
		// TODO Auto-generated method stub

	}

	//    private String makeDynamicSVG()
	//    {
	//         sw = new StringWriter();
	//        PrintWriter pw = new PrintWriter(sw);
	//        
	//        pw.println("<svg width=\"400\" height=\"400\" style=\"fill:none;stroke-width:4\">");
	//        pw.println("    <circle cx=\"200\" cy=\"200\" r=\"200\" style=\"stroke:blue\"/>");
	//        pw.println("    <circle cx=\"140\" cy=\"140\" r=\"40\" style=\"stroke:red\"/>");
	//        pw.println("    <circle cx=\"260\" cy=\"140\" r=\"40\" style=\"stroke:red\"/>");
	//        pw.println("    <polyline points=\"100 300 150 340 250 240 300 300\" style=\"stroke:red\"/>");
	//        pw.println("</svg>");
	//        
	//        pw.close();
	//        
	//        System.out.println(sw.toString());
	//        return sw.toString();
	//    }
}
