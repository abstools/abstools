package costabs.structures;

import java.awt.Graphics;
import java.util.List;

import javax.swing.JPanel;

import org.eclipse.core.resources.IFile;

import costabs.beans.Command;
import costabs.beans.Node;
import costabs.exceptions.CostabsException;

public abstract class CostabsGraph {
	
	public static String COLOR_MARKED = "red";
	public static String COLOR_SELECTED = "green";
	public static String COLOR_SOURCE_MARKED = "yellow";
	public static String COLOR_NO_SELECTED = "white";
	
	
	public abstract void loadGraph (IFile file,Command command) throws CostabsException;
	public abstract void interMarkNode (List<Node> nodes, String color) throws CostabsException ;
	public abstract int getWidth ();
	public abstract int getHeight ();
	public abstract void cleanMarked ();
	public abstract void cleanGraph ();
	public abstract void handleClick (int x, int y) throws CostabsException;
	public abstract void paint(Graphics g, JPanel panel);
	
}
