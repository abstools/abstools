/**
Copyright (C) 2010  E.Albert, P.Arenas, S.Genaim, G.Puebla, and D.Zanardini
                    https://costa.ls.fi.upm.es

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/
package costabs.listeners;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IMarkerDelta;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;

import costabs.console.ConsoleHandler;
import costabs.trackers.OutputManager;

/**
 * Class to manage the changed in resources
 * @author groman
 * @license GPL
 */
public class ResourceChangeListener implements IResourceChangeListener{

	public void resourceChanged(IResourceChangeEvent event) {
		try {
			IResourceDelta delta = event.getDelta();
			
			//if (!"abs".equals(delta.getResource().getFileExtension())) {
			//	return;
			//}
			// Ignore all events except the BUILD EVENT
			if (event.getType() != IResourceChangeEvent.POST_BUILD) {
				return;
			}
			
//			ConsoleHandler.write("Chang detected...");
//			ConsoleHandler.write("PATH " + delta.getFullPath());
//			ConsoleHandler.write("KIND " + delta.getKind());
//			ConsoleHandler.write("RES " + delta.getResource());			
//			
//			IResourceDelta resources[] = delta.getAffectedChildren();
//			for (int i = 0; i < resources.length; i ++) {
//				ConsoleHandler.write("Resource " + resources[i].getFullPath());
//				IResourceDelta delta2 [] = resources[i].getAffectedChildren();
//				for (int j = 0; j < delta2.length; j++) {
//					ConsoleHandler.write("   Delta2 " + delta2[i]);
//				}
//			}
//			ConsoleHandler.write("Finished the event handler");
			//System.out.println("Desactivando");
			//OutputManager.getOutputManager().getInterTracker().deactivate();
		}
		catch (Exception e) {
			e.printStackTrace();
		}
	};

	
//	private void changeAffectedMarkers (IResourceDelta delta) throws CoreException{
//
//		if (delta.getAffectedChildren().length == 0) {
//
//			// Create a marker in Java file
//			if (!"java".equals(delta.getResource().getFileExtension())) {
//				return;
//			}
//			
//			IMarkerDelta markers[] = delta.getMarkerDeltas();
//			for (int i = 0; i < markers.length; i ++) {
//				ConsoleHandler.write("            Marker[" + i + "] -> " + markers[i].getType() + " " + 						
//						markers[i].getKind() + " " + markers[i].getMarker().getClass());
//				
//				IMarker marker = delta.getResource().getMarker(markers[i].getId());
//				
//				//IMarker marker = (org.eclipse.core.internal.resources.Marker)markers[i];
//				marker.setAttribute(IMarker.MESSAGE, "Changed by the event handler");
//				
//			}
//
//			return;
//		}
//
//		// Continue in the tree to obtain the modified file
//		for (int i = 0; i < delta.getAffectedChildren().length; i ++) {
//			changeAffectedMarkers(delta.getAffectedChildren()[i]);
//		}
//	}
}