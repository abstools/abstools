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
package costabs.views;

import org.eclipse.ui.views.markers.MarkerSupportView;

/**
 * Costa view to show the costa markers
 * @author groman
 *
 */
public class CostabsView extends MarkerSupportView {
	
	public CostabsView () {
        super("costabs.plugin.CostabsMarkerContentGenerator");
    }
	
}