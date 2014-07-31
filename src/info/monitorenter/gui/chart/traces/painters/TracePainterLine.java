/*
 *  LinePainter.java,  <enter purpose here>.
 *  Copyright (c) 2004 - 2013  Achim Westermann, Achim.Westermann@gmx.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *
 *  If you modify or optimize the code in a useful way please let me know.
 *  Achim.Westermann@gmx.de
 */
package info.monitorenter.gui.chart.traces.painters;

import info.monitorenter.gui.chart.pointpainters.PointPainterLine;

/**
 * A trace painter that renders a trace by lines.
 * <p>
 * 
 * @author <a href="mailto:Achim.Westermann@gmx.de">Achim Westermann </a>
 * 
 * @version $Revision: 1.22 $
 * 
 */
public class TracePainterLine extends TracePainterConfigurable<PointPainterLine> {

  /** Generated <code>serialVersionUID</code>. */
  private static final long serialVersionUID = -3310431930065989648L;

  /**
   * Defcon.
   * <p>
   */
  public TracePainterLine() {    
    super(new PointPainterLine());
  }
}
