/*
 *  TracePainterDisc.java,  <enter purpose here>.
 *  Copyright (c) 2004 - 2013 Achim Westermann, Achim.Westermann@gmx.de
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

import info.monitorenter.gui.chart.ITracePoint2D;
import info.monitorenter.gui.chart.pointpainters.PointPainterDisc;

/**
 * Renders traces by painting a disc (hollow circle) with choosable diameter for
 * each {@link ITracePoint2D} to show.
 * <p>
 * 
 * @author <a href="mailto:Achim.Westermann@gmx.de">Achim Westermann </a>
 * 
 * @version $Revision: 1.20 $
 * 
 */
public class TracePainterDisc extends TracePainterConfigurable<PointPainterDisc> {

  /** Generated <code>serialVersionUID</code>. */
  private static final long serialVersionUID = 8919406018882664083L;


  /**
   * Creates an instance with a default disc size of 4.
   * <p>
   */
  public TracePainterDisc() {
    super(new PointPainterDisc(4));
  }

  /**
   * Creates an instance with the given disc size.
   * 
   * @param discSize
   *          the disc size in pixel to use.
   */
  public TracePainterDisc(final int discSize) {
    super(new PointPainterDisc(discSize));
  }

  /**
   * Returns the diameter of the discs to paint in pixel.
   * <p>
   * 
   * @return the diameter of the discs to paint in pixel.
   */
  public int getDiscSize() {
    return this.m_pointPainter.getDiscSize();
  }

  /**
   * Sets the diameter of the discs to paint in pixel.
   * <p>
   * 
   * @param discSize
   *          the diameter of the discs to paint in pixel.
   */
  public void setDiscSize(final int discSize) {
    this.m_pointPainter.setDiscSize(discSize);
  }
}
