/*
 *  TracePainterBar.java, a trace painter that renders a bar 
 *  for each point.
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

import info.monitorenter.gui.chart.Chart2D;
import info.monitorenter.gui.chart.ITracePoint2D;
import info.monitorenter.gui.chart.pointpainters.PointPainterVerticalBar;

/**
 * Renders traces by painting a bar with selectable width for each
 * {@link ITracePoint2D} to show.
 * <p>
 * Bars are placed around the x value to render: the middle of the bar in x
 * dimension is the exact x value.
 * <p>
 * 
 * FIXME: Look if this one could extend {@link TracePainterConfigurable}.
 * <p>
 * 
 * @author <a href="mailto:Achim.Westermann@gmx.de">Achim Westermann </a>
 * 
 * @version $Revision: 1.19 $
 * 
 */
public class TracePainterVerticalBar extends TracePainterConfigurable<PointPainterVerticalBar> {

  /** Generated <code>serialVersionUID</code>. */
  private static final long serialVersionUID = 6151930248938945671L;

  /**
   * Creates an instance with a default bar width size of 4.
   * <p>
   * 
   * @param chart
   *          needed for bound information.
   */
  public TracePainterVerticalBar(final Chart2D chart) {
    this(4, chart);
  }

  /**
   * Creates an instance with the bar width.
   * 
   * @param barWidth
   *          the bar width in pixel to use.
   * 
   * @param chart
   *          needed for bound information.
   */
  public TracePainterVerticalBar(final int barWidth, final Chart2D chart) {
    super(new PointPainterVerticalBar(barWidth, chart));
  }

  /**
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public boolean equals(final Object obj) {
    if (this == obj) {
      return true;
    }
    if (!super.equals(obj)) {
      return false;
    }
    if (this.getClass() != obj.getClass()) {
      return false;
    }
    final TracePainterVerticalBar other = (TracePainterVerticalBar) obj;
    if (this.m_pointPainter == null) {
      if (other.m_pointPainter != null) {
        return false;
      }
    } else if (!this.m_pointPainter.equals(other.m_pointPainter)) {
      return false;
    }
    return true;
  }

  /**
   * Returns the diameter of the discs to paint in pixel.
   * <p>
   * 
   * @return the diameter of the discs to paint in pixel.
   */
  public int getBarWidth() {
    return this.m_pointPainter.getBarWidth();
  }

  /**
   * Sets the width of the bars to paint in pixel.
   * <p>
   * 
   * @param barWidth
   *          the width of the bars to paint in pixel.
   */
  public void setBarWidth(final int barWidth) {
    this.m_pointPainter.setBarWidth(barWidth);
  }
}
