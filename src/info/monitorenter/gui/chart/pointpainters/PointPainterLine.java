/*
 *  LinePainter.java,  point painter that paints lines. 
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
package info.monitorenter.gui.chart.pointpainters;

import info.monitorenter.gui.chart.ITracePoint2D;

import java.awt.Graphics;

/**
 * A point painter that renders a trace by lines.
 * <p>
 * 
 * @author <a href="mailto:Achim.Westermann@gmx.de">Achim Westermann </a>
 * 
 * @version $Revision: 1.18 $
 * 
 */
public class PointPainterLine extends APointPainter<PointPainterLine> {

  /** Generated <code>serialVersionUID</code>. */
  private static final long serialVersionUID = 4325801979289678143L;

  /**
   * Defcon.
   * <p>
   */
  public PointPainterLine() {
    // nop
  }

  /**
   * @see info.monitorenter.gui.chart.IPointPainter#calculateMaxX(info.monitorenter.gui.chart.ITracePoint2D)
   */
  @Override
  public double calculateMaxX(final ITracePoint2D point) {
    return point.getX();
  }

  /**
   * @see info.monitorenter.gui.chart.IPointPainter#calculateMaxY(info.monitorenter.gui.chart.ITracePoint2D)
   */
  @Override
  public double calculateMaxY(final ITracePoint2D point) {
    return point.getY();
  }

  /**
   * @see info.monitorenter.gui.chart.IPointPainter#calculateMinX(info.monitorenter.gui.chart.ITracePoint2D)
   */
  @Override
  public double calculateMinX(final ITracePoint2D point) {
    return point.getX();
  }

  /**
   * @see info.monitorenter.gui.chart.IPointPainter#calculateMinY(info.monitorenter.gui.chart.ITracePoint2D)
   */
  @Override
  public double calculateMinY(final ITracePoint2D point) {
    return point.getY();
  }

  /**
   * @see info.monitorenter.gui.chart.IPointPainter#isAdditionalSpaceRequiredX()
   */
  @Override
  public boolean isAdditionalSpaceRequiredX() {
    return false;
  }

  /**
   * @see info.monitorenter.gui.chart.IPointPainter#isAdditionalSpaceRequiredY()
   */
  @Override
  public boolean isAdditionalSpaceRequiredY() {
    return false;
  }

  /**
   * @see info.monitorenter.gui.chart.IPointPainter#isPixelTransformationNeededX()
   */
  @Override
  public boolean isPixelTransformationNeededX() {
    return false;
  }

  /**
   * @see info.monitorenter.gui.chart.IPointPainter#isPixelTransformationNeededY()
   */
  @Override
  public boolean isPixelTransformationNeededY() {
    return false;
  }

  /**
   * Paints a line from current to next point.
   * <p>
   * 
   * @see info.monitorenter.gui.chart.IPointPainter#paintPoint(int, int, int,
   *      int, java.awt.Graphics, info.monitorenter.gui.chart.ITracePoint2D)
   */
  public void paintPoint(final int absoluteX, final int absoluteY, final int nextX, final int nextY, final Graphics g, final ITracePoint2D point) {
    g.drawLine(absoluteX, absoluteY, nextX, nextY);
  }

}
