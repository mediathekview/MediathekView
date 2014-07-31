/*
 * TracePainterConfigurable.java, an ITracePainter implementation that works on
 * a given IPointPainter. Copyright (c) 2004 - 2013 Achim Westermann,
 * Achim.Westermann@gmx.de
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 * 
 * If you modify or optimize the code in a useful way please let me know.
 * Achim.Westermann@gmx.de
 */
package info.monitorenter.gui.chart.traces.painters;

import info.monitorenter.gui.chart.IPointPainter;
import info.monitorenter.gui.chart.IPointPainterConfigurableUI;
import info.monitorenter.gui.chart.ITracePoint2D;

import java.awt.Graphics;

/**
 * An <code>{@link info.monitorenter.gui.chart.ITracePainter}</code>
 * implementation that works on a given <code>{@link IPointPainter}</code>. It
 * is configurable by using the point painter to paint the trace (wrapper /
 * delegate).
 * <p>
 * Configure painting by first calling: <code>
 * {@link TracePainterConfigurable#getPointPainter()}
 * </code>
 * 
 * and then:
 * 
 * <code>
 * {@link IPointPainterConfigurableUI#setColor(java.awt.Color)},
 * {@link IPointPainterConfigurableUI#setColorFill(java.awt.Color)} and
 * {@link IPointPainterConfigurableUI#setStroke(java.awt.Stroke)}
 * </code> .
 * <p>
 * 
 * 
 * 
 * @author <a href="mailto:Achim.Westermann@gmx.de">Achim Westermann </a>
 * 
 * @version $Revision: 1.14 $
 * 
 * @param <T>
 *          needed for comparable<T>.
 * 
 */
public class TracePainterConfigurable<T extends IPointPainterConfigurableUI<T>> extends ATracePainter {

  /** Generated <code>serialVersionUID</code>. */
  private static final long serialVersionUID = 548540923475344855L;

  /** The implementation for rendering the point as a disc. */
  protected final T m_pointPainter;

  /**
   * Creates an instance that works with the given point painter.
   * <p>
   * 
   * @param pointPainter
   *          the point painter to use.
   */
  public TracePainterConfigurable(final T pointPainter) {
    this.m_pointPainter = pointPainter;
  }

  /**
   * @see info.monitorenter.gui.chart.IPointPainter#calculateMaxX(info.monitorenter.gui.chart.ITracePoint2D)
   */
  @Override
  public final double calculateMaxX(final ITracePoint2D point) {
    return this.m_pointPainter.calculateMaxX(point);
  }

  /**
   * @see info.monitorenter.gui.chart.IPointPainter#calculateMaxY(info.monitorenter.gui.chart.ITracePoint2D)
   */
  @Override
  public final double calculateMaxY(final ITracePoint2D point) {
    return this.m_pointPainter.calculateMaxY(point);
  }

  /**
   * @see info.monitorenter.gui.chart.IPointPainter#calculateMinX(info.monitorenter.gui.chart.ITracePoint2D)
   */
  @Override
  public final double calculateMinX(final ITracePoint2D point) {
    return this.m_pointPainter.calculateMinX(point);
  }

  /**
   * @see info.monitorenter.gui.chart.IPointPainter#calculateMinY(info.monitorenter.gui.chart.ITracePoint2D)
   */
  @Override
  public final double calculateMinY(final ITracePoint2D point) {
    return this.m_pointPainter.calculateMinY(point);
  }

  /**
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   * 
   * @param o
   *          the instance to compare oneself to.
   * @return see super.
   */
  public final int compareTo(T o) {
    return this.m_pointPainter.compareTo(o);
  }

  /**
   * @see info.monitorenter.gui.chart.ITracePainter#endPaintIteration(java.awt.Graphics)
   */
  @Override
  public final void endPaintIteration(final Graphics g2d) {
    if (g2d != null) {
      int previousX = this.getPreviousX();
      int previousY = this.getPreviousY();
      if (previousX != Integer.MIN_VALUE || previousY != Integer.MIN_VALUE) {
        this.m_pointPainter.paintPoint(previousX, previousY, previousX, previousY, g2d, this.getPreviousPoint());
      }
    }
    this.m_pointPainter.endPaintIteration(g2d);
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
    final TracePainterConfigurable< ? > other = (TracePainterConfigurable< ? >) obj;
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
   * Returns the pointPainter.
   * <p>
   * 
   * @return the pointPainter
   */
  public T getPointPainter() {
    return this.m_pointPainter;
  }

  /**
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + ((this.m_pointPainter == null) ? 0 : this.m_pointPainter.hashCode());
    return result;
  }

  /**
   * @see info.monitorenter.gui.chart.IPointPainter#isAdditionalSpaceRequiredX()
   */
  @Override
  public boolean isAdditionalSpaceRequiredX() {
    return this.m_pointPainter.isAdditionalSpaceRequiredX();
  }

  /**
   * @see info.monitorenter.gui.chart.IPointPainter#isAdditionalSpaceRequiredY()
   */
  @Override
  public boolean isAdditionalSpaceRequiredY() {
    return this.m_pointPainter.isAdditionalSpaceRequiredY();
  }

  /**
   * @see info.monitorenter.gui.chart.IPointPainter#isPixelTransformationNeededX()
   */
  public final boolean isPixelTransformationNeededX() {
    return this.m_pointPainter.isPixelTransformationNeededX();
  }

  /**
   * @see info.monitorenter.gui.chart.IPointPainter#isPixelTransformationNeededY()
   */
  public final boolean isPixelTransformationNeededY() {
    return this.m_pointPainter.isPixelTransformationNeededY();
  }

  /**
   * @see info.monitorenter.gui.chart.traces.painters.ATracePainter#paintPoint(int,
   *      int, int, int, java.awt.Graphics,
   *      info.monitorenter.gui.chart.ITracePoint2D)
   */
  @Override
  public void paintPoint(final int absoluteX, final int absoluteY, final int nextX, final int nextY, final Graphics g, final ITracePoint2D original) {
    super.paintPoint(absoluteX, absoluteY, nextX, nextY, g, original);
    this.m_pointPainter.paintPoint(absoluteX, absoluteY, nextX, nextY, g, original);
  }

  /**
   * @see info.monitorenter.gui.chart.traces.painters.ATracePainter#startPaintIteration(java.awt.Graphics)
   */
  @Override
  public final void startPaintIteration(final Graphics g2d) {
    this.m_pointPainter.startPaintIteration(g2d);
  }

}
