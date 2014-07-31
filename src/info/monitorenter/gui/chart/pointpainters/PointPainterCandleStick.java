/*
 *  PointPainterCandleStick.java of project jchart2d, <enterpurposehere>. 
 *  Copyright (C) 2002 - 2013, Achim Westermann, created on Oct 9, 2012
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
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

import info.monitorenter.gui.chart.Chart2D;
import info.monitorenter.gui.chart.IAxis;
import info.monitorenter.gui.chart.ITrace2D;
import info.monitorenter.gui.chart.ITracePoint2D;
import info.monitorenter.gui.chart.tracepoints.CandleStick;
import info.monitorenter.gui.util.TracePoint2DUtil;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Stroke;

/**
 * A special point painter that will only be useful to render instances of
 * {@link CandleStick}.
 * <p>
 * 
 * 
 * @author <a href="mailto:Achim.Westermann@gmx.de">Achim Westermann </a>
 * 
 */
public class PointPainterCandleStick extends APointPainterCandleStick<PointPainterCandleStick>  {

  /** Generated <code>serialVersionUID</code>. **/
  private static final long serialVersionUID = -6708238540093878572L;

  /**
   * Constructor taking the width in pixels.
   * <p>
   * 
   * @param width
   *          the width of the {@link CandleStick} in pixels.
   **/
  public PointPainterCandleStick(final int width) {
    this.setWidth(width);
  }


  /**
   * @see info.monitorenter.gui.chart.IPointPainter#calculateMaxX(info.monitorenter.gui.chart.ITracePoint2D)
   */
  @Override
  public double calculateMaxX(final ITracePoint2D point) {
    IAxis< ? > axisX = TracePoint2DUtil.getAxisXOfTracePoint(point);
    double widthInValue = axisX.translatePxToValueRelative(point, this.getWidth() / 2);
    double result = point.getX() + widthInValue;
    return result;
  }

  /**
   * @see info.monitorenter.gui.chart.IPointPainter#calculateMaxY(info.monitorenter.gui.chart.ITracePoint2D)
   */
  @Override
  public double calculateMaxY(final ITracePoint2D point) {
    double result;
    CandleStick candleStick = (CandleStick) point;
    result = candleStick.getHigh();
    return result;
  }

  /**
   * 
   * @see info.monitorenter.gui.chart.IPointPainter#calculateMinX(info.monitorenter.gui.chart.ITracePoint2D)
   */
  @Override
  public double calculateMinX(final ITracePoint2D point) {
    IAxis< ? > axisX = TracePoint2DUtil.getAxisXOfTracePoint(point);
    double widthInValue = axisX.translatePxToValueRelative(point, -this.getWidth() / 2);
    double result = point.getX() + widthInValue;
    return result;
  }

  /**
   * @see info.monitorenter.gui.chart.IPointPainter#calculateMinY(info.monitorenter.gui.chart.ITracePoint2D)
   */
  @Override
  public double calculateMinY(final ITracePoint2D point) {
    double result;
    CandleStick candleStick = (CandleStick) point;
    result = candleStick.getLow();
    return result;
  }

  /**
   * @see info.monitorenter.gui.chart.IPointPainter#endPaintIteration(java.awt.Graphics)
   */
  @Override
  public void endPaintIteration(Graphics g2d) {
    // nop
  }





  /**
   * @see info.monitorenter.gui.chart.IPointPainter#isAdditionalSpaceRequiredX()
   */
  @Override
  public boolean isAdditionalSpaceRequiredX() {
    return this.getWidth() > 1;
  }

  /**
   * @see info.monitorenter.gui.chart.IPointPainter#isAdditionalSpaceRequiredY()
   */
  @Override
  public boolean isAdditionalSpaceRequiredY() {
    return true;
  }

  /**
   * @see info.monitorenter.gui.chart.IPointPainter#isPixelTransformationNeededX()
   */
  @Override
  public boolean isPixelTransformationNeededX() {
    return true;
  }

  /**
   * @see info.monitorenter.gui.chart.IPointPainter#isPixelTransformationNeededX()
   */
  @Override
  public boolean isPixelTransformationNeededY() {
    return false;
  }

  /**
   * @see info.monitorenter.gui.chart.IPointPainter#paintPoint(int, int, int,
   *      int, java.awt.Graphics, info.monitorenter.gui.chart.ITracePoint2D)
   */
  @Override
  public void paintPoint(int absoluteX, int absoluteY, int nextX, int nextY, Graphics g, ITracePoint2D original) {
    /*
     * absoluteX corresponds to getX(), absoluteY to getStart(). All other
     * coords have to be transformed to px.
     */
    CandleStick candleStick = (CandleStick) original;
    double x = absoluteX;
    double startYPx = absoluteY;
    /*
     * Get the corresponding chart for coordinate translation:
     */
    ITrace2D trace = original.getListener();
    if (trace == null) {
      throw new IllegalStateException("Given point is not attached to a trace yet. Cannot paint!");
    } else {
      Chart2D chart = trace.getRenderer();
      if (chart == null) {
        throw new IllegalStateException("Given point is in a trace that is not attached to a chart yet. Cannot paint!");
      } else {
        /*
         * Normalize y
         */
        double traceMaxY = trace.getMaxY();
        double traceMinY = trace.getMinY();
        double scalerY = traceMaxY - traceMinY;
        double endYNormalized = (candleStick.getEnd() - traceMinY) / scalerY;
        double highYNormalized = (candleStick.getHigh() - traceMinY) / scalerY;
        double lowYNormalized = (candleStick.getLow() - traceMinY) / scalerY;
        /*
         * Transform to px
         */
        double yChartStartPx = chart.getYChartStart();
        double yChartEndPx = chart.getYChartEnd();
        double rangeYPx = yChartStartPx - yChartEndPx;
        double endYPx = yChartStartPx - (int) Math.round(endYNormalized * rangeYPx);
        double highYPx = yChartStartPx - (int) Math.round(highYNormalized * rangeYPx);
        double lowYPx = yChartStartPx - (int) Math.round(lowYNormalized * rangeYPx);

        /*
         * Finally paint:
         */
        Color backupColor;
        if (candleStick.getStart() > candleStick.getEnd()) {

          /*
           * 1. box marking space between start and stop y
           */
          backupColor = this.installColor(g);
          g.drawRect((int) (x - this.getWidth() / 2), (int) startYPx, (int) this.getWidth(), (int) (endYPx - startYPx));
          /*
           * 2. upper wick
           */
          g.drawLine((int) x, (int) startYPx, (int) x, (int) highYPx);
          if(this.isDrawUpperWickDash()) {
            g.drawLine((int) x - this.getWidth() / 2, (int) highYPx, (int) x+this.getWidth() /2, (int) highYPx);
          }
          /*
           * 2. lower wick
           */
          g.drawLine((int) x, (int) endYPx, (int) x, (int) lowYPx);
          if(this.isDrawUpperWickDash()) {
            g.drawLine((int) x - this.getWidth() / 2, (int) lowYPx, (int) x+this.getWidth() /2, (int) lowYPx);
          }
        } else {

          /*
           * 1. box marking space between start and stop y
           */
          backupColor = this.installColorFill(g);
          g.fillRect((int) (x - this.getWidth() / 2), (int) endYPx, (int) this.getWidth(), (int) (startYPx - endYPx));
          this.installColor(g);
          g.drawRect((int) (x - this.getWidth() / 2), (int) endYPx, (int) this.getWidth(), (int) (startYPx - endYPx));
          /*
           * 2. upper wick
           */
          g.drawLine((int) x, (int) endYPx, (int) x, (int) highYPx);
          if(this.isDrawUpperWickDash()) {
            g.drawLine((int) x - this.getWidth() / 2, (int) highYPx, (int) x+this.getWidth() /2, (int) highYPx);
          }
          /*
           * 2. lower wick
           */
          g.drawLine((int) x, (int) startYPx, (int) x, (int) lowYPx);
          if(this.isDrawLowerWickDash()) {
            g.drawLine((int) x - this.getWidth() / 2, (int) lowYPx, (int) x+this.getWidth() /2, (int) lowYPx);
          }
        }
        g.setColor(backupColor);

      }
    }
  }

  /**
   * @see info.monitorenter.gui.chart.IPointPainter#startPaintIteration(java.awt.Graphics)
   */
  @Override
  public void startPaintIteration(Graphics g2d) {
    // nop

  }

}