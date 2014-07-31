/*
 *  AccumulationFunctionArithmeticMeanXY.java of project jchart2d, <enterpurposehere>. 
 *  Copyright (C) 2002 - 2013, Achim Westermann, created on Oct 9, 2011
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
 *
 *
 * File   : $Source: /cvsroot/jchart2d/jchart2d/codetemplates.xml,v $
 * Date   : $Date: 2009/02/24 16:45:41 $
 * Version: $Revision: 1.2 $
 */

package info.monitorenter.gui.chart.traces.accumulationfunctions;

import info.monitorenter.gui.chart.Chart2D;
import info.monitorenter.gui.chart.IPointPainter;
import info.monitorenter.gui.chart.ITracePoint2D;
import info.monitorenter.gui.chart.ITracePointProvider;
import info.monitorenter.util.math.MathUtil;

import java.util.Set;
import java.util.TreeSet;

/**
 * Returns the arithmetic mean (x and y) of all points being accumulated by the
 * calls to <code>{@link #addPointToAccumulate(ITracePoint2D)}</code> since the
 * call to <code>{@link #getAccumulatedPoint()}</code>.
 * <p>
 * 
 * @author <a href="mailto:Achim.Westermann@gmx.de">Achim Westermann </a>
 * 
 */
public class AccumulationFunctionArithmeticMeanXY extends AAccumulationFunction {

  /**
   * Needed as a divisor when {@link #getAccumulatedPoint()} is called to divide
   * the summation of all previous accumulated point which is tricky kept in
   * {@link #getAccumulatedPointCurrent()}.
   */
  private int m_accumulatedPointsCount = 0;

  /** To intermediately sum up all accumulations of x values. */
  private double m_accumulatedSumX = 0;

  /** To intermediately sum up all accumulations of Y values. */
  private double m_accumulatedSumY = 0;

  /**
   * During accumulation of a single output point all additional point painters
   * ({@link ITracePoint2D#getAdditionalPointPainters()}) are collected here to
   * finally be added to the outgoing point.
   */
  private Set<IPointPainter< ? >> m_collectedPointPainters = new TreeSet<IPointPainter< ? >>();

  /**
   * @see info.monitorenter.gui.chart.IAccumulationFunction#addPointToAccumulate(info.monitorenter.gui.chart.ITracePoint2D)
   */
  public void addPointToAccumulate(ITracePoint2D point) throws IllegalArgumentException {

    if (point.isDiscontinuation()) {
      throw new IllegalArgumentException("Do not attemp to consume a discontinuation by accumulation - preserve them for the chart!");
    }
    ITracePoint2D accumulatedPointCurrent = this.getAccumulatedPointCurrent();
    this.m_collectedPointPainters.addAll(point.getAdditionalPointPainters());
    if (accumulatedPointCurrent == null) {
      ITracePointProvider tracePointProvider = this.acquireTracePointProvider(point);

      accumulatedPointCurrent = tracePointProvider.createTracePoint(point.getX(), point.getY(), point.getListener());
      this.m_accumulatedSumX += point.getX();
      this.m_accumulatedSumY += point.getY();
      this.setAccumulatedPointCurrent(accumulatedPointCurrent);
    } else {
      /*
       * Caution this is not the correct arithmetic mean calculation but just
       * collection of the sum of all added points. Calculation is done in
       * getAccumulatedPoint()!
       */
      this.m_accumulatedSumX += point.getX();
      this.m_accumulatedSumY += point.getY();
    }
    this.m_accumulatedPointsCount++;
  }

  /**
   * @see info.monitorenter.gui.chart.traces.accumulationfunctions.AAccumulationFunction#getAccumulatedPoint()
   */
  @Override
  public ITracePoint2D getAccumulatedPoint() {

    ITracePoint2D accumulatedPointCurrent = this.getAccumulatedPointCurrent();
    if (accumulatedPointCurrent != null) {
      // transfer state:
      // 1. location: this also causes an update chain that scales the point:
      double accumulatedX = this.m_accumulatedSumX / this.m_accumulatedPointsCount;
      double accumulatedY = this.m_accumulatedSumY / this.m_accumulatedPointsCount;
      if (Chart2D.DEBUG_DATA_ACCUMULATION) {
        if (!MathUtil.isDouble(accumulatedX) || !MathUtil.isDouble(accumulatedY)) {
          throw new IllegalStateException("Accumulated to non double. x: " + accumulatedX + ", y: " + accumulatedY);
        }
      }
      accumulatedPointCurrent.setLocation(accumulatedX, accumulatedY);
      if (Chart2D.DEBUG_DATA_ACCUMULATION) {
        double scaledX = accumulatedPointCurrent.getScaledX();
        double scaledY = accumulatedPointCurrent.getScaledY();
        if(scaledX < 0 || scaledX >1 || scaledY< 0 || scaledY > 1) {
          System.err.println("Accumulated point scaled into invisibility: x.scaled = " + scaledX + ", y.scaled =  " + scaledY);
        }
      }      
      // 2. additional point painters:
      accumulatedPointCurrent.removeAllAdditionalPointPainters();
      for (IPointPainter< ? > pointPainter : this.m_collectedPointPainters) {
        accumulatedPointCurrent.addAdditionalPointPainter(pointPainter);
      }
      if (Chart2D.DEBUG_DATA_ACCUMULATION) {
        System.out.println(this.getClass().getName() + ": accumulated " + this.m_accumulatedPointsCount + " points into one: " + accumulatedPointCurrent);
      }

      this.m_accumulatedPointsCount = 0;
      this.m_accumulatedSumX = 0;
      this.m_accumulatedSumY = 0;
      this.m_collectedPointPainters.clear();
    }
    ITracePoint2D result = super.getAccumulatedPoint();
    return result;
  }

  /**
   * @see info.monitorenter.gui.chart.IAccumulationFunction#getAccumulatedPointCount()
   */
  @Override
  public int getAccumulatedPointCount() {
    return this.m_accumulatedPointsCount;
  }
  
  
}
