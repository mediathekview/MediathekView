/*
 *  AAccumulationFunction.java of project jchart2d, <enterpurposehere>. 
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
import info.monitorenter.gui.chart.IAccumulationFunction;
import info.monitorenter.gui.chart.ITrace2D;
import info.monitorenter.gui.chart.ITracePoint2D;
import info.monitorenter.gui.chart.ITracePointProvider;
import info.monitorenter.util.math.MathUtil;

public abstract class AAccumulationFunction implements IAccumulationFunction {

  /**
   * The current accumulated point, potentially not the result until
   * {@link #getAccumulatedPoint()} is called.
   */
  private ITracePoint2D m_accumulatedPointCurrent;

  /**
   * Sets the current accumulated point.
   * <p>
   * 
   * This is intended for internal (subclass) use only.
   * <p>
   * 
   * @param accumulatedPointCurrent
   *          the accumulatedPointCurrent to set
   */
  protected void setAccumulatedPointCurrent(ITracePoint2D accumulatedPointCurrent) {
    this.m_accumulatedPointCurrent = accumulatedPointCurrent;
  }

  /**
   * Returns the current accumulated point.
   * <p>
   * 
   * @return the current accumulated point.
   */
  public ITracePoint2D getAccumulatedPointCurrent() {
    return this.m_accumulatedPointCurrent;
  }

  /**
   * The previous accumulated point which might be used by implementations that
   * want to accumulate based on the previous accumulated point (e.g.: An
   * implementation that cherry picks only one point from the points to
   * accumulate that is the most different from the previous one).
   */
  private ITracePoint2D m_accumulatedPointPrevious;

  /**
   * Returns previous accumulated point which might be used by implementations
   * that want to accumulate based on the previous accumulated point (e.g.: An
   * implementation that cherry picks only one point from the points to
   * accumulate that is the most different from the previous one).
   * <p>
   * This is intended for internal (subclass) use only.
   * <p>
   * 
   * @return the previous accumulated point which might be used by
   *         implementations that want to accumulate based on the previous
   *         accumulated point (e.g.: An implementation that cherry picks only
   *         one point from the points to accumulate that is the most different
   *         from the previous one).
   */
  protected ITracePoint2D getAccumulatedPointPrevious() {
    return this.m_accumulatedPointPrevious;
  }

  /**
   * @see info.monitorenter.gui.chart.IAccumulationFunction#getAccumulatedPoint()
   */
  public ITracePoint2D getAccumulatedPoint() {
    ITracePoint2D result = this.getAccumulatedPointCurrent();
    this.m_accumulatedPointPrevious = result;
    this.setAccumulatedPointCurrent(null);
    if (result != null) {
      if (!MathUtil.isDouble(result.getX()) || !MathUtil.isDouble(result.getY())) {
        throw new IllegalStateException(
            "Result has a wrong value (programming and/or rounding error): " + result);
      }
    } else {
      /*
       * This happens when nothing was accumulated but an upper bound was
       * crossed by the accumulation iterator (ordered x).
       */
    }
    return result;
  }

  /**
   * Helper method to read a {@link ITracePointProvider} from the given point.
   * <p>
   * This assumes that the given point is assigned to an {@link ITrace2D} and
   * that that trace is assigned to a {@link Chart2D}. If this is not the case
   * an {@link IllegalStateException} is thrown.
   * <p>
   * 
   * @param point
   *          the point to search the trace point provider for.
   * 
   * @return the trace point provider that is assigned to the chart which this
   *         trace point belongs to.
   * 
   * @throws IllegalStateException
   *           if the point is not assigned to a trace which is assigned to a
   *           chart.
   */
  protected final ITracePointProvider acquireTracePointProvider(final ITracePoint2D point)
      throws IllegalStateException {
    ITrace2D trace = point.getListener();
    if (trace == null) {
      throw new IllegalStateException(
          "You cannot use accumulation functions for trace points that have not been assigned to a trace yet!");
    } else {
      ITracePointProvider tracePointProvider = trace.getTracePointProvider();
      return tracePointProvider;
    }

  }

}
