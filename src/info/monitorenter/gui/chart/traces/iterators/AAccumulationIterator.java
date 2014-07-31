/*
 *  AAccumulationIterator.java of project jchart2d, an Iterator that decorates 
 *  a given iterator by the feature of accumulation of points.
 *  Copyright 2011 - 2013 (C) Achim Westermann.
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
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA*
 *  If you modify or optimize the code in a useful way please let me know.
 *  Achim.Westermann@gmx.de
 *
 */
package info.monitorenter.gui.chart.traces.iterators;

import info.monitorenter.gui.chart.IAccumulationFunction;
import info.monitorenter.gui.chart.IAccumulationStrategy;
import info.monitorenter.gui.chart.IAccumulationStrategy.IAccumulationControl;
import info.monitorenter.gui.chart.ITrace2D;
import info.monitorenter.gui.chart.ITracePoint2D;

import java.util.Iterator;

/**
 * {@link Iterator} that decorates an iterator by the feature of accumulation of
 * points.
 * <p>
 * Contract:
 * <ul>
 * <li>
 * No point with an x value of <code>NaN</code> must be accumulated. Those are
 * discontinuations and must be preserved.</li>
 * <li>
 * The first call to {@link #next()} has to return the same as the first point
 * in the given / wrapped iterator in case that the visible range does not
 * exclude it.</li>
 * <li>
 * The last call to {@link #next()} has to return the same as the last point in
 * the given / wrapped iterator in case that the visible range does not exclude
 * it.</li>
 * <li>
 * The visible range may be ignored.</li>
 * <li>
 * If the visible range is not ignored and excludes the first point(s) of the
 * wrapped iterator then the first point returned from {@link #next()} has to be
 * an interpolated point at the exact coordinate of the lower bound of the
 * range.</li>
 * <li>
 * If the visible range is not ignored and excludes the last point(s) of the
 * wrapped iterator then the last point returned from {@link #next()} has to be
 * an interpolated point at the exact coordinate of the upper bound of the
 * range.</li>
 * <li>
 * After a discontinuation ({@link ITracePoint2D#isDiscontinuation()}) has been
 * returned the next visible point must not be accumulated but returned as-is to
 * prevent showing a bigger gap than actually exists!</li>
 * </ul>
 * <p>
 * 
 * 
 * @author <a href="mailto:Achim.Westermann@gmx.de">Achim Westermann </a>
 * 
 */
public abstract class AAccumulationIterator implements Iterator<ITracePoint2D> {

  /**
   * Internally stored stateful iterator.
   */
  private Iterator<ITracePoint2D> m_originalIterator;

  /** The accumulation function being used. */
  private final IAccumulationFunction m_accumulationFunction;

  /** The termination criteria for each accumulation. */
  private final IAccumulationStrategy.IAccumulationControl m_accumulationControl;

  /**
   * Returns the termination criteria for each accumulation.
   * <p>
   * 
   * @return the termination criteria for each accumulation.
   */
  protected IAccumulationStrategy.IAccumulationControl getAccumulationControl() {
    return this.m_accumulationControl;
  }

  /**
   * The original trace that is decorated with the point accumulation feature.
   */
  private ITrace2D m_originalTrace;

  /**
   * Sets original trace that is decorated with the point accumulation feature.
   * <p>
   * This will also reset the {@link #getOriginalIterator()}.
   * <p>
   * 
   * @param originalTrace
   *          the originalTrace to set
   */
  protected void setOriginalTrace(ITrace2D originalTrace) {
    this.setOriginalTrace(originalTrace);
  }

  /**
   * Constructor with all that is needed for accumulating points.
   * <p>
   * 
   * @param originalTrace
   *          the iterator to decorate with the feature of accumulating points.
   * 
   * @param accumulationFunction
   *          the function to use for point - accumulation.
   * 
   * @param accumulationControl
   *          termination criteria for a single accumulation run. Note that it's
   *          {@link IAccumulationControl#initializeControl(int, int)} method
   *          has to be been called before.
   * 
   */
  public AAccumulationIterator(final ITrace2D originalTrace, final IAccumulationFunction accumulationFunction,
      final IAccumulationStrategy.IAccumulationControl accumulationControl) {
    this.m_originalTrace = originalTrace;
    this.m_accumulationFunction = accumulationFunction;
    this.m_originalIterator = this.m_originalTrace.iterator();
    this.m_accumulationControl = accumulationControl;
  }

  /**
   * Returns the accumulationFunction.
   * <p>
   * 
   * @return the accumulationFunction
   */
  protected IAccumulationFunction getAccumulationFunction() {
    return this.m_accumulationFunction;
  }

  /**
   * Returns the original trace.
   * <p>
   * 
   * @return the original trace.
   */
  protected ITrace2D getOriginalTrace() {
    return this.m_originalTrace;
  }

  /**
   * Returns the stateful iterator of the original trace.
   * <p>
   * 
   * @return the stateful iterator of the original trace.
   */
  protected Iterator<ITracePoint2D> getOriginalIterator() {
    return this.m_originalIterator;
  }

  /**
   * Throws an {@link UnsupportedOperationException} as there is no 1-1
   * relationship between output of this iterator and source (as there is data
   * accumulation in effect).
   * <p>
   * 
   * @see java.util.Iterator#remove()
   */
  @Override
  public final void remove() {
    throw new UnsupportedOperationException(
        this.getClass().getName()
            + " does not support this as this is just a wrapper around a source iterator and there is no 1-1 relationship between returned results from next to the underlying data source (because the decoration called 'data accumulation' is in effect). ");

  }

}