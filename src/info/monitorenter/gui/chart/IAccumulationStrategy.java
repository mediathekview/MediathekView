/*
 *  IAccumulationStrategy.java of project jchart2d, <enterpurposehere>. 
 *  Copyright (C) 2002 - 2013, Achim Westermann, created on Dec 11, 2011
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

package info.monitorenter.gui.chart;

import info.monitorenter.gui.chart.traces.accumulationstrategies.AAccumulationStrategy;
import info.monitorenter.gui.chart.traces.accumulationstrategies.AccumulationStrategyAmountOfPoints;
import info.monitorenter.gui.chart.traces.iterators.AAccumulationIterator;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Iterator;

/**
 * Design helper to deal with the following: If a trace is unsorted then you
 * have to accumulated all n consecutive points into one regardless whether they
 * jump large distances in the value domain. The output might look like
 * something that has nothing in common with the original trace without
 * accumulations. Accumulation then is done just by deflating n consecutive
 * points (see {@link AccumulationStrategyAmountOfPoints}).
 * <p>
 * If a trace is sorted then accumulation may be done based on value regions.
 * Consider you have sorted ascending x values that have a high density of
 * tracepoints in certain regions but a very low density in other regions. If
 * you would accumulated just n consecutive points you would thin out the
 * regions with very little data points (low density) to become even less
 * precision in those large regions while the regions with high density only
 * loose little information. If you decide to split up the visible range in to
 * parts with same value-span then you can just accumulate depending on the
 * density of x values. You will not loose data in value-ranges with low density
 * but are able to drop lots of unnecessary values in high-density areas (see
 * {@link AccumulationStrategyXRangeWithRespectToDensity}).
 * <p>
 * 
 * <h3>Property Change events</h3>
 * The following <code>{@link java.beans.PropertyChangeEvent}</code> may be
 * fired to <code>{@link PropertyChangeListener}</code> instances that register
 * themselves with
 * <code>{@link #addPropertyChangeListener(String, PropertyChangeListener)}</code>.
 * <table border="0">
 * <tr>
 * <th><code>{@link PropertyChangeEvent#getPropertyName()}</code></th>
 * <th><code>{@link PropertyChangeEvent#getSource()}</code></th>
 * <th><code>{@link PropertyChangeEvent#getOldValue()}</code></th>
 * <th><code>{@link PropertyChangeEvent#getNewValue()}</code></th>
 * <th><code>When fired</code></th>
 * </tr>
 * <tr>
 * <td>
 * <code>{@link info.monitorenter.gui.chart.traces.accumulationstrategies.AAccumulationStrategy#PROPERTY_ACCUMULATION_FUNCTION}</code>
 * </td>
 * <td><code>{@link AAccumulationStrategy}</code> that changed</td>
 * <td><code>{@link IAccumulationFunction}</code>, the old value</td>
 * <td><code>{@link IAccumulationFunction}</code>, the new value</td>
 * <td>
 * <code>{@link AAccumulationStrategy#setAccumulationFunction(IAccumulationFunction)}</code>
 * was called.</td>
 * </tr>
 * <tr>
 * <td>
 * <code>{@link info.monitorenter.gui.chart.ITrace2DDataAccumulating#PROPERTY_ACCUMULATION_STRATEGY_ACCUMULATION_FUNCTION_CHANGED}</code>
 * </td>
 * <td><code>{@link ITrace2DDataAccumulating}</code> that changed</td>
 * <td><code>{@link IAccumulationFunction}</code>, the old value</td>
 * <td><code>{@link IAccumulationFunction}</code>, the new value</td>
 * <td>
 * <code>{@link AAccumulationStrategy#setAccumulationFunction(IAccumulationFunction)}</code>
 * was called on the current accumulation strategy.</td>
 * </tr>
 * </table>
 * <p>
 * 
 * @author <a href="mailto:Achim.Westermann@gmx.de">Achim Westermann </a>
 * 
 */
public interface IAccumulationStrategy {

  /**
   * Just accumulates n consecutive points.
   * <p>
   * 
   * @author <a href="mailto:Achim.Westermann@gmx.de">Achim Westermann </a>
   */
  public static class AccumulationControlConsecutivePoints implements IAccumulationControl {

    /**
     * Amount currently accumulated to function.
     */
    private int m_amountOfAccumulatedPoints;

    /**
     * Amount of points to accumulate.
     */
    private int m_amountToAccumulate = 0;

    /**
     * Default constructor which does not initialize everything needed as that
     * has to be done by the concrete {@link AAccumulationIterator}
     * implementation which works with this.
     * <p>
     * Those have to call {@link #initializeControl(int, int)} first!
     * <p>
     */
    public AccumulationControlConsecutivePoints() {
      this.m_amountOfAccumulatedPoints = 0;
    }

    /**
     * @see info.monitorenter.gui.chart.IAccumulationStrategy.IAccumulationControl#getAccumulatedPointIfAccumulationDone(info.monitorenter.gui.chart.IAccumulationFunction,
     *      info.monitorenter.gui.chart.ITracePoint2D)
     */
    @Override
    public ITracePoint2D getAccumulatedPointIfAccumulationDone(IAccumulationFunction function,
        ITracePoint2D current) {
      ITracePoint2D result = null;
      if (this.isAccumulationDone()) {
        result = function.getAccumulatedPoint();
        this.m_amountOfAccumulatedPoints = 0;
      }
      function.addPointToAccumulate(current);
      this.m_amountOfAccumulatedPoints++;
      return result;
    }

    /**
     * Returns the amount of points to accumulate.
     * <p>
     * 
     * @return the amount of points to accumulate.
     */
    public int getAmountToAccumulate() {
      return this.m_amountToAccumulate;
    }

    /**
     * @see info.monitorenter.gui.chart.IAccumulationStrategy.IAccumulationControl#initializeControl(int,
     *      int)
     */
    @Override
    public void initializeControl(int totalAmountOfPoints, int desiredAmountOfPoints) {
      int targetCount = desiredAmountOfPoints;

      if ((targetCount != 0) && (totalAmountOfPoints != 0)) {
        this.m_amountToAccumulate = (int) Math.floor((totalAmountOfPoints) / (targetCount));
      }
      if (this.m_amountToAccumulate == 0) {
        if (Chart2D.DEBUG_DATA_ACCUMULATION) {
          System.out.println("Count per next is zero! Defaulting to 1.");
        }
        this.m_amountToAccumulate = 1;
      }

      if (Chart2D.DEBUG_DATA_ACCUMULATION) {
        System.out.println(this.getClass().getName() + " accumulating " + this.m_amountToAccumulate
            + " point into one");
      }
    }

    /**
     * @see info.monitorenter.gui.chart.IAccumulationStrategy.IAccumulationControl#isAccumulationBypassable()
     */
    @Override
    public boolean isAccumulationBypassable() {
      boolean result = false;
      result = this.m_amountToAccumulate == 1;
      return result;
    }

    /**
     * @see info.monitorenter.gui.chart.IAccumulationStrategy.IAccumulationControl#isAccumulationDone()
     */
    @Override
    public boolean isAccumulationDone() {
      boolean result = this.m_amountOfAccumulatedPoints == this.m_amountToAccumulate;
      return result;
    }

  }

  /**
   * Interface for encapsulation of the control of "how many" points get
   * accumulated to one.
   * <p>
   * 
   * @author <a href="mailto:Achim.Westermann@gmx.de">Achim Westermann </a>
   */
  public interface IAccumulationControl {
    /**
     * Modifier that checks if the given function has accumulated enough points.
     * If so the accumulated point is returned, the function is reset and the
     * current point is added to the next accumulation in the function.
     * <p>
     * If accumulation is not done yet the current point is added to the
     * accumulation function and <code>null</code> is returned.
     * <p>
     * Implementations may use different strategies to find out if accumulation
     * is done or not: Accumulate a fixed amount of points or split the range up
     * to have density based accumulation,... .
     * <p>
     * 
     * 
     * @param function
     *          the accumulation function to use.
     * 
     * @param current
     *          the current iterated point.
     * 
     * @return the accumulation result or <code>null</code> if accumulation is
     *         not done yet.
     */
    public abstract ITracePoint2D getAccumulatedPointIfAccumulationDone(
        final IAccumulationFunction function, final ITracePoint2D current);

    /**
     * This has to be called by users of this instance (
     * {@link AAccumulationIterator} before starting an iteration / using this.
     * <p>
     * 
     * @param totalAmountOfPoints
     *          the total amount of points in the original data source.
     * 
     * @param desiredAmountOfPoints
     *          the desired amount of points coming out of the accumulation.
     *          Note that depending on the implementation this must not be hit
     *          exactly: Implementations might e.g. decide to accumulate
     *          n-consecutive points by an initial computation. In the
     *          accumulation run then it may happen that series of invisible
     *          points are found which are - roughly said -skipped.
     */
    public void initializeControl(final int totalAmountOfPoints, final int desiredAmountOfPoints);

    /**
     * Returns true if data accumulation is according to the configuration of
     * this control not needed.
     * <p>
     * This e.g. may be the case if the control accumulates n points into one
     * but detected that there more resulting points are desired than total
     * points exist.
     * <p>
     * Please note: Result is only valid if {@link #initializeControl(int, int)}
     * has been called before!
     * <p>
     * 
     * @return true if data accumulation is according to the configuration of
     *         this control not needed.
     */
    public boolean isAccumulationBypassable();

    /**
     * Accessor that returns true if the current accumulation is complete.
     * <p>
     * Nothing is modified.
     * <p>
     * Contract is that this returns true whenever a call to
     * {@link #getAccumulatedPointIfAccumulationDone(IAccumulationFunction, ITracePoint2D)}
     * directly afterwards would not return <code>null</code> but a valid
     * accumulated {@link ITracePoint2D}.
     * <p>
     * 
     * @return true if the current accumulation is complete.
     */
    public abstract boolean isAccumulationDone();

  }

  /**
   * Registers a property change listener that will be informed about changes of
   * the property identified by the given <code>propertyName</code>.
   * <p>
   * 
   * @param propertyName
   *          the name of the property the listener is interested in
   * @param listener
   *          a listener that will only be informed if the property identified
   *          by the argument <code>propertyName</code> changes
   */
  public abstract void addPropertyChangeListener(final String propertyName,
      final PropertyChangeListener listener);

  /**
   * Returns the accumulationFunction.
   * <p>
   * 
   * @return the accumulationFunction
   */
  public abstract IAccumulationFunction getAccumulationFunction();

  /**
   * Template method to return an iterator over accumulated points.
   * <p>
   * 
   * @param source
   *          the real points of this trace.
   * 
   * @param amountOfDesiredPoints
   *          would allow to filter out points to accumulate by just taking n
   *          consecutive trace points.
   * 
   * 
   * @return an iterator over the accumulated points from the given iterator.
   */
  public abstract Iterator<ITracePoint2D> iterator(final ITrace2D source,
      final int amountOfDesiredPoints);

  /**
   * Unregisters a property change listener that has been registered for
   * listening on all properties.
   * <p>
   * 
   * @param listener
   *          a listener that will only be informed if the property identified
   *          by the argument <code>propertyName</code> changes
   */
  public abstract void removePropertyChangeListener(final PropertyChangeListener listener);

  /**
   * Removes a property change listener for listening on the given property.
   * <p>
   * 
   * @param property
   *          one of the constants with the <code>PROPERTY_</code> prefix
   *          defined in this class or subclasses.
   * 
   * @param listener
   *          the listener for this property change.
   */
  public abstract void removePropertyChangeListener(final String property,
      final PropertyChangeListener listener);

  /**
   * Sets the accumulationFunction to use for this strategy.
   * <p>
   * 
   * @param accumulationFunction
   *          the accumulationFunction to set.
   * 
   * @return the previous accumulation function used.
   */
  public abstract IAccumulationFunction setAccumulationFunction(
      IAccumulationFunction accumulationFunction);

}