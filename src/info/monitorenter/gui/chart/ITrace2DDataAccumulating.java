/*
 * ITrace2DDataAccumulating, the interface for all traces used by the Chart2D.
 * Copyright (c) 2004 - 2013  Achim Westermann, Achim.Westermann@gmx.de
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
package info.monitorenter.gui.chart;


import info.monitorenter.gui.chart.traces.accumulationstrategies.AAccumulationStrategy;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.Serializable;
import java.util.Iterator;


/**
 * An <code>{@link IAxis}</code> sub interface intended for implementations that
 * are able to accumulate several points into a single one.
 * <p>
 * 
 * This is needed for performance reasons. Consider a trace containing 1.000.000
 * trace points. It would be very slow to move the chart window or in any case
 * various repaints are caused (e.g. by tooltips or spanning a rectangle to
 * zoom).
 * <p>
 * Therefore this sub-interface gives more information to the trace in order to
 * be able to drop certain points:
 * <ul>
 * <li>How many points should it's iterator return?</li>
 * <li>In which range should the points be (visibility)?</li>
 * </ul>
 * As a result only few points are really returned to the renderer (
 * <code>{@link Chart2D} </code>) and painting will become much faster but also
 * able to adapt to be more detailed in value subdomains (in the case of
 * zooming).
 * <p>
 * 
 * <h3>Property Change events</h3>
 * 
 * Please see the class documentation of {@link ITrace2D} for basic supported
 * events. The following <code>{@link java.beans.PropertyChangeEvent}</code> may
 * be additionally fired to <code>{@link PropertyChangeListener}</code>
 * instances that register themselves with
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
 * <code>{@link info.monitorenter.gui.chart.ITrace2DDataAccumulating#PROPERTY_ACCUMULATION_STRATEGY}</code>
 * </td>
 * <td><code>{@link ITrace2DDataAccumulating}</code> that changed</td>
 * <td><code>{@link AAccumulationStrategy}</code>, the old value</td>
 * <td><code>{@link AAccumulationStrategy}</code>, the new value</td>
 * <td>
 * <code>{@link ITrace2DDataAccumulating#setAccumulationStrategy(IAccumulationStrategy)}</code>
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
 * @version $Revision: 1.41 $
 */
public interface ITrace2DDataAccumulating extends ITrace2D, PropertyChangeListener, Serializable {

  /**
   * The property key defining the
   * <code>{@link #setAccumulationStrategy(IAccumulationStrategy)}</code>
   * property. Use in combination with
   * {@link #addPropertyChangeListener(String, PropertyChangeListener)}.
   */
  public static final String PROPERTY_ACCUMULATION_STRATEGY = "ITrace2DDataAccumulating.PROPERTY_ACCUMULATION_STRATEGY";

  /**
   * The property key defining a change of the
   * <code>{@link #setAccumulationStrategy(IAccumulationStrategy)}</code>
   * property: Namely
   * {@link AAccumulationStrategy#setAccumulationFunction(IAccumulationFunction)}
   * was called.
   * <p>
   * 
   * Use in combination with
   * {@link #addPropertyChangeListener(String, PropertyChangeListener)}.
   * <p>
   */
  public static final String PROPERTY_ACCUMULATION_STRATEGY_ACCUMULATION_FUNCTION_CHANGED = "ITrace2DDataAccumulating.PROPERTY_ACCUMULATION_STRATEGY_ACCUMULATION_FUNCTION_CHANGED";

  /**
   * Returns the current accumulation strategy.
   * <p>
   * 
   * @return the current accumulation strategy.
   */
  public IAccumulationStrategy getAccumulationStrategy();

  /**
   * Returns an <code>Iterator</code> over the internal <code>
   * {@link ITracePoint2D}</code> instances that might accumulate internal
   * {@link ITracePoint2D} instances into one.
   * <p>
   * Implementations should be synchronized. As this method may return
   * "synthetic" points created at runtime that are made up by accumulation of
   * several real internal points modifications of the instances returned are
   * lossy!
   * <p>
   * There is no guarantee that changes made to the contained tracepoints will
   * be reflected in the display immediately. The order the iterator returns the
   * <code>TracePoint2D</code> instances decides how the <code>Chart2D</code>
   * will paint the trace.
   * <p>
   * <b>Important contract</b><br/>
   * No bounds of traces in x dimension should ever be changed for sorted traces
   * (by x value). This means the lowest point in x-dimension and the highest
   * point in x-dimension have to be returned unchanged in order not to change
   * the x value domain.
   * <p>
   * 
   * @param amountOfDesiredPoints
   *          The amount of points that should at least be returned to the
   *          caller. Note that implementation may return twice as much points
   *          depending on the accumulation function that is used (e.g. an
   *          arithmetic mean function would need at least two points to fold
   *          them into one so if the internal amount of points is not twice as
   *          much as the requested amount accumulation has to be skipped).
   * 
   * @return an <code>Iterator</code> over the internal <code>
   *         {@link ITracePoint2D}</code> instances that might accumulate
   *         internal {@link ITracePoint2D} instances into one by taking visible
   *         range and desired amount of points into account.
   */
  public Iterator<ITracePoint2D> iterator(final int amountOfDesiredPoints);

  /**
   * Installs the given accumulation strategy.
   * <p>
   * 
   * @param accumulationStrategy
   *          the accumulation strategy to use.
   * 
   * @return the accumulation strategy used previously.
   */
  public IAccumulationStrategy setAccumulationStrategy(final IAccumulationStrategy accumulationStrategy);
}
