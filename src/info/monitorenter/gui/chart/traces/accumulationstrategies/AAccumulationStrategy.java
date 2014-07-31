/*
 *  AccumulationStrategy.java of project jchart2d, <enterpurposehere>. 
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

package info.monitorenter.gui.chart.traces.accumulationstrategies;

import info.monitorenter.gui.chart.IAccumulationFunction;
import info.monitorenter.gui.chart.IAccumulationStrategy;
import info.monitorenter.gui.chart.ITrace2D;
import info.monitorenter.gui.chart.ITrace2DDataAccumulating;
import info.monitorenter.gui.chart.ITracePoint2D;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.Iterator;

import javax.swing.event.SwingPropertyChangeSupport;

/**
 * Design helper to deal with the following: If a trace is unsorted then you
 * have to accumulated all n consecutive points into one regardless whether
 * they jump large distances in the value domain. The output might look like
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
 * density of x values. You will not loose data in value-ranges with low
 * density but are able to drop lots of unnecessary values in high-density
 * areas (see
 * {@link AccumulationStrategyXRangeWithRespectToDensity}).
 * <p>
 * 
 * <h3>Property Change events</h3>
 * The following <code>{@link java.beans.PropertyChangeEvent}</code> may be
 * fired to <code>{@link PropertyChangeListener}</code> instances that
 * register themselves with
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
public abstract class AAccumulationStrategy implements IAccumulationStrategy {
 
  /**
   * The property key defining the
   * <code>{@link #getAccumulationFunction()}</code> property. Use in
   * combination with
   * {@link #addPropertyChangeListener(String, PropertyChangeListener)}.
   */
  public static String PROPERTY_ACCUMULATION_FUNCTION = "AccumulationStrategy.PROPERTY_ACCUMULATION_FUNCTION";

  /**
   * The accumulation function used.
   */
  private IAccumulationFunction m_accumulationFunction;

  /**
   * The instance that add support for firing
   * <code>PropertyChangeEvents</code> and maintaining
   * <code>PropertyChangeListeners</code>.
   * <p>
   */
  protected PropertyChangeSupport m_propertyChangeSupport = new SwingPropertyChangeSupport(this);

  /**
   * Constructor taking the accumulation function to use.
   * <p>
   * 
   * @param accumulationFunction
   *          the accumulation function to use.
   */
  public AAccumulationStrategy(IAccumulationFunction accumulationFunction) {
    this.setAccumulationFunction(accumulationFunction);
  }

  /**
   * @see info.monitorenter.gui.chart.IAccumulationStrategy#addPropertyChangeListener(java.lang.String, java.beans.PropertyChangeListener)
   */
  @Override
  public void addPropertyChangeListener(final String propertyName, final PropertyChangeListener listener) {
    this.m_propertyChangeSupport.addPropertyChangeListener(propertyName, listener);
  }

  /**
   * Fires a property change event to the registered listeners.
   * <p>
   * 
   * @param property
   *          one of the <code>PROPERTY_XXX</code> constants defined in <code>
   *          {@link AAccumulationStrategy}</code>.
   * 
   * @param oldvalue
   *          the old value of the property.
   * 
   * @param newvalue
   *          the new value of the property.
   */
  protected final void firePropertyChange(final String property, final Object oldvalue, final Object newvalue) {
    this.m_propertyChangeSupport.firePropertyChange(property, oldvalue, newvalue);
  }

  /**
   * @see info.monitorenter.gui.chart.IAccumulationStrategy#getAccumulationFunction()
   */
  @Override
  public IAccumulationFunction getAccumulationFunction() {
    return this.m_accumulationFunction;
  }

  /**
   * @see info.monitorenter.gui.chart.IAccumulationStrategy#iterator(info.monitorenter.gui.chart.ITrace2D, int)
   */
  @Override
  public abstract Iterator<ITracePoint2D> iterator(final ITrace2D source, final int amountOfPoints);

  /**
   * @see info.monitorenter.gui.chart.IAccumulationStrategy#removePropertyChangeListener(java.beans.PropertyChangeListener)
   */
  @Override
  public void removePropertyChangeListener(final PropertyChangeListener listener) {
    this.m_propertyChangeSupport.removePropertyChangeListener(listener);
  }

  /**
   * @see info.monitorenter.gui.chart.IAccumulationStrategy#removePropertyChangeListener(java.lang.String, java.beans.PropertyChangeListener)
   */
  @Override
  public void removePropertyChangeListener(final String property, final PropertyChangeListener listener) {
    this.m_propertyChangeSupport.removePropertyChangeListener(property, listener);
  }

  /**
   * @see info.monitorenter.gui.chart.IAccumulationStrategy#setAccumulationFunction(info.monitorenter.gui.chart.IAccumulationFunction)
   */
  @Override
  public IAccumulationFunction setAccumulationFunction(IAccumulationFunction accumulationFunction) {
    IAccumulationFunction result = this.m_accumulationFunction;
    this.m_accumulationFunction = accumulationFunction;
    this.firePropertyChange(PROPERTY_ACCUMULATION_FUNCTION, result, accumulationFunction);
    return result;
  }
}