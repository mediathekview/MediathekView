/*
 *  TraceProxy.java of project jchart2d, allows mixing in behavior 
 *  (by overriding methods in anonymous class creation) for traces. 
 *  Copyright (C) 2002 - 2013, Achim Westermann, created on Oct 26, 2013
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

package info.monitorenter.gui.chart.traces;

import info.monitorenter.gui.chart.Chart2D;
import info.monitorenter.gui.chart.IErrorBarPolicy;
import info.monitorenter.gui.chart.IPointPainter;
import info.monitorenter.gui.chart.ITrace2D;
import info.monitorenter.gui.chart.ITracePainter;
import info.monitorenter.gui.chart.ITracePoint2D;
import info.monitorenter.gui.chart.ITracePoint2D.STATE;
import info.monitorenter.gui.chart.ITracePointProvider;

import java.awt.Color;
import java.awt.Stroke;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Iterator;
import java.util.Set;

/**
 * Allows mixing in behavior (by overriding methods in anonymous class creation)
 * for traces.
 * <p>
 * 
 * Use like:
 * 
 * <pre>
 * ITrace2D trace = new TraceProxy(myOtherTrace) {
 *   public void <overriddenmethod>(<args>) {
 *     super.<overridenmethod>(<args>);
 *     <overridencode>
 *   }
 * }
 * </pre>
 * 
 * @author <a href="mailto:Achim.Westermann@gmx.de">Achim Westermann </a>
 * 
 */
public class TraceProxy implements ITrace2D {

  /** Generated <code>serialVersionUID</code>. **/
  private static final long serialVersionUID = 2531087995813165405L;

  private final ITrace2D m_delegate;

  /**
   * Creates a proxy for the given trace.
   * <p>
   * 
   * @param delegate
   *          the delegate to wrap.
   */
  public TraceProxy(final ITrace2D delegate) {
    this.m_delegate = delegate;
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#addComputingTrace(info.monitorenter.gui.chart.ITrace2D)
   */
  @Override
  public void addComputingTrace(final ITrace2D trace) {
    this.m_delegate.addComputingTrace(trace);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#addErrorBarPolicy(info.monitorenter.gui.chart.IErrorBarPolicy)
   */
  @Override
  public boolean addErrorBarPolicy(final IErrorBarPolicy< ? > errorBarPolicy) {
    return this.m_delegate.addErrorBarPolicy(errorBarPolicy);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#addPoint(double, double)
   */
  @Override
  public boolean addPoint(final double x, final double y) {
    return this.m_delegate.addPoint(x, y);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#addPoint(info.monitorenter.gui.chart.ITracePoint2D)
   */
  @Override
  public boolean addPoint(final ITracePoint2D p) {
    return this.m_delegate.addPoint(p);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#addPoint(info.monitorenter.gui.chart.ITracePoint2D,
   *      info.monitorenter.gui.chart.ITrace2D)
   */
  @Override
  public boolean addPoint(final ITracePoint2D p, final ITrace2D wrapperOfMe) {
    return this.m_delegate.addPoint(p, wrapperOfMe);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#addPointHighlighter(info.monitorenter.gui.chart.IPointPainter)
   */
  @Override
  public boolean addPointHighlighter(final IPointPainter< ? > highlighter) {
    return this.m_delegate.addPointHighlighter(highlighter);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#addPropertyChangeListener(java.lang.String,
   *      java.beans.PropertyChangeListener)
   */
  @Override
  public void addPropertyChangeListener(final String propertyName, final PropertyChangeListener listener) {
    this.m_delegate.addPropertyChangeListener(propertyName, listener);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#addTracePainter(info.monitorenter.gui.chart.ITracePainter)
   */
  @Override
  public boolean addTracePainter(final ITracePainter< ? > painter) {
    return this.m_delegate.addTracePainter(painter);
  }

  /**
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
  @Override
  public int compareTo(final ITrace2D o) {
    return this.m_delegate.compareTo(o);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#containsTracePainter(info.monitorenter.gui.chart.ITracePainter)
   */
  @Override
  public boolean containsTracePainter(final ITracePainter< ? > painter) {
    return this.m_delegate.containsTracePainter(painter);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#descendingIterator()
   */
  @Override
  public Iterator<ITracePoint2D> descendingIterator() {
    return this.m_delegate.descendingIterator();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#firePointChanged(info.monitorenter.gui.chart.ITracePoint2D,
   *      info.monitorenter.gui.chart.ITracePoint2D.STATE, java.lang.Object,
   *      java.lang.Object)
   */
  @Override
  public void firePointChanged(final ITracePoint2D changed, final STATE state, final Object oldValue, final Object newValue) {
    this.m_delegate.firePointChanged(changed, state, oldValue, newValue);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getColor()
   */
  @Override
  public Color getColor() {
    return this.m_delegate.getColor();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getErrorBarPolicies()
   */
  @Override
  public Set<IErrorBarPolicy< ? >> getErrorBarPolicies() {
    return this.m_delegate.getErrorBarPolicies();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getHasErrorBars()
   */
  @Override
  public boolean getHasErrorBars() {
    return this.m_delegate.getHasErrorBars();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getLabel()
   */
  @Override
  public String getLabel() {
    return this.m_delegate.getLabel();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getMaxSize()
   */
  @Override
  public int getMaxSize() {
    return this.m_delegate.getMaxSize();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getMaxX()
   */
  @Override
  public double getMaxX() {
    return this.m_delegate.getMaxX();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getMaxY()
   */
  @Override
  public double getMaxY() {
    return this.m_delegate.getMaxY();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getMinX()
   */
  @Override
  public double getMinX() {
    return this.m_delegate.getMinX();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getMinY()
   */
  @Override
  public double getMinY() {
    return this.m_delegate.getMinY();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getName()
   */
  @Override
  public String getName() {
    return this.m_delegate.getName();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getNearestPointEuclid(double,
   *      double)
   */
  @Override
  public DistancePoint getNearestPointEuclid(final double x, final double y) {
    return this.m_delegate.getNearestPointEuclid(x, y);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getNearestPointManhattan(double,
   *      double)
   */
  @Override
  public DistancePoint getNearestPointManhattan(final double x, final double y) {
    return this.m_delegate.getNearestPointManhattan(x, y);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getPhysicalUnits()
   */
  @Override
  public String getPhysicalUnits() {
    return this.m_delegate.getPhysicalUnits();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getPhysicalUnitsX()
   */
  @Override
  public String getPhysicalUnitsX() {
    return this.m_delegate.getPhysicalUnitsX();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getPhysicalUnitsY()
   */
  @Override
  public String getPhysicalUnitsY() {
    return this.m_delegate.getPhysicalUnitsY();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getPointHighlighters()
   */
  @Override
  public Set<IPointPainter< ? >> getPointHighlighters() {
    return this.m_delegate.getPointHighlighters();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getPropertyChangeListeners(java.lang.String)
   */
  @Override
  public PropertyChangeListener[] getPropertyChangeListeners(final String property) {
    return this.m_delegate.getPropertyChangeListeners(property);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getRenderer()
   */
  @Override
  public Chart2D getRenderer() {
    return this.m_delegate.getRenderer();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getSize()
   */
  @Override
  public int getSize() {
    return this.m_delegate.getSize();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getStroke()
   */
  @Override
  public Stroke getStroke() {
    return this.m_delegate.getStroke();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getTracePainters()
   */
  @Override
  public Set<ITracePainter< ? >> getTracePainters() {
    return this.m_delegate.getTracePainters();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getTracePointProvider()
   */
  @Override
  public ITracePointProvider getTracePointProvider() {
    return this.m_delegate.getTracePointProvider();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getZIndex()
   */
  @Override
  public Integer getZIndex() {
    return this.m_delegate.getZIndex();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#initPaintIteration()
   */
  @Override
  public void initPaintIteration() {
    this.m_delegate.initPaintIteration();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#isAdditionalSpaceRequired()
   */
  @Override
  public boolean isAdditionalSpaceRequired() {
    return this.m_delegate.isAdditionalSpaceRequired();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#isEmpty()
   */
  @Override
  public boolean isEmpty() {
    return this.m_delegate.isEmpty();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#isPixelTransformationRequired()
   */
  @Override
  public boolean isPixelTransformationRequired() {
    return this.m_delegate.isPixelTransformationRequired();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#isVisible()
   */
  @Override
  public boolean isVisible() {
    return this.m_delegate.isVisible();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#iterator()
   */
  @Override
  public Iterator<ITracePoint2D> iterator() {
    return this.m_delegate.iterator();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#maxXSearch()
   */
  @Override
  public double maxXSearch() {
    return this.m_delegate.maxXSearch();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#maxYSearch()
   */
  @Override
  public double maxYSearch() {
    return this.m_delegate.maxYSearch();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#minXSearch()
   */
  @Override
  public double minXSearch() {
    return this.m_delegate.minXSearch();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#minYSearch()
   */
  @Override
  public double minYSearch() {
    return this.m_delegate.minYSearch();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#onAdded2ChartBeforeFirstPaint()
   */
  @Override
  public void onAdded2ChartBeforeFirstPaint() {
    this.m_delegate.onAdded2ChartBeforeFirstPaint();
  }

  /**
   * @see java.beans.PropertyChangeListener#propertyChange(java.beans.PropertyChangeEvent)
   */
  @Override
  public void propertyChange(final PropertyChangeEvent evt) {
    this.m_delegate.propertyChange(evt);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#removeAllPointHighlighters()
   */
  @Override
  public Set<IPointPainter< ? >> removeAllPointHighlighters() {
    return this.m_delegate.removeAllPointHighlighters();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#removeAllPoints()
   */
  @Override
  public void removeAllPoints() {
    this.m_delegate.removeAllPoints();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#removeComputingTrace(info.monitorenter.gui.chart.ITrace2D)
   */
  @Override
  public boolean removeComputingTrace(final ITrace2D trace) {
    return this.m_delegate.removeComputingTrace(trace);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#removeErrorBarPolicy(info.monitorenter.gui.chart.IErrorBarPolicy)
   */
  @Override
  public boolean removeErrorBarPolicy(final IErrorBarPolicy< ? > errorBarPolicy) {
    return this.m_delegate.removeErrorBarPolicy(errorBarPolicy);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#removePoint(info.monitorenter.gui.chart.ITracePoint2D)
   */
  @Override
  public boolean removePoint(final ITracePoint2D point) {
    return this.m_delegate.removePoint(point);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#removePointHighlighter(info.monitorenter.gui.chart.IPointPainter)
   */
  @Override 
  public boolean removePointHighlighter(final IPointPainter< ? > highlighter) {
    return this.m_delegate.removePointHighlighter(highlighter);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#removePropertyChangeListener(java.beans.PropertyChangeListener)
   */
  @Override
  public void removePropertyChangeListener(final PropertyChangeListener listener) {
    this.m_delegate.removePropertyChangeListener(listener);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#removePropertyChangeListener(java.lang.String,
   *      java.beans.PropertyChangeListener)
   */
  @Override
  public void removePropertyChangeListener(final String property, final PropertyChangeListener listener) {
    this.m_delegate.removePropertyChangeListener(property, listener);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#removeTracePainter(info.monitorenter.gui.chart.ITracePainter)
   */
  @Override
  public boolean removeTracePainter(final ITracePainter< ? > painter) {
    return this.m_delegate.removeTracePainter(painter);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#setColor(java.awt.Color)
   */
  @Override
  public void setColor(final Color color) {
    this.m_delegate.setColor(color);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#setErrorBarPolicy(info.monitorenter.gui.chart.IErrorBarPolicy)
   */
  @Override
  public Set<IErrorBarPolicy< ? >> setErrorBarPolicy(final IErrorBarPolicy< ? > errorBarPolicy) {
    return this.m_delegate.setErrorBarPolicy(errorBarPolicy);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#setName(java.lang.String)
   */
  @Override
  public void setName(final String name) {
    this.m_delegate.setName(name);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#setPhysicalUnits(java.lang.String,
   *      java.lang.String)
   */
  @Override
  public void setPhysicalUnits(final String xunit, final String yunit) {
    this.m_delegate.setPhysicalUnits(xunit, yunit);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#setPointHighlighter(info.monitorenter.gui.chart.IPointPainter)
   */
  @Override
  public Set<IPointPainter< ? >> setPointHighlighter(final IPointPainter< ? > highlighter) {
    return this.m_delegate.setPointHighlighter(highlighter);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#setRenderer(info.monitorenter.gui.chart.Chart2D)
   */
  @Override
  public void setRenderer(final Chart2D renderer) {
    this.m_delegate.setRenderer(renderer);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#setStroke(java.awt.Stroke)
   */
  @Override
  public void setStroke(final Stroke stroke) {
    this.m_delegate.setStroke(stroke);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#setTracePainter(info.monitorenter.gui.chart.ITracePainter)
   */
  @Override
  public Set<ITracePainter< ? >> setTracePainter(final ITracePainter< ? > painter) {
    return this.m_delegate.setTracePainter(painter);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#setTracePointProvider(info.monitorenter.gui.chart.ITracePointProvider)
   */
  @Override
  public void setTracePointProvider(final ITracePointProvider tracePointProvider) {
    this.m_delegate.setTracePointProvider(tracePointProvider);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#setVisible(boolean)
   */
  @Override
  public void setVisible(final boolean visible) {
    this.m_delegate.setVisible(visible);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#setZIndex(java.lang.Integer)
   */
  @Override
  public void setZIndex(final Integer zIndex) {
    this.m_delegate.setZIndex(zIndex);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#showsErrorBars()
   */
  @Override
  public boolean showsErrorBars() {
    return this.m_delegate.showsErrorBars();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#showsNegativeXErrorBars()
   */
  @Override
  public boolean showsNegativeXErrorBars() {
    return this.m_delegate.showsNegativeXErrorBars();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#showsNegativeYErrorBars()
   */
  @Override
  public boolean showsNegativeYErrorBars() {
    return this.m_delegate.showsNegativeYErrorBars();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#showsPositiveXErrorBars()
   */
  @Override
  public boolean showsPositiveXErrorBars() {
    return this.m_delegate.showsPositiveXErrorBars();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#showsPositiveYErrorBars()
   */
  @Override
  public boolean showsPositiveYErrorBars() {
    return this.m_delegate.showsPositiveYErrorBars();
  }
}
