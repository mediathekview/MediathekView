/*
 *  Trace2DCandleSticks.java of project jchart2d, an ITrace2D decorator 
 *  that adds the feature of drawing candlestick-traces. 
 *  Copyright (C) 2002 - 2013, Achim Westermann, created on Oct 2, 2012
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
import info.monitorenter.gui.chart.ITracePointProvider;
import info.monitorenter.gui.chart.pointpainters.PointPainterCandleStick;
import info.monitorenter.gui.chart.tracepoints.CandleStick;
import info.monitorenter.gui.chart.traces.painters.TracePainterConfigurable;

import java.awt.Color;
import java.awt.Stroke;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Iterator;
import java.util.Set;

/**
 * An ITrace2D decorator that adds the feature of drawing candlestick-traces.
 * <p>
 * Pass in the real trace implementation (e.g. <code>{@link Trace2DLtd}</code>
 * to the constructor to add the feature of drawing candlesticks to the
 * technical implementation. Like this:
 * 
 * <pre>
 * ITrace2D candleStickTrace = new Trace2DCandleSticks(new Trace2DLtd());
 * </pre>
 * <p>
 * 
 * @author <a href="mailto:Achim.Westermann@gmx.de">Achim Westermann </a>
 * 
 */
public class Trace2DCandleSticks implements ITrace2D {

  /** Generated <code>serialVersionUID</code>. **/
  private static final long serialVersionUID = -2358495593310332400L;

  /**
   * Reused candle stick point painter.
   */
//  private IPointPainter< ? > m_candleStickPainter;

  /**
   * The trace implementation that is being decorated with the
   * candlestick-feature.
   */
  private final ITrace2D m_delegate;

  /** The width of the candlestick. **/
  private int m_candleStickWidth = 2;
  /**
   * @see info.monitorenter.gui.chart.ITrace2D#initPaintIteration()
   */
  @Override
  public void initPaintIteration() {
    //  nop
  }



  /**
   * @see info.monitorenter.gui.chart.ITrace2D#onAdded2ChartBeforeFirstPaint()
   */
  @Override
  public void onAdded2ChartBeforeFirstPaint() {
    this.m_delegate.addTracePainter(new TracePainterConfigurable<PointPainterCandleStick>(new PointPainterCandleStick(this.m_candleStickWidth)));
  }



  /**
   * Constructor taking the trace implementation to decorate with candle stick
   * painting.
   * <p>
   * 
   * @param delegateThatIsEnrichedByCandlestickPainting
   *          impl that will deal with the basic trace functionality.
   * 
   * @param candleStickWidth
   *          width of the candlesticks.
   */
  public Trace2DCandleSticks(final ITrace2D delegateThatIsEnrichedByCandlestickPainting, final int candleStickWidth) {
    if(delegateThatIsEnrichedByCandlestickPainting == null) {
      throw new IllegalArgumentException("Given trace must not be null.");
    }
    this.m_delegate = delegateThatIsEnrichedByCandlestickPainting;
    this.m_candleStickWidth = candleStickWidth;
    /*
     * FIXME: Once method removeAllTracePainters() is available switch to that.
     */
    for (ITracePainter< ? > tracePainter : this.m_delegate.getTracePainters()) {
      this.m_delegate.removeTracePainter(tracePainter);
    }
  }
  
  

  /**
   * @param trace
   * @see info.monitorenter.gui.chart.ITrace2D#addComputingTrace(info.monitorenter.gui.chart.ITrace2D)
   */
  public void addComputingTrace(ITrace2D trace) {
    this.m_delegate.addComputingTrace(trace);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#addErrorBarPolicy(info.monitorenter.gui.chart.IErrorBarPolicy)
   */
  public boolean addErrorBarPolicy(IErrorBarPolicy< ? > errorBarPolicy) {
    return this.m_delegate.addErrorBarPolicy(errorBarPolicy);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#addPoint(double, double)
   */
  public boolean addPoint(double x, double y) {
    throw new UnsupportedOperationException("Don't use this on a " + this.getClass().getName()
        + " instance as this implementation needs a special ITracePoint2D implementation. Use addPoint(ITracePoint2D) with the proper "
        + CandleStick.class.getName() + " implementation.");
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#addPoint(info.monitorenter.gui.chart.ITracePoint2D)
   */
  public boolean addPoint(ITracePoint2D p) {
    CandleStick candleStick = (CandleStick) p;
    candleStick.removeAllAdditionalPointPainters();
//    candleStick.addAdditionalPointPainter(this.m_candleStickPainter);
    boolean result = this.m_delegate.addPoint(p, this);
    return result;
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#addPoint(info.monitorenter.gui.chart.ITracePoint2D,
   *      info.monitorenter.gui.chart.ITrace2D)
   */
  public boolean addPoint(ITracePoint2D p, ITrace2D wrapperOfMe) {
    return this.m_delegate.addPoint(p, wrapperOfMe);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#addPointHighlighter(info.monitorenter.gui.chart.IPointPainter)
   */
  public boolean addPointHighlighter(IPointPainter< ? > highlighter) {
    return this.m_delegate.addPointHighlighter(highlighter);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#addPropertyChangeListener(java.lang.String,
   *      java.beans.PropertyChangeListener)
   */
  public void addPropertyChangeListener(String propertyName, PropertyChangeListener listener) {
    this.m_delegate.addPropertyChangeListener(propertyName, listener);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#addTracePainter(info.monitorenter.gui.chart.ITracePainter)
   */
  public boolean addTracePainter(ITracePainter< ? > painter) {
    return this.m_delegate.addTracePainter(painter);
  }

  /**
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
  public int compareTo(ITrace2D o) {
    return this.m_delegate.compareTo(o);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#containsTracePainter(info.monitorenter.gui.chart.ITracePainter)
   */
  public boolean containsTracePainter(ITracePainter< ? > painter) {
    return this.m_delegate.containsTracePainter(painter);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#descendingIterator()
   */
  public Iterator<ITracePoint2D> descendingIterator() {
    return this.m_delegate.descendingIterator();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#firePointChanged(info.monitorenter.gui.chart.ITracePoint2D, info.monitorenter.gui.chart.ITracePoint2D.STATE, java.lang.Object, java.lang.Object)
   */
  public void firePointChanged(final ITracePoint2D changed, final ITracePoint2D.STATE state, final Object oldValue, final Object newValue) {
    this.m_delegate.firePointChanged(changed, state, oldValue, newValue);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getColor()
   */
  public Color getColor() {
    return this.m_delegate.getColor();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getErrorBarPolicies()
   */
  public Set<IErrorBarPolicy< ? >> getErrorBarPolicies() {
    return this.m_delegate.getErrorBarPolicies();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getHasErrorBars()
   */
  public boolean getHasErrorBars() {
    return this.m_delegate.getHasErrorBars();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getLabel()
   */
  public String getLabel() {
    return this.m_delegate.getLabel();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getMaxSize()
   */
  public int getMaxSize() {
    return this.m_delegate.getMaxSize();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getMaxX()
   */
  public double getMaxX() {
    /*
     * This works as delegate asks point which asks the special painter.
     */
    return this.m_delegate.getMaxX();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getMaxY()
   */
  public double getMaxY() {
    /*
     * This works as delegate asks point which asks the special painter.
     */
    return this.m_delegate.getMaxY();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getMinX()
   */
  public double getMinX() {
    /*
     * This works as delegate asks point which asks the special painter.
     */
    return this.m_delegate.getMinX();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getMinY()
   */
  public double getMinY() {
    /*
     * This works as delegate asks point which asks the special painter.
     */
    return this.m_delegate.getMinY();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getName()
   */
  public String getName() {
    return this.m_delegate.getName();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getNearestPointEuclid(double,
   *      double)
   */
  public DistancePoint getNearestPointEuclid(double x, double y) {
    // FIXME: search for the nearest point to the center of the candlestick
    // tracepoints.
    return this.m_delegate.getNearestPointEuclid(x, y);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getNearestPointManhattan(double,
   *      double)
   */
  public DistancePoint getNearestPointManhattan(double x, double y) {
    // FIXME: search for the nearest point to the center of the candlestick
    // tracepoints.
    return this.m_delegate.getNearestPointManhattan(x, y);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getPhysicalUnits()
   */
  public String getPhysicalUnits() {
    return this.m_delegate.getPhysicalUnits();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getPhysicalUnitsX()
   */
  public String getPhysicalUnitsX() {
    return this.m_delegate.getPhysicalUnitsX();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getPhysicalUnitsY()
   */
  public String getPhysicalUnitsY() {
    return this.m_delegate.getPhysicalUnitsY();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getPointHighlighters()
   */
  public Set<IPointPainter< ? >> getPointHighlighters() {
    return this.m_delegate.getPointHighlighters();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getPropertyChangeListeners(java.lang.String)
   */
  public PropertyChangeListener[] getPropertyChangeListeners(String property) {
    return this.m_delegate.getPropertyChangeListeners(property);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getRenderer()
   */
  public Chart2D getRenderer() {
    return this.m_delegate.getRenderer();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getSize()
   */
  public int getSize() {
    return this.m_delegate.getSize();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getStroke()
   */
  public Stroke getStroke() {
    return this.m_delegate.getStroke();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getTracePainters()
   */
  public Set<ITracePainter< ? >> getTracePainters() {
    return this.m_delegate.getTracePainters();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getTracePointProvider()
   */
  public ITracePointProvider getTracePointProvider() {
    return this.m_delegate.getTracePointProvider();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#getZIndex()
   */
  public Integer getZIndex() {
    return this.m_delegate.getZIndex();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#isAdditionalSpaceRequired()
   */
  public boolean isAdditionalSpaceRequired() {
    return this.m_delegate.isAdditionalSpaceRequired();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#isEmpty()
   */
  public boolean isEmpty() {
    return this.m_delegate.isEmpty();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#isPixelTransformationRequired()
   */
  public boolean isPixelTransformationRequired() {
    return this.m_delegate.isPixelTransformationRequired();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#isVisible()
   */
  public boolean isVisible() {
    return this.m_delegate.isVisible();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#iterator()
   */
  public Iterator<ITracePoint2D> iterator() {
    return this.m_delegate.iterator();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#maxXSearch()
   */
  public double maxXSearch() {
    return this.m_delegate.maxXSearch();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#maxYSearch()
   */
  public double maxYSearch() {
    return this.m_delegate.maxYSearch();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#minXSearch()
   */
  public double minXSearch() {
    return this.m_delegate.minXSearch();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#minYSearch()
   */
  public double minYSearch() {
    return this.m_delegate.minYSearch();
  }

  /**
   * @param evt
   * @see java.beans.PropertyChangeListener#propertyChange(java.beans.PropertyChangeEvent)
   */
  public void propertyChange(PropertyChangeEvent evt) {
    this.m_delegate.propertyChange(evt);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#removeAllPointHighlighters()
   */
  public Set<IPointPainter< ? >> removeAllPointHighlighters() {
    return this.m_delegate.removeAllPointHighlighters();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#removeAllPoints()
   */
  public void removeAllPoints() {
    this.m_delegate.removeAllPoints();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#removeComputingTrace(info.monitorenter.gui.chart.ITrace2D)
   */
  public boolean removeComputingTrace(ITrace2D trace) {
    return this.m_delegate.removeComputingTrace(trace);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#removeErrorBarPolicy(info.monitorenter.gui.chart.IErrorBarPolicy)
   */
  public boolean removeErrorBarPolicy(IErrorBarPolicy< ? > errorBarPolicy) {
    return this.m_delegate.removeErrorBarPolicy(errorBarPolicy);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#removePoint(info.monitorenter.gui.chart.ITracePoint2D)
   */
  public boolean removePoint(ITracePoint2D point) {
    return this.m_delegate.removePoint(point);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#removePointHighlighter(info.monitorenter.gui.chart.IPointPainter)
   */
  public boolean removePointHighlighter(IPointPainter< ? > highlighter) {
    return this.m_delegate.removePointHighlighter(highlighter);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#removePropertyChangeListener(java.beans.PropertyChangeListener)
   */
  public void removePropertyChangeListener(PropertyChangeListener listener) {
    this.m_delegate.removePropertyChangeListener(listener);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#removePropertyChangeListener(java.lang.String,
   *      java.beans.PropertyChangeListener)
   */
  public void removePropertyChangeListener(String property, PropertyChangeListener listener) {
    this.m_delegate.removePropertyChangeListener(property, listener);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#removeTracePainter(info.monitorenter.gui.chart.ITracePainter)
   */
  public boolean removeTracePainter(ITracePainter< ? > painter) {
    return this.m_delegate.removeTracePainter(painter);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#setColor(java.awt.Color)
   */
  public void setColor(Color color) {
    this.m_delegate.setColor(color);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#setErrorBarPolicy(info.monitorenter.gui.chart.IErrorBarPolicy)
   */
  public Set<IErrorBarPolicy< ? >> setErrorBarPolicy(IErrorBarPolicy< ? > errorBarPolicy) {
    return this.m_delegate.setErrorBarPolicy(errorBarPolicy);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#setName(java.lang.String)
   */
  public void setName(String name) {
    this.m_delegate.setName(name);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#setPhysicalUnits(java.lang.String,
   *      java.lang.String)
   */
  public void setPhysicalUnits(String xunit, String yunit) {
    this.m_delegate.setPhysicalUnits(xunit, yunit);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#setPointHighlighter(info.monitorenter.gui.chart.IPointPainter)
   */
  public Set<IPointPainter< ? >> setPointHighlighter(IPointPainter< ? > highlighter) {
    return this.m_delegate.setPointHighlighter(highlighter);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#setRenderer(info.monitorenter.gui.chart.Chart2D)
   */
  public void setRenderer(Chart2D renderer) {
    this.m_delegate.setRenderer(renderer);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#setStroke(java.awt.Stroke)
   */
  public void setStroke(Stroke stroke) {
    this.m_delegate.setStroke(stroke);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#setTracePainter(info.monitorenter.gui.chart.ITracePainter)
   */
  public Set<ITracePainter< ? >> setTracePainter(ITracePainter< ? > painter) {
    return this.m_delegate.setTracePainter(painter);
  }

  /**
   * @throws UnsupportedOperationException
   *           always.
   * 
   * @see info.monitorenter.gui.chart.ITrace2D#setTracePointProvider(info.monitorenter.gui.chart.ITracePointProvider)
   */
  public void setTracePointProvider(ITracePointProvider tracePointProvider) throws UnsupportedOperationException {
    throw new UnsupportedOperationException("Don't use this on a " + this.getClass().getName()
        + " instance as this implementation needs a special trace point provider implementation. ");
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#setVisible(boolean)
   */
  public void setVisible(boolean visible) {
    this.m_delegate.setVisible(visible);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#setZIndex(java.lang.Integer)
   */
  public void setZIndex(Integer zIndex) {
    this.m_delegate.setZIndex(zIndex);
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#showsErrorBars()
   */
  public boolean showsErrorBars() {
    return this.m_delegate.showsErrorBars();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#showsNegativeXErrorBars()
   */
  public boolean showsNegativeXErrorBars() {
    return this.m_delegate.showsNegativeXErrorBars();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#showsNegativeYErrorBars()
   */
  public boolean showsNegativeYErrorBars() {
    return this.m_delegate.showsNegativeYErrorBars();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#showsPositiveXErrorBars()
   */
  public boolean showsPositiveXErrorBars() {
    return this.m_delegate.showsPositiveXErrorBars();
  }

  /**
   * @see info.monitorenter.gui.chart.ITrace2D#showsPositiveYErrorBars()
   */
  public boolean showsPositiveYErrorBars() {
    return this.m_delegate.showsPositiveYErrorBars();
  }
}
