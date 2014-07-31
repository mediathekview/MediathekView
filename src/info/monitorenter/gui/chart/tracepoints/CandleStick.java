/*
 *  CandleStick.java of project jchart2d, <enterpurposehere>. 
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
 *
 *
 * File   : $Source: /cvsroot/jchart2d/jchart2d/codetemplates.xml,v $
 * Date   : $Date: 2009/02/24 16:45:41 $
 * Version: $Revision: 1.2 $
 */

package info.monitorenter.gui.chart.tracepoints;

import info.monitorenter.gui.chart.IAxis;
import info.monitorenter.gui.util.TracePoint2DUtil;

/**
 * Faked tracepoint that adds the properties to contain all data for a
 * candlestick.
 * <p>
 * 
 * See <a target="_blank" href="http://en.wikipedia.org/wiki/Candlestick_chart"
 * >http://en.wikipedia.org/wiki/Candlestick_chart</a>
 * <p>
 * 
 * This implementation only works correctly with a special candlestick point
 * painter.
 * <p>
 * The original {@link #getY()} method is mapped to {@link #getStart()}. The
 * original {@link #getX()} method is sufficient to be related to all other y
 * values as a candlestick has a single point in time (x value).
 * <p>
 * 
 * @author <a href="mailto:Achim.Westermann@gmx.de">Achim Westermann </a>
 * 
 */
public class CandleStick extends TracePoint2D {

  /**
   * To remember cached nearest spot detection. 
   * <p>
   */
  private enum NEAREST_SPOT {
      START,
      LOW,
      END,
      HIGH
  };
  
  /**
   * To remember cached nearest spot detection. 
   * <p>
   */
  private NEAREST_SPOT m_cachedNearestSpot = NEAREST_SPOT.START;
  
  /** Generated <code>serialVersionUID</code>. **/
  private static final long serialVersionUID = -135311007801611830L;

  /** The high y value. **/
  private double m_high;

  /** The low y value. **/
  private double m_low;

  /** The end y value. **/
  private double m_end;

  /**
   * Constructor with every argument needed.
   * <p>
   * See <a target="_blank"
   * href="http://en.wikipedia.org/wiki/Candlestick_chart"
   * >http://en.wikipedia.org/wiki/Candlestick_chart</a>
   * <p>
   * 
   * @param xValue
   *          the x coordinate.
   * 
   * @param startY
   *          the start y value.
   * 
   * @param endY
   *          the end y value.
   * 
   * @param highY
   *          the high y value.
   * 
   * @param lowY
   *          the low y value.
   */
  public CandleStick(final double xValue, final double startY, final double endY, final double highY, final double lowY) {
    super(xValue, startY);
    if (startY > highY) {
      throw new IllegalArgumentException("start Y must not be higher than high Y.");
    }
    if (startY < lowY) {
      throw new IllegalArgumentException("start Y must not be lower than low Y.");
    }
    if (endY > highY) {
      throw new IllegalArgumentException("end Y must not be higher than high Y.");

    }
    if (endY < lowY) {
      throw new IllegalArgumentException("endY must not be lower than low Y.");
    }
    this.m_end = endY;
    this.m_high = highY;
    this.m_low = lowY;
  }

  /**
   * Returns the end y value.
   * <p>
   * 
   * @return the end y value.
   */
  public double getEnd() {
    return this.m_end;
  }

  /**
   * @see info.monitorenter.gui.chart.tracepoints.TracePoint2D#getEuclidDistance(double,
   *      double)
   */
  @Override
  public double getEuclidDistance(final double xNormalized,final double yNormalized) {
    double result = java.lang.Double.MAX_VALUE;
    double improve = this.getEuclidDistance(xNormalized, yNormalized, this.getScaledX(), this.scaleY(this.getLow()));
    if(improve < result) {
      result = improve;
      this.m_cachedNearestSpot = NEAREST_SPOT.LOW;
    }
    improve = this.getEuclidDistance(xNormalized, yNormalized, this.getScaledX(), this.scaleY(this.getStart()));
    if(improve < result) {
      result = improve;
      this.m_cachedNearestSpot = NEAREST_SPOT.START;
    }
    improve = this.getEuclidDistance(xNormalized, yNormalized, this.getScaledX(), this.scaleY(this.getEnd()));
    if(improve < result) {
      result = improve;
      this.m_cachedNearestSpot = NEAREST_SPOT.END;
    }
    improve = this.getEuclidDistance(xNormalized, yNormalized, this.getScaledX(), this.scaleY(this.getHigh()));
    if(improve < result) {
      result = improve;
      this.m_cachedNearestSpot = NEAREST_SPOT.HIGH;
    }
    return result;
  }
  
  /**
   * Internal helper that returns the eculid distance between the given
   * "outside" coordinates (mouse move) and the "inside" coordinates. "Inside"
   * coordinates means: The tracepoint may have several areas of interest like
   * this candle stick. So finding the shortest distance to it may be the
   * question which area of interest is closest: The start value, the end value,
   * the high value or the low value.
   * <p>
   * 
   * @param xNormalized
   *          the normalized x coordinate between 0 and 1.0 to measure the
   *          Euclid distance to.
   * 
   * @param yNormalized
   *          the normalized y coordinate between 0 and 1.0 to measure the
   *          Euclid distance to.
   * 
   * @param myScaledX
   *          the normalized x "inside" coordinate between 0 and 1.0 to measure
   *          the Euclid distance to.
   * 
   * @param myScaledY
   *          the normalized y "inside" coordinate between 0 and 1.0 to measure
   *          the Euclid distance to.
   * 
   * @return the eculid distance between the given "outside" coordinates (mouse
   *         move) and the "inside" coordinates.
   * 
   */
  protected final double getEuclidDistance(final double xNormalized, final double yNormalized, final double myScaledX, final double myScaledY) {
    double result;
    final double xdist = Math.abs(myScaledX - xNormalized);
    final double ydist = Math.abs(myScaledY - yNormalized);
    result = Math.sqrt(Math.pow(xdist, 2) + Math.pow(ydist, 2));
    return result;
  }

  /**
   * Returns the high y value.
   * <p>
   * 
   * @return the high y value.
   */
  public double getHigh() {
    return this.m_high;
  }

  /**
   * Returns the low y value.
   * <p>
   * 
   * @return the low y value.
   */
  public double getLow() {
    return this.m_low;
  }

  /**
   * @see info.monitorenter.gui.chart.tracepoints.TracePoint2D#getManhattanDistance(double,
   *      double)
   */
  @Override
  public double getManhattanDistance(double xNormalized, double yNormalized) {
    double result = java.lang.Double.MAX_VALUE;
    double improve = this.getManhattanDistance(xNormalized, yNormalized, this.getScaledX(), this.scaleY(this.getLow()));
    if(improve < result) {
      result = improve;
      this.m_cachedNearestSpot = NEAREST_SPOT.LOW;
    }
    improve = this.getManhattanDistance(xNormalized, yNormalized, this.getScaledX(), this.scaleY(this.getStart()));
    if(improve < result) {
      result = improve;
      this.m_cachedNearestSpot = NEAREST_SPOT.START;
    }
    improve = this.getManhattanDistance(xNormalized, yNormalized, this.getScaledX(), this.scaleY(this.getEnd()));
    if(improve < result) {
      result = improve;
      this.m_cachedNearestSpot = NEAREST_SPOT.END;
    }
    improve = this.getManhattanDistance(xNormalized, yNormalized, this.getScaledX(), this.scaleY(this.getHigh()));
    if(improve < result) {
      result = improve;
      this.m_cachedNearestSpot = NEAREST_SPOT.HIGH;
    }
    return result;
  }

  /**
   * Internal helper that returns the manhattan distance between the given
   * "outside" coordinates (mouse move) and the "inside" coordinates. "Inside"
   * coordinates means: The tracepoint may have several areas of interest like
   * this candle stick. So finding the shortest distance to it may be the
   * question which area of interest is closest: The start value, the end value,
   * the high value or the low value.
   * <p>
   * 
   * @param xNormalized
   *          the normalized x coordinate between 0 and 1.0 to measure the
   *          manhattan distance to.
   * 
   * @param yNormalized
   *          the normalized y coordinate between 0 and 1.0 to measure the
   *          manhattan distance to.
   * 
   * @param myScaledX
   *          the normalized x "inside" coordinate between 0 and 1.0 to measure
   *          the manhattan distance to.
   * 
   * @param myScaledY
   *          the normalized y "inside" coordinate between 0 and 1.0 to measure
   *          the manhattan distance to.
   * 
   * @return the manhattan distance between the given "outside" coordinates (mouse
   *         move) and the "inside" coordinates.
   * 
   */
  public double getManhattanDistance(final double xNormalized, final double yNormalized, final double myScaledX, final double myScaledY) {
    double result;
    result = Math.abs(myScaledX - xNormalized) + Math.abs(myScaledY - yNormalized);
    return result;
  }
  /**
   * @see info.monitorenter.gui.chart.tracepoints.TracePoint2D#getNormalizedHighlightSweetSpotCoordinates()
   */
  @Override
  public double[] getNormalizedHighlightSweetSpotCoordinates() {
    double[] result = new double[2];
    result[0] = this.getScaledX();
    switch (this.m_cachedNearestSpot) {
      case START :{
        result[1] = this.scaleY(this.getStart());
        break;
      }
      case END :{
        result[1] = this.scaleY(this.getEnd());
        break;
      }
      case HIGH :{
        result[1] = this.scaleY(this.getHigh());
        break;
      }
      case LOW :{
        result[1] = this.scaleY(this.getLow());
        break;
      }
    }
    return result;
  }

  /**
   * Returns the start y value.
   * <p>
   * 
   * Note: this is the reused inherited {@link #getY()}.
   * <p>
   * 
   * @return the start y value.
   */
  public double getStart() {
    return this.getY();
  }

  /**
   * @see info.monitorenter.gui.chart.ITracePoint2D#getTooltipText()
   */
  @Override
  public String getTooltipText() {
    String result = null;
    StringBuffer buffer = new StringBuffer();
    IAxis< ? > yAxis = TracePoint2DUtil.getAxisYOfTracePoint(this);
    switch(this.m_cachedNearestSpot) {
      case END: {
        buffer.append("End: ");
        buffer.append(yAxis.getFormatter().format(this.getEnd()));
        break;
        
      }
      case HIGH: {
        buffer.append("High: ");
        buffer.append(yAxis.getFormatter().format(this.getHigh()));
        break;
      }
      case LOW: {
        buffer.append("Low: ");
        buffer.append(yAxis.getFormatter().format(this.getLow()));
        break;
      }
      case START: {
        buffer.append("Start: ");
        buffer.append(yAxis.getFormatter().format(this.getStart()));
        break;
      }
    }
    result = buffer.toString();
    return result;
  }

  
  /**
   * Helper to scale the additional values in this trace.  
   * <p>
   * FIXME: Think about caching or move this to the axis code along with setters? Should be profiled. 
   * <p> 
   * 
   * @param value the internal value to scale. 
   * 
   * @return the scaled value. 
   *
   */
  protected double scaleX(final double value) {
    IAxis<?> xAxis = TracePoint2DUtil.getAxisXOfTracePoint(this);
    double result = xAxis.getScaledValue(value);
    return result;
  }
  /**
   * Helper to scale the additional values in this trace.  
   * <p>
   * FIXME: Think about caching or move this to the axis code along with setters? Should be profiled. 
   * <p> 
   * 
   * @param value the internal value to scale. 
   * 
   * @return the scaled value. 
   *
   */
  protected double scaleY(final double value) {
    IAxis<?> xAxis = TracePoint2DUtil.getAxisYOfTracePoint(this);
    double result = xAxis.getScaledValue(value);
    return result;
  }
  
}
