package info.monitorenter.gui.chart;

/*
 *  IAccumulationFunction.java of project jchart2d, interface for tracepoint 
 *  accumulation functions. 
 *  Copyright (C) 2002 - 2013, Achim Westermann, created on Oct 6, 2011
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

/**
 * Interface for functions that may accumulate several
 * <code>{@link ITracePoint2D}</code> instances into one.
 * <p>
 * 
 * This is for example needed to let
 * <code>{@link ITrace2DDataAccumulating}</code> combine several tracepoints
 * into one.
 * <p>
 * 
 * Note that this does not describe the state-of-the-art plain interface for
 * accumulating trace points (or even generic data) but is designed in a
 * stateful manner that allows user-code to work on trace points in a stream
 * based manner. So the design might look (and be) a bit hard to use / not-self
 * explanatory. But this is done with performance in mind.
 * <p>
 * <b>How to use</b>
 * <ul>
 * <li>Create an instance.</li>
 * <li>
 * Start iterating over your own {@link ITracePoint2D} instances (user code
 * should be in the iterator implementations of {@link ITrace2D}
 * implementations). Iterate in the following manner:
 * <ul>
 * <li>Reset your loop counter for the amounts of points to accumulate.</li>
 * <li>Check if the current point is within proper bounds / visible / not NaN.</li>
 * <li>In case it is valid feed it to this function via
 * {@link #addPointToAccumulate(ITracePoint2D)} and increment your point
 * counter.</li>
 * <li>Check if you have accumulated enough points. If so, then get the
 * accumulated point via {@link #getAccumulatedPoint()} and return it from your
 * iterator. As a side effect the accumulation function is in the state empty
 * again, ready for accumulation of the next n points.</li>
 * <li>In case you are implementing an iterator for a trace that is sorted (by x
 * -value) and your counter is not full but your loop has no more points to
 * iterate return the last real point your own loop iterated. This is needed
 * because the contract of {@link ITrace2DDataAccumulating#iterator(int)} .</li>
 * <li>done!</li>
 * </ul>
 * </ul>
 * <p>
 * 
 * @author <a href="mailto:Achim.Westermann@gmx.de">Achim Westermann </a>
 **/
public interface IAccumulationFunction {

  /**
   * Adds a point to accumulated.
   * <p>
   * Contract: No point that returns true from
   * {@link ITracePoint2D#isDiscontinuation()} must be given to this method. An
   * {@link IllegalArgumentException} should punish violators of this contract.
   * Reason: Accumulating a discontinuation would consume it while it has to be
   * preserved to allow {@link Chart2D} to render this.
   * <p>
   * 
   * @param point
   *          a point to accumulated.
   * 
   * @throws IllegalArgumentException
   *           if null or a point with {@link ITracePoint2D#isDiscontinuation()}
   *           is provided.
   * 
   */
  public void addPointToAccumulate(final ITracePoint2D point) throws IllegalArgumentException;

  /**
   * Returns the accumulated point resulting from all the points fed via
   * {@link #addPointToAccumulate(ITracePoint2D)} since the last call this
   * method was invoked.
   * <p>
   * As a side effect all previous accumulation data is reset and this function
   * is empty again, ready to accumulate the next n points. Internally you might
   * want to store the result in case you want to cherry pick the following
   * point to accumulated in relation to it.
   * <p>
   * 
   * @return null or the accumulated point resulting from all the points fed via
   *         {@link #addPointToAccumulate(ITracePoint2D)} since the last call
   *         this method was invoked. If there was nothing to accumulate
   *         <code>null</code> is returned.
   */
  public ITracePoint2D getAccumulatedPoint();

  /**
   * Returns the current amount of accumulated points.
   * <p>
   * 
   * @return the current amount of accumulated points.
   */
  public int getAccumulatedPointCount();
  
  /**
   * Returns the current accumulated point.
   * <p>
   * 
   * @return the current accumulated point.
   */
  public ITracePoint2D getAccumulatedPointCurrent();

}
