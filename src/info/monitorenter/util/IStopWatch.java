/*
 * IStopWatch, a simple, "easy to  use" tool for measurement of durations.  
 * Copyright (C) 2002 - 2013 Achim Westermann, Achim.Westermann@gmx.de
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  If you modify or optimize the code in a useful way please let me know. 
 *  Achim.Westermann@gmx.de
 */

package info.monitorenter.util;

/**
 * A simple, "easy to  use" tool for measurement of durations.
 * <p>
 * 
 * @author <a href='mailto:Achim.Westermann@gmx.de'>Achim Westermann</a>
 * @version 1.2
 */
public interface IStopWatch {
  /**
   * Sets the state to running. This allows a call to <code>stop()</code> to
   * make a new measurement by taking the current time. If <code>reset()</code>
   * was invoked before, the m_start - time is set to the return value of
   * <code>{@link System#currentTimeMillis()}</code>. Else the old value is
   * preserved. False is returned if a measurement is already in progress. <b> A
   * call to m_start will only m_start a new measurement with the current Time,
   * if it is the first run or reset was called before. Else the time kept after
   * the next call to stop will be the sum of all previous runtimes.
   * 
   * @return false is returned if a measurement is already in progress (true
   *         else).
   **/
  public boolean start();

  /**
   * Stops the measurement by assigning current time in ms to the stop value.
   * Return true if a valid measurement has been made (StopWatch was running
   * before invoking this method).
   * 
   * @return true if a valid measurement has been made (StopWatch was running
   *         before invoking this method).
   **/
  public boolean stop();

  /**
   * This method may serve two purposes:<br/>
   * <ul>
   * <li>
   * Running- Reset: <br/>
   * The m_start- value of the measurement is set to the current time, even if
   * the StopWatch is running.</li>
   * <li>
   * Next- measurement- Reset:<br/>
   * A flag is set which causes the next call to m_start to delete the old
   * m_start - value.</li>
   * </ul>
   * Note that continuous calls to <code>m_start()</code> and
   * <code>stop()</code> without calling reset will always relate the
   * measurement of time to the first time <code>m_start()</code> was called!
   **/
  public void reset();

  /**
   * 
   * Returns the current value of the IStopWatch in ms. This has to be the sum
   * of all previous measurements (circles of <code>m_start()-stop()</code>) not
   * interrupted by calls to <code>reset()</code>.
   * <p>
   * This method does not change the state from running to !running but performs
   * an update of the overall measurement- data inside. The difference to
   * <code>stop()<code>:<br>
   *  After <code>stop()</code> has been called the state is set to !running
   * which causes a new m_start-value to be set during the next call to
   * <code>m_start()</code>. The call to <code>snapShot()</code> does not switch
   * the state. If afterwards <code>m_start()</code> is called, no new value
   * gets assigned to the m_start- value of the StopWatch. Despite of this
   * <code>snapShot</code> Adds the period from the m_start-value to now to the
   * internal total measurement- call. To avoid double - summation of the same
   * time- periods a new m_start- value is set directly.
   * <p>
   * 
   * @return the amount of milliseconds this stop watch has been running since
   *         last call to {@link #reset()}.
   **/
  public long snapShot();
}
