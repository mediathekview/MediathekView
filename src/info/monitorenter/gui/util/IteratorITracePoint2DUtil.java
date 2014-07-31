/*
 *  IteratorITracePoint2DUtil.java of project jchart2d, <enterpurposehere>. 
 *  Copyright (C) 2002 - 2013, Achim Westermann, created on Nov 13, 2011
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

package info.monitorenter.gui.util;

import info.monitorenter.gui.chart.ITrace2DDataAccumulating;
import info.monitorenter.gui.chart.ITracePoint2D;

import java.util.Iterator;

/**
 * Utility class helper, created for supporting data accumulation of
 * {@link ITrace2DDataAccumulating}.
 * <p>
 * 
 * @author <a href="mailto:Achim.Westermann@gmx.de">Achim Westermann </a>
 * 
 */
public class IteratorITracePoint2DUtil {

  /**
   * Utility class constructor: hide as instance not needed.
   * <p>
   */
  private IteratorITracePoint2DUtil() {
    // nop
  }

  /**
   * Simple struct to return the last skipped {@link ITracePoint2D} along with
   * the skip count.
   * <p>
   * 
   * 
   * @author <a href="mailto:Achim.Westermann@gmx.de">Achim Westermann </a>
   * 
   */
  public static class SkipResult {
    /**
     * The last invisible trace point seen.
     */
    private ITracePoint2D m_lastInvisible;

    /**
     * As there is no "peek-method" in iterator when searching for the first
     * visible point we will already have consumed it. This stores the first
     * visible result.
     * <p>
     * 
     */
    private ITracePoint2D m_firstVisible;

    /**
     * As there is no "peek-method" in iterator when searching for the first
     * visible point we will already have consumed it. This stores the first
     * visible result.
     * <p>
     * 
     * @return the first visible point skipped to.
     */
    public ITracePoint2D getFirstVisible() {
      return this.m_firstVisible;
    }

    /**
     * The amount of points skipped.
     */
    private int m_skipCount;

    /**
     * Returns the amount of points skipped.
     * <p>
     * 
     * @return the amount of points skipped.
     */
    public int getSkipCount() {
      return this.m_skipCount;
    }

    /**
     * Returns the last invisible trace point seen.
     * <p>
     * 
     * @return the last invisible trace point seen.
     */
    public ITracePoint2D getLastInvisible() {
      return this.m_lastInvisible;
    }

    /**
     * Creates an instance containing both values.
     * <p>
     * 
     * @param lastInvisible
     *          the last invisible trace point seen.
     * 
     * @param firstVisible
     *          the first visible trace point found.
     * 
     * @param skipCount
     *          the amount of points skipped.
     */
    SkipResult(final ITracePoint2D lastInvisible, final ITracePoint2D firstVisible, final int skipCount) {
      this.m_lastInvisible = lastInvisible;
      this.m_firstVisible = firstVisible;
      this.m_skipCount = skipCount;
    }

  }

  /**
   * Scrolls the given iterator to the first point that is in visible scaled x
   * range [0.0 .. 1.0].
   * <p>
   * Note: this only makes sense if the given iterator returns trace points with
   * order of x values (ascending or descending!
   * <p>
   * Assumption: The points in the iterator already have been scaled to the
   * visible range (by Chart2D). So this works by using
   * {@link ITracePoint2D#getScaledX()} and finding out if the value is within
   * [0.0 .. 1.0].
   * <p>
   * Note: the state of the given iterator is changed. After this call you may
   * continue iterating. You most probably will first consume the position 0 of
   * the result (interpolated point to the lower x bound) and position 1 of the
   * result (first trace point above given x point).
   * <p>
   * Note: There is no guarantee that the iterator will return visible points
   * after this call as y bounds or the upper x bound is not checked!
   * <p>
   * 
   * @param traceIt
   *          the source iterator to scroll.
   * 
   * @return the point that was found before getting into visible x range or the
   *         first point found if it was already visible or null if no point was
   *         found in the iterator.
   */
  public static SkipResult scrollToFirstVisibleXValue(final Iterator<ITracePoint2D> traceIt) {
    ITracePoint2D point = null;

    ITracePoint2D previous = null;
    ITracePoint2D current = null;
    int countSkip = 0;
    double xScaled;
    while (traceIt.hasNext()) {
      current = traceIt.next();
      xScaled = current.getScaledX();
      if ((xScaled >= 0.0) && (xScaled <= 1.0)) {
        if (previous == null) {
          point = current;
        } else {
          point = previous;
        }
        break;
      } else {
        previous = current;
        countSkip++;
      }
    }
    SkipResult result = new SkipResult(point, current, countSkip);
    return result;
  }

}
