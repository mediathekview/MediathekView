/*
 * IPointPainterCandleStick.java of project jchart2d, adds additional
 * configuration options for candlestick point painters. Copyright (C) 2002 -
 * 2013, Achim Westermann, created on Dec 7, 2013
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version. This library is distributed in the hope that it will be
 * useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 * 
 * If you modify or optimize the code in a useful way please let me know.
 * Achim.Westermann@gmx.de
 * 
 * 
 * File : $Source: /cvsroot/jchart2d/jchart2d/codetemplates.xml,v $ Date :
 * $Date: 2009/02/24 16:45:41 $ Version: $Revision: 1.2 $
 */

package info.monitorenter.gui.chart;

import info.monitorenter.gui.chart.tracepoints.CandleStick;

/**
 * Adds additional configuration options for candlestick point painters.
 * <p>
 * 
 * @param <T>
 *          needed for generics <code>{@link #compareTo(Object)}</code>.
 * 
 * @author <a href="mailto:Achim.Westermann@gmx.de">Achim Westermann </a>
 * 
 */

public interface IPointPainterCandleStick<T extends IPointPainterConfigurableUI<T>> extends IPointPainterConfigurableUI<T> {

  /**
   * Returns the width of the {@link CandleStick}.
   * <p>
   * 
   * @return the width of the {@link CandleStick}.
   */
  public int getWidth();

  /**
   * Returns whether the lower wick is terminated with a dash.
   * <p>
   * 
   * @return if true the lower wick will be terminated with a dash.
   */
  public boolean isDrawLowerWickDash();

  /**
   * Returns whether the upper wick is terminated with a dash.
   * <p>
   * 
   * @return if true the upper wick will be terminated with a dash.
   */
  public boolean isDrawUpperWickDash();

  /**
   * Set whether the lower wick should be terminated with a dash.
   * <p>
   * 
   * @param doit
   *          if true the lower wick will be terminated with a dash.
   */
  public void setDrawLowerWickDash(final boolean doit);

  /**
   * Set whether the upper wick should be terminated with a dash.
   * <p>
   * 
   * @param doit
   *          if true the upper wick will be terminated with a dash.
   */
  public void setDrawUpperWickDash(final boolean doit);

  /**
   * Sets the visible width of the candlestick.
   * <p>
   * 
   * @param width
   *          the visible width of the candlestick.
   * 
   * @return the former width of the candlestick.
   */
  public int setWidth(final int width);

}
