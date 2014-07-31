/*
 * APointPainterCandleStick.java of project jchart2d, adds additional
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

package info.monitorenter.gui.chart.pointpainters;

import info.monitorenter.gui.chart.IPointPainterCandleStick;
import info.monitorenter.gui.chart.IPointPainterConfigurableUI;

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
public abstract class APointPainterCandleStick<T extends IPointPainterConfigurableUI<T>> extends APointPainter<T> implements IPointPainterCandleStick<T> {

  /** Generated <code>serialVersionUID</code>. **/
  private static final long serialVersionUID = 2299510143045015739L;

  /**
   * If true lower wick will be terminated with a dash.
   */
  private boolean m_drawLowerWickDash = false;

  /**
   * If true upper wick will be terminated with a dash.
   */
  private boolean m_drawUpperWickDash = false;

  /** The width of the candlestick. */
  private int m_width;

  /**
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (!super.equals(obj)) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    APointPainterCandleStick<?> other = (APointPainterCandleStick<?>) obj;
    if (this.m_drawLowerWickDash != other.m_drawLowerWickDash) {
      return false;
    }
    if (this.m_drawUpperWickDash != other.m_drawUpperWickDash) {
      return false;
    }
    if (this.m_width != other.m_width) {
      return false;
    }
    return true;
  }

  /**
   * @see info.monitorenter.gui.chart.IPointPainterCandleStick#getWidth()
   */
  @Override
  public int getWidth() {
    return this.m_width;
  }

  /**
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + (this.m_drawLowerWickDash ? 1231 : 1237);
    result = prime * result + (this.m_drawUpperWickDash ? 1231 : 1237);
    result = prime * result + this.m_width;
    return result;
  }

  /**
   * @see info.monitorenter.gui.chart.IPointPainterCandleStick#isDrawLowerWickDash()
   */
  @Override
  public boolean isDrawLowerWickDash() {
    return this.m_drawLowerWickDash;
  }

  /**
   * @see info.monitorenter.gui.chart.IPointPainterCandleStick#isDrawUpperWickDash()
   */
  @Override
  public boolean isDrawUpperWickDash() {
    return this.m_drawUpperWickDash;
  }

  /**
   * @see info.monitorenter.gui.chart.IPointPainterCandleStick#setDrawLowerWickDash(boolean)
   */
  @Override
  public void setDrawLowerWickDash(final boolean doit) {
    this.m_drawLowerWickDash = doit;

  }

  /**
   * @see info.monitorenter.gui.chart.IPointPainterCandleStick#setDrawUpperWickDash(boolean)
   */
  @Override
  public void setDrawUpperWickDash(final boolean doit) {
    this.m_drawUpperWickDash = doit;
  }

  /**
   * @see info.monitorenter.gui.chart.IPointPainterCandleStick#setWidth(int)
   */
  @Override
  public int setWidth(final int width) {
    int result = this.m_width;
    this.m_width = width;
    return result;
  }

}
