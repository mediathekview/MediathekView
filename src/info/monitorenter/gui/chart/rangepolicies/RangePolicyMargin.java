/*
 *  RangePolicyMargin.java,  This policy add a selectable margin from max and min value, the margin is a ratio 
 *  of the range. A minimum margin can be defined.
 *  Copyright (c) 2004 - 2013  Achim Westermann, Achim.Westermann@gmx.de
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
package info.monitorenter.gui.chart.rangepolicies;

import info.monitorenter.gui.chart.rangepolicies.ARangePolicy;

/**
 * This policy add a selectable margin from max and min value, the margin is a
 * ratio of the range. A minimum margin can be defined.
 * <p>
 * 
 * @author Ramon Zambelli
 * 
 */
public final class RangePolicyMargin extends ARangePolicy {

  /** Generated <code>serialVersionUID.</code> **/
  private static final long serialVersionUID = 1008709094237454345L;

  /** the margin ratio. **/
  private double m_marginRatio;

  /** The minimum margin. **/
  private double m_marginMin;

  /**
   * Creates an instance with a margin ration of 0.1.
   * <p>
   */
  public RangePolicyMargin() {
    this(0.1, 0.1);
  }

  /**
   * Creates an instance with the given relative margin ratio and the absolute minimum margin. 
   * <p>
   * 
   * @param marginRatio the ratio for the margin. 
   * 
   * @param marginMin the minimum absolute (value domain) margin. 
   */
  public RangePolicyMargin(double marginRatio, double marginMin) {
    super();
    this.m_marginRatio = marginRatio;
    this.m_marginMin = marginMin;
  }

  /**
   * Calculates the margin.
   * <p>
   * 
   * @param chartMin
   *          minimum of the chart.
   * 
   * @param chartMax
   *          maximum of the chart.
   * 
   * @return the margin.
   */
  private double calculateMargin(final double chartMin,final  double chartMax) {
    double margin = (chartMax - chartMin) * this.m_marginRatio;
    if (margin < m_marginMin) {
      margin = this.m_marginMin;
    }
    return margin;
  }

  /**
   * @see info.monitorenter.gui.chart.IRangePolicy#getMax(double, double)
   */
  public double getMax(final double chartMin,final  double chartMax) {
    return chartMax + this.calculateMargin(chartMin, chartMax);
  }

  /**
   * @see info.monitorenter.gui.chart.IRangePolicy#getMin(double, double)
   */
  public double getMin(final double chartMin,final  double chartMax) {
    return chartMin - this.calculateMargin(chartMin, chartMax);
  }

}