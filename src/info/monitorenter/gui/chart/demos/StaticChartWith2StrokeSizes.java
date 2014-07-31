/*
 *  StaticChartWith2StrokeSizes.java of project jchart2d, a demonstration 
 *  that bigger strokes extend up, down, left and right. 
 *  Copyright (C) 2007 - 2013 Achim Westermann, created on 10.12.2004, 13:48:55
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
 *
 */
package info.monitorenter.gui.chart.demos;

import info.monitorenter.gui.chart.Chart2D;
import info.monitorenter.gui.chart.IAxis;
import info.monitorenter.gui.chart.IAxisScalePolicy;
import info.monitorenter.gui.chart.ITrace2D;
import info.monitorenter.gui.chart.axis.scalepolicy.AxisScalePolicyManualTicks;
import info.monitorenter.gui.chart.labelformatters.LabelFormatterNumber;
import info.monitorenter.gui.chart.rangepolicies.RangePolicyFixedViewport;
import info.monitorenter.gui.chart.traces.Trace2DSimple;
import info.monitorenter.util.Range;

import java.awt.BasicStroke;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.text.DecimalFormat;

import javax.swing.JFrame;
import javax.swing.JPanel;

/**
 * Description: A demonstration that bigger strokes extend up, down, left and
 * right
 * <p>
 * 
 * @author Achim Westermann
 * 
 * @version $Revision: 1.15 $
 */
public final class StaticChartWith2StrokeSizes extends JPanel {
  /**
   * Generated for <code>serialVersionUID</code>.
   */
  private static final long serialVersionUID = 3257009847668192306L;

  /**
   * Main entry.
   * <p>
   * 
   * @param args
   *          ignored.
   */
  public static void main(final String[] args) {
    for (int i = 0; i < 1; i++) {
      JFrame frame = new JFrame(StaticChartWith2StrokeSizes.class.getName());
      frame.getContentPane().add(new StaticChartWith2StrokeSizes());
      frame.addWindowListener(new WindowAdapter() {
        /**
         * @see java.awt.event.WindowAdapter#windowClosing(java.awt.event.WindowEvent)
         */
        @Override
        public void windowClosing(final WindowEvent e) {
          System.exit(0);
        }
      });
      frame.setSize(300, 300);
      frame.setLocation(i % 3 * 200, i / 3 * 100);
      frame.setVisible(true);
    }
  }

  /**
   * Defcon.
   */
  private StaticChartWith2StrokeSizes() {
    this.setLayout(new BorderLayout());
    Chart2D chart = new Chart2D();

    // Create an ITrace:
    // Note that dynamic charts need limited amount of values!!!
    // ITrace2D trace = new Trace2DLtd(200);
    ITrace2D trace1 = new Trace2DSimple();
    trace1.setColor(Color.RED);
    trace1.setStroke(new BasicStroke(10));

    ITrace2D trace2 = new Trace2DSimple();
    trace2.setColor(Color.BLUE);

    // Add the trace to the chart:
    chart.addTrace(trace1);
    chart.addTrace(trace2);
    
    IAxis<IAxisScalePolicy> xAxis = (IAxis<IAxisScalePolicy>)chart.getAxisX();
    xAxis.setRangePolicy(new RangePolicyFixedViewport(new Range(0, 50)));
    xAxis.setAxisScalePolicy(new AxisScalePolicyManualTicks());
    xAxis.setMinorTickSpacing(1);
    xAxis.setFormatter(new LabelFormatterNumber(new DecimalFormat("###,###")));
    xAxis.setPaintGrid(true);

    IAxis<IAxisScalePolicy> yAxis = (IAxis<IAxisScalePolicy>)chart.getAxisY();
    yAxis.setRangePolicy(new RangePolicyFixedViewport(new Range(0, 150)));
    yAxis.setAxisScalePolicy(new AxisScalePolicyManualTicks());
    yAxis.setMinorTickSpacing(0.5);

    yAxis.setFormatter(new LabelFormatterNumber(new DecimalFormat("#0.00")));

    // Add all points, as it is static:
    double rand;
    double current;
    boolean add;

    for (int i = 0; i < 120; i++) {
      rand = Math.random();
      add = rand > 0.5;
      if (add) {
        current = i + rand * i;
      } else {
        current = i - rand * i;
      }
      trace1.addPoint(i, current);
      trace2.addPoint(i, current);
    }

    chart.setToolTipType(Chart2D.ToolTipType.VALUE_SNAP_TO_TRACEPOINTS);

    chart.getAxisY().setPaintScale(false);
    chart.getAxisX().setPaintScale(false);

    // Make it visible:
    this.add(chart, BorderLayout.CENTER);

  }

}
