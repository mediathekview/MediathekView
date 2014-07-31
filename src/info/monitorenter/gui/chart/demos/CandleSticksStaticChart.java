/*
 * CandleSticksStaticChart.java of project jchart2d, a demonstration of using
 * candlesticks Copyright (C) 2013 Achim Westermann.
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 * 
 * If you modify or optimize the code in a useful way please let me know.
 * Achim.Westermann@gmx.de
 */
package info.monitorenter.gui.chart.demos;

import info.monitorenter.gui.chart.Chart2D;
import info.monitorenter.gui.chart.IPointPainterCandleStick;
import info.monitorenter.gui.chart.IPointPainterConfigurableUI;
import info.monitorenter.gui.chart.ITrace2D;
import info.monitorenter.gui.chart.labelformatters.LabelFormatterDate;
import info.monitorenter.gui.chart.pointpainters.PointPainterCandleStick;
import info.monitorenter.gui.chart.pointpainters.PointPainterDisc;
import info.monitorenter.gui.chart.tracepoints.CandleStick;
import info.monitorenter.gui.chart.traces.Trace2DCandleSticks;
import info.monitorenter.gui.chart.traces.Trace2DSimple;
import info.monitorenter.gui.chart.traces.painters.TracePainterConfigurable;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

import javax.swing.JFrame;
import javax.swing.JPanel;

/**
 * A demonstration of using {@link Trace2DCandleSticks} and 
 * {@link PointPainterCandleStick}
 * <p>
 * 
 * @author Achim Westermann
 * 
 * @version $Revision: 1.15 $
 */
public final class CandleSticksStaticChart extends JPanel {
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
    double value = 1.0E7;
    long lValue = (long) value;
    System.out.println(new Date(lValue));
    JFrame frame = new JFrame(CandleSticksStaticChart.class.getSimpleName());
    CandleSticksStaticChart stickChart = new CandleSticksStaticChart();
    frame.getContentPane().add(stickChart);
    frame.addWindowListener(new WindowAdapter() {
      /**
       * @see java.awt.event.WindowAdapter#windowClosing(java.awt.event.WindowEvent)
       */
      @Override
      public void windowClosing(final WindowEvent e) {
        System.exit(0);
      }
    });
    frame.setSize(1400, 300);
    stickChart.fill();
    frame.setVisible(true);

  }

  /**
   * Defcon.
   */
  private CandleSticksStaticChart() {
    this.setLayout(new BorderLayout());
    this.m_chart = new Chart2D();
    this.add(this.m_chart, BorderLayout.CENTER);
    this.m_chart.getAxisX().setFormatter(new LabelFormatterDate(new SimpleDateFormat("yyyy-MM-dd")));

  }

  private Chart2D m_chart;

  void fill() {

    // Create an ITrace:
    ITrace2D trace = new Trace2DCandleSticks(new Trace2DSimple(), 6);
    // Add the trace to the chart:
    this.m_chart.addTrace(trace);

    // Point highlighting:
    IPointPainterConfigurableUI< ? > pointHighlighter = new PointPainterDisc(10);
    pointHighlighter.setColor(Color.red);
    trace.setPointHighlighter(pointHighlighter);
    this.m_chart.enablePointHighlighting(true);

    // Fiddling with colours:
    TracePainterConfigurable< ? > painter = (TracePainterConfigurable< ? >) trace.getTracePainters().iterator().next();
    IPointPainterConfigurableUI< ? > pointPainter = painter.getPointPainter();
    pointPainter.setColor(Color.RED);
    pointPainter.setColorFill(Color.BLUE);
    pointPainter.setTransparencyFill(100);
    pointPainter.setTransparency(200);
    // Special for candlestick painter:
    IPointPainterCandleStick< ? > downCast = (IPointPainterCandleStick< ? >) pointPainter;
    downCast.setDrawLowerWickDash(true);
    downCast.setDrawUpperWickDash(true);

    // Add all points, as it is static:
    Calendar cal = Calendar.getInstance();
    cal.set(Calendar.HOUR_OF_DAY, 0);
    cal.set(Calendar.SECOND, 0);
    cal.set(Calendar.MILLISECOND, 0);
    cal.set(Calendar.DAY_OF_MONTH, 19);
    cal.set(Calendar.MONTH, 9);
    cal.set(Calendar.YEAR, 2012);

    // 03.10.2012
    cal.set(Calendar.DAY_OF_MONTH, 3);
    System.err.println("Calendar: " + new Date(cal.getTimeInMillis()));
    trace.addPoint(new CandleStick(cal.getTimeInMillis(), 7283.26, 7322.08, 7339.33, 7272.06));
    // 04.10.2012
    cal.set(Calendar.DAY_OF_MONTH, 4);
    trace.addPoint(new CandleStick(cal.getTimeInMillis(), 7368.21, 7305.21, 7374.86, 7283.48));
    // 05.10.2012
    cal.set(Calendar.DAY_OF_MONTH, 5);
    trace.addPoint(new CandleStick(cal.getTimeInMillis(), 7327.7, 7397.87, 7409.78, 7312.79));
    // 08.10.2012
    cal.set(Calendar.DAY_OF_MONTH, 8);
    trace.addPoint(new CandleStick(cal.getTimeInMillis(), 7340.39, 7291.21, 7341.28, 7286.24));
    // 09.10.2012
    cal.set(Calendar.DAY_OF_MONTH, 9);
    trace.addPoint(new CandleStick(cal.getTimeInMillis(), 7306.33, 7234.53, 7308.13, 7221.50));
    // 10.10.2012
    cal.set(Calendar.DAY_OF_MONTH, 10);
    trace.addPoint(new CandleStick(cal.getTimeInMillis(), 7219.46, 7205.23, 7245.17, 7201.09));
    // 11.10.2012
    cal.set(Calendar.DAY_OF_MONTH, 11);
    trace.addPoint(new CandleStick(cal.getTimeInMillis(), 7192.75, 7281.7, 7305.46, 7182.31));
    // 12.10.2012
    cal.set(Calendar.DAY_OF_MONTH, 12);
    trace.addPoint(new CandleStick(cal.getTimeInMillis(), 7260.57, 7232.49, 7291.24, 7232.42));
    // 15.10.2012
    cal.set(Calendar.DAY_OF_MONTH, 15);
    trace.addPoint(new CandleStick(cal.getTimeInMillis(), 7237.40, 7261.25, 7302.23, 7237.40));
    // 16.10.2012
    cal.set(Calendar.DAY_OF_MONTH, 16);
    trace.addPoint(new CandleStick(cal.getTimeInMillis(), 7309.08, 7376.27, 7387.78, 7293.90));
    // 17.10.2012
    cal.set(Calendar.DAY_OF_MONTH, 17);
    trace.addPoint(new CandleStick(cal.getTimeInMillis(), 7381.08, 7394.55, 7399.99, 7367.59));
    // 18.10.2012
    cal.set(Calendar.DAY_OF_MONTH, 18);
    trace.addPoint(new CandleStick(cal.getTimeInMillis(), 7401.41, 7437.23, 7447.81, 7389.01));
    // DAX 19.10.2012
    cal.set(Calendar.DAY_OF_MONTH, 19);
    trace.addPoint(new CandleStick(cal.getTimeInMillis(), 7413.69, 7380.64, 7430.12, 7363.50));

    this.m_chart.setToolTipType(Chart2D.ToolTipType.VALUE_SNAP_TO_TRACEPOINTS);

  }

}
