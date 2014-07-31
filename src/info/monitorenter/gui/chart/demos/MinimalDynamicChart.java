/*
 *  MinimalDynamicChart.java of project jchart2d, a demonstration 
 *  of the minimal code to set up a chart with dynamic data. 
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
import info.monitorenter.gui.chart.ITrace2D;
import info.monitorenter.gui.chart.traces.Trace2DLtd;

import java.awt.Color;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.Timer;
import java.util.TimerTask;

import javax.swing.JFrame;

/**
 * Demonstrates minimal effort to create a dynamic chart.
 * <p>
 * 
 * @author <a href="mailto:Achim.Westermann@gmx.de">Achim Westermann </a>
 * 
 */
public final class MinimalDynamicChart {

  /**
   * Main entry.
   * <p>
   * 
   * @param args
   *          ignored
   */
  public static void main(final String[] args) {
    // Create a chart:
    Chart2D chart = new Chart2D();
    // Create an ITrace:
    // Note that dynamic charts need limited amount of values!!!
    final ITrace2D trace = new Trace2DLtd(100);
    trace.setColor(Color.RED);

    // Add the trace to the chart:
    chart.addTrace(trace);
    IAxis<?> axisX = chart.getAxisX();
    axisX.setStartMajorTick(false);
    axisX.setMajorTickSpacing(10);
    // Make it visible:
    // Create a frame.
    JFrame frame = new JFrame("MinimalDynamicChart");
    // add the chart to the frame:
    frame.getContentPane().add(chart);
    frame.setSize(400, 300);
    // Enable the termination button [cross on the upper right edge]:
    frame.addWindowListener(new WindowAdapter() {
      /**
       * @see java.awt.event.WindowAdapter#windowClosing(java.awt.event.WindowEvent)
       */
      @Override
      public void windowClosing(final WindowEvent e) {
        System.exit(0);
      }
    });
    frame.setVisible(true);
    
    
      /* 
       * Now the dynamic adding of points. This is just a demo! 
       * 
       * Use a separate thread to simulate dynamic adding of date. 
       * Note that you do not have to copy this code. Dynamic charting is just about 
       * adding points to traces at runtime from another thread. Whenever you hook on 
       * to a serial port or some other data source with a polling Thread (or an event 
       * notification pattern) you will have your own thread that just has to add points 
       * to a trace. 
       */
    
    Timer timer = new Timer(true);
    TimerTask task = new TimerTask(){

      private double m_y = 0;
      private long m_starttime = System.currentTimeMillis();
      /**
       * @see java.util.TimerTask#run()
       */
      @Override
      public void run() {
        double rand = Math.random();
        boolean add = (rand >= 0.5) ? true : false;
        this.m_y = (add) ? this.m_y + Math.random() : this.m_y - Math.random();
        trace.addPoint(((double) System.currentTimeMillis() - this.m_starttime), this.m_y);
      }
      
    };
    // Every 200 milliseconds a new value is collected.
    timer.schedule(task, 1000, 200);
  }

  /** Defcon. */
  private MinimalDynamicChart() {
    // nop
  }
}
