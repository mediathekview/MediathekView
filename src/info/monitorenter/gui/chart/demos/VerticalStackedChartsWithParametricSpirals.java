package info.monitorenter.gui.chart.demos;

import info.monitorenter.gui.chart.Chart2D;
import info.monitorenter.gui.chart.ITrace2D;
import info.monitorenter.gui.chart.ZoomableChart;
import info.monitorenter.gui.chart.traces.Trace2DSimple;
import info.monitorenter.gui.chart.traces.painters.TracePainterDisc;

import java.awt.BasicStroke;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;

import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JPanel;

/**
 * Two charts aligned vertically (via
 * {@link Chart2D#setSynchronizedXStartChart(Chart2D)}) that display geometric
 * forms backed by mathematics I do not understand (any more) ;-) .
 * <p>
 * 
 * 
 * @author Jason S, modified by Achim Westermann
 * 
 */
public class VerticalStackedChartsWithParametricSpirals {
  /**
   * Constructor taking a panel to add itself to. 
   * <p> 
   * 
   * @param panel to add this {@link JComponent} to. 
   */
  public VerticalStackedChartsWithParametricSpirals(JPanel panel) {
    panel.setLayout(new BorderLayout());
    panel.setPreferredSize(new Dimension(800, 500));

    final Chart2D[] charts = new Chart2D[2];
    for (int i = 0; i < 2; ++i) {
      charts[i] = new ZoomableChart();
      charts[i].setUseAntialiasing(true);
    }
    panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
    panel.add(charts[0]);
    panel.add(charts[1]);

    ITrace2D trace, trace2;
    trace = new Trace2DSimple();
    trace.setStroke(new BasicStroke(2));
    trace2 = new Trace2DSimple();
    trace2.setTracePainter(new TracePainterDisc(4));
    trace2.setColor(Color.LIGHT_GRAY);
    charts[0].addTrace(trace);
    charts[0].addTrace(trace2);
    trace.setZIndex(Integer.valueOf(1));
    trace2.setZIndex(Integer.valueOf(0));
    for (int i = 0; i < 10000; ++i) {
      double t = i / 10000.0;
      trace.addPoint(t * Math.cos(t * 20 * Math.PI), t * Math.sin(t * 20 * Math.PI));
      if (i % 6 == 0) {
        double rand = Math.random()*i;
        trace2.addPoint(t * Math.cos(t * (20 +rand)* Math.PI), t * Math.sin(t * (20 +rand)* Math.PI));
      }
    }

    trace2 = new Trace2DSimple();
    trace2.setStroke(new BasicStroke(2));
    ITrace2D trace3 = new Trace2DSimple();
    charts[1].addTrace(trace2);
    charts[1].addTrace(trace3);
    final double ay = 1;
    for (int i = 0; i < 10000; ++i) {
      double t = i / 10000.0;
      trace2.addPoint(Math.cos(t * 2 * Math.PI) + 0.2 * Math.cos(t * 20 * Math.PI), ay
          * (Math.sin(t * 2 * Math.PI) + 0.2 * Math.sin(t * 20 * Math.PI)));
      trace3.addPoint(Math.cos(t * 2 * Math.PI) - 0.2 * Math.cos(t * 20 * Math.PI), ay
          * (Math.sin(t * 2 * Math.PI) - 0.2 * Math.sin(t * 20 * Math.PI)));
    }
    trace3.setColor(Color.RED);
    trace3.setStroke(new BasicStroke(2, BasicStroke.CAP_BUTT, BasicStroke.JOIN_BEVEL, 10,
        new float[] {10, 10 }, 0));
  }

  public static void main(String[] args) {
    JFrame frame = new JFrame("Vertical stacked chart");
    JPanel panel = new JPanel();
    frame.setContentPane(panel);
    new VerticalStackedChartsWithParametricSpirals(panel);

    frame.setSize(new Dimension(400, 720));
    // frame.pack();
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.setVisible(true);
  }
}