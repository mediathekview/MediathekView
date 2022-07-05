package mediathek.mainwindow;

import org.jetbrains.annotations.NotNull;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.DateAxis;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYSplineRenderer;
import org.jfree.chart.ui.RectangleInsets;
import org.jfree.data.time.Millisecond;
import org.jfree.data.time.TimeSeries;
import org.jfree.data.time.TimeSeriesCollection;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.text.DecimalFormat;
import java.util.concurrent.TimeUnit;

public class MemoryUsagePanel extends JPanel {

    private final TimeSeries total;

    public MemoryUsagePanel(int maxAge, @NotNull TimeUnit timeUnit) {

        super(new BorderLayout());

        total = new TimeSeries("Total Memory");
        total.setMaximumItemAge(TimeUnit.MILLISECONDS.convert(maxAge, timeUnit));
        TimeSeriesCollection dataset = new TimeSeriesCollection();
        dataset.addSeries(total);

        var domain = new DateAxis("Time");
        NumberAxis range = new NumberAxis("Memory");
        range.setAutoRange(true);

        var renderer = new XYSplineRenderer();
        renderer.setDefaultShapesVisible(false);
        renderer.setSeriesPaint(0, Color.red);

        var plot = new XYPlot(dataset, domain, range, renderer);
        plot.setBackgroundPaint(Color.BLACK);
        plot.setDomainGridlinePaint(Color.white);
        plot.setRangeGridlinePaint(Color.white);
        plot.setAxisOffset(new RectangleInsets(5.0, 5.0, 5.0, 5.0));

        domain.setAutoRange(true);
        domain.setLowerMargin(0.0);
        domain.setUpperMargin(0.0);
        domain.setTickLabelsVisible(true);

        range.setStandardTickUnits(NumberAxis.createIntegerTickUnits());
        range.setNumberFormatOverride(new DecimalFormat("#######.##"));

        var chart = new JFreeChart(plot);
        chart.removeLegend();
        add(new ChartPanel(chart));
    }

    private void addTotalObservation(double y) {
        total.add(new Millisecond(), y);
    }

    public class MemoryUsageDataGenerator extends Timer implements ActionListener {

        /**
         * Constructor.
         *
         * @param interval the interval
         */
        public MemoryUsageDataGenerator(int interval, @NotNull TimeUnit timeUnit) {
            super((int) TimeUnit.MILLISECONDS.convert(interval, timeUnit), null);
            addActionListener(this);
        }

        /**
         * Adds a new total memory reading (converted to MByte) to the dataset.
         *
         * @param event the action event.
         */
        public void actionPerformed(ActionEvent event) {
            long t = Runtime.getRuntime().totalMemory() / (1024 * 1024);
            addTotalObservation(t);
        }

    }
}
