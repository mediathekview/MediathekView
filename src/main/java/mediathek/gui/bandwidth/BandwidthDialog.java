/*
 * Created by JFormDesigner on Tue Jan 03 13:11:50 CET 2023
 */

package mediathek.gui.bandwidth;

import mediathek.tool.FileUtils;
import mediathek.tool.http.MVHttpClient;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
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
import java.text.SimpleDateFormat;
import java.util.concurrent.TimeUnit;

/**
 * @author christianfranzke
 */
public class BandwidthDialog extends JDialog {
    private final TimeSeries total = new TimeSeries("Bandwidth");
    private final DateAxis dateAxis = new DateAxis();
    private final NumberAxis bandwidthAxis = new NumberAxis();
    private final BandwidthUsageDataGenerator dataGenerator;

    public BandwidthDialog(Window owner) {
        super(owner);
        initComponents();

        dataGenerator = new BandwidthUsageDataGenerator(1, TimeUnit.SECONDS);

        setupChart();
    }

    private void setLabelColors() {
        var color = UIManager.getColor("Label.foreground");

        dateAxis.setLabelPaint(color);
        dateAxis.setTickLabelPaint(color);
        dateAxis.setTickMarkPaint(color);
        //prevent display of date x-axis labels
        dateAxis.setDateFormatOverride(new SimpleDateFormat(""));

        bandwidthAxis.setLabelPaint(color);
        bandwidthAxis.setTickLabelPaint(color);
        bandwidthAxis.setTickMarkPaint(color);
    }

    private void setupChart() {
        total.setMaximumItemAge(TimeUnit.MILLISECONDS.convert(1, TimeUnit.MINUTES));

        TimeSeriesCollection dataset = new TimeSeriesCollection();
        dataset.addSeries(total);

        bandwidthAxis.setAutoRange(true);

        setLabelColors();

        var renderer = new XYSplineRenderer();
        renderer.setDefaultShapesVisible(false);
        renderer.setSeriesPaint(0, Color.red);

        var plot = new XYPlot(dataset, dateAxis, bandwidthAxis, renderer);
        plot.setBackgroundPaint(Color.BLACK);
        plot.setDomainGridlinePaint(Color.white);
        plot.setRangeGridlinePaint(Color.white);
        plot.setAxisOffset(new RectangleInsets(5.0, 5.0, 5.0, 5.0));

        dateAxis.setAutoRange(true);
        dateAxis.setLowerMargin(0.0);
        dateAxis.setUpperMargin(0.0);
        dateAxis.setTickLabelsVisible(true);

        bandwidthAxis.setStandardTickUnits(NumberAxis.createIntegerTickUnits());
        bandwidthAxis.setNumberFormatOverride(new DecimalFormat("#######.##"));

        var chart = new JFreeChart(plot);
        chart.removeLegend();

        chartPanel1.setPopupMenu(null);
        chartPanel1.setChart(chart);

        //reset counters first as there will be spikes otherwise...
        MVHttpClient.getInstance().getByteCounter().resetCounters();
        dataGenerator.start();
    }

    @Override
    public void dispose() {
        dataGenerator.stop();

        super.dispose();
    }

    private void addTotalObservation(double y) {
        total.add(new Millisecond(), y);
    }

    public class BandwidthUsageDataGenerator extends Timer implements ActionListener {

        /**
         * Constructor.
         *
         * @param interval the interval
         */
        public BandwidthUsageDataGenerator(int interval, @NotNull TimeUnit timeUnit) {
            super((int) TimeUnit.MILLISECONDS.convert(interval, timeUnit), null);
            setName("BandwidthUsageGenerator");
            addActionListener(this);
        }

        /**
         * Calculate to current bandwidth usage.
         *
         * @return Used bandwidth in Megabits per second.
         */
        private double calculateBandwidthUsage() {
            var byteCounter = MVHttpClient.getInstance().getByteCounter();
            double bandwidth = byteCounter.bytesRead();
            byteCounter.resetCounters();

            //convert to MBits per second
            bandwidth = bandwidth * 8d / FileUtils.ONE_MB;
            if (bandwidth < 0d)
                bandwidth = 0d;

            return bandwidth;
        }

        /**
         * Adds a new total memory reading (converted to MByte) to the dataset.
         *
         * @param event the action event.
         */
        public void actionPerformed(ActionEvent event) {
            var usage = calculateBandwidthUsage();
            addTotalObservation(usage);

            lblBandwidth.setText(Long.toString(Math.round(usage)));
        }
    }

    private void initComponents() {
        // JFormDesigner - Component initialization - DO NOT MODIFY  //GEN-BEGIN:initComponents  @formatter:off
        // Generated using JFormDesigner non-commercial license
        lblBandwidth = new JLabel();
        var label1 = new JLabel();
        var chartContainer = new JPanel();
        chartPanel1 = new ChartPanel(null);

        //======== this ========
        setTitle("Bandbreite"); //NON-NLS
        setMinimumSize(new Dimension(400, 200));
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setType(Window.Type.UTILITY);
        setPreferredSize(new Dimension(400, 200));
        var contentPane = getContentPane();
        contentPane.setLayout(new MigLayout(
            new LC().fill().insets("5").hideMode(3), //NON-NLS
            // columns
            new AC()
                .grow().fill().gap()
                .align("right"), //NON-NLS
            // rows
            new AC()
                .align("center").gap() //NON-NLS
                .grow().fill()));

        //---- lblBandwidth ----
        lblBandwidth.setText("0"); //NON-NLS
        lblBandwidth.setHorizontalAlignment(SwingConstants.TRAILING);
        lblBandwidth.setVerticalAlignment(SwingConstants.BOTTOM);
        lblBandwidth.setFont(lblBandwidth.getFont().deriveFont(lblBandwidth.getFont().getStyle() | Font.BOLD, lblBandwidth.getFont().getSize() + 19f));
        contentPane.add(lblBandwidth, new CC().cell(0, 0).alignY("bottom").growY(0)); //NON-NLS

        //---- label1 ----
        label1.setText("MBit/s"); //NON-NLS
        label1.setFont(label1.getFont().deriveFont(label1.getFont().getSize() + 2f));
        contentPane.add(label1, new CC().cell(1, 0).alignY("bottom").growY(0)); //NON-NLS

        //======== chartContainer ========
        {
            chartContainer.setMinimumSize(new Dimension(240, 120));
            chartContainer.setLayout(new BorderLayout());
            chartContainer.add(chartPanel1, BorderLayout.CENTER);
        }
        contentPane.add(chartContainer, new CC().cell(0, 1, 2, 1));
        pack();
        setLocationRelativeTo(getOwner());
        // JFormDesigner - End of component initialization  //GEN-END:initComponents  @formatter:on
    }

    // JFormDesigner - Variables declaration - DO NOT MODIFY  //GEN-BEGIN:variables  @formatter:off
    // Generated using JFormDesigner non-commercial license
    private JLabel lblBandwidth;
    private ChartPanel chartPanel1;
    // JFormDesigner - End of variables declaration  //GEN-END:variables  @formatter:on
}
