/*
 * Created by JFormDesigner on Tue Jan 03 13:11:50 CET 2023
 */

package mediathek.gui.bandwidth;

import mediathek.gui.actions.ShowBandwidthUsageAction;
import mediathek.gui.messages.DarkModeChangeEvent;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.FileUtils;
import mediathek.tool.MessageBus;
import mediathek.tool.http.MVHttpClient;
import net.engio.mbassy.listener.Handler;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.apache.commons.configuration2.Configuration;
import org.apache.commons.configuration2.sync.LockMode;
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
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

/**
 * @author christianfranzke
 */
public class BandwidthDialog extends JDialog {
    protected static final String CONFIG_X = "bandwidth_monitor.x";
    protected static final String CONFIG_Y = "bandwidth_monitor.y";
    protected static final String CONFIG_HEIGHT = "bandwidth_monitor.height";
    protected static final String CONFIG_WIDTH = "bandwidth_monitor.width";
    private static final int DEFAULT_WIDTH = 300;
    private static final int DEFAULT_HEIGHT = 150;
    private final TimeSeries total = new TimeSeries("Bandwidth");
    private final DateAxis dateAxis = new DateAxis();
    private final NumberAxis bandwidthAxis = new NumberAxis();
    private final BandwidthUsageDataGenerator dataGenerator = new BandwidthUsageDataGenerator(1, TimeUnit.SECONDS);
    private final Configuration config = ApplicationConfiguration.getConfiguration();
    private JLabel lblBandwidth;
    private ChartPanel chartPanel1;

    public BandwidthDialog(@NotNull Window owner, @NotNull ShowBandwidthUsageAction menuAction) {
        super(owner);

        initComponents();
        setupChart();

        restoreSizeFromConfig();
        addComponentListener(new WriteConfigComponentListener(config, this));

        addWindowListener(new WindowAdapter() {
            @Override
            public void windowOpened(WindowEvent e) {
                storeVisibilityState(true);
                menuAction.setDialogOptional(Optional.of(BandwidthDialog.this));
                menuAction.setEnabled(false);
            }

            @Override
            public void windowClosed(WindowEvent e) {
                storeVisibilityState(false);
                menuAction.setDialogOptional(Optional.empty());
                menuAction.setEnabled(true);
            }
        });

        MessageBus.getMessageBus().subscribe(this);
    }

    @Handler
    private void handleDarkModeChange(DarkModeChangeEvent e) {
        SwingUtilities.invokeLater(this::setLabelColors);
    }

    private void calculateHudPosition() {
        final GraphicsDevice gd = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice();
        final DisplayMode dm = gd.getDisplayMode();
        setLocation(dm.getWidth() - DEFAULT_WIDTH, 0);
    }

    private void restoreSizeFromConfig() {
        try {
            config.lock(LockMode.READ);
            int x = config.getInt(CONFIG_X);
            int y = config.getInt(CONFIG_Y);
            int width = config.getInt(CONFIG_WIDTH);
            int height = config.getInt(CONFIG_HEIGHT);

            setSize(width, height);
            setLocation(x, y);
        } catch (Exception ex) {
            setSize(DEFAULT_WIDTH, DEFAULT_HEIGHT);
            calculateHudPosition();
        } finally {
            config.unlock(LockMode.READ);
        }
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
        total.setMaximumItemAge(TimeUnit.MILLISECONDS.convert(30, TimeUnit.SECONDS));

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

    public void storeVisibilityState(boolean newVar) {
        config.setProperty(ApplicationConfiguration.APPLICATION_UI_BANDWIDTH_MONITOR_VISIBLE, newVar);
    }

    private void initComponents() {
        lblBandwidth = new JLabel();
        var label1 = new JLabel();
        var chartContainer = new JPanel();
        chartPanel1 = new ChartPanel(null);

        setTitle("Bandbreite");
//        setMinimumSize(new Dimension(400, 200));
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setType(Window.Type.UTILITY);
        setPreferredSize(new Dimension(400, 200));
        var contentPane = getContentPane();
        contentPane.setLayout(new MigLayout(
                new LC().fill().insets("5").hideMode(3),
                // columns
                new AC()
                        .grow().fill().gap()
                        .align("right"),
                // rows
                new AC()
                        .align("center").gap()
                        .grow().fill()));

        lblBandwidth.setText("0");
        lblBandwidth.setHorizontalAlignment(SwingConstants.TRAILING);
        lblBandwidth.setVerticalAlignment(SwingConstants.BOTTOM);
        lblBandwidth.setFont(lblBandwidth.getFont().deriveFont(lblBandwidth.getFont().getStyle() | Font.BOLD, lblBandwidth.getFont().getSize() + 19f));
        contentPane.add(lblBandwidth, new CC().cell(0, 0).alignY("bottom").growY(0));

        label1.setText("MBit/s"); //NON-NLS
        label1.setFont(label1.getFont().deriveFont(label1.getFont().getSize() + 2f));
        contentPane.add(label1, new CC().cell(1, 0).alignY("bottom").growY(0));
        chartContainer.setMinimumSize(new Dimension(240, 120));
        chartContainer.setLayout(new BorderLayout());
        chartContainer.add(chartPanel1, BorderLayout.CENTER);
        contentPane.add(chartContainer, new CC().cell(0, 1, 2, 1));
        pack();
        setLocationRelativeTo(getOwner());
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
            total.add(new Millisecond(), usage);

            lblBandwidth.setText(Long.toString(Math.round(usage)));
        }
    }
}
