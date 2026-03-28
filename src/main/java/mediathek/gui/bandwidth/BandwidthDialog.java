/*
 * Copyright (c) 2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.gui.bandwidth;

import com.formdev.flatlaf.FlatLaf;
import mediathek.gui.actions.ShowBandwidthUsageAction;
import mediathek.gui.messages.DarkModeChangeEvent;
import mediathek.tool.ApplicationConfiguration;
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
import org.jfree.chart.plot.ValueMarker;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYSplineRenderer;
import org.jfree.chart.ui.Layer;
import org.jfree.chart.ui.RectangleInsets;
import org.jfree.data.time.Millisecond;
import org.jfree.data.time.TimeSeries;
import org.jfree.data.time.TimeSeriesCollection;
import org.jfree.data.time.TimeSeriesDataItem;

import javax.swing.*;
import java.awt.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.text.*;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

public class BandwidthDialog extends JDialog {
    protected static final String CONFIG_X = "bandwidth_monitor.x";
    protected static final String CONFIG_Y = "bandwidth_monitor.y";
    protected static final String CONFIG_HEIGHT = "bandwidth_monitor.height";
    protected static final String CONFIG_WIDTH = "bandwidth_monitor.width";

    private static final int DEFAULT_WIDTH = 300;
    private static final int DEFAULT_HEIGHT = 150;
    private static final int CHART_HISTORY_SECONDS = 30;
    private static final int REFRESH_INTERVAL_SECONDS = 1;
    private static final RectangleInsets CHART_INSETS = new RectangleInsets(5.0, 5.0, 5.0, 5.0);
    private static final BasicStroke CHART_STROKE = new BasicStroke(2f);
    private static final BasicStroke AVERAGE_STROKE = new BasicStroke(1.5f);
    private static final Color AVERAGE_LINE_LIGHT_THEME = new Color(0, 170, 0);
    private static final Color AVERAGE_LINE_DARK_THEME = new Color(144, 238, 144);
    private static final DecimalFormatSymbols FORMAT_SYMBOLS = createFormatSymbols();
    private static final DecimalFormat INTEGER_FORMAT = createIntegerFormat();

    private final Configuration config = ApplicationConfiguration.getConfiguration();
    private final TimeSeries bandwidthSeries = new TimeSeries("Bandwidth");
    private final DateAxis timeAxis = new DateAxis();
    private final NumberAxis bandwidthAxis = new NumberAxis();
    private final JLabel bandwidthValueLabel = new JLabel("0");
    private final JLabel bandwidthUnitLabel = new JLabel("Bit/s");
    private final ChartPanel chartPanel = new ChartPanel(null);
    private final Timer dataGenerator = createDataGenerator();
    private final ValueMarker averageMarker = createAverageMarker();

    public BandwidthDialog(@NotNull Window owner, @NotNull ShowBandwidthUsageAction menuAction) {
        super(owner);

        initDialog();
        buildUi();
        configureChart();
        restoreSizeFromConfig();
        addComponentListener(new WriteConfigComponentListener(config, this));
        addWindowListener(createWindowListener(menuAction));

        applyTheme();
        MessageBus.getMessageBus().subscribe(this);
    }

    @Handler
    private void handleDarkModeChange(DarkModeChangeEvent event) {
        SwingUtilities.invokeLater(this::applyTheme);
    }

    @Override
    public void dispose() {
        dataGenerator.stop();
        super.dispose();
    }

    public void storeVisibilityState(boolean visible) {
        config.setProperty(ApplicationConfiguration.APPLICATION_UI_BANDWIDTH_MONITOR_VISIBLE, visible);
    }

    private void initDialog() {
        setTitle("Bandbreite");
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setType(Window.Type.UTILITY);
        setPreferredSize(new Dimension(400, 200));
    }

    private void buildUi() {
        var contentPane = getContentPane();
        contentPane.setLayout(new MigLayout(
                new LC().fill().insets("5").hideMode(3),
                new AC().grow().fill().gap().align("right"),
                new AC().align("center").gap().grow().fill()));

        configureValueLabels();
        contentPane.add(bandwidthValueLabel, new CC().cell(0, 0).alignY("bottom").growY(0));
        contentPane.add(bandwidthUnitLabel, new CC().cell(1, 0).alignY("bottom").growY(0));
        contentPane.add(createChartContainer(), new CC().cell(0, 1, 2, 1));

        pack();
        setLocationRelativeTo(getOwner());
    }

    private void configureValueLabels() {
        bandwidthValueLabel.setHorizontalAlignment(SwingConstants.TRAILING);
        bandwidthValueLabel.setVerticalAlignment(SwingConstants.BOTTOM);
        bandwidthValueLabel.setFont(bandwidthValueLabel.getFont().deriveFont(
                bandwidthValueLabel.getFont().getStyle() | Font.BOLD,
                bandwidthValueLabel.getFont().getSize() + 19f));

        bandwidthUnitLabel.setVerticalAlignment(SwingConstants.BOTTOM);
        bandwidthUnitLabel.setFont(bandwidthUnitLabel.getFont().deriveFont(
                bandwidthUnitLabel.getFont().getSize() + 2f));
    }

    private JPanel createChartContainer() {
        var chartContainer = new JPanel(new BorderLayout());
        chartContainer.setMinimumSize(new Dimension(240, 120));
        chartContainer.add(chartPanel, BorderLayout.CENTER);
        return chartContainer;
    }

    private void configureChart() {
        bandwidthSeries.setMaximumItemAge(TimeUnit.MILLISECONDS.convert(CHART_HISTORY_SECONDS, TimeUnit.SECONDS));

        timeAxis.setAutoRange(true);
        timeAxis.setLowerMargin(0.0);
        timeAxis.setUpperMargin(0.0);
        timeAxis.setTickLabelsVisible(false);
        timeAxis.setTickMarksVisible(false);
        timeAxis.setAxisLineVisible(false);
        timeAxis.setDateFormatOverride(new SimpleDateFormat(""));

        bandwidthAxis.setAutoRange(true);
        bandwidthAxis.setStandardTickUnits(NumberAxis.createIntegerTickUnits());
        bandwidthAxis.setNumberFormatOverride(new BandwidthAxisFormat());

        var plot = new XYPlot(createDataset(), timeAxis, bandwidthAxis, createRenderer());
        plot.setAxisOffset(CHART_INSETS);
        plot.setDomainGridlinesVisible(false);
        plot.addRangeMarker(averageMarker, Layer.FOREGROUND);

        var chart = new JFreeChart(plot);
        chart.setAntiAlias(true);
        chart.removeLegend();

        configureChartPanel(chart);

        // Reset counters before starting to avoid an artificial spike in the first sample.
        MVHttpClient.getInstance().getByteCounter().resetCounters();
        dataGenerator.start();
    }

    private TimeSeriesCollection createDataset() {
        var dataset = new TimeSeriesCollection();
        dataset.addSeries(bandwidthSeries);
        return dataset;
    }

    private XYSplineRenderer createRenderer() {
        var renderer = new XYSplineRenderer();
        renderer.setDefaultShapesVisible(false);
        renderer.setSeriesPaint(0, Color.RED);
        renderer.setSeriesStroke(0, CHART_STROKE);
        return renderer;
    }

    private void configureChartPanel(JFreeChart chart) {
        chartPanel.setChart(chart);
        chartPanel.setPopupMenu(null);
        chartPanel.setMouseZoomable(false);
        chartPanel.setDomainZoomable(false);
        chartPanel.setRangeZoomable(false);
        chartPanel.setMouseWheelEnabled(false);
    }

    private Timer createDataGenerator() {
        return new Timer((int) TimeUnit.MILLISECONDS.convert(REFRESH_INTERVAL_SECONDS, TimeUnit.SECONDS), _ -> sampleBandwidthUsage());
    }

    private void sampleBandwidthUsage() {
        var sampleTime = new Millisecond();
        var bitsPerSecond = readBitsPerSecond();
        bandwidthSeries.addOrUpdate(sampleTime, bitsPerSecond);
        updateAverageMarker();
        updateBandwidthDisplay(BandwidthDisplayValue.fromBitsPerSecond(bitsPerSecond));
    }

    private double readBitsPerSecond() {
        var byteCounter = MVHttpClient.getInstance().getByteCounter();
        var bitsPerSecond = Math.max(0d, byteCounter.bytesRead() * 8d);
        byteCounter.resetCounters();
        return bitsPerSecond;
    }

    private void updateBandwidthDisplay(BandwidthDisplayValue displayValue) {
        bandwidthValueLabel.setText(displayValue.value());
        bandwidthUnitLabel.setText(displayValue.unit());
    }

    private ValueMarker createAverageMarker() {
        var marker = new ValueMarker(0d, AVERAGE_LINE_LIGHT_THEME, AVERAGE_STROKE);
        marker.setAlpha(0.9f);
        return marker;
    }

    private void updateAverageMarker() {
        averageMarker.setValue(calculateAverageBitsPerSecond());
    }

    private double calculateAverageBitsPerSecond() {
        var itemCount = bandwidthSeries.getItemCount();
        if (itemCount == 0) {
            return 0d;
        }

        double totalBitsPerSecond = 0d;
        for (var item : bandwidthSeries.getItems()) {
            totalBitsPerSecond += ((TimeSeriesDataItem) item).getValue().doubleValue();
        }
        return totalBitsPerSecond / itemCount;
    }

    private WindowAdapter createWindowListener(ShowBandwidthUsageAction menuAction) {
        return new WindowAdapter() {
            @Override
            public void windowOpened(WindowEvent event) {
                storeVisibilityState(true);
                menuAction.setDialogOptional(Optional.of(BandwidthDialog.this));
                menuAction.setEnabled(false);
            }

            @Override
            public void windowClosed(WindowEvent event) {
                storeVisibilityState(false);
                menuAction.setDialogOptional(Optional.empty());
                menuAction.setEnabled(true);
            }
        };
    }

    private void restoreSizeFromConfig() {
        try {
            config.lock(LockMode.READ);
            setSize(config.getInt(CONFIG_WIDTH), config.getInt(CONFIG_HEIGHT));
            setLocation(config.getInt(CONFIG_X), config.getInt(CONFIG_Y));
        } catch (Exception exception) {
            setSize(DEFAULT_WIDTH, DEFAULT_HEIGHT);
            moveToDefaultHudPosition();
        } finally {
            config.unlock(LockMode.READ);
        }
    }

    private void moveToDefaultHudPosition() {
        var graphicsDevice = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice();
        var displayMode = graphicsDevice.getDisplayMode();
        setLocation(displayMode.getWidth() - DEFAULT_WIDTH, 0);
    }

    private void applyTheme() {
        var labelColor = Optional.ofNullable(UIManager.getColor("Label.foreground")).orElse(Color.LIGHT_GRAY);
        var panelColor = Optional.ofNullable(UIManager.getColor("Panel.background")).orElse(Color.DARK_GRAY);
        var chart = chartPanel.getChart();
        var plot = (XYPlot) chart.getPlot();

        timeAxis.setLabelPaint(labelColor);
        timeAxis.setTickLabelPaint(labelColor);
        timeAxis.setTickMarkPaint(labelColor);

        bandwidthAxis.setLabelPaint(labelColor);
        bandwidthAxis.setTickLabelPaint(labelColor);
        bandwidthAxis.setTickMarkPaint(labelColor);
        bandwidthAxis.setAxisLinePaint(labelColor);
        averageMarker.setPaint(FlatLaf.isLafDark() ? AVERAGE_LINE_DARK_THEME : AVERAGE_LINE_LIGHT_THEME);

        chart.setBackgroundPaint(panelColor);
        plot.setBackgroundPaint(panelColor);
        plot.setOutlinePaint(labelColor);
        plot.setDomainGridlinePaint(labelColor);
        plot.setRangeGridlinePaint(labelColor);
    }

    private static DecimalFormatSymbols createFormatSymbols() {
        var symbols = DecimalFormatSymbols.getInstance();
        symbols.setDecimalSeparator(',');
        symbols.setGroupingSeparator('.');
        return symbols;
    }

    private static DecimalFormat createIntegerFormat() {
        var format = new DecimalFormat("#,##0");
        format.setDecimalFormatSymbols(FORMAT_SYMBOLS);
        format.setGroupingUsed(true);
        return format;
    }

    private record BandwidthDisplayValue(String value, String unit) {
        private static final double KILOBIT = 1_000d;
        private static final double MEGABIT = 1_000_000d;
        private static final double GIGABIT = 1_000_000_000d;

        static BandwidthDisplayValue fromBitsPerSecond(double bitsPerSecond) {
            var sanitizedValue = Math.max(0d, bitsPerSecond);
            if (sanitizedValue >= GIGABIT) {
                return scaled(sanitizedValue, GIGABIT, "GBit/s");
            }
            if (sanitizedValue >= MEGABIT) {
                return scaled(sanitizedValue, MEGABIT, "MBit/s");
            }
            if (sanitizedValue >= KILOBIT) {
                return scaled(sanitizedValue, KILOBIT, "KBit/s");
            }
            return new BandwidthDisplayValue(INTEGER_FORMAT.format(Math.round(sanitizedValue)), "Bit/s");
        }

        private static BandwidthDisplayValue scaled(double value, double divisor, String unit) {
            return new BandwidthDisplayValue(INTEGER_FORMAT.format(Math.round(value / divisor)), unit);
        }
    }

    private static final class BandwidthAxisFormat extends NumberFormat {
        @Override
        public StringBuffer format(double number, StringBuffer toAppendTo, FieldPosition pos) {
            var displayValue = BandwidthDisplayValue.fromBitsPerSecond(number);
            return toAppendTo.append(displayValue.value()).append(' ').append(displayValue.unit());
        }

        @Override
        public StringBuffer format(long number, StringBuffer toAppendTo, FieldPosition pos) {
            return format((double) number, toAppendTo, pos);
        }

        @Override
        public Number parse(String source, ParsePosition parsePosition) {
            parsePosition.setIndex(source.length());
            return 0d;
        }
    }
}
