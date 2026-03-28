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

package mediathek.mainwindow;

import com.formdev.flatlaf.FlatLaf;
import mediathek.gui.messages.DarkModeChangeEvent;
import mediathek.tool.MessageBus;
import net.engio.mbassy.listener.Handler;
import org.jetbrains.annotations.NotNull;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.DateAxis;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.plot.ValueMarker;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYAreaRenderer;
import org.jfree.chart.ui.Layer;
import org.jfree.chart.ui.RectangleInsets;
import org.jfree.data.time.Millisecond;
import org.jfree.data.time.TimeSeries;
import org.jfree.data.time.TimeSeriesCollection;
import org.jfree.data.time.TimeSeriesDataItem;

import javax.swing.*;
import java.awt.*;
import java.time.Duration;
import java.util.Objects;

public final class MemoryUsagePanel extends JPanel implements AutoCloseable {
    private static final RectangleInsets CHART_INSETS = new RectangleInsets(5.0, 5.0, 5.0, 5.0);
    private static final BasicStroke AVERAGE_STROKE = new BasicStroke(1.5f);
    private static final Color SERIES_FILL = new Color(255, 0, 0, 100);
    private static final Color SERIES_OUTLINE = Color.RED;
    private static final Color AVERAGE_LINE_LIGHT_THEME = new Color(0, 170, 0);
    private static final Color AVERAGE_LINE_DARK_THEME = new Color(144, 238, 144);
    private static final Color FALLBACK_LABEL_COLOR = Color.LIGHT_GRAY;
    private static final Color FALLBACK_BACKGROUND_COLOR = Color.DARK_GRAY;
    private static final Runtime RUNTIME = Runtime.getRuntime();

    private final MonitorConfiguration configuration;
    private final TimeSeries usedMemorySeries = new TimeSeries("Used Memory");
    private final DateAxis timeAxis = createTimeAxis();
    private final NumberAxis memoryAxis = createMemoryAxis();
    private final ValueMarker averageMarker = createAverageMarker();
    private final XYPlot plot;
    private final JFreeChart chart;
    private final Timer samplingTimer;
    private boolean subscribedToMessageBus;

    public MemoryUsagePanel(@NotNull Duration historyWindow, @NotNull Duration sampleInterval) {
        super(new BorderLayout());
        configuration = new MonitorConfiguration(historyWindow, sampleInterval);

        configureDataset();
        plot = createPlot();
        chart = createChart();
        samplingTimer = createSamplingTimer();

        add(createChartPanel(), BorderLayout.CENTER);
        applyTheme();
    }

    @Override
    public void addNotify() {
        super.addNotify();
        subscribeToMessageBus();
        samplingTimer.start();
    }

    @Override
    public void removeNotify() {
        samplingTimer.stop();
        unsubscribeFromMessageBus();
        super.removeNotify();
    }

    @Override
    public void close() {
        samplingTimer.stop();
        unsubscribeFromMessageBus();
    }

    @Handler
    private void handleDarkModeChange(DarkModeChangeEvent event) {
        SwingUtilities.invokeLater(this::applyTheme);
    }

    private void configureDataset() {
        usedMemorySeries.setMaximumItemAge(configuration.historyWindow().toMillis());
        usedMemorySeries.setMaximumItemCount(configuration.maxItemCount());
    }

    private DateAxis createTimeAxis() {
        var axis = new DateAxis("Zeit");
        axis.setAutoRange(true);
        axis.setLowerMargin(0.0);
        axis.setUpperMargin(0.0);
        axis.setTickLabelsVisible(true);
        return axis;
    }

    private NumberAxis createMemoryAxis() {
        var axis = new NumberAxis("MiB");
        axis.setAutoRange(true);
        axis.setAutoRangeIncludesZero(true);
        axis.setStandardTickUnits(NumberAxis.createIntegerTickUnits());
        return axis;
    }

    private XYPlot createPlot() {
        var dataset = new TimeSeriesCollection(usedMemorySeries);
        var renderer = new XYAreaRenderer();
        renderer.setOutline(true);
        renderer.setSeriesPaint(0, SERIES_FILL);
        renderer.setSeriesOutlinePaint(0, SERIES_OUTLINE);

        var xyPlot = new XYPlot(dataset, timeAxis, memoryAxis, renderer);
        xyPlot.setAxisOffset(CHART_INSETS);
        xyPlot.addRangeMarker(averageMarker, Layer.FOREGROUND);
        return xyPlot;
    }

    private JFreeChart createChart() {
        var freeChart = new JFreeChart(plot);
        freeChart.setAntiAlias(true);
        freeChart.removeLegend();
        return freeChart;
    }

    private ChartPanel createChartPanel() {
        var chartPanel = new ChartPanel(chart);
        chartPanel.setPopupMenu(null);
        chartPanel.setMouseZoomable(false);
        chartPanel.setDomainZoomable(false);
        chartPanel.setRangeZoomable(false);
        chartPanel.setMouseWheelEnabled(false);
        chartPanel.addMouseListener(new java.awt.event.MouseAdapter() {
            @Override
            public void mouseClicked(java.awt.event.MouseEvent event) {
                RUNTIME.gc();
            }
        });
        return chartPanel;
    }

    private Timer createSamplingTimer() {
        var delayMillis = Math.toIntExact(configuration.sampleInterval().toMillis());
        return new Timer(delayMillis, _ -> sampleUsedMemory());
    }

    private void sampleUsedMemory() {
        var usedMemoryInMiB = usedMemoryInMiB();
        usedMemorySeries.addOrUpdate(new Millisecond(), usedMemoryInMiB);
        averageMarker.setValue(calculateAverageUsedMemoryInMiB());
    }

    private double usedMemoryInMiB() {
        return (RUNTIME.totalMemory() - RUNTIME.freeMemory()) / (1024d * 1024d);
    }

    private ValueMarker createAverageMarker() {
        var marker = new ValueMarker(0d, AVERAGE_LINE_LIGHT_THEME, AVERAGE_STROKE);
        marker.setAlpha(0.9f);
        return marker;
    }

    private double calculateAverageUsedMemoryInMiB() {
        var itemCount = usedMemorySeries.getItemCount();
        if (itemCount == 0) {
            return 0d;
        }

        double totalUsedMemory = 0d;
        for (var item : usedMemorySeries.getItems()) {
            totalUsedMemory += ((TimeSeriesDataItem) item).getValue().doubleValue();
        }
        return totalUsedMemory / itemCount;
    }

    private void applyTheme() {
        var labelColor = Objects.requireNonNullElse(UIManager.getColor("Label.foreground"), FALLBACK_LABEL_COLOR);
        var panelColor = Objects.requireNonNullElse(UIManager.getColor("Panel.background"), FALLBACK_BACKGROUND_COLOR);

        timeAxis.setLabelPaint(labelColor);
        timeAxis.setTickLabelPaint(labelColor);
        timeAxis.setTickMarkPaint(labelColor);
        timeAxis.setAxisLinePaint(labelColor);

        memoryAxis.setLabelPaint(labelColor);
        memoryAxis.setTickLabelPaint(labelColor);
        memoryAxis.setTickMarkPaint(labelColor);
        memoryAxis.setAxisLinePaint(labelColor);

        averageMarker.setPaint(FlatLaf.isLafDark() ? AVERAGE_LINE_DARK_THEME : AVERAGE_LINE_LIGHT_THEME);
        chart.setBackgroundPaint(panelColor);
        plot.setBackgroundPaint(panelColor);
        plot.setOutlinePaint(labelColor);
        plot.setDomainGridlinePaint(labelColor);
        plot.setRangeGridlinePaint(labelColor);
    }

    private void subscribeToMessageBus() {
        if (!subscribedToMessageBus) {
            MessageBus.getMessageBus().subscribe(this);
            subscribedToMessageBus = true;
        }
    }

    private void unsubscribeFromMessageBus() {
        if (subscribedToMessageBus) {
            MessageBus.getMessageBus().unsubscribe(this);
            subscribedToMessageBus = false;
        }
    }

    public record MonitorConfiguration(Duration historyWindow, Duration sampleInterval) {
        public MonitorConfiguration {
            Objects.requireNonNull(historyWindow, "historyWindow");
            Objects.requireNonNull(sampleInterval, "sampleInterval");
            if (historyWindow.isZero() || historyWindow.isNegative()) {
                throw new IllegalArgumentException("historyWindow must be positive");
            }
            if (sampleInterval.isZero() || sampleInterval.isNegative()) {
                throw new IllegalArgumentException("sampleInterval must be positive");
            }
        }

        public int maxItemCount() {
            return Math.toIntExact(Math.max(2L, Math.ceilDiv(historyWindow.toMillis(), sampleInterval.toMillis()) + 1L));
        }
    }
}
