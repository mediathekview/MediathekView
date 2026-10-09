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

package mediathek.mainwindow

import com.formdev.flatlaf.FlatLaf
import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.gui.messages.DarkModeChangeEvent
import mediathek.tool.MessageBus
import net.engio.mbassy.listener.Handler
import org.jfree.chart.ChartPanel
import org.jfree.chart.JFreeChart
import org.jfree.chart.axis.DateAxis
import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.plot.ValueMarker
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.xy.XYAreaRenderer
import org.jfree.chart.ui.Layer
import org.jfree.chart.ui.RectangleInsets
import org.jfree.data.time.Millisecond
import org.jfree.data.time.TimeSeries
import org.jfree.data.time.TimeSeriesCollection
import org.jfree.data.time.TimeSeriesDataItem
import java.awt.BasicStroke
import java.awt.BorderLayout
import java.awt.Color
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import java.time.Duration
import javax.swing.JPanel
import javax.swing.SwingUtilities
import javax.swing.UIManager
import kotlin.math.max
import kotlin.time.toKotlinDuration

class MemoryUsagePanel(
    historyWindow: Duration,
    sampleInterval: Duration,
) : JPanel(BorderLayout()), AutoCloseable {
    private val configuration = MonitorConfiguration(historyWindow, sampleInterval)
    private val usedMemorySeries = TimeSeries("Used Memory")
    private val timeAxis = createTimeAxis()
    private val memoryAxis = createMemoryAxis()
    private val averageMarker = createAverageMarker()
    private val plot: XYPlot
    private val chart: JFreeChart
    private val coroutineScope = CoroutineScope(SupervisorJob() + Dispatchers.Swing)
    private var samplingJob: Job? = null
    private var subscribedToMessageBus = false

    init {
        configureDataset()
        plot = createPlot()
        chart = createChart()

        add(createChartPanel(), BorderLayout.CENTER)
        applyTheme()
    }

    override fun addNotify() {
        super.addNotify()
        subscribeToMessageBus()
        startSampling()
    }

    override fun removeNotify() {
        stopSampling()
        unsubscribeFromMessageBus()
        super.removeNotify()
    }

    override fun close() {
        stopSampling()
        coroutineScope.coroutineContext[Job]?.cancel()
        unsubscribeFromMessageBus()
    }

    @Suppress("UNUSED_PARAMETER")
    @Handler
    private fun handleDarkModeChange(event: DarkModeChangeEvent) {
        SwingUtilities.invokeLater(::applyTheme)
    }

    private fun configureDataset() {
        usedMemorySeries.maximumItemAge = configuration.historyWindow.toMillis()
        usedMemorySeries.maximumItemCount = configuration.maxItemCount
    }

    private fun createTimeAxis() = DateAxis("Zeit").apply {
        isAutoRange = true
        lowerMargin = 0.0
        upperMargin = 0.0
        isTickLabelsVisible = true
    }

    private fun createMemoryAxis() = NumberAxis("MiB").apply {
        isAutoRange = true
        autoRangeIncludesZero = true
        standardTickUnits = NumberAxis.createIntegerTickUnits()
    }

    private fun createPlot(): XYPlot {
        val dataset = TimeSeriesCollection(usedMemorySeries)
        val renderer = XYAreaRenderer().apply {
            isOutline = true
            setSeriesPaint(0, SERIES_FILL)
            setSeriesOutlinePaint(0, SERIES_OUTLINE)
        }

        return XYPlot(dataset, timeAxis, memoryAxis, renderer).apply {
            axisOffset = CHART_INSETS
            addRangeMarker(averageMarker, Layer.FOREGROUND)
        }
    }

    private fun createChart() = JFreeChart(plot).apply {
        antiAlias = true
        removeLegend()
    }

    private fun createChartPanel() = ChartPanel(chart).apply {
        popupMenu = null
        setMouseZoomable(false)
        isDomainZoomable = false
        isRangeZoomable = false
        isMouseWheelEnabled = false
        addMouseListener(object : MouseAdapter() {
            override fun mouseClicked(event: MouseEvent) {
                RUNTIME.gc()
            }
        })
    }

    private fun startSampling() {
        if (samplingJob?.isActive == true) {
            return
        }

        samplingJob = coroutineScope.launch {
            while (isActive) {
                delay(configuration.sampleInterval.toKotlinDuration())
                sampleUsedMemory()
            }
        }
    }

    private fun stopSampling() {
        samplingJob?.cancel()
        samplingJob = null
    }

    private fun sampleUsedMemory() {
        usedMemorySeries.addOrUpdate(Millisecond(), usedMemoryInMiB())
        averageMarker.value = calculateAverageUsedMemoryInMiB()
    }

    private fun usedMemoryInMiB(): Double =
        (RUNTIME.totalMemory() - RUNTIME.freeMemory()) / (1024.0 * 1024.0)

    private fun createAverageMarker() = ValueMarker(0.0, AVERAGE_LINE_LIGHT_THEME, AVERAGE_STROKE).apply {
        alpha = 0.9f
    }

    private fun calculateAverageUsedMemoryInMiB(): Double {
        val itemCount = usedMemorySeries.itemCount
        if (itemCount == 0) {
            return 0.0
        }

        val totalUsedMemory = usedMemorySeries.items.sumOf { item ->
            (item as TimeSeriesDataItem).value.toDouble()
        }
        return totalUsedMemory / itemCount
    }

    private fun applyTheme() {
        val labelColor = UIManager.getColor("Label.foreground") ?: FALLBACK_LABEL_COLOR
        val panelColor = UIManager.getColor("Panel.background") ?: FALLBACK_BACKGROUND_COLOR

        timeAxis.labelPaint = labelColor
        timeAxis.tickLabelPaint = labelColor
        timeAxis.tickMarkPaint = labelColor
        timeAxis.axisLinePaint = labelColor

        memoryAxis.labelPaint = labelColor
        memoryAxis.tickLabelPaint = labelColor
        memoryAxis.tickMarkPaint = labelColor
        memoryAxis.axisLinePaint = labelColor

        averageMarker.paint = if (FlatLaf.isLafDark()) AVERAGE_LINE_DARK_THEME else AVERAGE_LINE_LIGHT_THEME
        chart.backgroundPaint = panelColor
        plot.backgroundPaint = panelColor
        plot.outlinePaint = labelColor
        plot.domainGridlinePaint = labelColor
        plot.rangeGridlinePaint = labelColor
    }

    private fun subscribeToMessageBus() {
        if (!subscribedToMessageBus) {
            MessageBus.messageBus.subscribe(this)
            subscribedToMessageBus = true
        }
    }

    private fun unsubscribeFromMessageBus() {
        if (subscribedToMessageBus) {
            MessageBus.messageBus.unsubscribe(this)
            subscribedToMessageBus = false
        }
    }

    private data class MonitorConfiguration(
        val historyWindow: Duration,
        val sampleInterval: Duration,
    ) {
        init {
            require(!(historyWindow.isZero || historyWindow.isNegative)) { "historyWindow must be positive" }
            require(!(sampleInterval.isZero || sampleInterval.isNegative)) { "sampleInterval must be positive" }
        }

        val maxItemCount: Int =
            max(2L, Math.ceilDiv(historyWindow.toMillis(), sampleInterval.toMillis()) + 1L).toInt()
    }

    private companion object {
        private val CHART_INSETS = RectangleInsets(5.0, 5.0, 5.0, 5.0)
        private val AVERAGE_STROKE = BasicStroke(1.5f)
        private val SERIES_FILL = Color(255, 0, 0, 100)
        private val SERIES_OUTLINE = Color.RED
        private val AVERAGE_LINE_LIGHT_THEME = Color(0, 170, 0)
        private val AVERAGE_LINE_DARK_THEME = Color(144, 238, 144)
        private val FALLBACK_LABEL_COLOR = Color.LIGHT_GRAY
        private val FALLBACK_BACKGROUND_COLOR = Color.DARK_GRAY
        private val RUNTIME = Runtime.getRuntime()
    }
}
