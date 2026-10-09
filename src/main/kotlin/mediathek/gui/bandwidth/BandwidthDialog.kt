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

package mediathek.gui.bandwidth

import com.formdev.flatlaf.FlatLaf
import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.config.application.ApplicationConfiguration
import mediathek.gui.actions.ShowBandwidthUsageAction
import mediathek.gui.messages.DarkModeChangeEvent
import mediathek.tool.MessageBus
import mediathek.tool.http.MVHttpClient
import net.engio.mbassy.listener.Handler
import net.miginfocom.layout.AC
import net.miginfocom.layout.CC
import net.miginfocom.layout.LC
import net.miginfocom.swing.MigLayout
import org.jfree.chart.ChartPanel
import org.jfree.chart.JFreeChart
import org.jfree.chart.axis.DateAxis
import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.plot.ValueMarker
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.xy.XYSplineRenderer
import org.jfree.chart.ui.Layer
import org.jfree.chart.ui.RectangleInsets
import org.jfree.data.time.Millisecond
import org.jfree.data.time.TimeSeries
import org.jfree.data.time.TimeSeriesCollection
import org.jfree.data.time.TimeSeriesDataItem
import java.awt.*
import java.awt.event.ComponentAdapter
import java.awt.event.ComponentEvent
import java.text.*
import java.util.*
import javax.swing.*
import kotlin.time.Duration.Companion.seconds

class BandwidthDialog(
    owner: Window,
    private val menuAction: ShowBandwidthUsageAction,
) : JDialog(owner) {
    private val applicationConfiguration = ApplicationConfiguration.getInstance()
    private val bandwidthSeries = TimeSeries("Bandwidth")
    private val timeAxis = DateAxis()
    private val bandwidthAxis = NumberAxis()
    private val bandwidthValueLabel = JLabel("0")
    private val bandwidthUnitLabel = JLabel("Bit/s")
    private val chartPanel = ChartPanel(null)
    private val averageMarker = createAverageMarker()
    private val dialogJob = SupervisorJob()
    private val uiScope = CoroutineScope(dialogJob + Dispatchers.Swing)
    private var preserveVisibilityOnClose = false

    init {
        initDialog()
        buildUi()
        configureChart()
        restoreSizeFromConfig()
        addComponentListener(createBoundsListener())
        addWindowListener(createWindowListener())

        applyTheme()
        MessageBus.messageBus.subscribe(this)
        startSamplingLoop()
    }

    @Handler
    @Suppress("UNUSED_PARAMETER")
    private fun handleDarkModeChange(event: DarkModeChangeEvent) {
        uiScope.launch {
            applyTheme()
        }
    }

    override fun dispose() {
        dialogJob.cancel()
        MessageBus.messageBus.unsubscribe(this)
        super.dispose()
    }

    fun disposeForShutdown() {
        preserveVisibilityOnClose = true
        dispose()
    }

    fun storeVisibilityState(visible: Boolean) {
        applicationConfiguration.bandwidthMonitorVisible = visible
    }

    private fun initDialog() {
        title = "Bandbreite"
        defaultCloseOperation = DISPOSE_ON_CLOSE
        type = Type.UTILITY
        preferredSize = Dimension(400, 200)
    }

    private fun buildUi() {
        contentPane.layout = MigLayout(
            LC().fill().insets("5").hideMode(3),
            AC().grow().fill().gap().align("right"),
            AC().align("center").gap().grow().fill(),
        )

        configureValueLabels()
        contentPane.add(bandwidthValueLabel, CC().cell(0, 0).alignY("bottom").growY(0f))
        contentPane.add(bandwidthUnitLabel, CC().cell(1, 0).alignY("bottom").growY(0f))
        contentPane.add(createChartContainer(), CC().cell(0, 1, 2, 1))

        pack()
        setLocationRelativeTo(owner)
    }

    private fun configureValueLabels() {
        bandwidthValueLabel.horizontalAlignment = SwingConstants.TRAILING
        bandwidthValueLabel.verticalAlignment = SwingConstants.BOTTOM
        bandwidthValueLabel.font = bandwidthValueLabel.font.deriveFont(
            bandwidthValueLabel.font.style or Font.BOLD,
            bandwidthValueLabel.font.size + 19f,
        )

        bandwidthUnitLabel.verticalAlignment = SwingConstants.BOTTOM
        bandwidthUnitLabel.font = bandwidthUnitLabel.font.deriveFont(
            bandwidthUnitLabel.font.size + 2f,
        )
    }

    private fun createChartContainer(): JPanel =
        JPanel(BorderLayout()).apply {
            minimumSize = Dimension(240, 120)
            add(chartPanel, BorderLayout.CENTER)
        }

    private fun configureChart() {
        bandwidthSeries.maximumItemAge = CHART_HISTORY_SECONDS.inWholeMilliseconds

        timeAxis.isAutoRange = true
        timeAxis.lowerMargin = 0.0
        timeAxis.upperMargin = 0.0
        timeAxis.isTickLabelsVisible = false
        timeAxis.isTickMarksVisible = false
        timeAxis.isAxisLineVisible = false
        timeAxis.dateFormatOverride = SimpleDateFormat("")

        bandwidthAxis.isAutoRange = true
        bandwidthAxis.standardTickUnits = NumberAxis.createIntegerTickUnits()
        bandwidthAxis.numberFormatOverride = BandwidthAxisFormat()

        val plot = XYPlot(createDataset(), timeAxis, bandwidthAxis, createRenderer()).apply {
            axisOffset = CHART_INSETS
            isDomainGridlinesVisible = false
            addRangeMarker(averageMarker, Layer.FOREGROUND)
        }

        val chart = JFreeChart(plot).apply {
            setAntiAlias(true)
            removeLegend()
        }

        configureChartPanel(chart)

        // Reset counters before sampling to avoid an artificial spike in the first sample.
        MVHttpClient.byteCounter.resetCounters()
    }

    private fun startSamplingLoop() {
        uiScope.launch {
            while (isActive) {
                delay(REFRESH_INTERVAL_SECONDS)
                sampleBandwidthUsage()
            }
        }
    }

    private fun createDataset(): TimeSeriesCollection =
        TimeSeriesCollection().apply {
            addSeries(bandwidthSeries)
        }

    private fun createRenderer(): XYSplineRenderer =
        XYSplineRenderer().apply {
            setDefaultShapesVisible(false)
            setSeriesPaint(0, Color.RED)
            setSeriesStroke(0, CHART_STROKE)
        }

    private fun configureChartPanel(chart: JFreeChart) {
        chartPanel.chart = chart
        chartPanel.popupMenu = null
        chartPanel.setMouseZoomable(false)
        chartPanel.isDomainZoomable = false
        chartPanel.isRangeZoomable = false
        chartPanel.isMouseWheelEnabled = false
    }

    private fun sampleBandwidthUsage() {
        val sampleTime = Millisecond()
        val bitsPerSecond = readBitsPerSecond()
        bandwidthSeries.addOrUpdate(sampleTime, bitsPerSecond)
        updateAverageMarker()
        updateBandwidthDisplay(BandwidthDisplayValue.fromBitsPerSecond(bitsPerSecond))
    }

    private fun readBitsPerSecond(): Double {
        val byteCounter = MVHttpClient.byteCounter
        val bitsPerSecond = maxOf(0.0, byteCounter.bytesRead() * 8.0)
        byteCounter.resetCounters()
        return bitsPerSecond
    }

    private fun updateBandwidthDisplay(displayValue: BandwidthDisplayValue) {
        bandwidthValueLabel.text = displayValue.value
        bandwidthUnitLabel.text = displayValue.unit
    }

    private fun createAverageMarker(): ValueMarker =
        ValueMarker(0.0, AVERAGE_LINE_LIGHT_THEME, AVERAGE_STROKE).apply {
            alpha = 0.9f
        }

    private fun updateAverageMarker() {
        averageMarker.value = calculateAverageBitsPerSecond()
    }

    private fun calculateAverageBitsPerSecond(): Double {
        val itemCount = bandwidthSeries.itemCount
        if (itemCount == 0) {
            return 0.0
        }

        var totalBitsPerSecond = 0.0
        for (item in bandwidthSeries.items) {
            totalBitsPerSecond += (item as TimeSeriesDataItem).value.toDouble()
        }
        return totalBitsPerSecond / itemCount
    }

    private fun createWindowListener() =
        object : java.awt.event.WindowAdapter() {
            override fun windowOpened(event: java.awt.event.WindowEvent) {
                storeVisibilityState(true)
                menuAction.dialogOptional = Optional.of(this@BandwidthDialog)
                menuAction.isEnabled = false
            }

            override fun windowClosed(event: java.awt.event.WindowEvent) {
                storeVisibilityState(preserveVisibilityOnClose)
                menuAction.dialogOptional = Optional.empty()
                menuAction.isEnabled = true
            }
        }

    private fun restoreSizeFromConfig() {
        try {
            val state = applicationConfiguration.bandwidthMonitorDialogState
            if (state.hasStoredBounds()) {
                setSize(state.width, state.height)
                setLocation(state.x, state.y)
            } else {
                setSize(DEFAULT_WIDTH, DEFAULT_HEIGHT)
                moveToDefaultHudPosition()
            }
        } catch (_: Exception) {
            setSize(DEFAULT_WIDTH, DEFAULT_HEIGHT)
            moveToDefaultHudPosition()
        }
    }

    private fun createBoundsListener() =
        object : ComponentAdapter() {
            override fun componentResized(event: ComponentEvent) {
                storeBounds()
            }

            override fun componentMoved(event: ComponentEvent) {
                storeBounds()
            }
        }

    private fun storeBounds() {
        val bounds = bounds
        applicationConfiguration.setBandwidthMonitorDialogBounds(bounds.x, bounds.y, bounds.width, bounds.height)
    }

    private fun moveToDefaultHudPosition() {
        val graphicsDevice = GraphicsEnvironment.getLocalGraphicsEnvironment().defaultScreenDevice
        val displayMode = graphicsDevice.displayMode
        setLocation(displayMode.width - DEFAULT_WIDTH, 0)
    }

    private fun applyTheme() {
        val labelColor = UIManager.getColor("Label.foreground") ?: Color.LIGHT_GRAY
        val panelColor = UIManager.getColor("Panel.background") ?: Color.DARK_GRAY
        val chart = chartPanel.chart
        val plot = chart.plot as XYPlot

        timeAxis.labelPaint = labelColor
        timeAxis.tickLabelPaint = labelColor
        timeAxis.tickMarkPaint = labelColor

        bandwidthAxis.labelPaint = labelColor
        bandwidthAxis.tickLabelPaint = labelColor
        bandwidthAxis.tickMarkPaint = labelColor
        bandwidthAxis.axisLinePaint = labelColor
        averageMarker.paint = if (FlatLaf.isLafDark()) AVERAGE_LINE_DARK_THEME else AVERAGE_LINE_LIGHT_THEME

        chart.backgroundPaint = panelColor
        plot.backgroundPaint = panelColor
        plot.outlinePaint = labelColor
        plot.domainGridlinePaint = labelColor
        plot.rangeGridlinePaint = labelColor
    }

    private data class BandwidthDisplayValue(
        val value: String,
        val unit: String,
    ) {
        companion object {
            private const val KILOBIT = 1_000.0
            private const val MEGABIT = 1_000_000.0
            private const val GIGABIT = 1_000_000_000.0

            fun fromBitsPerSecond(bitsPerSecond: Double): BandwidthDisplayValue {
                val sanitizedValue = maxOf(0.0, bitsPerSecond)
                return when {
                    sanitizedValue >= GIGABIT -> scaled(sanitizedValue, GIGABIT, "GBit/s")
                    sanitizedValue >= MEGABIT -> scaled(sanitizedValue, MEGABIT, "MBit/s")
                    sanitizedValue >= KILOBIT -> scaled(sanitizedValue, KILOBIT, "KBit/s")
                    else -> BandwidthDisplayValue(INTEGER_FORMAT.format(kotlin.math.round(sanitizedValue)), "Bit/s")
                }
            }

            private fun scaled(value: Double, divisor: Double, unit: String): BandwidthDisplayValue =
                BandwidthDisplayValue(INTEGER_FORMAT.format(kotlin.math.round(value / divisor)), unit)
        }
    }

    private class BandwidthAxisFormat : NumberFormat() {
        override fun format(number: Double, toAppendTo: StringBuffer, pos: FieldPosition): StringBuffer {
            val displayValue = BandwidthDisplayValue.fromBitsPerSecond(number)
            return toAppendTo.append(displayValue.value).append(' ').append(displayValue.unit)
        }

        override fun format(number: Long, toAppendTo: StringBuffer, pos: FieldPosition): StringBuffer =
            format(number.toDouble(), toAppendTo, pos)

        override fun parse(source: String, parsePosition: ParsePosition): Number {
            parsePosition.index = source.length
            return 0.0
        }
    }

    companion object {
        private const val DEFAULT_WIDTH = 300
        private const val DEFAULT_HEIGHT = 150
        private val CHART_HISTORY_SECONDS = 30.seconds
        private val REFRESH_INTERVAL_SECONDS = 1.seconds
        private val CHART_INSETS = RectangleInsets(5.0, 5.0, 5.0, 5.0)
        private val CHART_STROKE = BasicStroke(2f)
        private val AVERAGE_STROKE = BasicStroke(1.5f)
        private val AVERAGE_LINE_LIGHT_THEME = Color(0, 170, 0)
        private val AVERAGE_LINE_DARK_THEME = Color(144, 238, 144)
        private val FORMAT_SYMBOLS = createFormatSymbols()
        private val INTEGER_FORMAT = createIntegerFormat()

        private fun createFormatSymbols(): DecimalFormatSymbols =
            DecimalFormatSymbols.getInstance().apply {
                decimalSeparator = ','
                groupingSeparator = '.'
            }

        private fun createIntegerFormat(): DecimalFormat =
            DecimalFormat("#,##0").apply {
                decimalFormatSymbols = FORMAT_SYMBOLS
                isGroupingUsed = true
            }
    }
}
