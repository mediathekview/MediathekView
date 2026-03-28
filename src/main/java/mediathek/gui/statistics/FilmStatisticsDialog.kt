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

package mediathek.gui.statistics

import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.config.Daten
import mediathek.daten.Country
import mediathek.daten.DatenFilm
import mediathek.tool.ApplicationConfiguration
import mediathek.tool.EscapeKeyHandler
import mediathek.tool.GermanStringSorter
import mediathek.tool.datum.DatumFilm
import org.apache.logging.log4j.LogManager
import org.jfree.chart.ChartFactory
import org.jfree.chart.ChartPanel
import org.jfree.chart.JFreeChart
import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.labels.CategoryItemLabelGenerator
import org.jfree.chart.plot.CategoryPlot
import org.jfree.chart.renderer.category.BarRenderer
import org.jfree.chart.renderer.category.StandardBarPainter
import org.jfree.chart.ui.RectangleInsets
import org.jfree.data.category.DefaultCategoryDataset
import java.awt.*
import java.awt.geom.Rectangle2D
import java.lang.Runnable
import java.time.LocalDate
import java.time.ZoneId
import java.time.temporal.ChronoUnit
import java.util.*
import java.util.concurrent.Executor
import javax.swing.*
import javax.swing.border.EmptyBorder
import javax.swing.table.AbstractTableModel
import javax.swing.table.TableCellRenderer
import javax.swing.table.TableColumnModel
import kotlin.coroutines.CoroutineContext

class FilmStatisticsDialog(
    owner: Window,
    private val action: AbstractAction
) : JDialog(owner), CoroutineScope {

    override val coroutineContext: CoroutineContext = SupervisorJob() + Dispatchers.Swing

    private val workerDispatcher = ExecutorBackedDispatcher(Daten.getInstance().decoratedPool)

    private val senderTableModel = SenderStatisticsTableModel()
    private val intervalDataset = DefaultCategoryDataset()
    private val intervalChart: JFreeChart = ChartFactory.createBarChart(
        "Maximal verfügbare Filme",
        "Zeitraum in Tagen",
        "Anzahl Filme",
        intervalDataset
    )

    private val tblSenderStats = JTable()
    private val lblTotalFilms = JLabel("-")
    private val lblHighQualityFilms = JLabel("-")
    private val lblRegularQualityFilms = JLabel("-")
    private val lblSubtitleDownloads = JLabel("-")
    private val lblBurnedInSubtitles = JLabel("-")
    private val lblSignLanguageFilms = JLabel("-")
    private val lblTrailerFilms = JLabel("-")
    private val lblThemaCount = JLabel("-")
    private val lblLivestreams = JLabel("-")
    private val lblGeoBlockedFilms = JLabel("-")
    private val lblStatus = JLabel()
    private val progressBar = JProgressBar()
    private val closeButton = JButton("Schließen")

    companion object {
        private val logger = LogManager.getLogger()
        private val INTERVAL_DAYS = intArrayOf(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 14, 16, 18, 20, 25, 30, 60, 90, 180, 365)
        private const val LOADING_TEXT = "Berechne Statistik aus der ungefilterten Filmliste..."
        private const val READY_TEXT = "Statistik basiert auf der ungefilterten Filmliste ohne Livestreams."
        private const val ERROR_TEXT = "Statistik konnte nicht berechnet werden."
    }

    init {
        initComponents()
        EscapeKeyHandler.installHandler(this, ::dispose)
        tblSenderStats.model = senderTableModel
        applyChartTheme()

        action.isEnabled = false
        loadStatistics()
    }

    override fun dispose() {
        action.isEnabled = true
        cancel()
        super.dispose()
    }

    private fun loadStatistics() {
        lblStatus.text = LOADING_TEXT
        progressBar.isVisible = true
        progressBar.isIndeterminate = true

        launch {
            try {
                val statistics = computeStatistics()
                applyStatistics(statistics)
                lblStatus.text = READY_TEXT
            } catch (_: CancellationException) {
                return@launch
            } catch (ex: Exception) {
                logger.error("Failed to compute film statistics", ex)
                lblStatus.text = ERROR_TEXT
            } finally {
                progressBar.isIndeterminate = false
                progressBar.isVisible = false
            }
        }
    }

    private suspend fun computeStatistics(): ComputedStatistics = withContext(workerDispatcher) {
        val sorter = GermanStringSorter.getInstance()
        val today = LocalDate.now()
        val zoneId = ZoneId.systemDefault()
        val currentGeoLocation = ApplicationConfiguration.getInstance().geographicLocation

        val allEntries = Daten.getInstance().listeFilme.parallelStream().toList()
        val livestreams = allEntries.count(DatenFilm::isLivestream).toLong()
        val filmsWithoutLivestreams = allEntries.filterNot(DatenFilm::isLivestream)
        val totalFilms = filmsWithoutLivestreams.size.toLong()

        val highQualityFilmsDeferred = async {
            filmsWithoutLivestreams.count(DatenFilm::isHighQuality).toLong()
        }
        val subtitleDownloadsDeferred = async {
            filmsWithoutLivestreams.count(DatenFilm::hasSubtitle).toLong()
        }
        val burnedInSubtitlesDeferred = async {
            filmsWithoutLivestreams.count(DatenFilm::hasBurnedInSubtitles).toLong()
        }
        val signLanguageFilmsDeferred = async {
            filmsWithoutLivestreams.count(DatenFilm::isSignLanguage).toLong()
        }
        val trailerFilmsDeferred = async {
            filmsWithoutLivestreams.count(DatenFilm::isTrailerTeaser).toLong()
        }
        val geoBlockedFilmsDeferred = async {
            filmsWithoutLivestreams.count { film -> isGeoBlockedForLocation(film, currentGeoLocation) }.toLong()
        }
        val themaCountDeferred = async {
            filmsWithoutLivestreams.asSequence()
                .map(DatenFilm::getThema)
                .map(String::trim)
                .filter(String::isNotEmpty)
                .toCollection(TreeSet(sorter))
                .size
                .toLong()
        }
        val senderCountsDeferred = async {
            filmsWithoutLivestreams.groupingBy { normalizeLabel(it.sender) }.eachCount().mapValues { it.value.toLong() }
        }
        val filmAgesInDaysDeferred = async {
            filmsWithoutLivestreams.map { film -> getFilmAgeInDays(film, today, zoneId) }.filter { age -> age >= 0L }
        }

        val highQualityFilms = highQualityFilmsDeferred.await()
        val senderCounts = senderCountsDeferred.await()
        val filmAgesInDays = filmAgesInDaysDeferred.await()

        val sortedSenderStatistics = senderCounts.entries.asSequence()
            .map { SenderStatistic(it.key, it.value) }
            .sortedWith(compareByDescending<SenderStatistic> { it.count }.thenBy(sorter) { it.sender })
            .toList()

        val intervalCounts = LinkedHashMap<Int, Long>(INTERVAL_DAYS.size)
        for (intervalDay in INTERVAL_DAYS) {
            intervalCounts[intervalDay] = filmAgesInDays.count { age -> age <= intervalDay }.toLong()
        }

        ComputedStatistics(
            totalFilms = totalFilms,
            senderStatistics = sortedSenderStatistics,
            highQualityFilms = highQualityFilms,
            regularQualityFilms = totalFilms - highQualityFilms,
            subtitleDownloads = subtitleDownloadsDeferred.await(),
            burnedInSubtitles = burnedInSubtitlesDeferred.await(),
            signLanguageFilms = signLanguageFilmsDeferred.await(),
            trailerFilms = trailerFilmsDeferred.await(),
            themaCount = themaCountDeferred.await(),
            livestreams = livestreams,
            geoBlockedFilms = geoBlockedFilmsDeferred.await(),
            intervalCounts = intervalCounts
        )
    }

    private fun isGeoBlockedForLocation(film: DatenFilm, currentGeoLocation: Country): Boolean {
        if (!film.hasCountries()) {
            return false
        }
        if (film.hasCountry(Country.EU)) {
            return !(film.hasCountry(currentGeoLocation) || Country.EU_COUNTRIES.contains(currentGeoLocation))
        }
        return !film.hasCountry(currentGeoLocation)
    }

    private fun getFilmAgeInDays(film: DatenFilm, today: LocalDate, zoneId: ZoneId): Long {
        val filmDate = film.datumFilm
        if (filmDate == DatumFilm.UNDEFINED_FILM_DATE) {
            return -1
        }

        val localFilmDate = filmDate.toInstant().atZone(zoneId).toLocalDate()
        return maxOf(0, ChronoUnit.DAYS.between(localFilmDate, today))
    }

    private fun applyStatistics(statistics: ComputedStatistics) {
        lblTotalFilms.text = formatCount(statistics.totalFilms)
        lblHighQualityFilms.text = formatCount(statistics.highQualityFilms)
        lblRegularQualityFilms.text = formatCount(statistics.regularQualityFilms)
        lblSubtitleDownloads.text = formatCount(statistics.subtitleDownloads)
        lblBurnedInSubtitles.text = formatCount(statistics.burnedInSubtitles)
        lblSignLanguageFilms.text = formatCount(statistics.signLanguageFilms)
        lblTrailerFilms.text = formatCount(statistics.trailerFilms)
        lblThemaCount.text = formatCount(statistics.themaCount)
        lblLivestreams.text = formatCount(statistics.livestreams)
        lblGeoBlockedFilms.text = formatCount(statistics.geoBlockedFilms)

        senderTableModel.setRows(statistics.senderStatistics)
        resizeSenderColumnWidth(tblSenderStats)

        intervalDataset.clear()
        statistics.intervalCounts.forEach { (interval, count) ->
            intervalDataset.addValue(count, "Filme", interval.toString())
        }
    }

    private fun resizeSenderColumnWidth(table: JTable) {
        val columnModel: TableColumnModel = table.columnModel
        var width = 120
        for (row in 0 until table.rowCount) {
            val renderer: TableCellRenderer = table.getCellRenderer(row, 0)
            val comp = table.prepareRenderer(renderer, row, 0)
            width = maxOf(comp.preferredSize.width + 10, width)
        }
        columnModel.getColumn(0).preferredWidth = width
        columnModel.getColumn(1).preferredWidth = 100
    }

    private fun applyChartTheme() {
        val labelColor = UIManager.getColor("Label.foreground") ?: Color.DARK_GRAY
        val panelColor = UIManager.getColor("Panel.background") ?: Color.WHITE

        intervalChart.backgroundPaint = panelColor
        intervalChart.removeLegend()
        val plot = intervalChart.categoryPlot
        plot.backgroundPaint = panelColor
        plot.outlinePaint = labelColor
        plot.domainGridlinePaint = labelColor
        plot.rangeGridlinePaint = labelColor
        plot.axisOffset = RectangleInsets(5.0, 5.0, 5.0, 5.0)

        plot.domainAxis.apply {
            labelPaint = labelColor
            tickLabelPaint = labelColor
            tickMarkPaint = labelColor
        }

        val rangeAxis = plot.rangeAxis as NumberAxis
        rangeAxis.labelPaint = labelColor
        rangeAxis.tickLabelPaint = labelColor
        rangeAxis.tickMarkPaint = labelColor
        rangeAxis.standardTickUnits = NumberAxis.createIntegerTickUnits()

        val renderer = object : BarRenderer() {
            override fun drawItemLabel(
                g2: Graphics2D,
                data: org.jfree.data.category.CategoryDataset,
                row: Int,
                column: Int,
                plot: CategoryPlot,
                generator: CategoryItemLabelGenerator?,
                bar: Rectangle2D,
                negative: Boolean
            ) {
                if (generator == null) {
                    return
                }

                val label = generator.generateLabel(data, row, column)
                if (label.isNullOrEmpty()) {
                    return
                }

                g2.font = getItemLabelFont(row, column)
                g2.paint = getItemLabelPaint(row, column)

                val bounds = g2.fontMetrics.getStringBounds(label, g2)
                val rotatedHeight = bounds.width
                val centerX = bar.centerX
                val padding = 4.0
                val drawInsideBar = rotatedHeight + padding <= bar.height
                val centerY = if (drawInsideBar) {
                    bar.centerY
                } else {
                    bar.minY - padding - rotatedHeight / 2.0
                }

                g2.paint = if (drawInsideBar) Color.WHITE else getItemLabelPaint(row, column)
                val oldTransform = g2.transform
                g2.translate(centerX, centerY)
                g2.rotate(-Math.PI / 2)
                g2.drawString(label, (-bounds.centerX).toFloat(), (-bounds.centerY).toFloat())
                g2.transform = oldTransform
            }
        }

        plot.renderer = renderer
        renderer.setSeriesPaint(0, Color(0x4C, 0x78, 0xA8))
        renderer.setShadowVisible(false)
        renderer.barPainter = StandardBarPainter()
        renderer.isDrawBarOutline = false
        renderer.maximumBarWidth = 0.08
        renderer.defaultItemLabelGenerator = object : CategoryItemLabelGenerator {
            override fun generateRowLabel(dataset: org.jfree.data.category.CategoryDataset, row: Int): String {
                return dataset.getRowKey(row).toString()
            }

            override fun generateColumnLabel(dataset: org.jfree.data.category.CategoryDataset, column: Int): String {
                return dataset.getColumnKey(column).toString()
            }

            override fun generateLabel(dataset: org.jfree.data.category.CategoryDataset, row: Int, column: Int): String {
                val value = dataset.getValue(row, column)
                return value?.toLong()?.toString().orEmpty()
            }
        }
        renderer.defaultItemLabelsVisible = true
        renderer.defaultItemLabelPaint = labelColor
    }

    private fun formatCount(count: Long): String = count.toString()

    private fun normalizeLabel(value: String?): String {
        val trimmed = value?.trim().orEmpty()
        return trimmed.ifEmpty { "<Unbekannt>" }
    }

    private fun createSummaryRow(description: String, valueLabel: JLabel): JPanel {
        val panel = JPanel(BorderLayout(10, 0))
        panel.isOpaque = false
        panel.add(JLabel(description), BorderLayout.CENTER)
        valueLabel.horizontalAlignment = SwingConstants.TRAILING
        valueLabel.font = valueLabel.font.deriveFont(valueLabel.font.style or Font.BOLD)
        panel.add(valueLabel, BorderLayout.EAST)
        return panel
    }

    private fun initComponents() {
        title = "Filmlisten-Statistik"
        defaultCloseOperation = DISPOSE_ON_CLOSE
        isModal = true
        preferredSize = Dimension(1220, 720)

        closeButton.addActionListener { dispose() }
        rootPane.defaultButton = closeButton

        tblSenderStats.apply {
            fillsViewportHeight = true
            autoCreateRowSorter = true
            setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
            showVerticalLines = false
            preferredScrollableViewportSize = Dimension(250, 420)
        }

        val senderScrollPane = JScrollPane(tblSenderStats)
        val senderPanel = JPanel(BorderLayout()).apply {
            border = BorderFactory.createTitledBorder("Filme pro Sender")
            add(senderScrollPane, BorderLayout.CENTER)
        }

        val chartComponent = ChartPanel(intervalChart).apply {
            popupMenu = null
            setMouseZoomable(false)
            isDomainZoomable = false
            isRangeZoomable = false
            isMouseWheelEnabled = false
        }
        val chartPanel = JPanel(BorderLayout()).apply {
            add(chartComponent, BorderLayout.CENTER)
        }

        val splitPane = JSplitPane().apply {
            resizeWeight = 0.28
            leftComponent = senderPanel
            rightComponent = chartPanel
        }

        val summaryPanel = JPanel(GridLayout(0, 2, 18, 8)).apply {
            border = BorderFactory.createCompoundBorder(
                BorderFactory.createTitledBorder("Übersicht"),
                EmptyBorder(5, 5, 5, 5)
            )
            add(createSummaryRow("Gesamtzahl Filme", lblTotalFilms))
            add(createSummaryRow("Anzahl unterschiedlicher Themen", lblThemaCount))
            add(createSummaryRow("Filme in hoher Qualität", lblHighQualityFilms))
            add(createSummaryRow("Filme in normaler Qualität", lblRegularQualityFilms))
            add(createSummaryRow("Filme mit separatem Untertitel-Download", lblSubtitleDownloads))
            add(createSummaryRow("Filme mit eingebrannten Untertiteln", lblBurnedInSubtitles))
            add(createSummaryRow("Filme mit Gebärdensprache", lblSignLanguageFilms))
            add(createSummaryRow("Trailer / Teaser / Vorschau", lblTrailerFilms))
            add(createSummaryRow("Livestreams", lblLivestreams))
            add(createSummaryRow("Geo-blockierte Filme für den aktuellen Standort", lblGeoBlockedFilms))
        }

        progressBar.isVisible = false
        val statusPanel = JPanel(BorderLayout(10, 0)).apply {
            add(progressBar, BorderLayout.WEST)
            add(lblStatus, BorderLayout.CENTER)
        }

        val buttonBar = JPanel(FlowLayout(FlowLayout.RIGHT, 0, 0)).apply {
            add(closeButton)
        }

        val contentPanel = JPanel(BorderLayout(0, 12)).apply {
            add(summaryPanel, BorderLayout.NORTH)
            add(splitPane, BorderLayout.CENTER)
            add(statusPanel, BorderLayout.SOUTH)
        }

        val dialogPane = JPanel(BorderLayout(0, 12)).apply {
            border = EmptyBorder(12, 12, 12, 12)
            add(contentPanel, BorderLayout.CENTER)
            add(buttonBar, BorderLayout.SOUTH)
        }

        contentPane = dialogPane
        pack()
        setLocationRelativeTo(null)
    }

    private data class SenderStatistic(val sender: String, val count: Long)

    private data class ComputedStatistics(
        val totalFilms: Long,
        val senderStatistics: List<SenderStatistic>,
        val highQualityFilms: Long,
        val regularQualityFilms: Long,
        val subtitleDownloads: Long,
        val burnedInSubtitles: Long,
        val signLanguageFilms: Long,
        val trailerFilms: Long,
        val themaCount: Long,
        val livestreams: Long,
        val geoBlockedFilms: Long,
        val intervalCounts: Map<Int, Long>
    )

    private class SenderStatisticsTableModel : AbstractTableModel() {
        private val rows = mutableListOf<SenderStatistic>()

        override fun getRowCount(): Int = rows.size

        override fun getColumnCount(): Int = 2

        override fun getColumnName(column: Int): String = when (column) {
            0 -> "Sender"
            1 -> "Anzahl"
            else -> ""
        }

        override fun getColumnClass(columnIndex: Int): Class<*> = when (columnIndex) {
            0 -> String::class.java
            1 -> Long::class.javaObjectType
            else -> Any::class.java
        }

        override fun getValueAt(rowIndex: Int, columnIndex: Int): Any? {
            val row = rows[rowIndex]
            return when (columnIndex) {
                0 -> row.sender
                1 -> row.count
                else -> null
            }
        }

        fun setRows(senderStatistics: List<SenderStatistic>) {
            rows.clear()
            rows.addAll(senderStatistics)
            fireTableDataChanged()
        }
    }

    private class ExecutorBackedDispatcher(private val executor: Executor) : CoroutineDispatcher() {
        override fun dispatch(context: CoroutineContext, block: Runnable) {
            executor.execute(block)
        }
    }
}
