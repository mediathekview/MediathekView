/*
 * Created by JFormDesigner on Mon Oct 21 17:52:41 CEST 2024
 */

package mediathek.gui.duplicates.statistics

import ca.odell.glazedlists.EventList
import ca.odell.glazedlists.SortedList
import ca.odell.glazedlists.swing.AdvancedTableModel
import ca.odell.glazedlists.swing.eventTableModelWithThreadProxyList
import mediathek.config.application.ApplicationConfiguration
import mediathek.filmlisten.FilmCatalog
import mediathek.gui.duplicates.FilmStatistics
import mediathek.tool.withReadLock
import org.apache.logging.log4j.LogManager
import java.awt.Window
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent
import javax.swing.AbstractAction
import javax.swing.JTable
import javax.swing.table.DefaultTableModel

class DuplicateStatisticsDialog(
    owner: Window,
    private val filmCatalog: FilmCatalog,
    private val action: AbstractAction,
) : DuplicateStatisticsDialogBase(owner) {
    private val applicationConfiguration = ApplicationConfiguration.getInstance()
    private val tableFormat = DuplicateStatisticsTableFormat()
    private val commonStats = SortedList(filmCatalog.commonStatistics, compareBy(FilmStatistics::sender))
    private val duplicateStats = SortedList(filmCatalog.duplicateStatistics, compareBy(FilmStatistics::sender))
    private lateinit var commonModel: AdvancedTableModel<FilmStatistics>
    private lateinit var duplicateModel: AdvancedTableModel<FilmStatistics>
    private var disposed = false

    init {
        setupCommonTable()
        setupDuplicatesTable()

        updateTotalCommonStats()
        updateTotalDuplicatesStats()

        tblCommon.columnModel.getColumn(IDX_NUM).preferredWidth = COL_NUM_WIDTH
        tblDuplicates.columnModel.getColumn(IDX_NUM).preferredWidth = COL_NUM_WIDTH
        resizeSenderColumnWidth(tblCommon)
        resizeSenderColumnWidth(tblDuplicates)

        action.isEnabled = false

        restorePosition()
        addWindowListener(object : WindowAdapter() {
            override fun windowClosed(event: WindowEvent) {
                savePosition()
            }
        })
    }

    override fun dispose() {
        if (disposed) {
            super.dispose()
            return
        }
        disposed = true
        action.isEnabled = true
        tblCommon.model = DefaultTableModel()
        tblDuplicates.model = DefaultTableModel()
        try {
            disposeResource("common statistics table model", commonModel::dispose)
            disposeResource("duplicate statistics table model", duplicateModel::dispose)
            disposeResource("common statistics list", commonStats::dispose)
            disposeResource("duplicate statistics list", duplicateStats::dispose)
        } finally {
            super.dispose()
        }
    }

    private fun disposeResource(name: String, dispose: () -> Unit) {
        runCatching(dispose)
            .onFailure { failure -> logger.warn("Failed to dispose {}", name, failure) }
    }

    private fun setupCommonTable() {
        commonModel = commonStats.eventTableModelWithThreadProxyList(tableFormat)
        commonModel.addTableModelListener { updateTotalCommonStats() }
        tblCommon.model = commonModel
    }

    private fun setupDuplicatesTable() {
        duplicateModel = duplicateStats.eventTableModelWithThreadProxyList(tableFormat)
        duplicateModel.addTableModelListener { updateTotalDuplicatesStats() }
        tblDuplicates.model = duplicateModel
    }

    private fun resizeSenderColumnWidth(table: JTable) {
        val columnModel = table.columnModel
        var width = MIN_SENDER_COLUMN_WIDTH
        for (row in 0 until table.rowCount) {
            val renderer = table.getCellRenderer(row, 0)
            val component = table.prepareRenderer(renderer, row, 0)
            width = maxOf(component.preferredSize.width + 1, width)
        }
        columnModel.getColumn(0).preferredWidth = width
    }

    private fun updateTotalCommonStats() {
        resizeSenderColumnWidth(tblCommon)
        val statisticsList = filmCatalog.commonStatistics
        val total = statisticsList.sumCounts()
        lblTotalCommon.text = "Gesamtanzahl Filme: $total"
    }

    private fun updateTotalDuplicatesStats() {
        resizeSenderColumnWidth(tblDuplicates)
        val statisticsList = filmCatalog.duplicateStatistics
        val total = statisticsList.sumCounts()
        lblTotalDuplicates.text = "Gesamtanzahl Duplikate: $total"
    }

    private fun restorePosition() {
        try {
            val state = applicationConfiguration.duplicateStatisticsDialogState
            if (state.hasStoredBounds()) {
                setSize(state.width, state.height)
                setLocation(state.x, state.y)
            } else {
                pack()
            }
        } catch (ex: Exception) {
            logger.error("Unhandled Exception", ex)
            pack()
        }
    }

    private fun savePosition() {
        val currentSize = size
        val currentLocation = location
        applicationConfiguration.setDuplicateStatisticsDialogBounds(
            currentLocation.x,
            currentLocation.y,
            currentSize.width,
            currentSize.height,
        )
    }

    private fun EventList<FilmStatistics>.sumCounts(): Long =
        withReadLock {
            sumOf { statistics -> statistics.count }
        }

    private companion object {
        private const val COL_NUM_WIDTH = 90
        private const val IDX_NUM = 1
        private const val MIN_SENDER_COLUMN_WIDTH = 120
        private val logger = LogManager.getLogger()
    }
}
