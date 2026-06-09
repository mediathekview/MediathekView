/*
 * Created by JFormDesigner on Mon Oct 21 17:52:41 CEST 2024
 */

package mediathek.gui.duplicates.statistics

import ca.odell.glazedlists.EventList
import ca.odell.glazedlists.SortedList
import ca.odell.glazedlists.swing.GlazedListsSwing
import mediathek.config.Daten
import mediathek.config.application.ApplicationConfiguration
import mediathek.gui.duplicates.FilmStatistics
import mediathek.tool.withReadLock
import org.apache.logging.log4j.LogManager
import java.awt.Window
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent
import javax.swing.AbstractAction
import javax.swing.JTable

class DuplicateStatisticsDialog(
    owner: Window,
    private val action: AbstractAction,
) : DuplicateStatisticsDialogBase(owner) {
    private val applicationConfiguration = ApplicationConfiguration.getInstance()
    private val tableFormat = DuplicateStatisticsTableFormat()

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
        action.isEnabled = true
        super.dispose()
    }

    private fun setupCommonTable() {
        val commonStats = Daten.getInstance().commonStatistics
        val sortedList = SortedList(commonStats, compareBy(FilmStatistics::sender))
        val model = GlazedListsSwing.eventTableModelWithThreadProxyList(sortedList, tableFormat)
        model.addTableModelListener { updateTotalCommonStats() }
        tblCommon.model = model
    }

    private fun setupDuplicatesTable() {
        val duplicateStats = Daten.getInstance().duplicateStatistics
        val sortedList = SortedList(duplicateStats, compareBy(FilmStatistics::sender))
        val model = GlazedListsSwing.eventTableModelWithThreadProxyList(sortedList, tableFormat)
        model.addTableModelListener { updateTotalDuplicatesStats() }
        tblDuplicates.model = model
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
        val statisticsList = Daten.getInstance().commonStatistics
        val total = statisticsList.sumCounts()
        lblTotalCommon.text = "Gesamtanzahl Filme: $total"
    }

    private fun updateTotalDuplicatesStats() {
        resizeSenderColumnWidth(tblDuplicates)
        val statisticsList = Daten.getInstance().duplicateStatistics
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
