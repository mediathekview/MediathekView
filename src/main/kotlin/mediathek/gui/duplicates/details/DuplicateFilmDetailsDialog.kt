/*
 * Created by JFormDesigner on Tue Oct 22 13:37:47 CEST 2024
 */

package mediathek.gui.duplicates.details

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.SortedList
import ca.odell.glazedlists.gui.AbstractTableComparatorChooser
import ca.odell.glazedlists.swing.AdvancedTableModel
import ca.odell.glazedlists.swing.TableComparatorChooser
import ca.odell.glazedlists.swing.eventTableModel
import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.config.application.ApplicationConfiguration
import mediathek.daten.DatenFilm
import mediathek.filmlisten.FilmCatalog
import org.apache.logging.log4j.LogManager
import java.awt.Window
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent
import javax.swing.table.DefaultTableModel

class DuplicateFilmDetailsDialog(
    owner: Window,
    private val filmCatalog: FilmCatalog,
    private val film: DatenFilm,
) : DuplicateFilmDetailsDialogBase(owner) {
    private val applicationConfiguration = ApplicationConfiguration.getInstance()
    private val dialogScope = CoroutineScope(SupervisorJob() + Dispatchers.Swing)
    private val duplicateList = BasicEventList<DatenFilm>()
    private val sortedList = SortedList(duplicateList)
    private lateinit var tableModel: AdvancedTableModel<DatenFilm>
    private lateinit var comparatorChooser: TableComparatorChooser<DatenFilm>
    private var disposed = false

    init {
        okButton.addActionListener { dispose() }
        setupTable()
        restorePosition()
        addWindowListener(object : WindowAdapter() {
            override fun windowClosed(event: WindowEvent) {
                savePosition()
            }
        })
        loadDuplicates()
    }

    override fun dispose() {
        if (disposed) return
        disposed = true
        dialogScope.cancel()
        table1.model = DefaultTableModel()
        try {
            disposeResource("duplicate details comparator chooser", comparatorChooser::dispose)
            disposeResource("duplicate details table model", tableModel::dispose)
            disposeResource("duplicate details sorted list", sortedList::close)
            disposeResource("duplicate details source list", duplicateList::close)
        } finally {
            super.dispose()
        }
    }

    private fun setupTable() {
        tableModel = sortedList.eventTableModel(
            DuplicateFilmDetailsTableFormat(),
        )
        table1.model = tableModel
        comparatorChooser = TableComparatorChooser.install(
            table1,
            sortedList,
            AbstractTableComparatorChooser.SINGLE_COLUMN,
        )

        table1.columnModel.getColumn(0).preferredWidth = 90
        table1.columnModel.getColumn(1).preferredWidth = 120
        table1.columnModel.getColumn(2).preferredWidth = 200
        table1.columnModel.getColumn(5).preferredWidth = 400
        table1.columnModel.getColumn(6).preferredWidth = 400
    }

    private fun disposeResource(name: String, dispose: () -> Unit) {
        runCatching(dispose)
            .onFailure { failure -> logger.warn("Failed to dispose {}", name, failure) }
    }

    private fun loadDuplicates() {
        val url = film.urlNormalQuality
        dialogScope.launch {
            val duplicates = withContext(Dispatchers.Default) {
                filmCatalog.allFilms
                    .asSequence()
                    .filter { it.urlNormalQuality == url }
                    .toList()
            }
            duplicateList.clear()
            duplicateList.addAll(duplicates)
        }
    }

    private fun restorePosition() {
        try {
            val state = applicationConfiguration.duplicateFilmDetailsDialogState
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
        applicationConfiguration.setDuplicateFilmDetailsDialogBounds(
            currentLocation.x,
            currentLocation.y,
            currentSize.width,
            currentSize.height,
        )
    }

    private companion object {
        private val logger = LogManager.getLogger()
    }
}
