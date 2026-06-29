/*
 * Created by JFormDesigner on Tue Oct 22 13:37:47 CEST 2024
 */

package mediathek.gui.duplicates.details

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.SortedList
import ca.odell.glazedlists.gui.AbstractTableComparatorChooser
import ca.odell.glazedlists.swing.GlazedListsSwing
import ca.odell.glazedlists.swing.TableComparatorChooser
import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.config.application.ApplicationConfiguration
import mediathek.daten.DatenFilm
import mediathek.filmlisten.FilmCatalog
import org.apache.logging.log4j.LogManager
import java.awt.Window
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent

class DuplicateFilmDetailsDialog(
    owner: Window,
    private val filmCatalog: FilmCatalog,
    private val film: DatenFilm,
) : DuplicateFilmDetailsDialogBase(owner) {
    private val applicationConfiguration = ApplicationConfiguration.getInstance()
    private val dialogScope = CoroutineScope(SupervisorJob() + Dispatchers.Swing)
    private val duplicateList = BasicEventList<DatenFilm>()
    private val sortedList = SortedList(duplicateList)

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
        dialogScope.cancel()
        super.dispose()
    }

    private fun setupTable() {
        table1.model = GlazedListsSwing.eventTableModelWithThreadProxyList(
            sortedList,
            DuplicateFilmDetailsTableFormat(),
        )
        TableComparatorChooser.install(
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
