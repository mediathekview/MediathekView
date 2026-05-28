/*
 * Created by JFormDesigner on Tue Oct 22 13:37:47 CEST 2024
 */

package mediathek.gui.duplicates.details

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.SortedList
import ca.odell.glazedlists.gui.AbstractTableComparatorChooser
import ca.odell.glazedlists.swing.GlazedListsSwing
import ca.odell.glazedlists.swing.TableComparatorChooser
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.cancel
import kotlinx.coroutines.launch
import kotlinx.coroutines.swing.Swing
import kotlinx.coroutines.withContext
import mediathek.config.Daten
import mediathek.daten.DatenFilm
import mediathek.tool.ApplicationConfiguration
import mediathek.tool.withLock
import org.apache.commons.configuration2.sync.LockMode
import org.apache.logging.log4j.LogManager
import java.awt.Window
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent
import java.util.NoSuchElementException

class DuplicateFilmDetailsDialog(
    owner: Window,
    private val film: DatenFilm,
) : DuplicateFilmDetailsDialogBase(owner) {
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
                Daten.getInstance().listeFilme
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
            ApplicationConfiguration.getConfiguration().withLock(LockMode.READ) {
                val x = getInt(CONFIG_X)
                val y = getInt(CONFIG_Y)
                val width = getInt(CONFIG_WIDTH)
                val height = getInt(CONFIG_HEIGHT)

                setSize(width, height)
                setLocation(x, y)
            }
        } catch (_: NoSuchElementException) {
            pack()
        } catch (ex: Exception) {
            logger.error("Unhandled Exception", ex)
            pack()
        }
    }

    private fun savePosition() {
        ApplicationConfiguration.getConfiguration().withLock(LockMode.WRITE) {
            val currentSize = size
            val currentLocation = location
            setProperty(CONFIG_WIDTH, currentSize.width)
            setProperty(CONFIG_HEIGHT, currentSize.height)
            setProperty(CONFIG_X, currentLocation.x)
            setProperty(CONFIG_Y, currentLocation.y)
        }
    }

    private companion object {
        private const val CONFIG_X = "duplicate_film_details_dialog.x"
        private const val CONFIG_Y = "duplicate_film_details_dialog.y"
        private const val CONFIG_HEIGHT = "duplicate_film_details_dialog.height"
        private const val CONFIG_WIDTH = "duplicate_film_details_dialog.width"
        private val logger = LogManager.getLogger()
    }
}
