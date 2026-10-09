/*
 * Created by JFormDesigner on Sat Apr 27 12:49:11 CEST 2024
 */

package mediathek.gui.tabs.tab_film

import ca.odell.glazedlists.EventList
import ca.odell.glazedlists.swing.GlazedListsSwing
import mediathek.config.application.ApplicationConfiguration
import mediathek.tool.withWriteLock
import org.apache.logging.log4j.LogManager
import java.awt.Window
import java.awt.event.KeyAdapter
import java.awt.event.KeyEvent
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent
import javax.swing.JMenuItem

class EditHistoryDialog(
    owner: Window,
    menuItem: JMenuItem,
    private val eventList: EventList<String>,
) : EditHistoryDialogBase(owner) {
    private val applicationConfiguration = ApplicationConfiguration.getInstance()
    private val keyAdapter = DeleteKeyAdapter()
    private var keyAdapterInstalled = false

    init {
        menuItem.isEnabled = false
        addWindowListener(object : WindowAdapter() {
            override fun windowClosed(event: WindowEvent) {
                menuItem.isEnabled = true
                savePosition()
            }
        })

        list.model = GlazedListsSwing.eventListModelWithThreadProxyList(eventList)
        list.selectionModel.addListSelectionListener { event ->
            if (!event.valueIsAdjusting) {
                adjustButtons()
            }
        }
        adjustButtons()

        btnDeleteEntries.addActionListener { deleteEntries() }
        btnUp.addActionListener {
            val idx = moveEntry(list.selectedIndex) { it - 1 }
            list.selectedIndex = idx
        }
        btnDown.addActionListener {
            val idx = moveEntry(list.selectedIndex) { it + 1 }
            list.selectedIndex = idx
        }

        restorePosition()
    }

    private fun deleteEntries() {
        val listModel = list.model
        val changeList = list.selectedIndices.map { listModel.getElementAt(it) }

        eventList.withWriteLock {
            changeList.forEach(eventList::remove)
        }
    }

    private fun moveEntry(idx: Int, operator: (Int) -> Int): Int {
        return eventList.withWriteLock {
            val obj = eventList[idx]
            eventList.removeAt(idx)
            val newIdx = operator(idx)
            eventList.add(newIdx, obj)
            newIdx
        }
    }

    private fun restorePosition() {
        try {
            val state = applicationConfiguration.editHistoryDialogState
            if (state.hasStoredBounds()) {
                setSize(state.width, state.height)
                setLocation(state.x, state.y)
            }
        } catch (ex: Exception) {
            logger.error("Unhandled exception", ex)
        }
    }

    private fun savePosition() {
        val size = size
        val location = location
        applicationConfiguration.setEditHistoryDialogBounds(
            location.x,
            location.y,
            size.width,
            size.height,
        )
    }

    private fun adjustButtons() {
        val itemCount = list.selectionModel.selectedItemsCount
        val singleSelection = itemCount == 1
        btnDeleteEntries.isEnabled = itemCount > 0
        btnUp.isEnabled = singleSelection
        btnDown.isEnabled = singleSelection
        if (singleSelection) {
            val idx = list.selectionModel.leadSelectionIndex
            if (idx == 0) {
                btnUp.isEnabled = false
            }
            if (idx == list.model.size - 1) {
                btnDown.isEnabled = false
            }
        }
        setupKeyListener(itemCount)
    }

    private fun setupKeyListener(itemCount: Int) {
        if (itemCount > 0 && !keyAdapterInstalled) {
            list.addKeyListener(keyAdapter)
            keyAdapterInstalled = true
        } else if (itemCount == 0 && keyAdapterInstalled) {
            list.removeKeyListener(keyAdapter)
            keyAdapterInstalled = false
        }
    }

    private inner class DeleteKeyAdapter : KeyAdapter() {
        override fun keyReleased(event: KeyEvent) {
            if (event.keyCode == KeyEvent.VK_DELETE) {
                event.consume()
                deleteEntries()
            }
        }
    }

    private companion object {
        private val logger = LogManager.getLogger()
    }
}
