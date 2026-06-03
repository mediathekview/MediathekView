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

package mediathek.audiothek.ui.main

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.EventList
import com.formdev.flatlaf.FlatClientProperties
import com.formdev.flatlaf.icons.FlatSearchWithHistoryIcon
import mediathek.audiothek.ui.download.CircularProgressIcon
import mediathek.audiothek.ui.download.DownloadSummary
import mediathek.gui.tabs.tab_film.EditHistoryDialog
import mediathek.tool.withReadLock
import mediathek.tool.withWriteLock
import org.jdesktop.swingx.JXBusyLabel
import org.kordamp.ikonli.fontawesome6.FontAwesomeSolid
import org.kordamp.ikonli.materialdesign2.MaterialDesignT
import org.kordamp.ikonli.swing.FontIcon
import java.awt.Dimension
import java.awt.event.ActionEvent
import java.awt.event.KeyEvent
import java.util.function.Consumer
import javax.swing.*
import javax.swing.text.JTextComponent

class AudiothekToolBar : JToolBar() {
    companion object {
        private const val SEARCH_FIELD_COLUMNS = 28
        private const val ICON_SIZE = 18
        private const val MAX_SEARCH_FIELD_WIDTH = 500
        private const val PODCAST_SEARCH_TOOLTIP = "Podcastindex-Suche läuft"
    }

    private val searchField = JTextField(SEARCH_FIELD_COLUMNS)
    private val searchHistory = AudiothekSearchHistory()
    private val searchHistoryList: EventList<String> = BasicEventList()
    private val searchHistoryButton = JButton(FlatSearchWithHistoryIcon(true)).apply {
        toolTipText = "Vorherige Suchen"
        isFocusable = false
    }
    private val reloadButton = JButton().apply {
        icon = FontIcon.of(FontAwesomeSolid.RECYCLE, ICON_SIZE)
        toolTipText = "Lokale Podcast-Daten aktualisieren"
        isFocusable = false
    }
    private val helpButton = JButton()
    private val onlineSearchCheckBox = JCheckBox("Online-Suche", true).apply {
        toolTipText = "Online-Suche via podcastindex.org nutzen"
    }
    private val podcastSearchBusyLabel = JXBusyLabel().apply {
        toolTipText = PODCAST_SEARCH_TOOLTIP
        isBusy = false
        isVisible = false
    }
    private val downloadManagerButton = JButton()
    private val downloadManagerIdleIcon = FontIcon.of(MaterialDesignT.TRAY_ARROW_DOWN, ICON_SIZE)
    private val downloadProgressIcon = CircularProgressIcon()

    init {
        isFloatable = false
        configureSearchField()
        configureButtons()
        configureSearchHistory()
        configureEmbeddedSearchActions()
        buildLayout()
    }

    fun addFilterSubmitListener(action: (String) -> Unit) {
        searchField.addActionListener {
            addSearchHistoryEntry(searchField.text)
            action(searchField.text)
        }
    }

    fun addReloadListener(action: () -> Unit) {
        reloadButton.addActionListener { action() }
    }

    fun addDownloadManagerListener(action: () -> Unit) {
        downloadManagerButton.addActionListener { action() }
    }

    fun addClearSearchListener(action: () -> Unit) {
        searchField.putClientProperty("JTextField.clearCallback", Consumer<JTextComponent> {
            searchField.text = ""
            action()
        })
        searchField.inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), "clearSearchField")
        searchField.actionMap.put("clearSearchField", object : AbstractAction() {
            override fun actionPerformed(event: ActionEvent?) {
                if (searchField.text.isNotEmpty()) {
                    searchField.text = ""
                    action()
                }
            }
        })
    }

    fun addOnlineSearchToggleListener(action: (Boolean) -> Unit) {
        onlineSearchCheckBox.addActionListener { action(onlineSearchCheckBox.isSelected) }
    }

    fun setHelpAction(action: Action) {
        helpButton.action = action
        helpButton.text = null
        helpButton.toolTipText = action.getValue(Action.SHORT_DESCRIPTION) as? String
        helpButton.isFocusable = false
    }

    fun downloadManagerAnchor(): JButton = downloadManagerButton

    fun isOnlineSearchEnabled(): Boolean = onlineSearchCheckBox.isSelected

    fun setOnlineSearchEnabled(enabled: Boolean) {
        onlineSearchCheckBox.isSelected = enabled
    }

    fun setPodcastSearchBusy(busy: Boolean) {
        podcastSearchBusyLabel.isBusy = busy
        podcastSearchBusyLabel.isVisible = busy
    }

    fun setDownloadProgress(summary: DownloadSummary) {
        if (summary.activeCount <= 0) {
            downloadManagerButton.icon = downloadManagerIdleIcon
            downloadManagerButton.toolTipText = "Download-Manager"
            return
        }

        downloadProgressIcon.progress = summary.progress
        downloadManagerButton.icon = downloadProgressIcon
        downloadManagerButton.toolTipText = "${(summary.progress * 100).toInt()} %"
        downloadManagerButton.repaint()
    }

    fun setLoading(loading: Boolean) {
        searchField.isEnabled = !loading
        reloadButton.isEnabled = !loading
        helpButton.isEnabled = !loading
        onlineSearchCheckBox.isEnabled = !loading
    }

    fun currentQuery(): String = searchField.text

    private fun configureSearchField() {
        searchField.maximumSize = Dimension(MAX_SEARCH_FIELD_WIDTH, searchField.preferredSize.height)
        searchField.putClientProperty(FlatClientProperties.PLACEHOLDER_TEXT, "Audiothek-Suche")
        searchField.putClientProperty(FlatClientProperties.TEXT_FIELD_SHOW_CLEAR_BUTTON, true)
        searchField.putClientProperty(FlatClientProperties.TEXT_FIELD_LEADING_COMPONENT, searchHistoryButton)
    }

    private fun configureButtons() {
        helpButton.isFocusable = false
        onlineSearchCheckBox.isFocusable = false
        downloadManagerButton.isFocusable = false
        downloadManagerButton.icon = downloadManagerIdleIcon
        downloadManagerButton.horizontalTextPosition = RIGHT
        downloadManagerButton.iconTextGap = 6
    }

    private fun configureSearchHistory() {
        searchHistoryList.withWriteLock {
            addAll(searchHistory.load())
        }
        searchHistoryList.addListEventListener { saveSearchHistory() }
        searchHistoryButton.addActionListener { showSearchHistoryPopup() }
    }

    private fun addSearchHistoryEntry(query: String) {
        val normalized = query.trim()
        if (normalized.isEmpty()) {
            return
        }

        searchHistoryList.withWriteLock {
            remove(normalized)
            add(0, normalized)
        }
    }

    private fun showSearchHistoryPopup() {
        val popupMenu = JPopupMenu()
        val clearHistoryItem = JMenuItem("Alles löschen").apply {
            addActionListener {
                searchHistoryList.withWriteLock {
                    clear()
                }
            }
        }
        val editHistoryItem = JMenuItem("Einträge bearbeiten").apply {
            addActionListener { showEditHistoryDialog(this) }
        }

        popupMenu.add(clearHistoryItem)
        popupMenu.add(editHistoryItem)
        searchHistoryList.withReadLock {
            if (isNotEmpty()) {
                popupMenu.addSeparator()
                forEach { item ->
                    popupMenu.add(JMenuItem(item).apply {
                        addActionListener {
                            searchField.text = item
                            searchField.postActionEvent()
                        }
                    })
                }
            }
        }
        popupMenu.show(searchHistoryButton, 0, searchHistoryButton.height)
    }

    private fun showEditHistoryDialog(menuItem: JMenuItem) {
        val owner = SwingUtilities.getWindowAncestor(this) ?: JOptionPane.getRootFrame()
        EditHistoryDialog(owner, menuItem, searchHistoryList).isVisible = true
    }

    private fun saveSearchHistory() {
        searchHistoryList.withReadLock {
            searchHistory.save(this)
        }
    }

    private fun buildLayout() {
        add(reloadButton)
        addSeparator()
        add(JLabel("Suche:"))
        add(searchField)
        addSeparator()
        add(onlineSearchCheckBox)
        add(podcastSearchBusyLabel)
        addSeparator()
        add(downloadManagerButton)
    }

    private fun configureEmbeddedSearchActions() {
        val trailingToolbar = JToolBar().apply {
            isFloatable = false
            isOpaque = false
            border = null
            addSeparator()
            add(helpButton)
        }
        searchField.putClientProperty(FlatClientProperties.TEXT_FIELD_TRAILING_COMPONENT, trailingToolbar)
    }
}
