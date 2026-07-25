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

package mediathek.gui.tabs.tab_film.search

import com.formdev.flatlaf.FlatClientProperties
import com.formdev.flatlaf.extras.FlatSVGIcon
import com.formdev.flatlaf.icons.FlatSearchWithHistoryIcon
import mediathek.config.MVColor
import mediathek.config.application.ApplicationConfiguration
import mediathek.gui.search.SearchHistoryModel
import mediathek.gui.tabs.tab_film.EditHistoryDialog
import mediathek.tool.*
import org.apache.logging.log4j.LogManager
import java.awt.Color
import java.awt.Dimension
import java.awt.Window
import java.awt.event.KeyAdapter
import java.awt.event.KeyEvent
import java.beans.PropertyChangeListener
import java.beans.PropertyChangeSupport
import java.util.function.Consumer
import javax.swing.*
import javax.swing.event.DocumentEvent
import javax.swing.event.DocumentListener
import javax.swing.text.JTextComponent

private val DEFAULT_DIMENSION = Dimension(500, 100)
private val LUCENE_DEFAULT_DIMENSION = Dimension(700, 100)
private const val SEARCHMODE_PROPERTY_STRING = "searchMode"

abstract class SearchField(protected val host: Host) : JTextField("", 40) {
    interface Host {
        val showLuceneTutorialAction: Action

        fun ownerWindow(): Window
        fun loadTable()
        fun loadTable(fromSearchField: Boolean)
    }

    private val pcs = PropertyChangeSupport(this)
    private var currentSearchMode: SearchControlFieldMode? = null

    init {
        maximumSize = DEFAULT_DIMENSION
        putClientProperty(FlatClientProperties.TEXT_FIELD_SHOW_CLEAR_BUTTON, true)
        putClientProperty("JTextField.clearCallback", Consumer<JTextComponent> { clearSearchField() })

        addKeyListener(EscapeKeyAdapter())
        addActionListener { performSearch() }

        createTrailingComponents()
        setupContextMenu()
    }

    private fun setupContextMenu() {
        val handler = TextCopyPasteHandler(this)
        componentPopupMenu = handler.getPopupMenu()
    }

    protected abstract fun createTrailingComponents()

    protected abstract fun performSearch()

    protected fun clearSearchField() {
        text = ""
        fireActionPerformed()
    }

    fun addSearchModeChangeListener(listener: PropertyChangeListener) {
        pcs.addPropertyChangeListener(SEARCHMODE_PROPERTY_STRING, listener)
    }

    fun getSearchMode(): SearchControlFieldMode = checkNotNull(currentSearchMode)

    fun setSearchMode(mode: SearchControlFieldMode) {
        val oldValue = currentSearchMode
        currentSearchMode = mode
        pcs.firePropertyChange(SEARCHMODE_PROPERTY_STRING, oldValue, mode)
    }

    private inner class EscapeKeyAdapter : KeyAdapter() {
        override fun keyPressed(e: KeyEvent) {
            if (e.keyChar == KeyEvent.VK_ESCAPE.toChar()) {
                clearSearchField()
            }
        }
    }

    inner class SearchHistoryButton(mode: SearchControlFieldMode?) : JButton(FlatSearchWithHistoryIcon(true)) {
        private val applicationConfiguration = ApplicationConfiguration.getInstance()
        private val luceneSearch = mode == SearchControlFieldMode.LUCENE
        private val history = SearchHistoryModel(loadHistory(), ::saveHistory)
        private val historyList = history.entries
        private val miClearHistory = JMenuItem("Alles löschen")
        private val miEditHistory = JMenuItem("Einträge bearbeiten")

        init {
            toolTipText = "Vorherige Suchen"

            miClearHistory.addActionListener { history.clear() }

            miEditHistory.addActionListener {
                val dialog = EditHistoryDialog(host.ownerWindow(), miEditHistory, historyList)
                dialog.isVisible = true
            }

            addActionListener { showHistoryPopup() }
        }

        fun addHistoryEntry(text: String) {
            history.addMostRecent(text)
        }

        private fun showHistoryPopup() {
            val popupMenu = JPopupMenu()
            popupMenu.add(miClearHistory)
            popupMenu.add(miEditHistory)
            historyList.withReadLock {
                if (!historyList.isEmpty()) {
                    popupMenu.addSeparator()
                    for (item in historyList) {
                        val historyItem = JMenuItem(item)
                        historyItem.addActionListener {
                            this@SearchField.text = item
                            this@SearchField.fireActionPerformed()
                        }
                        popupMenu.add(historyItem)
                    }
                }
            }
            popupMenu.show(this, 0, height)
        }

        private fun loadHistory(): List<String> =
            try {
                readHistoryEntries()
            } catch (ex: Exception) {
                logger.error("Failed to load search history", ex)
                emptyList()
            }

        private fun saveHistory(entries: List<String>) {
            try {
                val json = JsonStringUtils.toJsonStringArray(entries)
                applicationConfiguration.setSearchHistoryItems(luceneSearch, json)
            } catch (ex: Exception) {
                logger.error("Failed to write search history", ex)
            }
        }

        private fun readHistoryEntries(): List<String> {
            return when (val rawValue = applicationConfiguration.getSearchHistoryItems(luceneSearch)) {
                null -> emptyList()
                is Collection<*> -> {
                    val entries = rawValue.mapNotNull { it?.toString() }
                    applicationConfiguration.setSearchHistoryItems(
                        luceneSearch,
                        JsonStringUtils.toJsonStringArray(entries),
                    )
                    entries
                }
                is String -> {
                    val parsed = parseLegacyJsonArray(rawValue)
                    if (parsed.isNotEmpty() || rawValue.trim() == "[]") {
                        ArrayList(parsed)
                    } else if (rawValue.isNotBlank()) {
                        listOf(rawValue)
                    } else {
                        emptyList()
                    }
                }
                else -> emptyList()
            }
        }

        private fun parseLegacyJsonArray(json: String?): List<String> {
            val trimmed = json?.trim().orEmpty()
            if (!trimmed.startsWith("[") || !trimmed.endsWith("]")) {
                return emptyList()
            }

            val entries = ArrayList<String>()
            var index = 1
            while (index < trimmed.length - 1) {
                if (trimmed[index] == '"') {
                    val parsed = JsonStringUtils.parseQuotedJsonString(trimmed, index)
                    if (parsed != null) {
                        entries.add(parsed.value)
                        index = parsed.endIndex
                    }
                }
                index++
            }
            return entries
        }
    }

    private companion object {
        private val logger = LogManager.getLogger()
    }
}

class LuceneSearchField(host: Host) : SearchField(host) {
    private val luceneSearchHistoryButton = SearchHistoryButton(SearchControlFieldMode.LUCENE)

    init {
        maximumSize = LUCENE_DEFAULT_DIMENSION
        setSearchMode(SearchControlFieldMode.LUCENE)

        putClientProperty(FlatClientProperties.PLACEHOLDER_TEXT, "Lucene Search Query")
        putClientProperty(FlatClientProperties.TEXT_FIELD_LEADING_COMPONENT, luceneSearchHistoryButton)
    }

    override fun createTrailingComponents() {
        val searchToolbar = JToolBar()
        searchToolbar.addSeparator()

        val luceneButton = JButton(host.showLuceneTutorialAction)
        luceneButton.text = null
        searchToolbar.add(luceneButton)
        putClientProperty(FlatClientProperties.TEXT_FIELD_TRAILING_COMPONENT, searchToolbar)
    }

    override fun performSearch() {
        val searchText = text
        if (searchText.isNotEmpty()) {
            luceneSearchHistoryButton.addHistoryEntry(searchText)
        }

        host.loadTable(true)
    }
}

class RegularSearchField(host: Host) : SearchField(host) {
    private val regularSearchHistoryButton = SearchHistoryButton(null)

    init {
        addSearchModeChangeListener { setupHelperTexts() }
        setupPlaceholderText()

        putClientProperty(FlatClientProperties.TEXT_FIELD_LEADING_COMPONENT, regularSearchHistoryButton)

        installDocumentListener()
    }

    private fun setupPlaceholderText() {
        val searchThroughDescription = ApplicationConfiguration.getInstance().searchUseFilmDescriptions
        if (searchThroughDescription) {
            setSearchMode(SearchControlFieldMode.IRGENDWO)
        } else {
            setSearchMode(SearchControlFieldMode.THEMA_TITEL)
        }
    }

    override fun performSearch() {
        val searchText = text
        if (searchText.isNotEmpty()) {
            regularSearchHistoryButton.addHistoryEntry(searchText)
        }

        host.loadTable(true)
    }

    private fun installDocumentListener() {
        document.addDocumentListener(object : DocumentListener {
            override fun insertUpdate(e: DocumentEvent) = doCheck()

            override fun removeUpdate(e: DocumentEvent) = doCheck()

            override fun changedUpdate(e: DocumentEvent) = doCheck()

            private fun doCheck() {
                val searchText = text
                checkPatternValidity(searchText)
                setForegroundTextColor(searchText)
            }
        })
    }

    private fun setForegroundTextColor(text: String) {
        foreground = if (Filter.isPattern(text)) {
            MVColor.getRegExPatternColor()
        } else {
            UIManager.getColor("TextField.foreground")
        }
    }

    private fun isPatternValid(text: String): Boolean = Filter.makePatternNoCache(text) != null

    private fun checkPatternValidity(text: String) {
        if (Filter.isPattern(text)) {
            GuiFunktionen.showErrorIndication(this, !isPatternValid(text))
        } else {
            GuiFunktionen.showErrorIndication(this, false)
        }
    }

    private fun setupHelperTexts() {
        val text = when (getSearchMode()) {
            SearchControlFieldMode.IRGENDWO -> "Thema/Titel/Beschreibung"
            SearchControlFieldMode.THEMA_TITEL -> "Thema/Titel"
            SearchControlFieldMode.LUCENE -> "Lucene Query"
        }
        putClientProperty(FlatClientProperties.PLACEHOLDER_TEXT, text)

        toolTipText = if (getSearchMode() == SearchControlFieldMode.IRGENDWO ||
            getSearchMode() == SearchControlFieldMode.THEMA_TITEL
        ) {
            "$text durchsuchen"
        } else {
            "Lucene Query Syntax für die Suche"
        }
    }

    override fun createTrailingComponents() {
        val searchToolbar = JToolBar()
        searchToolbar.addSeparator()
        searchToolbar.add(ToggleSearchFieldToggleButton())
        putClientProperty(FlatClientProperties.TEXT_FIELD_TRAILING_COMPONENT, searchToolbar)
    }

    private inner class ToggleSearchFieldToggleButton : JToggleButton() {
        init {
            val selectedIcon = SVGIconUtilities.createSVGIcon("icons/fontawesome/envelope-open-text.svg")
            selectedIcon.colorFilter = FlatSVGIcon.ColorFilter { MVColor.SELECTED_COLOR.color }
            val normalIcon = SVGIconUtilities.createSVGIcon("icons/fontawesome/envelope-open-text.svg")
            normalIcon.colorFilter = FlatSVGIcon.ColorFilter { Color.GRAY }
            icon = normalIcon
            this.selectedIcon = selectedIcon

            val searchThroughDescription = ApplicationConfiguration.getInstance().searchUseFilmDescriptions
            isSelected = searchThroughDescription
            setupToolTip(searchThroughDescription)

            addActionListener {
                when (getSearchMode()) {
                    SearchControlFieldMode.IRGENDWO -> {
                        setSearchMode(SearchControlFieldMode.THEMA_TITEL)
                        setupToolTip(false)
                    }
                    SearchControlFieldMode.THEMA_TITEL -> {
                        setSearchMode(SearchControlFieldMode.IRGENDWO)
                        setupToolTip(true)
                    }
                    SearchControlFieldMode.LUCENE -> Unit
                }

                ApplicationConfiguration.getInstance().searchUseFilmDescriptions =
                    getSearchMode() == SearchControlFieldMode.IRGENDWO

                host.loadTable()
            }
        }

        private fun setupToolTip(active: Boolean) {
            toolTipText = if (active) {
                "Suche in Beschreibung aktiviert"
            } else {
                "Suche in Beschreibung deaktiviert"
            }
        }
    }
}
