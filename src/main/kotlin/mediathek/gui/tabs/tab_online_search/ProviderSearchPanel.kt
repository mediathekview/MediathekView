package mediathek.gui.tabs.tab_online_search

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.EventList
import com.formdev.flatlaf.FlatClientProperties
import com.formdev.flatlaf.icons.FlatSearchWithHistoryIcon
import mediathek.gui.tabs.tab_film.EditHistoryDialog
import mediathek.tool.withReadLock
import mediathek.tool.withWriteLock
import net.miginfocom.swing.MigLayout
import java.awt.event.ActionListener
import java.util.function.Consumer
import javax.swing.*
import javax.swing.event.DocumentEvent
import javax.swing.event.DocumentListener
import javax.swing.text.JTextComponent

class ProviderSearchPanel : JPanel(MigLayout("insets 8", "[pref!][pref!][pref!]", "[][][][]")) {
    val queryField = OnlineSearchTextField("Suchbegriff")
    val urlField = OnlineSearchTextField("URL Filmseite")
    val searchButton = JButton("Suchen")
    val cancelButton = JButton("Abbrechen")
    val urlSearchButton = JButton("URL suchen")
    val progressBar = JProgressBar().apply {
        isIndeterminate = true
        isVisible = false
    }

    var queryText: String
        get() = queryField.text.orEmpty()
        set(value) {
            queryField.text = value
        }

    var urlText: String
        get() = urlField.text.orEmpty()
        set(value) {
            urlField.text = value
        }

    init {
        add(JLabel("Livesuche"), "span 3, wrap")
        add(queryField, "w ${SEARCH_FIELD_MAXIMUM_WIDTH}px!")
        add(searchButton)
        add(cancelButton, "wrap")
        add(JLabel("URL Filmseite"), "span 3, wrap")
        add(urlField, "w ${SEARCH_FIELD_MAXIMUM_WIDTH}px!")
        add(urlSearchButton, "span 2, wrap")
        add(progressBar, "span 3, growx")

        installDocumentListener(queryField) { updateRunningState(progressBar.isVisible) }
        installDocumentListener(urlField) { updateRunningState(progressBar.isVisible) }
        updateRunningState(running = false)
    }

    override fun doLayout() {
        super.doLayout()
        limitSearchFieldWidth(queryField)
        limitSearchFieldWidth(urlField)
    }

    private fun limitSearchFieldWidth(field: JTextField) {
        if (field.width > SEARCH_FIELD_MAXIMUM_WIDTH) {
            field.setBounds(field.x, field.y, SEARCH_FIELD_MAXIMUM_WIDTH, field.height)
        }
    }

    fun addSearchListener(listener: ActionListener) {
        searchButton.addActionListener(listener)
        queryField.addActionListener(listener)
    }

    fun addCancelListener(listener: ActionListener) = cancelButton.addActionListener(listener)

    fun addQueryClearedListener(listener: () -> Unit) {
        installDocumentListener(queryField) {
            if (queryText.trim().isEmpty()) {
                listener()
            }
        }
    }

    fun addUrlClearedListener(listener: () -> Unit) {
        installDocumentListener(urlField) {
            if (urlText.trim().isEmpty()) {
                listener()
            }
        }
    }

    fun addUrlSearchListener(listener: ActionListener) {
        urlSearchButton.addActionListener(listener)
        urlField.addActionListener(listener)
    }

    fun addQueryHistorySelectionListener(listener: (String) -> Unit) = queryField.addHistorySelectionListener(listener)

    fun addUrlHistorySelectionListener(listener: (String) -> Unit) = urlField.addHistorySelectionListener(listener)

    fun addQueryHistoryChangeListener(listener: (List<String>) -> Unit) = queryField.addHistoryChangeListener(listener)

    fun addUrlHistoryChangeListener(listener: (List<String>) -> Unit) = urlField.addHistoryChangeListener(listener)

    fun setQueryHistory(entries: List<String>) = queryField.setHistory(entries)
    fun setUrlHistory(entries: List<String>) = urlField.setHistory(entries)

    fun clearSearchFields() {
        queryText = ""
        urlText = ""
    }

    fun updateRunningState(running: Boolean) {
        progressBar.isVisible = running
        queryField.isEnabled = !running
        urlField.isEnabled = !running
        searchButton.isEnabled = !running && queryText.trim().length >= MIN_SEARCH_LENGTH
        cancelButton.isEnabled = running
        urlSearchButton.isEnabled = !running && urlText.trim().isNotEmpty()
    }

    private fun installDocumentListener(textField: JTextField, update: () -> Unit) {
        textField.document.addDocumentListener(object : DocumentListener {
            override fun insertUpdate(e: DocumentEvent?) = update()
            override fun removeUpdate(e: DocumentEvent?) = update()
            override fun changedUpdate(e: DocumentEvent?) = update()
        })
    }

    private companion object {
        private const val MIN_SEARCH_LENGTH = 3
        private const val SEARCH_FIELD_MAXIMUM_WIDTH = 500
    }
}

class OnlineSearchTextField(
    placeholderText: String,
) : JTextField("", 40) {
    private val historyEntries: EventList<String> = BasicEventList()
    private val historySelectionListeners = mutableListOf<(String) -> Unit>()
    private val historyChangeListeners = mutableListOf<(List<String>) -> Unit>()
    private val clearHistoryItem = JMenuItem("Alles löschen").apply {
        addActionListener {
            historyEntries.withWriteLock { clear() }
        }
    }
    private val editHistoryItem = JMenuItem("Einträge bearbeiten").apply {
        addActionListener { showEditHistoryDialog(this) }
    }
    private val historyButton = JButton(FlatSearchWithHistoryIcon(true)).apply {
        toolTipText = "Vorherige Suchen"
        addActionListener { showHistoryPopup() }
    }

    init {
        putClientProperty(FlatClientProperties.PLACEHOLDER_TEXT, placeholderText)
        putClientProperty(FlatClientProperties.TEXT_FIELD_SHOW_CLEAR_BUTTON, true)
        putClientProperty("JTextField.clearCallback", Consumer<JTextComponent> { clear() })
        putClientProperty(FlatClientProperties.TEXT_FIELD_LEADING_COMPONENT, historyButton)
        historyEntries.addListEventListener {
            val entries = historyEntries.withReadLock { toList() }
            historyChangeListeners.forEach { it(entries) }
        }
    }

    fun setHistory(entries: List<String>) {
        historyEntries.withWriteLock {
            clear()
            addAll(entries)
        }
    }

    fun addHistorySelectionListener(listener: (String) -> Unit) {
        historySelectionListeners.add(listener)
    }

    fun addHistoryChangeListener(listener: (List<String>) -> Unit) {
        historyChangeListeners.add(listener)
    }

    fun selectHistoryEntry(entry: String): Boolean {
        if (entry !in historyEntries) {
            return false
        }
        text = entry
        historySelectionListeners.forEach { it(entry) }
        return true
    }

    private fun clear() {
        text = ""
        fireActionPerformed()
    }

    private fun showHistoryPopup() {
        createHistoryPopup().show(historyButton, 0, historyButton.height)
    }

    internal fun createHistoryPopup(): JPopupMenu {
        val popup = JPopupMenu()
        popup.add(clearHistoryItem)
        popup.add(editHistoryItem)
        historyEntries.withReadLock {
            if (isNotEmpty()) {
                popup.addSeparator()
            }
            historyEntries.forEach { entry ->
                popup.add(JMenuItem(entry).apply {
                    addActionListener {
                        selectHistoryEntry(entry)
                    }
                })
            }
        }
        return popup
    }

    private fun showEditHistoryDialog(menuItem: JMenuItem) {
        val owner = SwingUtilities.getWindowAncestor(this) ?: JOptionPane.getRootFrame()
        EditHistoryDialog(owner, menuItem, historyEntries).isVisible = true
    }
}
