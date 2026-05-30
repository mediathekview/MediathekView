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

package mediathek.audiothek.ui.table

import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json
import mediathek.audiothek.model.AudioEntry
import mediathek.config.Konstanten
import mediathek.config.MVColor
import mediathek.controller.history.SeenHistoryController
import mediathek.tool.ApplicationConfiguration
import org.apache.logging.log4j.LogManager
import org.kordamp.ikonli.fontawesome6.FontAwesomeSolid
import org.kordamp.ikonli.swing.FontIcon
import java.awt.Component
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import java.time.LocalDate
import java.time.LocalTime
import java.time.format.DateTimeFormatter
import javax.swing.*
import javax.swing.event.ListSelectionListener
import javax.swing.event.TableColumnModelEvent
import javax.swing.event.TableColumnModelListener
import javax.swing.plaf.UIResource
import javax.swing.table.TableCellRenderer
import javax.swing.table.TableColumn

class AudiothekTable(
    private val onOpenAudio: (AudioEntry) -> Unit,
    private val onDownload: (AudioEntry) -> Unit
) : JTable(AudioTableModel()) {
    private val logger = LogManager.getLogger(AudiothekTable::class.java)
    private val audioTableModel = model as AudioTableModel
    private val seenHistoryController = SeenHistoryController().apply { prepareMemoryCache() }
    private val sorter = TriStateTableRowSorter(audioTableModel)
    private val allColumns = linkedMapOf<Int, TableColumn>()
    private val lastKnownViewIndexes = mutableMapOf<Int, Int>()
    private val centeredTextCellRenderer = CenteredTextCellRenderer()
    private val buttonHeaderRenderer = TableCellRenderer { table, _, _, _, _, column ->
        val component = table.tableHeader.defaultRenderer
            .getTableCellRendererComponent(table, "", false, false, -1, column)
        if (component is JLabel) {
            component.text = ""
            component.icon = null
            component.disabledIcon = null
            component.horizontalAlignment = SwingConstants.CENTER
        }
        component
    }
    private var luceneIndex = AudiothekLuceneIndex()
    private val customColumnWidths = mutableMapOf<Int, Int>()
    private var allEntries: List<AudioEntry> = emptyList()
    private var externalSearchEntries: List<AudioEntry> = emptyList()
    private var currentFilterQuery = ""
    private var restoringState = false

    init {
        autoCreateRowSorter = false
        rowSorter = sorter
        setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
        fillsViewportHeight = true
        rowHeight = Konstanten.TABLE_DEFAULT_ROW_HEIGHT
        setAutoResizeMode(AUTO_RESIZE_OFF)

        configureSorting()
        configureColumns()
        restoreState()
        installHeaderPopup()
        installRowPopup()
        installColumnTracking()
        installStatePersistence()
    }

    fun setRows(entries: List<AudioEntry>) {
        applyPreparedRows(prepareRows(entries, currentFilterQuery, visibleSearchFields()))
    }

    fun visibleSearchFieldsSnapshot(): List<String> = visibleSearchFields()

    fun applyPreparedRows(preparedRows: PreparedRows) {
        allEntries = preparedRows.entries
        currentFilterQuery = preparedRows.query
        externalSearchEntries = emptyList()
        luceneIndex.close()
        luceneIndex = preparedRows.luceneIndex
        audioTableModel.setRows(preparedRows.filteredEntries)
    }

    fun setExternalSearchEntries(entries: List<AudioEntry>) {
        externalSearchEntries = entries
        applyFilter(currentFilterQuery)
    }

    fun hasCurrentFilterQuery(query: String): Boolean =
        currentFilterQuery == query.trim()

    fun selectedEntry(): AudioEntry? {
        val viewRow = selectedRow
        if (viewRow < 0) {
            return null
        }
        return audioTableModel.getEntry(convertRowIndexToModel(viewRow))
    }

    fun hasEntries(): Boolean = allEntries.isNotEmpty()

    fun saveState() {
        writeState()
    }

    fun dispose() {
        luceneIndex.close()
        seenHistoryController.close()
    }

    fun addEntrySelectionListener(listener: ListSelectionListener) {
        selectionModel.addListSelectionListener(listener)
    }

    fun applyFilter(query: String) {
        currentFilterQuery = query
        val normalized = query.trim()
        if (normalized.isEmpty()) {
            audioTableModel.setRows(allEntries)
            return
        }
        val localResults = luceneIndex.search(normalized, visibleSearchFields())
        audioTableModel.setRows(mergeSearchResults(localResults, externalSearchEntries))
    }

    fun selectFirstRow() {
        if (rowCount > 0) {
            setRowSelectionInterval(0, 0)
        }
    }

    fun refreshSeenState() {
        repaint()
    }

    override fun prepareRenderer(renderer: TableCellRenderer, row: Int, column: Int): Component {
        val component = super.prepareRenderer(renderer, row, column)
        if (!isRowSelected(row)) {
            component.background = defaultRowBackground(row)
            val entry = audioTableModel.getEntry(convertRowIndexToModel(row))
            if (entry != null && seenHistoryController.hasBeenSeen(entry)) {
                component.background = MVColor.FILM_HISTORY.color
            }
        }
        return component
    }

    private fun defaultRowBackground(row: Int): java.awt.Color {
        if (row % 2 != 0) {
            UIManager.getColor("Table.alternateRowColor")?.let { return it }
        }
        return background.takeUnless { it is UIResource }
            ?: UIManager.getColor("Table.background")
            ?: background
    }

    private fun mergeSearchResults(localResults: List<AudioEntry>, externalResults: List<AudioEntry>): List<AudioEntry> {
        if (externalResults.isEmpty()) {
            return localResults
        }

        return buildList(localResults.size + externalResults.size) {
            val seenKeys = hashSetOf<String>()
            (localResults + externalResults).forEach { entry ->
                if (seenKeys.add(entryKey(entry))) {
                    add(entry)
                }
            }
        }
    }

    private fun entryKey(entry: AudioEntry): String {
        return listOf(
            entry.channel,
            entry.title,
            entry.audioUrl?.toString().orEmpty(),
            entry.websiteUrl?.toString().orEmpty(),
            entry.publishedAt?.toString().orEmpty()
        ).joinToString("\u0000")
    }

    private fun configureSorting() {
        sorter.setSortable(AudioTableColumn.PLAY.modelIndex, false)
        sorter.setSortable(AudioTableColumn.DOWNLOAD.modelIndex, false)
        sorter.setComparator(AudioTableColumn.DATE.modelIndex) { left, right ->
            compareNullable(parseDate(left), parseDate(right))
        }
        sorter.setComparator(AudioTableColumn.TIME.modelIndex) { left, right ->
            compareNullable(parseTime(left), parseTime(right))
        }
        sorter.setComparator(AudioTableColumn.DURATION.modelIndex) { left, right ->
            compareNullable(parseInt(left), parseInt(right))
        }
        sorter.setComparator(AudioTableColumn.SIZE.modelIndex) { left, right ->
            compareNullable(parseInt(left), parseInt(right))
        }
    }

    private fun configureColumns() {
        for (index in 0 until columnModel.columnCount) {
            val column = columnModel.getColumn(index)
            column.identifier = AudioTableColumn.entries[column.modelIndex].name
            allColumns[column.modelIndex] = column
            val definition = AudioTableColumn.entries[column.modelIndex]
            if (definition.toggleable) {
                lastKnownViewIndexes[column.modelIndex] = index
            }
        }

        AudioTableColumn.entries.forEach { definition ->
            val column = columnModel.getColumn(definition.modelIndex)
            if (definition.centered) {
                column.cellRenderer = centeredTextCellRenderer
            }
            applyDataColumnWidth(column)
        }

        configureButtonColumn(AudioTableColumn.PLAY, FontIcon.of(FontAwesomeSolid.PLAY, 14)) { modelRow ->
            audioTableModel.getEntry(modelRow)?.let(onOpenAudio)
        }
        configureButtonColumn(AudioTableColumn.DOWNLOAD, FontIcon.of(FontAwesomeSolid.CLOUD_DOWNLOAD_ALT, 14)) { modelRow ->
            audioTableModel.getEntry(modelRow)?.let(onDownload)
        }
    }

    private fun configureButtonColumn(columnDefinition: AudioTableColumn, icon: Icon? = null, onClick: (Int) -> Unit) {
        val buttonColumn = TableButtonColumn(columnDefinition.name, icon, onClick)
        val column = columnModel.getColumn(columnDefinition.modelIndex)
        column.cellRenderer = buttonColumn
        column.cellEditor = buttonColumn
        column.headerRenderer = buttonHeaderRenderer
        val width = columnDefinition.preferredWidth ?: if (icon == null) 52 else 32
        applyFixedWidth(column, width)
    }

    private fun applyDataColumnWidth(column: TableColumn) {
        val width = customColumnWidths[column.modelIndex]
            ?: AudioTableColumn.entries[column.modelIndex].preferredWidth
            ?: return
        column.preferredWidth = width
    }

    private fun applyFixedWidth(column: TableColumn, width: Int) {
        column.minWidth = width
        column.maxWidth = width
        column.preferredWidth = width
    }

    private fun installHeaderPopup() {
        tableHeader.addMouseListener(object : MouseAdapter() {
            override fun mousePressed(event: MouseEvent) = maybeShowHeaderPopup(event)
            override fun mouseReleased(event: MouseEvent) = maybeShowHeaderPopup(event)
        })
    }

    private fun installRowPopup() {
        addMouseListener(object : MouseAdapter() {
            override fun mouseClicked(event: MouseEvent) = maybeOpenRow(event)
            override fun mousePressed(event: MouseEvent) = maybeShowRowPopup(event)
            override fun mouseReleased(event: MouseEvent) = maybeShowRowPopup(event)
        })
    }

    private fun installColumnTracking() {
        columnModel.addColumnModelListener(object : TableColumnModelListener {
            override fun columnAdded(e: TableColumnModelEvent?) = Unit
            override fun columnRemoved(e: TableColumnModelEvent?) = Unit
            override fun columnSelectionChanged(e: javax.swing.event.ListSelectionEvent?) = Unit
            override fun columnMarginChanged(e: javax.swing.event.ChangeEvent?) {
                syncVisibleColumnWidths()
                writeState()
            }

            override fun columnMoved(e: TableColumnModelEvent?) {
                if (e != null && e.fromIndex != e.toIndex) {
                    updateVisibleColumnPositions()
                    writeState()
                }
            }
        })
    }

    private fun installStatePersistence() {
        rowSorter.addRowSorterListener {
            writeState()
        }
    }

    private fun maybeShowHeaderPopup(event: MouseEvent) {
        if (!event.isPopupTrigger) {
            return
        }

        val popup = JPopupMenu()
        for (definition in AudioTableColumn.searchableColumns) {
            val modelIndex = definition.modelIndex
            val item = JCheckBoxMenuItem(definition.title, isColumnVisible(modelIndex))
            item.addActionListener { toggleColumn(modelIndex, item.isSelected) }
            popup.add(item)
        }
        popup.show(event.component, event.x, event.y)
    }

    private fun maybeShowRowPopup(event: MouseEvent) {
        if (!event.isPopupTrigger) {
            return
        }

        val viewRow = rowAtPoint(event.point)
        if (viewRow < 0) {
            return
        }

        setRowSelectionInterval(viewRow, viewRow)
        val modelRow = convertRowIndexToModel(viewRow)
        val entry = audioTableModel.getEntry(modelRow) ?: return

        JPopupMenu().apply {
            add(createRowActionItem("Abspielen", FontAwesomeSolid.PLAY, entry, onOpenAudio))
            add(createRowActionItem("Download", FontAwesomeSolid.CLOUD_DOWNLOAD_ALT, entry, onDownload))
            addSeparator()
            add(createSeenHistoryMenuItem(entry))
            show(event.component, event.x, event.y)
        }
    }

    private fun maybeOpenRow(event: MouseEvent) {
        if (!SwingUtilities.isLeftMouseButton(event) || event.clickCount != 2) {
            return
        }

        val viewRow = rowAtPoint(event.point)
        if (viewRow < 0) {
            return
        }

        setRowSelectionInterval(viewRow, viewRow)
        selectedEntry()?.let(onOpenAudio)
    }

    private fun createRowActionItem(
        title: String,
        iconLiteral: FontAwesomeSolid,
        entry: AudioEntry,
        action: (AudioEntry) -> Unit
    ): JMenuItem {
        return JMenuItem(title, FontIcon.of(iconLiteral, 14)).apply {
            addActionListener { action(entry) }
        }
    }

    private fun createSeenHistoryMenuItem(entry: AudioEntry): JMenuItem {
        val hasBeenSeen = seenHistoryController.hasBeenSeen(entry)
        val title = if (hasBeenSeen) "Als ungesehen markieren" else "Als gesehen markieren"
        val icon = if (hasBeenSeen) FontAwesomeSolid.UNDO else FontAwesomeSolid.CHECK
        return JMenuItem(title, FontIcon.of(icon, 14)).apply {
            addActionListener {
                if (hasBeenSeen) {
                    seenHistoryController.markUnseen(entry)
                } else {
                    seenHistoryController.markSeen(entry)
                }
                refreshSeenState()
            }
        }
    }

    private fun isColumnVisible(modelIndex: Int): Boolean {
        return (0 until columnModel.columnCount).any { columnModel.getColumn(it).modelIndex == modelIndex }
    }

    private fun toggleColumn(modelIndex: Int, visible: Boolean) {
        val definition = AudioTableColumn.entries.getOrNull(modelIndex) ?: return
        if (!definition.toggleable) {
            return
        }
        val currentlyVisible = isColumnVisible(modelIndex)
        if (visible == currentlyVisible) {
            return
        }

        if (visible) {
            val column = allColumns[modelIndex] ?: return
            columnModel.addColumn(column)
            applyDataColumnWidth(column)
            val newViewIndex = columnModel.columnCount - 1
            val targetViewIndex = lastKnownViewIndexes[modelIndex]
                ?.coerceIn(0, newViewIndex)
                ?: newViewIndex
            if (targetViewIndex in 0..newViewIndex && targetViewIndex != newViewIndex) {
                columnModel.moveColumn(newViewIndex, targetViewIndex)
            }
        } else {
            val viewIndex = (0 until columnModel.columnCount)
                .firstOrNull { columnModel.getColumn(it).modelIndex == modelIndex } ?: return
            lastKnownViewIndexes[modelIndex] = viewIndex
            columnModel.removeColumn(columnModel.getColumn(viewIndex))
        }
        updateVisibleColumnPositions()
        applyFilter(currentFilterQuery)
        writeState()
    }

    private fun updateVisibleColumnPositions() {
        for (viewIndex in 0 until columnModel.columnCount) {
            val modelIndex = columnModel.getColumn(viewIndex).modelIndex
            if (AudioTableColumn.entries[modelIndex].toggleable) {
                lastKnownViewIndexes[modelIndex] = viewIndex
            }
        }
    }

    private fun syncVisibleColumnWidths() {
        for (viewIndex in 0 until columnModel.columnCount) {
            val column = columnModel.getColumn(viewIndex)
            val definition = AudioTableColumn.entries[column.modelIndex]
            if (definition.isFixedWidth()) {
                continue
            }
            customColumnWidths[definition.modelIndex] = column.width
        }
    }

    private fun restoreState() {
        val rawState = ApplicationConfiguration.getConfiguration()
            .getString(ApplicationConfiguration.APPLICATION_UI_AUDIOTHEK_TABLE_STATE, "")
            .takeIf(String::isNotBlank)
            ?: return

        val state = runCatching { TABLE_STATE_JSON.decodeFromString<AudiothekTableState>(rawState) }
            .onFailure { logger.warn("Failed to restore Audiothek table state", it) }
            .getOrNull()
            ?: return

        restoringState = true
        try {
            applyState(state)
        } finally {
            restoringState = false
        }
    }

    private fun applyState(state: AudiothekTableState) {
        val mergedColumns = AudioTableColumn.entries.associateWith { definition ->
            val saved = state.columns.firstOrNull { it.id == definition.name }
            RestoredColumnState(
                definition = definition,
                visible = if (definition.toggleable) saved?.visible ?: true else true,
                width = saved?.width?.takeIf { it > 0 } ?: defaultWidth(definition),
                position = saved?.position ?: definition.modelIndex
            )
        }

        while (columnModel.columnCount > 0) {
            columnModel.removeColumn(columnModel.getColumn(0))
        }

        mergedColumns.values
            .filter { it.visible || !it.definition.toggleable }
            .sortedWith(compareBy<RestoredColumnState> { it.position }.thenBy { it.definition.modelIndex })
            .forEach { stateColumn ->
                val definition = stateColumn.definition
                val column = allColumns[definition.modelIndex] ?: return@forEach
                columnModel.addColumn(column)
                applyRestoredWidth(column, definition, stateColumn.width)
            }

        updateVisibleColumnPositions()
        restoreSort(state.sort)
    }

    private fun restoreSort(sortState: AudiothekSortState?) {
        val sortDefinition = AudioTableColumn.entries.firstOrNull { it.name == sortState?.id }
            ?: run {
                sorter.sortKeys = emptyList()
                return
            }
        if (!isColumnVisible(sortDefinition.modelIndex) || sortDefinition.isFixedWidth()) {
            sorter.sortKeys = emptyList()
            return
        }

        val sortOrder = sortState?.order
            ?.let { runCatching { SortOrder.valueOf(it) }.getOrNull() }
            ?.takeIf { it == SortOrder.ASCENDING || it == SortOrder.DESCENDING }
            ?: return

        sorter.sortKeys = listOf(RowSorter.SortKey(sortDefinition.modelIndex, sortOrder))
    }

    private fun applyRestoredWidth(column: TableColumn, definition: AudioTableColumn, width: Int) {
        if (definition.isFixedWidth()) {
            applyFixedWidth(column, defaultWidth(definition))
            return
        }
        customColumnWidths[definition.modelIndex] = width
        applyDataColumnWidth(column)
    }

    private fun defaultWidth(definition: AudioTableColumn): Int {
        return definition.preferredWidth ?: allColumns[definition.modelIndex]?.preferredWidth ?: 75
    }

    private fun writeState() {
        if (restoringState) {
            return
        }

        syncVisibleColumnWidths()
        val state = AudiothekTableState(
            version = TABLE_STATE_VERSION,
            columns = AudioTableColumn.entries.map { definition ->
                val viewIndex = currentViewIndex(definition.modelIndex)
                RestoredColumnState(
                    definition = definition,
                    visible = viewIndex >= 0,
                    width = currentWidth(definition),
                    position = if (viewIndex >= 0) viewIndex else lastKnownViewIndexes[definition.modelIndex] ?: definition.modelIndex
                ).toSerializable()
            },
            sort = currentSortState()
        )

        runCatching {
            ApplicationConfiguration.getConfiguration().setProperty(
                ApplicationConfiguration.APPLICATION_UI_AUDIOTHEK_TABLE_STATE,
                TABLE_STATE_JSON.encodeToString(AudiothekTableState.serializer(), state)
            )
        }.onFailure {
            logger.warn("Failed to save Audiothek table state", it)
        }
    }

    private fun currentSortState(): AudiothekSortState? {
        val sortKey = sorter.sortKeys.firstOrNull() ?: return null
        val definition = AudioTableColumn.entries.getOrNull(sortKey.column) ?: return null
        if (definition.isFixedWidth()) {
            return null
        }
        val order = sortKey.sortOrder
        if (order != SortOrder.ASCENDING && order != SortOrder.DESCENDING) {
            return null
        }
        return AudiothekSortState(definition.name, order.name)
    }

    private fun currentWidth(definition: AudioTableColumn): Int {
        val visibleWidth = currentViewIndex(definition.modelIndex)
            .takeIf { it >= 0 }
            ?.let { columnModel.getColumn(it).width }
        return visibleWidth
            ?: customColumnWidths[definition.modelIndex]
            ?: allColumns[definition.modelIndex]?.width
            ?: defaultWidth(definition)
    }

    private fun currentViewIndex(modelIndex: Int): Int {
        return (0 until columnModel.columnCount)
            .firstOrNull { columnModel.getColumn(it).modelIndex == modelIndex }
            ?: -1
    }

    private fun visibleSearchFields(): List<String> {
        return (0 until columnModel.columnCount)
            .map { columnModel.getColumn(it).modelIndex }
            .mapNotNull { AudioTableColumn.entries[it].searchField }
    }

    private fun parseDate(value: Any?): LocalDate? =
        value?.toString()
            ?.takeIf(String::isNotBlank)
            ?.let { LocalDate.parse(it, DATE_FORMAT) }

    private fun parseTime(value: Any?): LocalTime? =
        value?.toString()
            ?.takeIf(String::isNotBlank)
            ?.let { LocalTime.parse(it, TIME_FORMAT) }

    private fun parseInt(value: Any?): Int? =
        value?.toString()
            ?.takeIf(String::isNotBlank)
            ?.toIntOrNull()

    private fun <T : Comparable<T>> compareNullable(left: T?, right: T?): Int = when {
        left == null && right == null -> 0
        left == null -> 1
        right == null -> -1
        else -> left.compareTo(right)
    }

    private fun AudioTableColumn.isFixedWidth(): Boolean {
        return this == AudioTableColumn.PLAY || this == AudioTableColumn.DOWNLOAD
    }

    companion object {
        private const val TABLE_STATE_VERSION = 1
        private val DATE_FORMAT: DateTimeFormatter = DateTimeFormatter.ofPattern("dd.MM.yyyy")
        private val TIME_FORMAT: DateTimeFormatter = DateTimeFormatter.ofPattern("HH:mm")
        private val TABLE_STATE_JSON = Json {
            ignoreUnknownKeys = true
            encodeDefaults = true
        }

        fun prepareRows(
            entries: List<AudioEntry>,
            query: String,
            visibleSearchFields: List<String>,
        ): PreparedRows {
            val luceneIndex = AudiothekLuceneIndex.build(entries)
            val filteredEntries = filterEntries(
                luceneIndex = luceneIndex,
                entries = entries,
                query = query,
                visibleSearchFields = visibleSearchFields,
            )
            return PreparedRows(
                entries = entries,
                query = query,
                luceneIndex = luceneIndex,
                filteredEntries = filteredEntries,
            )
        }

        private fun filterEntries(
            luceneIndex: AudiothekLuceneIndex,
            entries: List<AudioEntry>,
            query: String,
            visibleSearchFields: List<String>,
        ): List<AudioEntry> {
            val normalized = query.trim()
            return if (normalized.isEmpty()) {
                entries
            } else {
                luceneIndex.search(normalized, visibleSearchFields)
            }
        }
    }

    data class PreparedRows(
        val entries: List<AudioEntry>,
        val query: String,
        val luceneIndex: AudiothekLuceneIndex,
        val filteredEntries: List<AudioEntry>,
    )

    @Serializable
    private data class AudiothekTableState(
        val version: Int = TABLE_STATE_VERSION,
        val columns: List<AudiothekColumnState> = emptyList(),
        val sort: AudiothekSortState? = null
    )

    @Serializable
    private data class AudiothekColumnState(
        val id: String,
        val visible: Boolean = true,
        val width: Int? = null,
        val position: Int? = null
    )

    @Serializable
    private data class AudiothekSortState(
        val id: String,
        val order: String? = null
    )

    private data class RestoredColumnState(
        val definition: AudioTableColumn,
        val visible: Boolean,
        val width: Int,
        val position: Int
    ) {
        fun toSerializable(): AudiothekColumnState {
            return AudiothekColumnState(
                id = definition.name,
                visible = visible,
                width = width,
                position = position
            )
        }
    }
}
