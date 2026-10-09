package mediathek.gui.tabs.tab_online_search

import ca.odell.glazedlists.EventList
import ca.odell.glazedlists.SortedList
import ca.odell.glazedlists.gui.AbstractTableComparatorChooser
import ca.odell.glazedlists.swing.GlazedListsSwing
import ca.odell.glazedlists.swing.TableComparatorChooser
import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json
import mediathek.config.application.ApplicationConfiguration
import javax.swing.JTable
import javax.swing.ListSelectionModel
import javax.swing.table.TableColumn

class OnlineSearchResultTable(
    source: EventList<OnlineSearchResult>,
    private val stateStore: OnlineSearchTableStateStore = ApplicationOnlineSearchTableStateStore,
) : JTable() {
    private val sortedResults = SortedList(source, null)
    private val allColumns = mutableListOf<TableColumn>()

    init {
        model = GlazedListsSwing.eventTableModelWithThreadProxyList(
            sortedResults,
            OnlineSearchResultTableFormat(),
        )
        TableComparatorChooser.install(
            this,
            sortedResults,
            AbstractTableComparatorChooser.SINGLE_COLUMN,
        )
        autoResizeMode = AUTO_RESIZE_OFF
        setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION)
        fillsViewportHeight = true
        setDefaultColumnWidths()
        captureColumns()
        restoreColumnState()
    }

    fun selectedResult(): OnlineSearchResult? {
        val viewRow = selectedRow
        return resultAtViewRow(viewRow)
    }

    fun resultAtViewRow(viewRow: Int): OnlineSearchResult? {
        if (viewRow < 0) return null
        val modelRow = convertRowIndexToModel(viewRow)
        return sortedResults.getOrNull(modelRow)
    }

    fun selectedResults(): List<OnlineSearchResult> = selectedRows
        .asSequence()
        .map { convertRowIndexToModel(it) }
        .filter { it in sortedResults.indices }
        .map { sortedResults[it] }
        .toList()

    fun saveColumnState() {
        val state = OnlineSearchTableState(
            columns = allColumns.map { column ->
                val viewIndex = currentViewIndex(column.modelIndex)
                OnlineSearchColumnState(
                    id = column.identifier.toString(),
                    width = currentWidth(column, viewIndex),
                    position = if (viewIndex >= 0) viewIndex else column.modelIndex,
                )
            },
        )
        stateStore.write(TABLE_STATE_JSON.encodeToString(OnlineSearchTableState.serializer(), state))
    }

    private fun setDefaultColumnWidths() {
        val widths = intArrayOf(90, 220, 320, 90, 90, 360)
        for (index in 0 until minOf(widths.size, columnModel.columnCount)) {
            val column = columnModel.getColumn(index)
            column.identifier = model.getColumnName(index)
            column.preferredWidth = widths[index]
        }
    }

    private fun captureColumns() {
        for (index in 0 until columnModel.columnCount) {
            allColumns += columnModel.getColumn(index)
        }
    }

    private fun restoreColumnState() {
        val rawState = stateStore.read().takeIf(String::isNotBlank) ?: return
        val state = runCatching { TABLE_STATE_JSON.decodeFromString<OnlineSearchTableState>(rawState) }
            .getOrNull()
            ?: return
        val restoredColumns = allColumns.map { column ->
            val saved = state.columns.firstOrNull { it.id == column.identifier.toString() }
            RestoredColumnState(
                column = column,
                width = saved?.width?.takeIf { it > 0 } ?: column.preferredWidth,
                position = saved?.position ?: column.modelIndex,
            )
        }

        while (columnModel.columnCount > 0) {
            columnModel.removeColumn(columnModel.getColumn(0))
        }

        restoredColumns
            .sortedWith(compareBy<RestoredColumnState> { it.position }.thenBy { it.column.modelIndex })
            .forEach { stateColumn ->
                columnModel.addColumn(stateColumn.column)
                stateColumn.column.preferredWidth = stateColumn.width
                stateColumn.column.width = stateColumn.width
            }
    }

    private fun currentViewIndex(modelIndex: Int): Int =
        (0 until columnModel.columnCount)
            .firstOrNull { columnModel.getColumn(it).modelIndex == modelIndex }
            ?: -1

    private fun currentWidth(column: TableColumn, viewIndex: Int): Int =
        viewIndex.takeIf { it >= 0 }
            ?.let { columnModel.getColumn(it).width }
            ?: column.width.takeIf { it > 0 }
            ?: column.preferredWidth

    private data class RestoredColumnState(
        val column: TableColumn,
        val width: Int,
        val position: Int,
    )

    private companion object {
        private val TABLE_STATE_JSON = Json {
            ignoreUnknownKeys = true
            encodeDefaults = true
        }
    }
}

interface OnlineSearchTableStateStore {
    fun read(): String
    fun write(state: String)
}

private object ApplicationOnlineSearchTableStateStore : OnlineSearchTableStateStore {
    override fun read(): String = ApplicationConfiguration.getInstance().getTableColumnSettings(CONFIG_PREFIX)

    override fun write(state: String) {
        ApplicationConfiguration.getInstance().setTableColumnSettings(CONFIG_PREFIX, state)
    }

    private const val CONFIG_PREFIX = "online-search"
}

@Serializable
private data class OnlineSearchTableState(
    val columns: List<OnlineSearchColumnState> = emptyList(),
)

@Serializable
private data class OnlineSearchColumnState(
    val id: String,
    val width: Int? = null,
    val position: Int? = null,
)
