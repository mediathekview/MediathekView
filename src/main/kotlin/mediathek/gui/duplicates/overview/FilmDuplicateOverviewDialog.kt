/*
 * Created by JFormDesigner on Wed Oct 23 21:39:11 CEST 2024
 */

package mediathek.gui.duplicates.overview

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.EventList
import ca.odell.glazedlists.swing.GlazedListsSwing
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.Job
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.cancel
import kotlinx.coroutines.launch
import kotlinx.coroutines.swing.Swing
import kotlinx.coroutines.withContext
import mediathek.config.Daten
import mediathek.daten.DatenFilm
import mediathek.gui.duplicates.details.DuplicateFilmDetailsTableFormat
import mediathek.tool.EscapeKeyHandler
import java.awt.Window
import javax.swing.ToolTipManager
import javax.swing.tree.DefaultMutableTreeNode
import javax.swing.tree.DefaultTreeModel
import javax.swing.tree.TreeSelectionModel

class FilmDuplicateOverviewDialog(owner: Window) : FilmDuplicateOverviewDialogBase(owner) {
    private val dialogScope = CoroutineScope(SupervisorJob() + Dispatchers.Swing)
    private val filmList: EventList<DatenFilm> = BasicEventList()
    private var selectionJob: Job? = null

    init {
        EscapeKeyHandler.installHandler(this, this::dispose)
        okButton.addActionListener { dispose() }
        setupTable()
        setupTree()
        loadDuplicateTree()
    }

    override fun dispose() {
        dialogScope.cancel()
        super.dispose()
    }

    private fun setupTable() {
        table.model = GlazedListsSwing.eventTableModelWithThreadProxyList(
            filmList,
            DuplicateFilmDetailsTableFormat(),
        )
        resetColumnWidths()
    }

    private fun setupTree() {
        ToolTipManager.sharedInstance().registerComponent(tree)
        tree.selectionModel.selectionMode = TreeSelectionModel.SINGLE_TREE_SELECTION
        tree.cellRenderer = CustomTreeCellRenderer()
        tree.addTreeSelectionListener { updateSelectedDuplicateFilms() }
    }

    private fun loadDuplicateTree() {
        dialogScope.launch {
            val rootNode = withContext(Dispatchers.Default) {
                createDuplicateRootNode(Daten.getInstance().listeFilme.snapshot())
            }
            tree.model = DefaultTreeModel(rootNode)
        }
    }

    private fun createDuplicateRootNode(films: List<DatenFilm>): DefaultMutableTreeNode {
        val rootNode = DefaultMutableTreeNode(ROOT_NODE_LABEL, true)
        val duplicateFilmsBySender = films.asSequence()
            .filter { film -> film.isDuplicate }
            .groupBy { film -> film.sender }

        duplicateFilmsBySender.keys
            .sortedWith(::compareSenders)
            .forEach { sender ->
                val senderNode = DefaultMutableTreeNode(sender)
                duplicateFilmsBySender.getValue(sender)
                    .sortedBy { film -> film.title }
                    .forEach { film -> senderNode.add(DefaultMutableTreeNode(film)) }
                rootNode.add(senderNode)
            }

        return rootNode
    }

    private fun updateSelectedDuplicateFilms() {
        val node = tree.lastSelectedPathComponent as? DefaultMutableTreeNode ?: return
        val film = node.userObject as? DatenFilm
        selectionJob?.cancel()
        if (film == null) {
            filmList.clear()
            resetColumnWidths()
            return
        }

        selectionJob = dialogScope.launch {
            val matchingFilms = withContext(Dispatchers.Default) {
                findDuplicateFilms(film)
            }
            filmList.clear()
            filmList.addAll(matchingFilms)
            calculateColumnWidths()
        }
    }

    private fun findDuplicateFilms(film: DatenFilm): List<DatenFilm> {
        val normalUrl = film.urlNormalQuality
        val highQualityUrl = film.highQualityUrl
        return Daten.getInstance().listeFilme.snapshot()
            .asSequence()
            .filter { item -> !item.isLivestream }
            .filter { item ->
                item.urlNormalQuality == normalUrl &&
                    item.highQualityUrl == highQualityUrl
            }
            .toList()
    }

    private fun calculateColumnWidths() {
        table.columnModel.columns.asIterator().forEach { column ->
            var preferredWidth = column.minWidth
            val columnIndex = column.modelIndex
            val headerRenderer = column.headerRenderer ?: table.tableHeader.defaultRenderer
            val header = headerRenderer.getTableCellRendererComponent(
                table,
                column.headerValue,
                false,
                false,
                0,
                columnIndex,
            )
            val maxWidth = header.preferredSize.width

            for (row in 0 until table.rowCount) {
                val cellRenderer = table.getCellRenderer(row, columnIndex)
                val component = table.prepareRenderer(cellRenderer, row, columnIndex)
                val width = component.preferredSize.width + table.intercellSpacing.width
                preferredWidth = maxOf(preferredWidth, width)

                if (preferredWidth <= maxWidth) {
                    preferredWidth = maxWidth
                    break
                }
            }
            column.preferredWidth = preferredWidth
        }

        table.doLayout()
    }

    private fun resetColumnWidths() {
        table.columnModel.columns.asIterator()
            .forEach { column -> column.preferredWidth = DEFAULT_COLUMN_WIDTH }
        table.doLayout()
    }

    private fun compareSenders(first: String, second: String): Int {
        val firstPenalty = senderPenalty(first)
        val secondPenalty = senderPenalty(second)
        if (firstPenalty != secondPenalty) {
            return firstPenalty.compareTo(secondPenalty)
        }
        return first.compareTo(second)
    }

    private fun senderPenalty(sender: String): Int =
        if (sender in PENALIZED_SENDERS) 1 else 0

    private companion object {
        private const val ROOT_NODE_LABEL = "Filmduplikate"
        private const val DEFAULT_COLUMN_WIDTH = 90
        private val PENALIZED_SENDERS = setOf("ARD", "ZDF")
    }
}
