package mediathek.gui.dialogEinstellungen

import mediathek.config.DatenConfigurationPersistence
import mediathek.config.Konstanten
import mediathek.config.application.ApplicationConfiguration
import mediathek.daten.DatenPset
import mediathek.daten.ProgramSetRepository
import mediathek.daten.blacklist.BlacklistServices
import mediathek.filmlisten.FilmCatalog
import mediathek.filmlisten.FilmeLaden
import mediathek.gui.dialogEinstellungen.allgemein.LuceneDirectoryModePanel
import mediathek.gui.dialogEinstellungen.allgemein.PanelEinstellungen
import mediathek.gui.dialogEinstellungen.blacklist.PanelBlacklist
import mediathek.mainwindow.SettingsDialogHost
import mediathek.tool.EscapeKeyHandler
import mediathek.tool.GetIcon
import java.awt.BorderLayout
import java.awt.Component
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent
import java.util.*
import java.util.function.BiConsumer
import javax.swing.JPanel
import javax.swing.tree.DefaultMutableTreeNode
import javax.swing.tree.DefaultTreeModel
import javax.swing.tree.TreePath
import javax.swing.tree.TreeSelectionModel

class DialogEinstellungen(
    private val host: SettingsDialogHost,
    private val programSets: ProgramSetRepository,
    private val filmCatalog: FilmCatalog,
    private val filmListLoader: FilmeLaden,
    private val blacklist: BlacklistServices,
    private val configurationPersistence: DatenConfigurationPersistence,
    private val programSetExporter: BiConsumer<Array<DatenPset>, String>,
) : DialogEinstellungenBase() {
    init {
        initTree()
        restoreSizeFromConfig()

        iconImage = GetIcon.getIcon("MediathekView.png", "/mediathek/res/", 58, 58).image
        jButtonBeenden.addActionListener { beenden() }
        addWindowListener(object : WindowAdapter() {
            override fun windowClosing(e: WindowEvent) {
                beenden()
            }
        })

        EscapeKeyHandler.installHandler(this, this::beenden)
    }

    private fun restoreSizeFromConfig() {
        val state = ApplicationConfiguration.getInstance().settingsDialogState

        if (state.hasStoredSize()) {
            setSize(state.width(), state.height())
        }

        if (state.x() > 0 && state.y() > 0) {
            setLocation(state.x(), state.y())
        } else {
            val parentFrame = host.ownerFrame()
            setLocationRelativeTo(parentFrame)
        }
    }

    private fun initTree() {
        val allgemeineEinstellungen = SettingsPage(
            NAME_ALLGEMEINE_EINSTELLUNGEN,
            createPanel = { PanelEinstellungen(host) },
        )
        val notifications = SettingsPage(NAME_NOTIFICATIONS, createPanel = { PanelNotifications() })
        val erweiterteEinstellungen =
            SettingsPage(
                NAME_ALLGEMEINE_EINSTELLUNGEN_ERWEITERT,
                createPanel = { PanelEinstellungenErweitert(host.ownerFrame()) },
            )
        val geoEinstellungen = SettingsPage(
            NAME_ALLGEMEINE_EINSTELLUNGEN_GEO,
            createPanel = { PanelEinstellungenGeo(this, blacklist = blacklist) },
        )
        val colorEinstellungen = SettingsPage(
            NAME_ALLGEMEINE_EINSTELLUNGEN_COLOR,
            createPanel = { PanelEinstellungenColor(host) },
        )
        val luceneTuning = SettingsPage(
            NAME_ALLGEMEINE_EINSTELLUNGEN_LUCENE,
            createPanel = { LuceneDirectoryModePanel() },
        )

        val filmlisteLaden = SettingsPage(
            NAME_FILMLISTE_LADEN,
            createPanel = { PanelFilmlisteLaden(true, host.ownerFrame(), filmCatalog, filmListLoader) },
        )
        val blacklistSettings = SettingsPage(
            NAME_BLACKLIST,
            createPanel = { PanelBlacklist(blacklist, filmCatalog, filmListLoader, this) },
        )

        val dateinamen = SettingsPage(NAME_DATEINAME, createPanel = { PanelDateinamen() })
        val pset = SettingsPage(
            NAME_PROGRAMMSET,
            createPanel = { PanelPset(this, programSets, programSetExporter) },
        )
        val psetImport = SettingsPage(
            NAME_PROGRAMMSET_IMPORTIEREN,
            createPanel = { PanelPsetImport(programSets, programSetExporter, this) },
        )
        val download = SettingsPage(NAME_BANDWIDTH, createPanel = { PanelDownload() })

        val einstellungen = SettingsPage(
            title = NAME_EINSTELLUNGEN,
            children = listOf(
                allgemeineEinstellungen,
                notifications,
                erweiterteEinstellungen,
                geoEinstellungen,
                colorEinstellungen,
                luceneTuning,
            ),
            redirectTo = allgemeineEinstellungen,
        )
        val filme = SettingsPage(
            title = NAME_FILMLISTE,
            children = listOf(filmlisteLaden, blacklistSettings),
            redirectTo = filmlisteLaden,
        )
        val aufzeichnen = SettingsPage(
            title = NAME_AUFZEICHNEN,
            children = listOf(dateinamen, download, pset, psetImport),
            redirectTo = dateinamen,
        )
        val rootPage = SettingsPage(
            title = Konstanten.PROGRAMMNAME,
            children = listOf(einstellungen, filme, aufzeichnen),
        )
        val pageNodes = IdentityHashMap<SettingsPage, DefaultMutableTreeNode>()
        fun buildTreeNode(page: SettingsPage): DefaultMutableTreeNode =
            DefaultMutableTreeNode(page).also { node ->
                pageNodes[page] = node
                page.children.forEach { child -> node.add(buildTreeNode(child)) }
            }

        jTree1.model = DefaultTreeModel(buildTreeNode(rootPage))
        jTree1.selectionModel.selectionMode = TreeSelectionModel.SINGLE_TREE_SELECTION
        jTree1.isRootVisible = false
        jTree1.addTreeSelectionListener {
            val page = (jTree1.lastSelectedPathComponent as? DefaultMutableTreeNode)
                ?.userObject as? SettingsPage
            if (page == null) {
                showPanel(JPanel())
                return@addTreeSelectionListener
            }

            title = page.title
            page.redirectTo?.let { redirectedPage ->
                selectTreePage(pageNodes, redirectedPage)
                return@addTreeSelectionListener
            }

            showPanel(page.createPanel?.invoke() ?: JPanel())
        }

        expandRows()

        selectTreePage(pageNodes, allgemeineEinstellungen)
    }

    private fun expandRows() {
        var row = 0
        while (row < jTree1.rowCount) {
            jTree1.expandRow(row)
            row++
        }
    }

    private fun selectTreePage(
        pageNodes: Map<SettingsPage, DefaultMutableTreeNode>,
        page: SettingsPage,
    ) {
        selectTreeNode(requireNotNull(pageNodes[page]) { "Missing tree node for ${page.title}" })
    }

    private fun selectTreeNode(node: DefaultMutableTreeNode) {
        jTree1.selectionPath = TreePath(node.path)
    }

    private fun showPanel(panel: Component) {
        jPanelExtra.removeAll()
        jPanelExtra.add(panel, BorderLayout.CENTER)
        jPanelExtra.revalidate()
        jPanelExtra.repaint()
    }

    private fun storeSizeInConfig() {
        val size = size
        val location = location
        ApplicationConfiguration.getInstance()
            .setSettingsDialogBounds(location.x, location.y, size.width, size.height)
    }

    private fun beenden() {
        storeSizeInConfig()
        configurationPersistence.saveAll()
        dispose()
    }

    private companion object {
        private const val NAME_EINSTELLUNGEN = "Einstellungen"
        private const val NAME_ALLGEMEINE_EINSTELLUNGEN = "Allgemein"
        private const val NAME_NOTIFICATIONS = "Benachrichtigungen"
        private const val NAME_BANDWIDTH = "Download"
        private const val NAME_ALLGEMEINE_EINSTELLUNGEN_ERWEITERT = "Erweitert"
        private const val NAME_ALLGEMEINE_EINSTELLUNGEN_GEO = "Standort & Geoblocking"
        private const val NAME_ALLGEMEINE_EINSTELLUNGEN_COLOR = "Farben"
        private const val NAME_ALLGEMEINE_EINSTELLUNGEN_LUCENE = "Lucene-Tuning"
        private const val NAME_FILMLISTE = "Filmliste"
        private const val NAME_FILMLISTE_LADEN = "Filmliste laden"
        private const val NAME_BLACKLIST = "Blacklist"
        private const val NAME_AUFZEICHNEN = "Aufzeichnen und Abspielen"
        private const val NAME_DATEINAME = "Datei- und Pfadnamen"
        private const val NAME_PROGRAMMSET = "Set bearbeiten"
        private const val NAME_PROGRAMMSET_IMPORTIEREN = "Set importieren"
    }

    private class SettingsPage(
        val title: String,
        val createPanel: (() -> Component)? = null,
        val children: List<SettingsPage> = emptyList(),
        val redirectTo: SettingsPage? = null,
    ) {
        override fun toString(): String = title
    }
}
