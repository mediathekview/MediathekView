package mediathek.mainwindow

import mediathek.config.CommandLineOptions
import mediathek.config.Daten
import mediathek.gui.actions.*
import mediathek.gui.actions.export.ExportDecompressedFilmlistAction
import mediathek.gui.actions.export.ExportReadableFilmlistAction
import mediathek.gui.actions.import_actions.ImportOldAbosAction
import mediathek.gui.actions.import_actions.ImportOldBlacklistAction
import mediathek.gui.actions.import_actions.ImportOldReplacementListAction
import mediathek.gui.duplicates.overview.FilmDuplicateOverviewDialog
import mediathek.gui.history.ResetAboHistoryAction
import mediathek.gui.history.ResetDownloadHistoryAction
import mediathek.gui.tabs.tab_downloads.GuiDownloads
import mediathek.gui.tabs.tab_film.GuiFilme
import mediathek.logging.LogDialog
import mediathek.sqlite.RecoverHistoryDbAction
import mediathek.tool.GuiFunktionen
import java.util.function.Supplier
import javax.swing.Action
import javax.swing.JMenu
import javax.swing.JMenuBar
import javax.swing.JMenuItem

class MainWindowMenuBuilder(
    private val owner: MediathekGui,
    private val daten: Daten,
    private val menuBar: JMenuBar,
    private val fileMenu: JMenu,
    private val filmMenu: JMenu,
    private val downloadMenu: JMenu,
    private val aboMenu: JMenu,
    private val fontMenu: JMenu,
    private val viewMenu: JMenu,
    private val helpMenu: JMenu,
    private val menuPolicy: MainWindowMenuPolicy,
    private val tabRegistry: MainWindowTabRegistry,
    private val filmTab: Supplier<GuiFilme>,
    private val downloadsTab: Supplier<GuiDownloads>,
    private val logDialog: LogDialog,
    private val loadFilmListAction: Action,
    private val settingsAction: Action,
    private val showMemoryMonitorAction: Action,
    private val manageAboAction: Action,
    private val showBandwidthUsageAction: Action,
    private val showLuceneTutorialAction: Action,
    private val showFilmInformationAction: Action,
    private val manageBookmarkAction: Action,
    private val searchProgramUpdateAction: Action,
) {
    fun createMenuBar(): JMenuBar {
        configureMenu(fileMenu, 'd', "Datei")
        menuBar.add(fileMenu)

        configureMenu(filmMenu, 'F', "Filme")
        menuBar.add(filmMenu)

        configureMenu(downloadMenu, 'O', "Downloads")
        menuBar.add(downloadMenu)

        configureMenu(aboMenu, 'b', "Abos")
        menuBar.add(aboMenu)

        if (menuPolicy.supportsFontMenu) {
            menuBar.add(fontMenu)
        }

        configureMenu(viewMenu, 'a', "Ansicht")
        menuBar.add(viewMenu)

        configureMenu(helpMenu, 'h', "Hilfe")
        menuBar.add(helpMenu)

        return menuBar
    }

    fun initializeMenus() {
        createFileMenu()
        filmTab.get().installMenuEntries(filmMenu)
        downloadsTab.get().installMenuEntries(downloadMenu)

        createFontMenu()
        createViewMenu()

        createAboMenu()
        if (CommandLineOptions.isDebugModeEnabled()) {
            createDeveloperMenu()
        }
        createHelpMenu()
    }

    private fun configureMenu(menu: JMenu, mnemonic: Char, text: String) {
        menu.mnemonic = mnemonic.code
        menu.text = text
    }

    private fun createFileMenu() {
        fileMenu.add(loadFilmListAction)
        fileMenu.addSeparator()

        val exportMenu = JMenu("Export")
        exportMenu.add(ExportReadableFilmlistAction(owner))
        exportMenu.add(ExportDecompressedFilmlistAction(owner))

        val importMenu = JMenu("Import")
        importMenu.add(ImportOldAbosAction(owner))
        importMenu.add(ImportOldBlacklistAction(owner))
        importMenu.add(ImportOldReplacementListAction(owner))

        fileMenu.add(exportMenu)
        fileMenu.add(importMenu)

        menuPolicy.addSettingsItem(fileMenu, settingsAction)
        menuPolicy.addQuitItem(fileMenu, owner)
    }

    private fun createViewMenu() {
        val filmTab = filmTab.get()
        filmTab.installViewMenuEntry(viewMenu)
        tabRegistry.installViewMenuEntries(viewMenu)
        viewMenu.addSeparator()
        viewMenu.add(showMemoryMonitorAction)
        viewMenu.add(showBandwidthUsageAction)
        viewMenu.addSeparator()
        viewMenu.add(ShowFilmStatisticsAction(owner))
        viewMenu.add(ShowDuplicateStatisticsAction(owner))
        viewMenu.add(JMenuItem("Übersicht aller Duplikate anzeigen...").apply {
            addActionListener {
                FilmDuplicateOverviewDialog(owner).isVisible = true
            }
        })
        viewMenu.addSeparator()
        viewMenu.add(filmTab.toggleFilterDialogVisibilityAction())
        viewMenu.addSeparator()
        viewMenu.add(showFilmInformationAction)
        viewMenu.addSeparator()
        viewMenu.add(manageBookmarkAction)
    }

    private fun createFontMenu() {
        if (!menuPolicy.supportsFontMenu) {
            return
        }
        FontManager(fontMenu).restoreConfigData()
    }

    private fun createHelpMenu() {
        helpMenu.add(ShowOnlineHelpAction(owner))
        helpMenu.add(showLuceneTutorialAction)
        helpMenu.add(ShowOnlineFaqAction(owner))
        helpMenu.addSeparator()
        helpMenu.add(ShowLogWindowAction(logDialog))
        helpMenu.addSeparator()
        helpMenu.add(ResetSettingsAction(owner))
        helpMenu.add(ResetDownloadHistoryAction(owner))
        helpMenu.add(ResetAboHistoryAction(owner))
        helpMenu.addSeparator()
        helpMenu.add(DeleteLocalFilmlistAction(owner))
        helpMenu.add(DeleteBookmarksAction(owner))
        helpMenu.addSeparator()
        helpMenu.add(ResetFilterDialogPosition(owner))
        helpMenu.addSeparator()
        createHelperToolsEntries()
        helpMenu.addSeparator()

        if (GuiFunktionen.isNotUsingExternalUpdater()) {
            helpMenu.add(searchProgramUpdateAction)
        }
        helpMenu.add(ShowProgramInfosAction(owner))

        menuPolicy.addHelpTail(helpMenu, owner)
    }

    private fun createHelperToolsEntries() {
        val menu = JMenu("Hilfsmittel")
        menu.add(OptimizeHistoryDbAction(owner))
        menu.add(RecoverHistoryDbAction(owner))
        menu.addSeparator()
        menu.add(CleanupApplicationConfigurationAction(owner))
        helpMenu.add(menu)
    }

    private fun createDeveloperMenu() {
        val devMenu = JMenu("Entwickler")
        devMenu.add(JMenuItem("GC ausführen").apply {
            addActionListener { System.gc() }
        })

        val index = menuBar.getComponentIndex(viewMenu)
        menuBar.add(devMenu, index + 1)
    }

    private fun createAboMenu() {
        aboMenu.add(CreateNewAboAction(daten.listeAbo) { owner })
        aboMenu.add(ShowAboHistoryAction(owner))
        aboMenu.addSeparator()
        aboMenu.add(manageAboAction)
    }
}
