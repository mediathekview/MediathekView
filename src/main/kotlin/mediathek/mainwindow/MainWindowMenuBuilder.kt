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

package mediathek.mainwindow

import mediathek.config.CommandLineOptions
import mediathek.daten.DatenPset
import mediathek.daten.ProgramSetRepository
import mediathek.daten.abo.AboServices
import mediathek.daten.blacklist.BlacklistServices
import mediathek.filmlisten.FilmCatalog
import mediathek.gui.actions.*
import mediathek.gui.actions.export.ExportDecompressedFilmlistAction
import mediathek.gui.actions.export.ExportReadableFilmlistAction
import mediathek.gui.actions.import_actions.ImportOldAbosAction
import mediathek.gui.actions.import_actions.ImportOldBlacklistAction
import mediathek.gui.actions.import_actions.ImportOldReplacementListAction
import mediathek.gui.bookmark.BookmarkServices
import mediathek.gui.dialog.MissingProgramSetDialog
import mediathek.gui.duplicates.overview.FilmDuplicateOverviewDialog
import mediathek.gui.history.ResetAboHistoryAction
import mediathek.gui.history.ResetDownloadHistoryAction
import mediathek.gui.tabs.tab_downloads.GuiDownloads
import mediathek.gui.tabs.tab_film.GuiFilme
import mediathek.logging.LogDialog
import mediathek.sqlite.RecoverHistoryDbAction
import mediathek.tool.GuiFunktionen
import mediathek.tool.GuiFunktionenProgramme
import java.util.function.BiConsumer
import java.util.function.Supplier
import javax.swing.*

class MainWindowMenuBuilder(
    private val ownerFrame: JFrame,
    private val settingsResetHost: SettingsResetHost,
    private val quitHost: MainWindowQuitHost,
    private val filmBookmarkHost: FilmBookmarkHost,
    private val programSets: ProgramSetRepository,
    private val filmCatalog: FilmCatalog,
    private val abos: AboServices,
    private val blacklist: BlacklistServices,
    private val bookmarks: BookmarkServices,
    private val programSetExporter: BiConsumer<Array<DatenPset>, String>,
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
        exportMenu.add(ExportReadableFilmlistAction(filmCatalog.allFilms, ownerFrame))
        exportMenu.add(ExportDecompressedFilmlistAction(filmCatalog.allFilms, ownerFrame))

        val importMenu = JMenu("Import")
        importMenu.add(ImportOldAbosAction(ownerFrame, abos, blacklist))
        importMenu.add(ImportOldBlacklistAction(ownerFrame, abos, blacklist))
        importMenu.add(ImportOldReplacementListAction(ownerFrame, abos, blacklist))

        fileMenu.add(exportMenu)
        fileMenu.add(importMenu)

        menuPolicy.addSettingsItem(fileMenu, settingsAction)
        menuPolicy.addQuitItem(fileMenu, quitHost::quitApplication)
    }

    private fun createViewMenu() {
        val filmTab = filmTab.get()
        filmTab.installViewMenuEntry(viewMenu)
        tabRegistry.installViewMenuEntries(viewMenu)
        viewMenu.addSeparator()
        viewMenu.add(showMemoryMonitorAction)
        viewMenu.add(showBandwidthUsageAction)
        viewMenu.addSeparator()
        viewMenu.add(ShowFilmStatisticsAction(ownerFrame, filmCatalog))
        viewMenu.add(ShowDuplicateStatisticsAction(ownerFrame, filmCatalog))
        viewMenu.add(JMenuItem("Übersicht aller Duplikate anzeigen...").apply {
            addActionListener {
                FilmDuplicateOverviewDialog(ownerFrame, filmCatalog).isVisible = true
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
        helpMenu.add(ShowOnlineHelpAction(ownerFrame))
        helpMenu.add(showLuceneTutorialAction)
        helpMenu.add(ShowOnlineFaqAction(ownerFrame))
        helpMenu.addSeparator()
        helpMenu.add(ShowLogWindowAction(logDialog))
        helpMenu.addSeparator()
        helpMenu.add(ResetSettingsAction(settingsResetHost, programSets, programSetExporter))
        helpMenu.add(ResetDownloadHistoryAction(ownerFrame))
        helpMenu.add(ResetAboHistoryAction(ownerFrame, abos.historyController))
        helpMenu.addSeparator()
        helpMenu.add(DeleteLocalFilmlistAction(quitHost))
        helpMenu.add(DeleteBookmarksAction(bookmarks, filmBookmarkHost))
        helpMenu.addSeparator()
        helpMenu.add(ResetFilterDialogPosition(filmBookmarkHost))
        helpMenu.addSeparator()
        createHelperToolsEntries()
        helpMenu.addSeparator()

        if (GuiFunktionen.isNotUsingExternalUpdater()) {
            helpMenu.add(searchProgramUpdateAction)
        }
        helpMenu.add(ShowProgramInfosAction(ownerFrame))

        menuPolicy.addHelpTail(helpMenu, ownerFrame)
    }

    private fun createHelperToolsEntries() {
        val menu = JMenu("Hilfsmittel")
        menu.add(OptimizeHistoryDbAction(ownerFrame))
        menu.add(RecoverHistoryDbAction(ownerFrame))
        menu.addSeparator()
        menu.add(CleanupApplicationConfigurationAction(ownerFrame))
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
        aboMenu.add(
            CreateNewAboAction(
                programSets,
                filmCatalog,
                abos,
                { ownerFrame },
                { parent ->
                    MissingProgramSetDialog.ensureAboProgramSetAvailable(parent, programSets) { importParent, standardSets ->
                        GuiFunktionenProgramme.addSetVorlagen(
                            importParent,
                            programSets,
                            standardSets,
                            true,
                            programSetExporter,
                        )
                    }
                },
            )
        )
        aboMenu.add(ShowAboHistoryAction(ownerFrame, abos.historyController))
        aboMenu.addSeparator()
        aboMenu.add(manageAboAction)
    }
}
