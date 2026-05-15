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

package mediathek.gui.tabs.tab_film

import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.config.Daten
import mediathek.config.Konstanten
import mediathek.config.StandardLocations
import mediathek.controller.history.SeenHistoryController
import mediathek.controller.starter.Start
import mediathek.daten.DatenFilm
import mediathek.daten.DatenPset
import mediathek.daten.FilmResolution
import mediathek.daten.blacklist.BlacklistRule
import mediathek.filmlisten.writer.FilmListWriter
import mediathek.gui.actions.CreateNewAboAction
import mediathek.gui.actions.UrlHyperlinkAction
import mediathek.gui.duplicates.details.DuplicateFilmDetailsDialog
import mediathek.mainwindow.MediathekGui
import mediathek.tool.ApplicationConfiguration
import mediathek.tool.FileDialogs
import mediathek.tool.GuiFunktionen
import mediathek.tool.MVInfoFile
import mediathek.tool.table.MVFilmTable
import org.apache.logging.log4j.LogManager
import java.awt.Point
import java.awt.event.*
import java.awt.print.PrinterException
import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.util.*
import javax.swing.*

/**
 * Implements the context menu for tab film.
 */
class TableContextMenuHandler(
    private val host: Host,
) : MouseAdapter() {
    interface Host {
        fun table(): MVFilmTable
        fun getCurrentlySelectedFilm(): Optional<DatenFilm>
        fun getFilm(row: Int): Optional<DatenFilm>
        fun playSelectedFilm()
        fun saveSelectedFilm()
        fun startFilmWithPset(pSet: DatenPset)
        fun setSelectionUpdatesSuspended(suspended: Boolean)
        fun gui(): MediathekGui
        fun playFilmAction(): Action
        fun saveFilmAction(): Action
        fun bookmarkAddFilmAction(): Action
        fun bookmarkRemoveFilmAction(): Action
        fun showFilmInformationAction(): Action
        fun downloadSubtitleAction(): Action
    }

    private val daten = Daten.getInstance()
    private val uiScope = CoroutineScope(SupervisorJob() + Dispatchers.Swing)
    private val createAboAction = CreateNewAboAction(daten.listeAbo) { host.gui() }
    private val beobPrint = BeobPrint()
    private val beobAbo = BeobAbo(false)
    private val beobAboMitTitel = BeobAbo(true)
    private val unseenActionListener = BeobHistory(false)
    private val seenActionListener = BeobHistory(true)
    private val jDownloadHelper = JDownloadHelper()
    private val pyLoadHelper = PyLoadHelper()
    private var popupPoint: Point? = null

    override fun mouseClicked(event: MouseEvent) {
        if (event.button == MouseEvent.BUTTON1) {
            if (event.clickCount == 1) {
                popupPoint = event.point
                val point = popupPoint ?: return
                val row = host.table().rowAtPoint(point)
                val column = host.table().columnAtPoint(point)
                if (row >= 0) {
                    buttonTable(row, column)
                }
            } else if (event.clickCount > 1) {
                host.gui().filmInfoDialog?.let { infoDialog ->
                    if (!infoDialog.isVisible) {
                        infoDialog.showInfo()
                    }
                }
            }
        }
    }

    override fun mousePressed(event: MouseEvent) {
        if (event.isPopupTrigger) {
            showMenu(event)
        }
    }

    override fun mouseReleased(event: MouseEvent) {
        if (event.isPopupTrigger) {
            showMenu(event)
        }
    }

    private fun buttonTable(row: Int, column: Int) {
        if (row == -1) {
            return
        }

        when (host.table().convertColumnIndexToModel(column)) {
            DatenFilm.FILM_ABSPIELEN -> host.getCurrentlySelectedFilm().ifPresent { film ->
                var dontPlay = false
                val download = daten.listeDownloadsButton.getDownloadUrlFilm(film.urlNormalQuality)
                if (download != null && download.start != null && download.start.status == Start.STATUS_RUN) {
                    dontPlay = true
                    daten.listeDownloadsButton.delDownloadButton(film.urlNormalQuality)
                }
                if (!dontPlay) {
                    host.playSelectedFilm()
                }
            }

            DatenFilm.FILM_AUFZEICHNEN -> host.saveSelectedFilm()
            DatenFilm.FILM_MERKEN -> host.getCurrentlySelectedFilm().ifPresent { film ->
                if (!film.isLivestream) {
                    if (film.isBookmarked) {
                        host.bookmarkRemoveFilmAction().actionPerformed(null)
                    } else {
                        host.bookmarkAddFilmAction().actionPerformed(null)
                    }
                }
            }
        }
    }

    private fun createStartWithPsetItems(popupMenu: JPopupMenu) {
        val submenu = JMenu("Film mit Set starten")
        popupMenu.add(submenu)
        val liste = Daten.getInstance().listePset.listeButton
        for (pset in liste) {
            if (pset.listeProg.isEmpty() && pset.name.isEmpty()) {
                continue
            }

            val item = JMenuItem(pset.name)
            pset.foregroundColor.ifPresent(item::setForeground)
            if (pset.listeProg.isNotEmpty()) {
                item.addActionListener { host.startFilmWithPset(pset) }
            }
            submenu.add(item)
        }
    }

    private fun showMenu(event: MouseEvent) {
        popupPoint = event.point
        val point = popupPoint ?: return
        val row = host.table().rowAtPoint(point)
        if (row < 0) {
            return
        }
        host.table().setRowSelectionInterval(row, row)

        val popupMenu = createContextMenu(host.getFilm(row))
        popupMenu.show(event.component, event.x, event.y)
    }

    private fun createContextMenu(selectedFilm: Optional<DatenFilm>): JPopupMenu =
        JPopupMenu().apply {
            addPrimaryContextActions(this, selectedFilm)
            addFilmProgramsMenu(this)
            addBlacklistMenu(this)
            selectedFilm.ifPresent { film -> addFilmSpecificContextActions(this, film) }
            addPrintAndInfoActions(this, selectedFilm)
            selectedFilm.ifPresent { film -> addFileAndDuplicateActions(this, film) }
        }

    private fun addPrimaryContextActions(popupMenu: JPopupMenu, selectedFilm: Optional<DatenFilm>) {
        popupMenu.add(host.playFilmAction())
        popupMenu.add(host.saveFilmAction())

        val bookmarkMenuItem = JMenuItem(host.bookmarkAddFilmAction())
        popupMenu.add(bookmarkMenuItem)
        popupMenu.addSeparator()
        addAboMenu(popupMenu, selectedFilm)
        updateBookmarkMenuItem(popupMenu, bookmarkMenuItem, selectedFilm)
    }

    private fun addAboMenu(popupMenu: JPopupMenu, selectedFilm: Optional<DatenFilm>) {
        val submenuAbo = JMenu("Abo")
        popupMenu.add(submenuAbo)

        val itemAbo = JMenuItem("Abo mit Sender und Thema anlegen")
        val itemAboMitTitel = JMenuItem("Abo mit Sender und Thema und Titel anlegen")

        selectedFilm.ifPresent { film -> configureAboMenuItems(film, itemAbo, itemAboMitTitel) }

        submenuAbo.add(itemAbo)
        submenuAbo.add(itemAboMitTitel)
    }

    private fun configureAboMenuItems(
        film: DatenFilm,
        itemAbo: JMenuItem,
        itemAboMitTitel: JMenuItem,
    ) {
        if (daten.listeAbo.getAboFuerFilm_schnell(film, false) != null) {
            itemAbo.isEnabled = false
            itemAboMitTitel.isEnabled = false
        } else {
            itemAbo.addActionListener(beobAbo)
            itemAboMitTitel.addActionListener(beobAboMitTitel)
        }
    }

    private fun updateBookmarkMenuItem(
        popupMenu: JPopupMenu,
        bookmarkMenuItem: JMenuItem,
        selectedFilm: Optional<DatenFilm>,
    ) {
        selectedFilm.ifPresent { film ->
            if (film.isLivestream) {
                popupMenu.remove(bookmarkMenuItem)
            } else {
                bookmarkMenuItem.text = if (film.isBookmarked) {
                    "Film aus Merkliste entfernen"
                } else {
                    "Film merken"
                }
            }
        }
    }

    private fun addFilmProgramsMenu(popupMenu: JPopupMenu) {
        createStartWithPsetItems(popupMenu)
    }

    private fun addBlacklistMenu(popupMenu: JPopupMenu) {
        val submenuBlack = JMenu("Blacklist")
        popupMenu.add(submenuBlack)

        val itemBlackSender = JMenuItem("Sender in die Blacklist einfügen")
        itemBlackSender.addActionListener {
            addBlacklistRuleForSelectedFilm { film ->
                daten.listeBlacklist.add(BlacklistRule(film.sender, "", "", ""))
            }
        }
        submenuBlack.add(itemBlackSender)

        val itemBlackThema = JMenuItem("Thema in die Blacklist einfügen")
        itemBlackThema.addActionListener {
            addBlacklistRuleForSelectedFilm { film ->
                daten.listeBlacklist.add(BlacklistRule("", film.thema, "", ""))
            }
        }
        submenuBlack.add(itemBlackThema)

        val itemAddTitleToBlacklist = JMenuItem("Titel in die Blacklist einfügen")
        itemAddTitleToBlacklist.addActionListener {
            addBlacklistRuleForSelectedFilm { film ->
                daten.listeBlacklist.add(BlacklistRule("", "", film.title, ""))
            }
        }
        submenuBlack.add(itemAddTitleToBlacklist)

        val itemBlackSenderThema = JMenuItem("Sender und Thema in die Blacklist einfügen")
        itemBlackSenderThema.addActionListener {
            addBlacklistRuleForSelectedFilm { film ->
                daten.listeBlacklist.add(BlacklistRule(film.sender, film.thema, "", ""))
            }
        }
        submenuBlack.add(itemBlackSenderThema)
    }

    private fun addFilmSpecificContextActions(popupMenu: JPopupMenu, film: DatenFilm) {
        popupMenu.addSeparator()
        jDownloadHelper.installContextMenu(film, popupMenu)
        popupMenu.addSeparator()
        pyLoadHelper.installContextMenu(film, popupMenu)
        popupMenu.addSeparator()
        setupCopyToClipboardContextMenu(film, popupMenu)
        popupMenu.addSeparator()
        setupSearchEntries(popupMenu, film)

        if (film.hasSubtitle()) {
            popupMenu.add(host.downloadSubtitleAction())
            popupMenu.addSeparator()
        }
    }

    private fun addPrintAndInfoActions(popupMenu: JPopupMenu, selectedFilm: Optional<DatenFilm>) {
        val printTableMenuItem = JMenuItem("Tabelle drucken")
        printTableMenuItem.addActionListener(beobPrint)
        popupMenu.add(printTableMenuItem)

        popupMenu.add(host.showFilmInformationAction())
        selectedFilm.ifPresent { film -> setupHistoryContextActions(popupMenu, film) }
    }

    private fun addFileAndDuplicateActions(popupMenu: JPopupMenu, film: DatenFilm) {
        if (!film.isLivestream) {
            popupMenu.addSeparator()
            popupMenu.add(createInfoFileMenuItem(film))
        }

        if (film.isDuplicate) {
            popupMenu.addSeparator()
            popupMenu.add(createDuplicateDetailsMenuItem(film))
        }

        if (!film.isLivestream) {
            popupMenu.addSeparator()
            popupMenu.add(createRemoveDuplicatesMenuItem(film))
        }
    }

    private fun createInfoFileMenuItem(film: DatenFilm): JMenuItem =
        JMenuItem("Infodatei erzeugen...").apply {
            addActionListener {
                val file = FileDialogs.chooseSaveFileLocation(MediathekGui.ui(), "Infodatei speichern", "")
                if (file != null) {
                    try {
                        MVInfoFile().writeManualInfoFile(film, file.toPath())
                    } catch (e: Exception) {
                        throw RuntimeException(e)
                    }
                }
            }
        }

    private fun createDuplicateDetailsMenuItem(film: DatenFilm): JMenuItem =
        JMenuItem("Zusammengehörige Filme anzeigen...").apply {
            addActionListener {
                DuplicateFilmDetailsDialog(MediathekGui.ui(), film).isVisible = true
            }
        }

    private fun createRemoveDuplicatesMenuItem(film: DatenFilm): JMenuItem =
        JMenuItem("Duplikate entfernen...").apply {
            addActionListener { performDuplicateRemoval(film) }
        }

    private fun performDuplicateRemoval(film: DatenFilm) {
        val completeFilmList = daten.listeFilme
        val filteredFilmList = daten.listeBlacklist
        val duplicateList = ArrayList(
            completeFilmList.parallelStream()
                .filter { it.sender.equals(film.sender, ignoreCase = true) }
                .filter { it.thema.equals(film.thema, ignoreCase = true) }
                .filter { it.title.equals(film.title, ignoreCase = true) }
                .filter { it.urlNormalQuality.equals(film.urlNormalQuality, ignoreCase = true) }
                .toList(),
        )
        val filmCount = duplicateList.size

        if (filmCount <= 1) {
            JOptionPane.showMessageDialog(
                host.gui(),
                "Es wurden keine Duplikate gefunden.",
                Konstanten.PROGRAMMNAME,
                JOptionPane.INFORMATION_MESSAGE,
            )
            return
        }

        val duplicateCount = filmCount - 1
        val duplicateString = if (duplicateCount == 1) "Duplikat" else "Duplikate"
        val message = "Es wurden $duplicateCount $duplicateString gefunden.\nMöchten Sie diese entfernen?"
        val result = JOptionPane.showConfirmDialog(
            host.gui(),
            message,
            Konstanten.PROGRAMMNAME,
            JOptionPane.YES_NO_OPTION,
        )
        if (result != JOptionPane.YES_OPTION) {
            return
        }

        duplicateList.remove(film)
        completeFilmList.removeAll(duplicateList.toSet())

        uiScope.launch {
            val writeResult = withContext(Dispatchers.IO) {
                runCatching {
                    FilmListWriter(false).writeFilmList(
                        StandardLocations.getFilmlistFilePathString(),
                        completeFilmList,
                    )
                }
            }

            writeResult
                .onSuccess {
                    filteredFilmList.filterListAndNotifyListeners()
                    JOptionPane.showMessageDialog(
                        host.gui(),
                        "Duplikate wurden entfernt.",
                        Konstanten.PROGRAMMNAME,
                        JOptionPane.INFORMATION_MESSAGE,
                    )
                }
                .onFailure { error ->
                    logger.error("Could not persist duplicate-removal changes.", error)
                    JOptionPane.showMessageDialog(
                        host.gui(),
                        "Duplikate konnten nicht gespeichert werden.",
                        Konstanten.PROGRAMMNAME,
                        JOptionPane.ERROR_MESSAGE,
                    )
                }
        }
    }

    private fun setupHistoryContextActions(popupMenu: JPopupMenu, film: DatenFilm) {
        if (!film.isLivestream) {
            SeenHistoryController().use { history ->
                val historyMenuItem = if (history.hasBeenSeen(film)) {
                    JMenuItem("Film als ungesehen markieren").apply {
                        addActionListener(unseenActionListener)
                    }
                } else {
                    JMenuItem("Film als gesehen markieren").apply {
                        addActionListener(seenActionListener)
                    }
                }
                popupMenu.add(historyMenuItem)
            }
        }
    }

    private fun setupCopyToClipboardContextMenu(film: DatenFilm, popupMenu: JPopupMenu) {
        val copyToClipboardMenu = JMenu("In Zwischenablage kopieren")

        JMenuItem("Titel").also {
            it.addActionListener { GuiFunktionen.copyToClipboard(film.title) }
            copyToClipboardMenu.add(it)
        }

        JMenuItem("Thema").also {
            it.addActionListener { GuiFunktionen.copyToClipboard(film.thema) }
            copyToClipboardMenu.add(it)
        }

        JMenuItem("Thema - Titel").also {
            it.addActionListener { GuiFunktionen.copyToClipboard("${film.thema} - ${film.title}") }
            copyToClipboardMenu.add(it)
        }

        JMenuItem("Sender - Thema - Titel").also {
            it.addActionListener { GuiFunktionen.copyToClipboard("${film.sender} - ${film.thema} - ${film.title}") }
            copyToClipboardMenu.add(it)
        }

        JMenuItem("Beschreibung").also {
            it.addActionListener { GuiFunktionen.copyToClipboard(film.description) }
            copyToClipboardMenu.add(it)
        }

        setupFilmUrlCopyToClipboardEntries(copyToClipboardMenu, film)
        popupMenu.add(copyToClipboardMenu)
    }

    private fun setupFilmUrlCopyToClipboardEntries(parentMenu: JMenu, film: DatenFilm) {
        parentMenu.addSeparator()

        val normalUrl = film.getUrlFuerAufloesung(FilmResolution.Enum.NORMAL)
        var highQualityUrl = film.getUrlFuerAufloesung(FilmResolution.Enum.HIGH_QUALITY)
        var lowQualityUrl = film.getUrlFuerAufloesung(FilmResolution.Enum.LOW)
        if (highQualityUrl == normalUrl) {
            highQualityUrl = ""
        }
        if (lowQualityUrl == normalUrl) {
            lowQualityUrl = ""
        }

        if (normalUrl.isNotEmpty()) {
            val copyNormalUrlListener = ActionListener { GuiFunktionen.copyToClipboard(normalUrl) }
            if (highQualityUrl.isNotEmpty() || lowQualityUrl.isNotEmpty()) {
                val submenuUrl = JMenu("Film-URL")
                if (highQualityUrl.isNotEmpty()) {
                    JMenuItem("höchste/hohe Qualität").also {
                        it.accelerator = KeyStroke.getKeyStroke(
                            KeyEvent.VK_H,
                            GuiFunktionen.getPlatformControlKey() or KeyEvent.SHIFT_DOWN_MASK or KeyEvent.ALT_DOWN_MASK,
                        )
                        it.addActionListener {
                            GuiFunktionen.copyToClipboard(film.getUrlFuerAufloesung(FilmResolution.Enum.HIGH_QUALITY))
                        }
                        submenuUrl.add(it)
                    }
                }

                JMenuItem("mittlere Qualität").also {
                    it.addActionListener(copyNormalUrlListener)
                    it.accelerator = KeyStroke.getKeyStroke(
                        KeyEvent.VK_N,
                        GuiFunktionen.getPlatformControlKey() or KeyEvent.SHIFT_DOWN_MASK or KeyEvent.ALT_DOWN_MASK,
                    )
                    submenuUrl.add(it)
                }

                if (lowQualityUrl.isNotEmpty()) {
                    JMenuItem("niedrige Qualität").also {
                        it.addActionListener {
                            GuiFunktionen.copyToClipboard(film.getUrlFuerAufloesung(FilmResolution.Enum.LOW))
                        }
                        submenuUrl.add(it)
                    }
                }
                parentMenu.add(submenuUrl)
            } else {
                JMenuItem("Verfügbare URL").also {
                    it.addActionListener(copyNormalUrlListener)
                    parentMenu.add(it)
                }
            }
        }

        if (film.subtitleUrl.isNotEmpty()) {
            JMenuItem("Untertitel-URL").also {
                it.addActionListener { GuiFunktionen.copyToClipboard(film.subtitleUrl) }
                parentMenu.add(it)
            }
        }
    }

    private fun setupSearchEntries(popupMenu: JPopupMenu, film: DatenFilm) {
        val onlineSearchMenu = JMenu("Online-Suche nach")
        val themaMenu = JMenu("Thema")
        val titelMenu = JMenu("Titel")

        for (provider in OnlineSearchProviders.entries) {
            if (!film.isLivestream) {
                JMenuItem(provider.toString()).also {
                    it.addActionListener {
                        val url = provider.queryUrl + URLEncoder.encode(film.thema, StandardCharsets.UTF_8)
                        UrlHyperlinkAction.openURL(url)
                    }
                    themaMenu.add(it)
                }
            }

            JMenuItem(provider.toString()).also {
                it.addActionListener {
                    val url = provider.queryUrl + URLEncoder.encode(film.title, StandardCharsets.UTF_8)
                    UrlHyperlinkAction.openURL(url)
                }
                titelMenu.add(it)
            }
        }

        if (!film.isLivestream) {
            onlineSearchMenu.add(themaMenu)
        }
        onlineSearchMenu.add(titelMenu)
        popupMenu.add(onlineSearchMenu)
        popupMenu.addSeparator()
    }

    private fun selectedFilmAtPopupPoint(): DatenFilm? {
        val point = popupPoint ?: return null
        val row = host.table().rowAtPoint(point)
        if (row == -1) {
            return null
        }
        return host.getFilm(row).orElse(null)
    }

    private inner class BeobHistory(
        private val seen: Boolean,
    ) : ActionListener {
        private fun updateHistory(film: DatenFilm) {
            SeenHistoryController().use { history ->
                if (seen) {
                    history.markSeen(film)
                } else {
                    history.markUnseen(film)
                }
            }
        }

        override fun actionPerformed(event: ActionEvent?) {
            selectedFilmAtPopupPoint()?.let(::updateHistory)
        }
    }

    private inner class BeobPrint : ActionListener {
        override fun actionPerformed(event: ActionEvent?) {
            try {
                host.table().print()
            } catch (ex: PrinterException) {
                logger.error(ex)
            }
        }
    }

    private inner class BeobAbo(
        private val mitTitel: Boolean,
    ) : ActionListener {
        override fun actionPerformed(event: ActionEvent?) {
            selectedFilmAtPopupPoint()?.let { film ->
                host.setSelectionUpdatesSuspended(true)
                try {
                    val datenAbo = daten.listeAbo.getAboFuerFilm_schnell(film, false)
                    if (datenAbo != null) {
                        daten.listeAbo.aboLoeschen(datenAbo)
                    } else {
                        createAboAction.createAbo(
                            aboname = film.thema,
                            filmSender = film.sender,
                            filmThema = film.thema,
                            filmTitel = if (mitTitel) film.title else "",
                        )
                    }
                } finally {
                    host.setSelectionUpdatesSuspended(false)
                }
            }
        }
    }

    private fun turnOnBlacklist() {
        ApplicationConfiguration.getConfiguration().setProperty(ApplicationConfiguration.BLACKLIST_IS_ON, true)
    }

    private fun addBlacklistRuleForSelectedFilm(blacklistRuleAppender: (DatenFilm) -> Unit) {
        selectedFilmAtPopupPoint()?.let { film ->
            turnOnBlacklist()
            blacklistRuleAppender(film)
        }
    }

    companion object {
        private val logger = LogManager.getLogger()
    }
}
