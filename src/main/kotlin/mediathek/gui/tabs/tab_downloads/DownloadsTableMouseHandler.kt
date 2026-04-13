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

package mediathek.gui.tabs.tab_downloads

import mediathek.config.Daten
import mediathek.controller.starter.Start
import mediathek.daten.DatenDownload
import mediathek.daten.DatenFilm
import mediathek.daten.DatenPset
import mediathek.daten.abo.DatenAbo
import mediathek.gui.dialog.DialogAboNoSet
import mediathek.gui.dialog.DialogEditAbo
import mediathek.mainwindow.MediathekGui
import mediathek.swing.IconUtils
import mediathek.tool.GuiFunktionen
import mediathek.tool.SVGIconUtilities
import mediathek.tool.table.MVDownloadsTable
import org.apache.commons.lang3.SystemUtils
import org.kordamp.ikonli.fontawesome6.FontAwesomeSolid
import java.awt.Point
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import javax.swing.JMenu
import javax.swing.JMenuItem
import javax.swing.JOptionPane
import javax.swing.JPopupMenu

class DownloadsTableMouseHandler(
    private val downloadsTab: GuiDownloads,
    private val tabelle: MVDownloadsTable,
    private val daten: Daten,
    private val mediathekGui: MediathekGui
) : MouseAdapter() {
    private var datenDownload: DatenDownload? = null
    private var point: Point = Point()

    override fun mouseClicked(event: MouseEvent) {
        if (event.button == MouseEvent.BUTTON1) {
            if (event.clickCount == 1) {
                val clickPoint = event.point
                point = clickPoint
                val row = tabelle.rowAtPoint(clickPoint)
                val column = tabelle.columnAtPoint(clickPoint)
                if (row >= 0) {
                    buttonTable(row, column)
                }
            } else if (event.clickCount > 1) {
                downloadsTab.editDownload()
            }
        }
    }

    fun doWork(event: MouseEvent)  {
        selectDownloadAt(event.point)
        if (event.isPopupTrigger) {
            showMenu(event)
        }
    }

    override fun mousePressed(event: MouseEvent) {
        doWork(event)
    }

    override fun mouseReleased(event: MouseEvent) {
        doWork(event)
    }

    private fun selectDownloadAt(point: Point) {
        this.point = point
        val row = tabelle.rowAtPoint(point)
        if (row >= 0) {
            datenDownload = downloadAtViewRow(row)
        }
    }

    private fun downloadAtViewRow(row: Int): DatenDownload =
        tabelle.model.getValueAt(tabelle.convertRowIndexToModel(row), DatenDownload.DOWNLOAD_REF) as DatenDownload

    private fun buttonTable(row: Int, column: Int) {
        if (row == -1) {
            return
        }

        datenDownload = downloadAtViewRow(row)
        when (tabelle.convertColumnIndexToModel(column)) {
            DatenDownload.DOWNLOAD_BUTTON_START -> handleStartButton()
            DatenDownload.DOWNLOAD_BUTTON_DEL -> handleDeleteButton()
        }
    }

    private fun handleStartButton() {
        val download = datenDownload ?: return
        if (download.start != null && !download.isDownloadManager) {
            if (download.start.status == Start.STATUS_FERTIG) {
                downloadsTab.filmAbspielen()
            } else {
                downloadsTab.filmStartenWiederholenStoppen(false, download.start.status == Start.STATUS_ERR, true, false)
            }
        } else {
            downloadsTab.filmStartenWiederholenStoppen(false, true, true, false)
        }
    }

    private fun handleDeleteButton() {
        val download = datenDownload ?: return
        if (download.start != null && download.start.status >= Start.STATUS_FERTIG) {
            downloadsTab.downloadsAufraeumen(download)
        } else {
            downloadsTab.downloadLoeschen(true)
        }
    }

    private fun showMenu(evt: MouseEvent) {
        point = evt.point
        val nr = tabelle.rowAtPoint(point)
        if (nr != -1) {
            tabelle.setRowSelectionInterval(nr, nr)
        }

        val popupMenu = JPopupMenu()
        addDownloadControlItems(popupMenu)
        addAboMenu(popupMenu)
        addPlaybackItems(popupMenu)

        popupMenu.show(evt.component, evt.x, evt.y)
    }

    private fun addDownloadControlItems(popupMenu: JPopupMenu) {
        val waitingOrRunning = selectedDownloadWaitingOrRunning()

        val itemStarten = JMenuItem("Download starten").apply {
            icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/caret-down.svg")
            isEnabled = !waitingOrRunning
            addActionListener { downloadsTab.filmStartenWiederholenStoppen(false, true, true, false) }
        }
        popupMenu.add(itemStarten)

        val itemStoppen = JMenuItem("Download stoppen").apply {
            isEnabled = waitingOrRunning
            addActionListener { downloadsTab.filmStartenWiederholenStoppen(false, false, true, false) }
        }
        popupMenu.add(itemStoppen)

        popupMenu.addSeparator()
        popupMenu.add(downloadsTab.advanceDownloadsAction)
        popupMenu.add(downloadsTab.deferDownloadsAction)
        popupMenu.add(downloadsTab.deleteDownloadsAction)
        popupMenu.add(downloadsTab.editDownloadAction)

        popupMenu.addSeparator()
        popupMenu.add(downloadsTab.startAllDownloadsAction)
        popupMenu.add(downloadsTab.stopAllDownloadsAction)

        val itemWartendeStoppen = JMenuItem("Wartende Downloads stoppen").apply {
            addActionListener { downloadsTab.stopAllWaitingDownloads() }
        }
        popupMenu.add(itemWartendeStoppen)

        popupMenu.add(downloadsTab.refreshDownloadListAction)
        popupMenu.add(downloadsTab.cleanupDownloadListAction)
        popupMenu.addSeparator()
        popupMenu.add(downloadsTab.playDownloadAction)
        popupMenu.add(downloadsTab.deleteDownloadAction)
        popupMenu.add(downloadsTab.openTargetFolderAction)
        popupMenu.addSeparator()
    }

    private fun selectedDownloadWaitingOrRunning(): Boolean {
        val row = tabelle.selectedRow
        if (row == -1) {
            return false
        }

        val download = downloadAtViewRow(row)
        return download.start != null && download.start.status <= Start.STATUS_RUN
    }

    private fun addAboMenu(popupMenu: JPopupMenu) {
        val submenueAbo = JMenu("Abo")
        val itemChangeAbo = JMenuItem("Abo ändern")
        val itemDelAbo = JMenuItem("Abo löschen")
        val datenAbo = datenDownload
            ?.film
            ?.let { film -> daten.listeAbo.getAboFuerFilm_schnell(film, false) }

        if (datenAbo == null) {
            submenueAbo.isEnabled = false
            itemChangeAbo.isEnabled = false
            itemDelAbo.isEnabled = false
        } else {
            enableAboActions(itemChangeAbo, itemDelAbo, datenAbo)
        }

        submenueAbo.add(itemDelAbo)
        submenueAbo.add(itemChangeAbo)
        popupMenu.add(submenueAbo)
        popupMenu.addSeparator()
    }

    private fun enableAboActions(itemChangeAbo: JMenuItem, itemDelAbo: JMenuItem, datenAbo: DatenAbo) {
        itemDelAbo.addActionListener { daten.listeAbo.aboLoeschen(datenAbo) }
        itemChangeAbo.addActionListener {
            if (!DialogAboNoSet.ensureAboProgramSetAvailable(mediathekGui)) {
                return@addActionListener
            }
            val dialog = DialogEditAbo(mediathekGui, datenAbo, false)
            dialog.isVisible = true
            if (dialog.successful()) {
                daten.listeAbo.aenderungMelden()
            }
        }
    }

    private fun addPlaybackItems(popupMenu: JPopupMenu) {
        val itemPlayer = JMenuItem("Film (URL) abspielen").apply {
            icon = IconUtils.of(FontAwesomeSolid.PLAY_CIRCLE)
            addActionListener { playUrlAtPopupRow() }
        }
        popupMenu.add(itemPlayer)

        val itemUrl = JMenuItem("URL kopieren").apply {
            addActionListener { copyUrlAtPopupRow() }
        }
        popupMenu.add(itemUrl)

        popupMenu.add(mediathekGui.showFilmInformationAction)
    }

    private fun playUrlAtPopupRow() {
        val row = tabelle.rowAtPoint(point)
        if (row == -1) {
            return
        }

        val pSetPlay = Daten.getInstance().listePset.psetAbspielen
        if (pSetPlay == null) {
            showMissingPlayerMessage()
        } else {
            playUrlWithProgram(row, pSetPlay)
        }
    }

    private fun playUrlWithProgram(row: Int, gruppe: DatenPset) {
        val download = downloadAtViewRow(row)
        val film = download.film ?: return
        val filmClone = DatenFilm(film).apply {
            setNormalQualityUrl(download.arr[DatenDownload.DOWNLOAD_URL])
            lowQualityUrl = ""
        }
        daten.starterClass.urlMitProgrammStarten(gruppe, filmClone, "")
    }

    private fun showMissingPlayerMessage() {
        val menuPath = if (SystemUtils.IS_OS_MAC_OSX) {
            "MediathekView->Einstellungen…->Aufzeichnen und Abspielen->Set bearbeiten"
        } else {
            "Datei->Einstellungen->Set bearbeiten"
        }
        JOptionPane.showMessageDialog(
            mediathekGui,
            "Bitte legen Sie im Menü \"$menuPath\" ein Programm zum Abspielen fest.",
            "Kein Videoplayer!",
            JOptionPane.INFORMATION_MESSAGE
        )
    }

    private fun copyUrlAtPopupRow() {
        val row = tabelle.rowAtPoint(point)
        if (row != -1) {
            GuiFunktionen.copyToClipboard(
                tabelle.model.getValueAt(
                    tabelle.convertRowIndexToModel(row),
                    DatenDownload.DOWNLOAD_URL
                ).toString()
            )
        }
    }
}
