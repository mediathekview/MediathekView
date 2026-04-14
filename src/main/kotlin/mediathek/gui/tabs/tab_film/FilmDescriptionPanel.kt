/*
 * Copyright (c) 2025 derreisende77.
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

import mediathek.audiothek.model.AudioEntry
import mediathek.config.Konstanten
import mediathek.daten.DatenFilm
import mediathek.gui.actions.UrlHyperlinkAction
import mediathek.gui.dialog.DialogFilmBeschreibung
import mediathek.mainwindow.MediathekGui
import mediathek.tool.CopyToClipboardAction
import mediathek.tool.GuiFunktionen
import mediathek.tool.SwingErrorDialog
import net.miginfocom.layout.AC
import net.miginfocom.layout.CC
import net.miginfocom.layout.LC
import net.miginfocom.swing.MigLayout
import org.jdesktop.swingx.JXHyperlink
import java.awt.Desktop
import java.awt.Dimension
import java.awt.Font
import java.net.URI
import java.util.*
import java.util.function.Supplier
import javax.swing.*

class FilmDescriptionPanel : JPanel() {
    private val scrollPane1 = JScrollPane()
    private val popupMenu = JPopupMenu()
    private val lblIcon = SenderIconLabel()
    private val lblThema = JLabel()
    private val lblTitel = JLabel()
    private val textArea = JTextArea()
    private val hyperlink = JXHyperlink()
    private val editDescriptionItem = JMenuItem("Beschreibung ändern...")
    private val editSeparator = JPopupMenu.Separator()
    private val copyDescriptionItem = JMenuItem("Beschreibung in Zwischenablage kopieren")
    private val copyBaseInfoItem = JMenuItem("Filmbasisinformationen in Zwischenablage kopieren")
    private var currentFilm: DatenFilm? = null
    private var currentAudioEntry: AudioEntry? = null

    init {
        initComponents()

        hyperlink.addActionListener {
            val toolTipText = hyperlink.toolTipText
            if (toolTipText.isNullOrEmpty()) {
                return@addActionListener
            }

            if (Desktop.isDesktopSupported()) {
                val desktop = Desktop.getDesktop()
                if (desktop.isSupported(Desktop.Action.BROWSE)) {
                    try {
                        desktop.browse(URI(toolTipText))
                    } catch (ex: Exception) {
                        SwingErrorDialog.showExceptionMessage(
                            MediathekGui.ui(),
                            "Es trat ein Fehler beim Öffnen des Links auf.\nSollte dies häufiger auftreten kontaktieren Sie bitte das Entwicklerteam.",
                            ex
                        )
                    }
                    return@addActionListener
                }
            }

            UrlHyperlinkAction.openURL(toolTipText)
        }

        createPopupMenu()
        setAllFieldsEmpty()
    }

    private fun createPopupMenu() {
        editDescriptionItem.addActionListener {
            val film = currentFilm ?: return@addActionListener
            DialogFilmBeschreibung(MediathekGui.ui(), film).isVisible = true
        }
        popupMenu.add(editDescriptionItem)
        popupMenu.add(editSeparator)

        copyDescriptionItem.addActionListener { GuiFunktionen.copyToClipboard(currentDescription) }
        popupMenu.add(copyDescriptionItem)

        copyBaseInfoItem.addActionListener { GuiFunktionen.copyToClipboard(currentBaseInfo) }
        popupMenu.add(copyBaseInfoItem)

        popupMenu.addSeparator()
        val copySelectionItem = JMenuItem("Auswahl kopieren")
        copySelectionItem.addActionListener {
            val selected = (textArea.selectionEnd - textArea.selectionStart) > 0
            if (!selected) {
                JOptionPane.showMessageDialog(this, "Kein Text markiert!", Konstanten.PROGRAMMNAME, JOptionPane.ERROR_MESSAGE)
            } else {
                GuiFunktionen.copyToClipboard(textArea.selectedText)
            }
        }
        popupMenu.add(copySelectionItem)

        componentPopupMenu = popupMenu
        textArea.componentPopupMenu = popupMenu
        updatePopupMenuState()
    }

    private fun initComponents() {
        layout = MigLayout(
            LC().hideMode(3),
            AC().fill().gap().grow().fill(),
            AC().gap().gap().gap()
        )

        lblIcon.preferredSize = Dimension(96, 96)
        lblIcon.verticalAlignment = SwingConstants.TOP
        add(lblIcon, CC().cell(0, 0, 1, 3).alignX("center").alignY("top").grow(0f, 0f))

        lblThema.font = lblThema.font.deriveFont(lblThema.font.style or Font.BOLD)
        add(lblThema, CC().cell(1, 0))

        lblTitel.font = lblTitel.font.deriveFont(lblTitel.font.style or Font.BOLD)
        add(lblTitel, CC().cell(1, 1))

        scrollPane1.preferredSize = Dimension(299, 75)
        scrollPane1.maximumSize = Dimension(Int.MAX_VALUE, 75)
        scrollPane1.minimumSize = Dimension(23, 75)
        scrollPane1.horizontalScrollBarPolicy = JScrollPane.HORIZONTAL_SCROLLBAR_NEVER

        textArea.isEditable = false
        textArea.wrapStyleWord = true
        textArea.lineWrap = true
        scrollPane1.setViewportView(textArea)
        add(scrollPane1, CC().cell(1, 2).grow())

        add(hyperlink, CC().cell(1, 3))
    }

    fun setCurrentFilm(film: DatenFilm?) {
        currentFilm = film
        currentAudioEntry = null
        if (film == null) {
            setAllFieldsEmpty()
        } else {
            showFilmDescription(film)
        }
        updatePopupMenuState()
    }

    fun setCurrentAudioEntry(entry: AudioEntry?) {
        currentAudioEntry = entry
        currentFilm = null
        if (entry == null) {
            setAllFieldsEmpty()
        } else {
            showAudioDescription(entry)
        }
        updatePopupMenuState()
    }

    fun install(tabbedPane: JTabbedPane, tabelle: JTable, filmSupplier: Supplier<Optional<DatenFilm>>) {
        tabbedPane.add("Beschreibung", this)
        tabelle.selectionModel.addListSelectionListener {
            filmSupplier.get().ifPresentOrElse(::setCurrentFilm) { setCurrentFilm(null) }
        }
    }

    private fun setAllFieldsEmpty() {
        setDescriptionPopupMenuEnabled(true)
        resetHyperlink()
        textArea.text = ""
        lblIcon.icon = null
        lblThema.text = ""
        lblTitel.text = ""
    }

    private fun updatePopupMenuState() {
        val hasFilm = currentFilm != null
        val hasEntry = hasFilm || currentAudioEntry != null
        editDescriptionItem.isVisible = hasFilm
        editSeparator.isVisible = hasFilm
        editDescriptionItem.isEnabled = hasFilm
        copyDescriptionItem.isEnabled = hasEntry
        copyBaseInfoItem.isEnabled = hasEntry
    }

    private val currentDescription: String
        get() = currentFilm?.description ?: currentAudioEntry?.description.orEmpty()

    private val currentBaseInfo: String
        get() = currentFilm?.let { "${it.sender} - ${it.thema} - ${it.title}" }
            ?: currentAudioEntry?.let { "${it.channel} - ${it.theme} - ${it.title}" }
            .orEmpty()

    private fun showFilmDescription(film: DatenFilm) {
        setDescriptionPopupMenuEnabled(true)
        lblThema.text = film.thema
        lblTitel.text = film.title

        try {
            showHyperlink(film.websiteUrl, JPopupMenu().apply { add(CopyToClipboardAction(film.websiteUrl)) })
        } catch (_: Exception) {
            hyperlink.text = "Link nicht verfügbar"
            hyperlink.isVisible = true
            hyperlink.toolTipText = film.websiteUrl
            hyperlink.componentPopupMenu = null
        }

        textArea.text = film.description
        SwingUtilities.invokeLater { scrollPane1.verticalScrollBar.value = 0 }
        lblIcon.setSender(film.sender)
    }

    private fun showAudioDescription(entry: AudioEntry) {
        setDescriptionPopupMenuEnabled(false)
        lblThema.text = entry.theme
        lblTitel.text = entry.title.ifBlank { "(ohne Titel)" }

        val websiteUrl = entry.websiteUrl
        if (websiteUrl != null) {
            val websiteUrlText = websiteUrl.toString()
            showHyperlink(websiteUrlText, JPopupMenu().apply { add(CopyToClipboardAction(websiteUrlText)) })
        } else {
            resetHyperlink()
        }

        val description = entry.description
        textArea.text = description.ifBlank { "Keine Beschreibung vorhanden." }
        SwingUtilities.invokeLater { scrollPane1.verticalScrollBar.value = 0 }
        lblIcon.setSender(entry.channel)
    }

    private fun setDescriptionPopupMenuEnabled(enabled: Boolean) {
        val popup = popupMenu.takeIf { enabled }
        componentPopupMenu = popup
        textArea.componentPopupMenu = popup
    }

    private fun showHyperlink(url: String, popupMenu: JPopupMenu) {
        hyperlink.isVisible = true
        hyperlink.text = "Link zur Webseite"
        hyperlink.isClicked = false
        hyperlink.toolTipText = url
        hyperlink.componentPopupMenu = popupMenu
    }

    private fun resetHyperlink() {
        hyperlink.isVisible = false
        hyperlink.text = ""
        hyperlink.toolTipText = ""
        hyperlink.componentPopupMenu = null
    }
}
