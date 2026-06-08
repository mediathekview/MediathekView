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

package mediathek.gui.tabs.tab_film.table

import mediathek.config.MVConfig
import mediathek.daten.DatenFilm
import mediathek.daten.FilmResolution
import mediathek.gui.tabs.tab_film.actions.CopyUrlToClipboardAction
import mediathek.gui.tabs.tab_film.actions.FilmActionHost
import mediathek.gui.tabs.tab_film.actions.FilmUiActions
import mediathek.gui.tabs.tab_film.context.TableContextMenuHandler
import mediathek.tool.cellrenderer.CellRendererFilme
import mediathek.tool.datum.DatumFilm
import mediathek.tool.listener.BeobTableHeader
import mediathek.tool.models.TModelFilm
import mediathek.tool.table.MVFilmTable
import java.awt.Component
import java.awt.event.ComponentAdapter
import java.awt.event.ComponentEvent
import java.awt.event.KeyEvent
import javax.swing.JScrollPane
import javax.swing.KeyStroke
import javax.swing.ListSelectionModel

class FilmTableInstaller(private val host: Host) {
    interface Host {
        fun table(): MVFilmTable
        fun filmListScrollPane(): JScrollPane
        fun ownerComponent(): Component
        fun tableContextMenuHost(): TableContextMenuHandler.Host
        fun filmActionHost(): FilmActionHost
        fun actions(): FilmUiActions
        fun updateSelectedListItemsCount()
        fun onComponentShown()
        fun updateFilmData()
        fun selectionUpdatesSuspended(): Boolean
    }

    fun writeTableConfigurationData() {
        host.table().writeTableConfigurationData()
    }

    fun setupFilmListTable() {
        host.filmListScrollPane().setViewportView(host.table())
    }

    fun setupFilmSelectionPropertyListener() {
        host.table().selectionModel.addListSelectionListener { event ->
            if (!event.valueIsAdjusting) {
                host.updateSelectedListItemsCount()
                val selectedRows = host.table().selectedRowCount
                host.actions().playFilm.isEnabled = selectedRows <= 1
            }
        }

        host.ownerComponent().addComponentListener(object : ComponentAdapter() {
            override fun componentShown(event: ComponentEvent) {
                host.updateSelectedListItemsCount()
                host.onComponentShown()
            }
        })
    }

    fun setupTable() {
        setupKeyMapping()

        host.table().model = TModelFilm()
        host.table().addMouseListener(TableContextMenuHandler(host.tableContextMenuHost()))
        host.table().selectionModel.addListSelectionListener { event ->
            val model = event.source as ListSelectionModel
            if (!model.isSelectionEmpty && !model.valueIsAdjusting && !host.selectionUpdatesSuspended()) {
                host.updateFilmData()
            }
        }

        setupCellRenderer()

        host.table().setLineBreak(MVConfig.getBoolean(MVConfig.Configs.SYSTEM_TAB_FILME_LINEBREAK))

        setupHeaderPopupMenu()

        host.table().readColumnConfigurationData()
        if (host.table().rowCount > 0) {
            host.table().setRowSelectionInterval(0, 0)
        }
    }

    private fun setupKeyMapping() {
        val focusedWindowMap = host.table().inputMap

        focusedWindowMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_P, 0), ACTION_MAP_KEY_PLAY_FILM)
        focusedWindowMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0), ACTION_MAP_KEY_PLAY_FILM)
        focusedWindowMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_D, 0), ACTION_MAP_KEY_SAVE_FILM)
        focusedWindowMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_B, 0), ACTION_MAP_KEY_BOOKMARK_FILM)
        focusedWindowMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_H, 0), ACTION_MAP_KEY_COPY_HD_URL)
        focusedWindowMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_N, 0), ACTION_MAP_KEY_COPY_NORMAL_URL)
        focusedWindowMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_K, 0), ACTION_MAP_KEY_COPY_KLEIN_URL)
        focusedWindowMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_G, 0), ACTION_MAP_KEY_MARK_SEEN)
        focusedWindowMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_U, 0), ACTION_MAP_KEY_MARK_UNSEEN)

        val actionMap = host.table().actionMap
        val actions = host.actions()
        actionMap.put(ACTION_MAP_KEY_PLAY_FILM, actions.playFilm)
        actionMap.put(ACTION_MAP_KEY_SAVE_FILM, actions.saveFilm)
        actionMap.put(ACTION_MAP_KEY_BOOKMARK_FILM, actions.bookmarkAddFilm)
        actionMap.put(ACTION_MAP_KEY_COPY_NORMAL_URL, actions.copyNormalUrlToClipboard)
        actionMap.put(ACTION_MAP_KEY_COPY_HD_URL, actions.copyHqUrlToClipboard)
        actionMap.put(
            ACTION_MAP_KEY_COPY_KLEIN_URL,
            CopyUrlToClipboardAction(host.filmActionHost(), FilmResolution.Enum.LOW)
        )
        actionMap.put(ACTION_MAP_KEY_MARK_SEEN, actions.markFilmAsSeen)
        actionMap.put(ACTION_MAP_KEY_MARK_UNSEEN, actions.markFilmAsUnseen)
    }

    private fun setupCellRenderer() {
        val cellRenderer = CellRendererFilme()
        host.table().setDefaultRenderer(Any::class.java, cellRenderer)
        host.table().setDefaultRenderer(DatumFilm::class.java, cellRenderer)
        host.table().setDefaultRenderer(Int::class.javaObjectType, cellRenderer)
    }

    private fun setupHeaderPopupMenu() {
        val headerListener = BeobTableHeader(
            host.table(),
            FilmColumnVisibility.store(),
            HIDDEN_COLUMNS,
            BUTTON_COLUMNS,
            true,
            MVConfig.Configs.SYSTEM_TAB_FILME_LINEBREAK
        )

        host.table().tableHeader.addMouseListener(headerListener)
    }

    private companion object {
        private const val ACTION_MAP_KEY_PLAY_FILM = "film_abspielen"
        private const val ACTION_MAP_KEY_SAVE_FILM = "download_film"
        private const val ACTION_MAP_KEY_BOOKMARK_FILM = "bookmark_film"
        private const val ACTION_MAP_KEY_COPY_NORMAL_URL = "copy_url"
        private const val ACTION_MAP_KEY_COPY_HD_URL = "copy_url_hd"
        private const val ACTION_MAP_KEY_COPY_KLEIN_URL = "copy_url_klein"
        private const val ACTION_MAP_KEY_MARK_SEEN = "seen"
        private const val ACTION_MAP_KEY_MARK_UNSEEN = "unseen"

        private val HIDDEN_COLUMNS = intArrayOf(
            DatenFilm.FILM_ABSPIELEN,
            DatenFilm.FILM_AUFZEICHNEN,
            DatenFilm.FILM_MERKEN
        )
        private val BUTTON_COLUMNS = intArrayOf(
            DatenFilm.FILM_ABSPIELEN,
            DatenFilm.FILM_AUFZEICHNEN,
            DatenFilm.FILM_MERKEN
        )
    }
}
