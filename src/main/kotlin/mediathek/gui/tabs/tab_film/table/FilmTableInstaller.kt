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

import mediathek.controller.starter.DownloadServices
import mediathek.daten.FilmResolution
import mediathek.gui.tabs.tab_film.actions.CopyUrlToClipboardAction
import mediathek.gui.tabs.tab_film.actions.FilmActionHost
import mediathek.gui.tabs.tab_film.actions.FilmUiActions
import mediathek.gui.tabs.tab_film.context.TableContextMenuHandler
import mediathek.tool.GuiFunktionen
import mediathek.tool.cellrenderer.*
import mediathek.tool.datum.DatumFilm
import mediathek.tool.models.FilmColumn
import org.pushingpixels.radiance.swing.ktx.addDelayedComponentListener
import java.awt.Component
import java.awt.event.KeyEvent
import javax.swing.*

class FilmTableInstaller(private val host: Host) {
    interface Host {
        fun table(): JTable
        fun downloads(): DownloadServices
        fun filmListScrollPane(): JScrollPane
        fun ownerComponent(): Component
        fun tableContextMenuHost(): TableContextMenuHandler.Host
        fun filmActionHost(): FilmActionHost
        fun actions(): FilmUiActions
        fun updateSelectedListItemsCount()
        fun onComponentShown()
        fun updateFilmData()
        fun selectionUpdatesSuspended(): Boolean
        fun saveTableConfiguration()
        fun appearance(): FilmTableAppearance
    }

    fun writeTableConfigurationData() {
        host.saveTableConfiguration()
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

        host.ownerComponent().addDelayedComponentListener(
            onComponentShown = {
                host.updateSelectedListItemsCount()
                host.onComponentShown()
            }
        )
    }

    fun setupTable() {
        setupKeyMapping()

        host.table().addMouseListener(TableContextMenuHandler(host.tableContextMenuHost()))
        host.table().selectionModel.addListSelectionListener { event ->
            val model = event.source as ListSelectionModel
            if (!model.isSelectionEmpty && !model.valueIsAdjusting && !host.selectionUpdatesSuspended()) {
                host.updateFilmData()
            }
        }

        setupCellRenderer()

        if (host.table().rowCount > 0) {
            host.table().setRowSelectionInterval(0, 0)
        }
    }

    private fun setupKeyMapping() {
        val focusedInputMap = host.table().inputMap

        focusedInputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_P, 0), ACTION_MAP_KEY_PLAY_FILM)
        focusedInputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0), ACTION_MAP_KEY_PLAY_FILM)
        focusedInputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_D, 0), ACTION_MAP_KEY_SAVE_FILM)
        focusedInputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_B, 0), ACTION_MAP_KEY_BOOKMARK_FILM)
        focusedInputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_H, 0), ACTION_MAP_KEY_COPY_HD_URL)
        focusedInputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_N, 0), ACTION_MAP_KEY_COPY_NORMAL_URL)
        focusedInputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_K, 0), ACTION_MAP_KEY_COPY_KLEIN_URL)
        focusedInputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_G, 0), ACTION_MAP_KEY_MARK_SEEN)
        focusedInputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_U, 0), ACTION_MAP_KEY_MARK_UNSEEN)
        installFilmUrlCopyAccelerators(focusedInputMap, GuiFunktionen.getPlatformControlKey())

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
        val table = host.table()
        val appearance = host.appearance()
        val textRenderer = FilmTextCellRenderer(appearance)
        table.setDefaultRenderer(Any::class.java, textRenderer)
        table.setDefaultRenderer(DatumFilm::class.java, textRenderer)
        table.setDefaultRenderer(Int::class.javaObjectType, textRenderer)

        val formattedValueRenderer = FilmFormattedValueCellRenderer(appearance)
        val actionRenderer = FilmActionCellRenderer(host.downloads(), appearance)
        val specializedRenderers = mapOf(
            FilmColumn.SENDER to FilmSenderCellRenderer(appearance),
            FilmColumn.TITLE to FilmTitleCellRenderer(appearance),
            FilmColumn.PLAY to actionRenderer,
            FilmColumn.SAVE to actionRenderer,
            FilmColumn.BOOKMARK to actionRenderer,
            FilmColumn.TIME to formattedValueRenderer,
            FilmColumn.DURATION to formattedValueRenderer,
            FilmColumn.SIZE to formattedValueRenderer,
            FilmColumn.GEO to FilmGeoCellRenderer(appearance),
        )

        for (viewColumn in 0 until table.columnModel.columnCount) {
            val tableColumn = table.columnModel.getColumn(viewColumn)
            tableColumn.cellRenderer = specializedRenderers[FilmColumn.fromIndex(tableColumn.modelIndex)]
        }
    }

    private companion object {
        private const val ACTION_MAP_KEY_PLAY_FILM = "film_abspielen"
        private const val ACTION_MAP_KEY_SAVE_FILM = "download_film"
        private const val ACTION_MAP_KEY_BOOKMARK_FILM = "bookmark_film"
        private const val ACTION_MAP_KEY_COPY_KLEIN_URL = "copy_url_klein"
        private const val ACTION_MAP_KEY_MARK_SEEN = "seen"
        private const val ACTION_MAP_KEY_MARK_UNSEEN = "unseen"
    }
}

private const val ACTION_MAP_KEY_COPY_NORMAL_URL = "copy_url"
private const val ACTION_MAP_KEY_COPY_HD_URL = "copy_url_hd"

internal fun installFilmUrlCopyAccelerators(inputMap: InputMap, platformControlKey: Int) {
    val modifiers = platformControlKey or KeyEvent.SHIFT_DOWN_MASK or KeyEvent.ALT_DOWN_MASK
    inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_H, modifiers), ACTION_MAP_KEY_COPY_HD_URL)
    inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_N, modifiers), ACTION_MAP_KEY_COPY_NORMAL_URL)
}
