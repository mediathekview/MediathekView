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

package mediathek.gui.tabs.tab_film.context

import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.CancellationException
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.launch
import kotlinx.coroutines.withContext
import mediathek.config.Daten
import mediathek.config.Konstanten
import mediathek.config.StandardLocations
import mediathek.daten.DatenFilm
import mediathek.filmlisten.writer.FilmListWriter
import mediathek.gui.duplicates.details.DuplicateFilmDetailsDialog
import mediathek.mainwindow.MediathekGui
import mediathek.tool.FileDialogs
import mediathek.tool.MVInfoFile
import org.apache.logging.log4j.LogManager
import java.util.ArrayList
import javax.swing.JMenuItem
import javax.swing.JOptionPane
import javax.swing.JPopupMenu

class FilmFileAndDuplicateContextActions(
    private val host: TableContextMenuHandler.Host,
    private val daten: Daten,
    private val uiScope: CoroutineScope,
) {
    fun addActions(popupMenu: JPopupMenu, film: DatenFilm) {
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
                    ?: return@addActionListener

                isEnabled = false
                uiScope.launch {
                    try {
                        withContext(Dispatchers.IO) {
                            MVInfoFile().writeManualInfoFile(film, file.toPath())
                        }
                    } catch (ex: CancellationException) {
                        throw ex
                    } catch (ex: Exception) {
                        logger.error("Could not write info file.", ex)
                        JOptionPane.showMessageDialog(
                            host.gui(),
                            "Infodatei konnte nicht geschrieben werden.",
                            Konstanten.PROGRAMMNAME,
                            JOptionPane.ERROR_MESSAGE,
                        )
                    } finally {
                        isEnabled = true
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

    private companion object {
        private val logger = LogManager.getLogger()
    }
}
