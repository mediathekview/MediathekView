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

import mediathek.config.Daten
import mediathek.config.Konstanten
import mediathek.controller.starter.DownloadStartActions
import mediathek.daten.*
import mediathek.gui.dialog.MissingProgramSetDialog
import mediathek.gui.dialog.add_download.DialogAddDownload
import mediathek.gui.dialog.add_download.DialogAddMoreDownload
import mediathek.gui.messages.DownloadListChangedEvent
import mediathek.tool.MessageBus
import java.util.*
import javax.swing.JFrame
import javax.swing.JOptionPane

fun startDownloads(
    parent: JFrame,
    films: List<DatenFilm>,
    pSet: DatenPset?,
    requestedResolution: FilmResolution.Enum?,
) {
    if (films.isEmpty()) {
        return
    }

    if (!Daten.getInstance().listePset.hasDownloadProgramSet()) {
        MissingProgramSetDialog.showMissingDownloadProgramSet(parent)
        return
    }

    val effectiveProgramSet = pSet ?: Daten.getInstance().listePset.listeSpeichern.first()
    val downloadsList = Daten.getInstance().listeDownloads

    if (films.size > 1) {
        val dialog = DialogAddMoreDownload(parent, effectiveProgramSet)
        val result = dialog.showDialog()
        if (dialog.wasCancelled()) {
            return
        }

        for (film in films) {
            if (downloadsList.getDownloadUrlFilm(film.urlNormalQuality) != null && !confirmDuplicateDownload(parent)) {
                continue
            }

            if (result.addAllWithDefaults()) {
                val datenDownload = DatenDownload(
                    effectiveProgramSet,
                    film,
                    DownloadSource.DOWNLOAD,
                    null,
                    "",
                    result.path(),
                    "",
                    result.info(),
                    result.subtitle(),
                )
                downloadsList.addMitNummer(datenDownload)
                MessageBus.messageBus.publishAsync(DownloadListChangedEvent())
                if (result.startImmediately()) {
                    DownloadStartActions.start(datenDownload)
                }
            } else {
                showSingleDownloadDialog(parent, film, effectiveProgramSet, requestedResolution)
            }
        }

        return
    }

    val film = films.first()
    if (downloadsList.getDownloadUrlFilm(film.urlNormalQuality) != null && !confirmDuplicateDownload(parent)) {
        return
    }

    showSingleDownloadDialog(parent, film, effectiveProgramSet, requestedResolution)
}

private fun confirmDuplicateDownload(parent: JFrame): Boolean {
    return JOptionPane.showConfirmDialog(
        parent,
        "Download für den Film existiert bereits.\nNochmal anlegen?",
        Konstanten.PROGRAMMNAME,
        JOptionPane.YES_NO_OPTION,
    ) == JOptionPane.YES_OPTION
}

private fun showSingleDownloadDialog(
    parent: JFrame,
    datenFilm: DatenFilm,
    pSet: DatenPset,
    requestedResolution: FilmResolution.Enum?,
) {
    DialogAddDownload(parent, datenFilm, pSet, Optional.ofNullable(requestedResolution)).isVisible = true
}
