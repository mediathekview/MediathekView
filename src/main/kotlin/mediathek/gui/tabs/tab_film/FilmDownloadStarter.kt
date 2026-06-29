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

import mediathek.config.Konstanten
import mediathek.controller.starter.DownloadServices
import mediathek.controller.starter.DownloadStartActions
import mediathek.daten.*
import mediathek.gui.dialog.MissingProgramSetDialog
import mediathek.gui.dialog.add_download.DialogAddMoreDownload
import mediathek.gui.messages.DownloadListChangedEvent
import mediathek.tool.GuiFunktionenProgramme
import mediathek.tool.MessageBus
import java.util.function.BiConsumer
import javax.swing.JFrame
import javax.swing.JOptionPane

fun startDownloads(
    programSets: ProgramSetRepository,
    downloads: DownloadServices,
    parent: JFrame,
    films: List<DatenFilm>,
    pSet: DatenPset?,
    requestedResolution: FilmResolution.Enum?,
    programSetExporter: BiConsumer<Array<DatenPset>, String>,
    showSingleDownloadDialog: (DatenFilm, DatenPset, FilmResolution.Enum?) -> Unit,
) {
    if (films.isEmpty()) {
        return
    }

    if (!programSets.list.hasDownloadProgramSet()) {
        MissingProgramSetDialog.showMissingDownloadProgramSet(parent, programSets) { importParent, standardSets ->
            GuiFunktionenProgramme.addSetVorlagen(
                importParent,
                programSets,
                standardSets,
                true,
                programSetExporter,
            )
        }
        return
    }

    val effectiveProgramSet = pSet ?: programSets.list.listeSpeichern.first()

    if (films.size > 1) {
        val dialog = DialogAddMoreDownload(parent, effectiveProgramSet)
        val result = dialog.showDialog()
        if (dialog.wasCancelled()) {
            return
        }

        for (film in films) {
            if (downloads.findDownloadByFilmUrl(film.urlNormalQuality) != null && !confirmDuplicateDownload(parent)) {
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
                downloads.addDownload(datenDownload)
                MessageBus.messageBus.publishAsync(DownloadListChangedEvent())
                if (result.startImmediately()) {
                    DownloadStartActions.start(datenDownload)
                }
            } else {
                showSingleDownloadDialog(film, effectiveProgramSet, requestedResolution)
            }
        }

        return
    }

    val film = films.first()
    if (downloads.findDownloadByFilmUrl(film.urlNormalQuality) != null && !confirmDuplicateDownload(parent)) {
        return
    }

    showSingleDownloadDialog(film, effectiveProgramSet, requestedResolution)
}

private fun confirmDuplicateDownload(parent: JFrame): Boolean {
    return JOptionPane.showConfirmDialog(
        parent,
        "Download für den Film existiert bereits.\nNochmal anlegen?",
        Konstanten.PROGRAMMNAME,
        JOptionPane.YES_NO_OPTION,
    ) == JOptionPane.YES_OPTION
}
