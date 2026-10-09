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

package mediathek.config

import mediathek.controller.IoXmlSchreiben
import mediathek.controller.starter.DownloadServices
import mediathek.daten.DatenPset
import mediathek.daten.ProgramSetRepository
import mediathek.daten.abo.AboServices
import mediathek.daten.blacklist.BlacklistServices
import mediathek.daten.watchlist.WatchlistServices
import mediathek.filmlisten.FilmCatalog
import mediathek.filmlisten.FilmListLoadCoordinator
import mediathek.gui.bookmark.BookmarkServices
import mediathek.gui.dialog.MissingProgramSetDialog
import mediathek.tool.GuiFunktionenProgramme
import mediathek.tool.ReplacementRules
import mediathek.tool.notification.NotificationService
import java.util.function.BiConsumer

class Daten {
    val programSets: ProgramSetRepository = ProgramSetRepository()
    val filmCatalog: FilmCatalog = FilmCatalog()
    val blacklist: BlacklistServices = BlacklistServices(filmCatalog)
    val bookmarks: BookmarkServices = BookmarkServices(filmCatalog.allFilms)
    val abos: AboServices = AboServices(filmCatalog.allFilms)
    val replacementRules: ReplacementRules = ReplacementRules()
    val notifications: NotificationService = NotificationService()
    val watchlist: WatchlistServices = WatchlistServices(filmCatalog.allFilms, notifications)
    val filmListLoader: FilmListLoadCoordinator = FilmListLoadCoordinator(filmCatalog, abos, blacklist)
    val downloads: DownloadServices = DownloadServices(
        filmCatalog,
        programSets,
        abos,
        blacklist,
        replacementRules,
        notifications,
        showMissingAboProgramSet = { parent ->
            MissingProgramSetDialog.showMissingAboProgramSet(parent, programSets) { importParent, standardSets ->
                GuiFunktionenProgramme.addSetVorlagen(
                    importParent,
                    programSets,
                    standardSets,
                    true,
                    programSetExporter(),
                )
            }
        },
    )

    val configurationPersistence: DatenConfigurationPersistence = DatenConfigurationPersistence(
        programSets = programSets,
        downloads = downloads,
        blacklist = blacklist,
        abos = abos,
        bookmarks = bookmarks,
        replacementRules = replacementRules,
    )

    private fun programSetExporter(): BiConsumer<Array<DatenPset>, String> =
        BiConsumer { programSets, target ->
            IoXmlSchreiben(DatenXmlConfigDataFactory.from(this)).exportPset(programSets, target)
        }
}
