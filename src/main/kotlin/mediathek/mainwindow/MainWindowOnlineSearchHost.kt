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

import mediathek.daten.DatenFilm
import mediathek.gui.actions.UrlHyperlinkAction
import mediathek.gui.tabs.tab_film.startDownloads
import mediathek.gui.tabs.tab_online_search.OnlineSearchFilmAdapter
import mediathek.gui.tabs.tab_online_search.OnlineSearchHost
import mediathek.gui.tabs.tab_online_search.OnlineSearchResult
import java.util.function.Consumer
import javax.swing.JFrame

class MainWindowOnlineSearchHost(
    private val ownerFrame: JFrame,
    private val updateCurrentFilm: Consumer<DatenFilm?>,
    private val showFilmInfoAction: Runnable,
) : OnlineSearchHost {
    override fun updateCurrentResult(result: OnlineSearchResult?) {
        updateCurrentFilm.accept(result?.toDatenFilm())
    }

    override fun showFilmInfo(result: OnlineSearchResult) {
        updateCurrentResult(result)
        showFilmInfoAction.run()
    }

    override fun startDownload(results: List<OnlineSearchResult>) {
        startDownloads(
            ownerFrame,
            results.map { it.toDatenFilm() },
            null,
            null,
        )
    }

    override fun playResult(result: OnlineSearchResult) {
        UrlHyperlinkAction.openURL(result.normalQualityUrl)
    }

    private fun OnlineSearchResult.toDatenFilm(): DatenFilm =
        OnlineSearchFilmAdapter.toDatenFilm(this)
}
