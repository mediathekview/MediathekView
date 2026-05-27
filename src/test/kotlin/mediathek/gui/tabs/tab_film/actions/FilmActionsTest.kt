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

package mediathek.gui.tabs.tab_film.actions

import mediathek.daten.DatenFilm
import mediathek.daten.DatenPset
import mediathek.gui.bookmark.BookmarkData
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import java.util.Optional

internal class FilmActionsTest {
    @Test
    fun bookmarkAddOnlyUpdatesUnbookmarkedNonLivestreamFilms() {
        val eligible = film()
        val alreadyBookmarked = film(bookmarked = true)
        val livestream = film(livestream = true)
        val host = TestFilmActionHost(listOf(eligible, alreadyBookmarked, livestream))

        BookmarkAddFilmAction(host).actionPerformed(null)

        assertEquals(listOf(eligible), host.updatedFilms)
    }

    @Test
    fun bookmarkAddDoesNotUpdateWhenNoFilmIsEligible() {
        val host = TestFilmActionHost(listOf(film(bookmarked = true), film(livestream = true)))

        BookmarkAddFilmAction(host).actionPerformed(null)

        assertTrue(host.updatedFilms.isEmpty())
    }

    @Test
    fun bookmarkRemoveOnlyUpdatesBookmarkedFilms() {
        val bookmarked = film(bookmarked = true)
        val unbookmarked = film()
        val host = TestFilmActionHost(listOf(bookmarked, unbookmarked))

        BookmarkRemoveFilmAction(host).actionPerformed(null)

        assertEquals(listOf(bookmarked), host.updatedFilms)
    }

    @Test
    fun bookmarkRemoveDoesNotUpdateWhenNoFilmIsBookmarked() {
        val host = TestFilmActionHost(listOf(film(), film(livestream = true)))

        BookmarkRemoveFilmAction(host).actionPerformed(null)

        assertTrue(host.updatedFilms.isEmpty())
    }

    private class TestFilmActionHost(
        private val selectedFilms: List<DatenFilm>,
    ) : FilmActionHost {
        val updatedFilms = mutableListOf<DatenFilm>()

        override fun saveFilm(pSet: DatenPset?) = Unit

        override fun selectedFilms(): List<DatenFilm> = selectedFilms

        override fun updateBookmarkListAndRefresh(films: List<DatenFilm>) {
            updatedFilms.addAll(films)
        }

        override fun currentlySelectedFilm(): Optional<DatenFilm> = Optional.empty()

        override fun toggleFilterDialogVisibility() = Unit
    }

    private companion object {
        fun film(bookmarked: Boolean = false, livestream: Boolean = false): DatenFilm =
            DatenFilm().apply {
                sender = "sender"
                thema = "thema"
                title = "title"
                setNormalQualityUrl("https://example.org/${hashCode()}.mp4")
                isLivestream = livestream
                if (bookmarked) {
                    bookmark = BookmarkData(this)
                }
            }
    }
}
