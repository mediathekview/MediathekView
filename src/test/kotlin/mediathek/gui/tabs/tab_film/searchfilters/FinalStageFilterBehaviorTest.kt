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

package mediathek.gui.tabs.tab_film.searchfilters

import mediathek.daten.DatenFilm
import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test

internal class FinalStageFilterNoPatternTest {
    @Test
    fun matchesWhenTopicContainsSearchTextCaseInsensitive() {
        val filter = FinalStageFilterNoPattern(arrayOf("sendung"))
        val film = film(thema = "Meine Sendung", title = "Folge 1")
        assertTrue(filter.test(film))
    }

    @Test
    fun matchesWhenTitleContainsSearchTextCaseInsensitive() {
        val filter = FinalStageFilterNoPattern(arrayOf("folge"))
        val film = film(thema = "Thema", title = "Große Folge")
        assertTrue(filter.test(film))
    }

    @Test
    fun doesNotMatchWhenNeitherTopicNorTitleContainsSearchText() {
        val filter = FinalStageFilterNoPattern(arrayOf("nichtvorhanden"))
        val film = film(thema = "Thema", title = "Titel")
        assertFalse(filter.test(film))
    }

    @Test
    fun matchesWhenSearchTextIsLowerCaseAndTopicIsMixedCase() {
        val filter = FinalStageFilterNoPattern(arrayOf("sendung"))
        val film = film(thema = "Meine SENDUNG", title = "Titel")
        assertTrue(filter.test(film))
    }

    @Test
    fun doesNotMatchWhenSearchTextIsSubstringButNotPresent() {
        val filter = FinalStageFilterNoPattern(arrayOf("xyz"))
        val film = film(thema = "Thema", title = "Titel")
        assertFalse(filter.test(film))
    }

    private fun film(thema: String, title: String) = DatenFilm().apply {
        this.thema = thema
        this.title = title
        urlNormalQuality = "https://example.invalid/test.mp4"
    }
}

internal class FinalStageFilterNoPatternWithDescriptionTest {
    @Test
    fun matchesWhenTopicContainsSearchText() {
        val filter = FinalStageFilterNoPatternWithDescription(arrayOf("sendung"))
        val film = film(thema = "Meine Sendung", title = "Folge 1", description = "")
        assertTrue(filter.test(film))
    }

    @Test
    fun matchesWhenTitleContainsSearchText() {
        val filter = FinalStageFilterNoPatternWithDescription(arrayOf("folge"))
        val film = film(thema = "Thema", title = "Große Folge", description = "")
        assertTrue(filter.test(film))
    }

    @Test
    fun matchesWhenDescriptionContainsSearchText() {
        val filter = FinalStageFilterNoPatternWithDescription(arrayOf("beschreibung"))
        val film = film(thema = "Thema", title = "Titel", description = "Eine Beschreibung hier")
        assertTrue(filter.test(film))
    }

    @Test
    fun doesNotMatchWhenDescriptionEmptyAndNoTopicOrTitleMatch() {
        val filter = FinalStageFilterNoPatternWithDescription(arrayOf("nichtvorhanden"))
        val film = film(thema = "Thema", title = "Titel", description = "")
        assertFalse(filter.test(film))
    }

    @Test
    fun matchesWhenSearchTextIsLowerCaseAndDescriptionIsMixedCase() {
        val filter = FinalStageFilterNoPatternWithDescription(arrayOf("beschreibung"))
        val film = film(thema = "Thema", title = "Titel", description = "Eine BESCHREIBUNG hier")
        assertTrue(filter.test(film))
    }

    @Test
    fun doesNotMatchWhenDescriptionIsNullAndNoTopicOrTitleMatch() {
        val filter = FinalStageFilterNoPatternWithDescription(arrayOf("nichtvorhanden"))
        val film = film(thema = "Thema", title = "Titel", description = null)
        assertFalse(filter.test(film))
    }

    private fun film(thema: String, title: String, description: String?) = DatenFilm().apply {
        this.thema = thema
        this.title = title
        if (description != null) this.description = description
        urlNormalQuality = "https://example.invalid/test.mp4"
    }
}
