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

package mediathek.tool.cellrenderer

import mediathek.config.application.ApplicationConfiguration
import mediathek.daten.DatenFilm
import mediathek.tool.models.FilmColumn
import org.junit.jupiter.api.Assertions.assertNotNull
import org.junit.jupiter.api.Assertions.assertNull
import org.junit.jupiter.api.Test
import javax.swing.JTable
import javax.swing.table.DefaultTableModel

class CellRendererBaseWithStartTest {
    @Test
    fun `geo indicator moves to title when geo column is hidden`() {
        val film = DatenFilm().apply {
            markGeoBlockedForLocation(ApplicationConfiguration.getInstance().geographicLocation)
        }

        assertIndicatorFollowsColumnVisibility(FilmColumn.GEO, film)
    }

    @Test
    fun `high quality indicator moves to title when HQ column is hidden`() {
        val film = DatenFilm().apply {
            highQualityUrl = "https://example.org/video-high.mp4"
        }

        assertIndicatorFollowsColumnVisibility(FilmColumn.HIGH_QUALITY, film)
    }

    @Test
    fun `subtitle indicator moves to title when UT column is hidden`() {
        val film = DatenFilm().apply {
            subtitleUrl = "https://example.org/subtitle.vtt"
        }

        assertIndicatorFollowsColumnVisibility(FilmColumn.SUBTITLE, film)
    }

    private fun assertIndicatorFollowsColumnVisibility(indicatorColumn: FilmColumn, film: DatenFilm) {
        val table = JTable(DefaultTableModel(0, FilmColumn.entries.size))
        val renderer = TestRenderer()

        setColumnVisible(table, indicatorColumn, visible = true)
        renderer.renderIndicators(table, film)
        assertNull(renderer.icon)

        setColumnVisible(table, indicatorColumn, visible = false)
        renderer.renderIndicators(table, film)
        assertNotNull(renderer.icon)

        setColumnVisible(table, indicatorColumn, visible = true)
        renderer.renderIndicators(table, film)
        assertNull(renderer.icon)
    }

    private fun setColumnVisible(table: JTable, filmColumn: FilmColumn, visible: Boolean) {
        table.columnModel.getColumn(filmColumn.index).apply {
            if (visible) {
                minWidth = 15
                maxWidth = 3000
                preferredWidth = 75
                width = 75
            } else {
                minWidth = 0
                maxWidth = 0
                preferredWidth = 0
                width = 0
            }
        }
    }

    private class TestRenderer : CellRendererBaseWithStart() {
        fun renderIndicators(table: JTable, film: DatenFilm) {
            resetComponent()
            setIndicatorIcons(table, film, isSelected = false)
        }
    }
}
