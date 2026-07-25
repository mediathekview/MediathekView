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

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.gui.TableFormat
import ca.odell.glazedlists.swing.eventTableModel
import mediathek.config.application.ApplicationConfiguration
import mediathek.daten.DatenFilm
import mediathek.gui.messages.SenderIconStyleChangedEvent
import mediathek.gui.tabs.tab_film.table.FilmTableAppearance
import mediathek.tool.MessageBus
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.CsvSource
import java.awt.Color
import java.awt.Dimension
import java.awt.Rectangle
import java.awt.image.BufferedImage
import javax.swing.JTable

class ArteLocaleBadgeTest {
    @ParameterizedTest
    @CsvSource(
        "ARTE.DE, DE",
        "arte.en, EN",
        "ARTE.ES, ES",
        "arte.fr, FR",
        "ARTE.IT, IT",
        "arte.pl, PL",
    )
    fun `localized ARTE sender provides uppercase locale code`(sender: String, expectedLocale: String) {
        assertEquals(expectedLocale, ArteLocaleBadge.localeCode(sender))
    }

    @Test
    fun `plain and malformed ARTE senders do not provide locale code`() {
        assertNull(ArteLocaleBadge.localeCode("ARTE"))
        assertNull(ArteLocaleBadge.localeCode("ARTE.DEU"))
        assertNull(ArteLocaleBadge.localeCode("OTHER.DE"))
    }

    @Test
    fun `badge is painted at upper right of icon bounds`() {
        val image = BufferedImage(80, 30, BufferedImage.TYPE_INT_ARGB)
        val iconBounds = Rectangle(10, 5, 60, 20)
        val graphics = image.createGraphics()
        graphics.color = SOLID_COLOR
        graphics.fillRect(iconBounds.x, iconBounds.y, iconBounds.width, iconBounds.height)

        val badgeBounds = try {
            ArteLocaleBadge.paint(graphics, "DE", iconBounds)
        } finally {
            graphics.dispose()
        }

        assertEquals(iconBounds.x + iconBounds.width, badgeBounds.x + badgeBounds.width)
        assertEquals(iconBounds.y, badgeBounds.y)
        val solidRgb = SOLID_COLOR.rgb
        val badgeContainsPaint = (badgeBounds.x until badgeBounds.x + badgeBounds.width).any { x ->
            (badgeBounds.y until badgeBounds.y + badgeBounds.height).any { y -> image.getRGB(x, y) != solidRgb }
        }
        assertTrue(badgeContainsPaint)
        assertEquals(solidRgb, image.getRGB(iconBounds.x + 1, iconBounds.y + iconBounds.height - 2))
        assertNotEquals(solidRgb, image.getRGB(badgeBounds.x, badgeBounds.y))
    }

    @Test
    fun `oversized icon anchors badge inside visible cell bounds`() {
        val iconBounds = Rectangle(20, -18, 60, 60)
        val cellBounds = Rectangle(0, 0, 100, 24)

        val visibleIconBounds = ArteLocaleBadge.visibleIconBounds(iconBounds, cellBounds)

        assertEquals(Rectangle(20, 0, 60, 24), visibleIconBounds)
        val image = BufferedImage(cellBounds.width, cellBounds.height, BufferedImage.TYPE_INT_ARGB)
        val graphics = image.createGraphics()
        val badgeBounds = try {
            ArteLocaleBadge.paint(graphics, "DE", visibleIconBounds)
        } finally {
            graphics.dispose()
        }
        assertEquals(0, badgeBounds.y)
        assertTrue(cellBounds.contains(badgeBounds))
    }

    @Test
    fun `film sender renderer paints localized ARTE differently from plain ARTE`() {
        val configuration = ApplicationConfiguration.getInstance()
        val previousLocalSenderIcons = configuration.localSenderIcons
        try {
            configuration.localSenderIcons = true
            MessageBus.messageBus.publish(SenderIconStyleChangedEvent())

            val localizedImage = renderSenderCell("ARTE.DE")
            val plainImage = renderSenderCell("ARTE")

            val differingPixels = (0 until localizedImage.width).sumOf { x ->
                (0 until localizedImage.height).count { y ->
                    localizedImage.getRGB(x, y) != plainImage.getRGB(x, y)
                }
            }
            assertTrue(differingPixels > 20, "Expected the localized badge to change rendered cell pixels")
        } finally {
            configuration.localSenderIcons = previousLocalSenderIcons
            MessageBus.messageBus.publish(SenderIconStyleChangedEvent())
        }
    }

    private fun renderSenderCell(sender: String): BufferedImage {
        val film = DatenFilm().apply { this.sender = sender }
        val films = BasicEventList<DatenFilm>().apply { add(film) }
        val model = films.eventTableModel(SENDER_TABLE_FORMAT)
        try {
            val table = JTable(model).apply {
                rowHeight = CELL_HEIGHT
                columnModel.getColumn(0).width = CELL_WIDTH
            }
            val renderer = FilmSenderCellRenderer(FilmTableAppearance(false, true, false))
            renderer.getTableCellRendererComponent(table, sender, false, false, 0, 0)
            renderer.size = Dimension(CELL_WIDTH, CELL_HEIGHT)
            val image = BufferedImage(CELL_WIDTH, CELL_HEIGHT, BufferedImage.TYPE_INT_ARGB)
            val graphics = image.createGraphics()
            try {
                renderer.paint(graphics)
            } finally {
                graphics.dispose()
            }
            return image
        } finally {
            model.dispose()
            films.dispose()
        }
    }

    private companion object {
        const val CELL_WIDTH = 100
        const val CELL_HEIGHT = 30
        val SOLID_COLOR = Color.MAGENTA
        val SENDER_TABLE_FORMAT = object : TableFormat<DatenFilm> {
            override fun getColumnCount(): Int = 1

            override fun getColumnName(column: Int): String = "Sender"

            override fun getColumnValue(baseObject: DatenFilm, column: Int): Any = baseObject.sender
        }
    }
}
