/*
 * Copyright (c) 2025-2026 derreisende77.
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

package mediathek.gui.tabs.tab_livestreams.icons

import com.formdev.flatlaf.extras.FlatSVGIcon
import java.awt.Dimension
import java.awt.Font
import java.net.URL
import javax.swing.JLabel

class SvgSenderIconLabel : JLabel() {

    private var iconUrl: URL? = null
    private var iconSize = 24

    fun setSenderIcon(url: URL, boundingSize: Int = 24, regionLabel: String? = null) {
        iconUrl = url
        iconSize = boundingSize
        icon = iconUrl?.let {
            FlatSVGIcon(it).derive(iconSize, iconSize)
        }

        val text = regionLabel?.trim().orEmpty()
        if (text.isNotEmpty()) {
            this.text = text
            font = regionLabelFont()
            horizontalTextPosition = CENTER
            verticalTextPosition = BOTTOM
            iconTextGap = 2
        } else {
            this.text = ""
        }

        horizontalAlignment = CENTER
        verticalAlignment = CENTER
        revalidate()
        repaint()
    }

    override fun getPreferredSize(): Dimension {
        val metrics = getFontMetrics(regionLabelFont())
        val labelWidth = maxOf(iconSize, longestRegionLabelWidth(metrics))
        if (text.isBlank()) {
            return Dimension(labelWidth, iconSize)
        }

        val textHeight = metrics.height + iconTextGap
        return Dimension(labelWidth, iconSize + textHeight)
    }

    private fun longestRegionLabelWidth(metrics: java.awt.FontMetrics): Int {
        return REGION_LABELS.maxOf { metrics.stringWidth(it) }
    }

    private fun regionLabelFont(): Font = super.getFont().deriveFont(regionLabelFontSizePt)

    companion object {
        @JvmStatic
        var regionLabelFontSizePt: Float = 10f

        private val REGION_LABELS = listOf(
            "Sachsen",
            "Sachsen-Anhalt",
            "Thüringen",
            "Hamburg",
            "Mecklenburg-Vorpommern",
            "Schleswig-Holstein",
            "Niedersachsen",
            "Baden-Württemberg",
            "Rheinland-Pfalz",
            "Berlin",
            "Brandenburg"
        )
    }
}
