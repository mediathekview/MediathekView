/*
 * Copyright (c) 2025 derreisende77.
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

package mediathek.gui.tabs.tab_livestreams

import mediathek.gui.tabs.tab_livestreams.icons.SvgSenderIconLabel
import net.miginfocom.layout.AC
import net.miginfocom.layout.CC
import net.miginfocom.layout.LC
import net.miginfocom.swing.MigLayout
import org.apache.commons.lang3.SystemUtils
import java.awt.Color
import java.awt.Dimension
import java.awt.Font
import javax.swing.JLabel
import javax.swing.JPanel
import javax.swing.JProgressBar

class ListCell : JPanel() {

    val lblSender = SvgSenderIconLabel()
    val lblTitle = JLabel()
    private val lblSubtitle = JLabel()
    val lblZeitraum = JLabel()
    val progressBar = JProgressBar()

    private fun getCellHeight(): Dimension {
        return if (SystemUtils.IS_OS_WINDOWS) {
            Dimension(215, 90)
        } else if (SystemUtils.IS_OS_MAC_OSX) {
            Dimension(215, 115)
        } else if (SystemUtils.IS_OS_UNIX) {
            Dimension(215, 120)
        } else {
            throw IllegalStateException("Unknown OS")
        }
    }

    init {
        isOpaque = false
        val size = getCellHeight()
        preferredSize = size
        minimumSize = size
        maximumSize = size

        layout = MigLayout(
            LC().insets("0").hideMode(3),
            AC().gap().fill().grow(),
            AC().gap().fill().grow()
        )

        lblSender.horizontalAlignment = JLabel.CENTER
        lblSender.verticalAlignment = JLabel.TOP

        add(
            lblSender, CC()
                .cell(0, 0)
                .alignY("top")
                .growY()
                .minWidth("pref")
                .minHeight("pref")
                .gapAfter("5px")
        )

        val panel = JPanel(MigLayout(LC().hideMode(3), AC().fill().grow(), AC().gap().gap().gap()))
        panel.isOpaque = false

        lblTitle.font = lblTitle.font.deriveFont(lblTitle.font.style or Font.BOLD)
        panel.add(lblTitle, CC().cell(0, 0))
        panel.add(lblSubtitle, CC().cell(0, 1))
        panel.add(lblZeitraum, CC().cell(0, 2))
        panel.add(progressBar, CC().cell(0, 3).growX())

        add(panel, CC().cell(1, 0).growX().growY())
    }

    fun setSubtitle(title: String) {
        if (title.isNotEmpty()) {
            lblSubtitle.text = title
            lblSubtitle.isVisible = true
        } else {
            lblSubtitle.isVisible = false
            lblSubtitle.text = title
        }
    }

    fun setSubtitleForegroundColor(color: Color) {
        lblSubtitle.foreground = color
    }
}
