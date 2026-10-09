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

package mediathek.gui.filmInformation

import javax.swing.JLabel

class HtmlMultilineLabel : JLabel() {
    override fun setText(text: String?) {
        super.setText(prepareString(text))
    }

    private fun prepareString(text: String?): String {
        val value = text.orEmpty()
        val prefix = if (value.startsWith(HTML_START)) "" else HTML_START
        val suffix = if (value.endsWith(HTML_END)) "" else HTML_END
        return prefix + value + suffix
    }

    private companion object {
        const val HTML_START = "<html>"
        const val HTML_END = "</html>"
    }
}
