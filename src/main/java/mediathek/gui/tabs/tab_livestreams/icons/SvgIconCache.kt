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

package mediathek.gui.tabs.tab_livestreams.icons

import java.net.URL

object SvgIconCache {
    private val senderIconMap = mapOf(
        "arte" to SvgSenderIconLabel::class.java.getResource("/icons/sender/arte.svg"),
        "3sat" to SvgSenderIconLabel::class.java.getResource("/icons/sender/3sat.svg"),
        "ard-alpha" to SvgSenderIconLabel::class.java.getResource("/icons/sender/ard-alpha.svg"),
        "br nord" to SvgSenderIconLabel::class.java.getResource("/icons/sender/br.svg"),
        "br süd" to SvgSenderIconLabel::class.java.getResource("/icons/sender/br.svg"),
        "das erste" to SvgSenderIconLabel::class.java.getResource("/icons/sender/ard.svg"),
        "hr" to SvgSenderIconLabel::class.java.getResource("/icons/sender/hr.svg"),
        "kika" to SvgSenderIconLabel::class.java.getResource("/icons/sender/kika.svg"),
        "mdr sachsen" to SvgSenderIconLabel::class.java.getResource("/icons/sender/mdr.svg"),
        "mdr sachsen-anhalt" to SvgSenderIconLabel::class.java.getResource("/icons/sender/mdr.svg"),
        "mdr thüringen" to SvgSenderIconLabel::class.java.getResource("/icons/sender/mdr.svg"),
        "ndr hamburg" to SvgSenderIconLabel::class.java.getResource("/icons/sender/ndr.svg"),
        "ndr mecklenburg-vorpommern" to SvgSenderIconLabel::class.java.getResource("/icons/sender/ndr.svg"),
        "ndr schleswig-holstein" to SvgSenderIconLabel::class.java.getResource("/icons/sender/ndr.svg"),
        "nrd niedersachsen" to SvgSenderIconLabel::class.java.getResource("/icons/sender/ndr.svg"),
        "one" to SvgSenderIconLabel::class.java.getResource("/icons/sender/one.svg"),
        "parlamentsfernsehen kanal 1" to SvgSenderIconLabel::class.java.getResource("/icons/sender/Deutscher_Bundestag.svg"),
        "parlamentsfernsehen kanal 2" to SvgSenderIconLabel::class.java.getResource("/icons/sender/Deutscher_Bundestag.svg"),
        "phoenix" to SvgSenderIconLabel::class.java.getResource("/icons/sender/phoenix.svg"),
        "radio bremen" to SvgSenderIconLabel::class.java.getResource("/icons/sender/radio-bremen.svg"),
        "rbb fernsehen berlin" to SvgSenderIconLabel::class.java.getResource("/icons/sender/rbb.svg"),
        "rbb fernsehen brandenburg" to SvgSenderIconLabel::class.java.getResource("/icons/sender/rbb.svg"),
        "sr" to SvgSenderIconLabel::class.java.getResource("/icons/sender/sr.svg"),
        "swr baden-württemberg" to SvgSenderIconLabel::class.java.getResource("/icons/sender/swr.svg"),
        "swr rheinland-pfalz" to SvgSenderIconLabel::class.java.getResource("/icons/sender/swr.svg"),
        "tagesschau24" to SvgSenderIconLabel::class.java.getResource("/icons/sender/tagesschau24.svg"),
        "wdr" to SvgSenderIconLabel::class.java.getResource("/icons/sender/wdr.svg"),
        "zdf" to SvgSenderIconLabel::class.java.getResource("/icons/sender/zdf.svg"),
        "zdfinfo" to SvgSenderIconLabel::class.java.getResource("/icons/sender/ZDFinfo.svg"),
        "zdfneo" to SvgSenderIconLabel::class.java.getResource("/icons/sender/ZDFneo.svg")
    )
    private val cache = mutableMapOf<String, URL>()

    fun getIconUrl(senderKey: String): URL {
        return cache.getOrPut(senderKey) {
            senderIconMap[senderKey] as URL
        }
    }
}