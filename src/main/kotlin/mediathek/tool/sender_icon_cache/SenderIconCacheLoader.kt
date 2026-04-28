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

package mediathek.tool.sender_icon_cache

import com.formdev.flatlaf.extras.FlatSVGIcon
import org.apache.logging.log4j.LogManager
import java.util.*
import java.util.concurrent.atomic.AtomicBoolean
import javax.swing.ImageIcon

internal class SenderIconCacheLoader(
    private val useLocalIcons: AtomicBoolean,
) {
    private fun getSvgResource(sender: String): String? = when (sender.lowercase(Locale.ROOT)) {
        "3sat" -> "/icons/sender/3sat.svg"
        "ard", "das erste" -> "/icons/sender/ard.svg"
        "ard-alpha" -> "/icons/sender/ard-alpha.svg"
        "arte.de", "arte.en", "arte.es", "arte.fr", "arte.it", "arte.pl", "arte" -> "/icons/sender/arte.svg"
        "br" -> "/icons/sender/br.svg"
        "deutschlandradio" -> "/icons/sender/Deutschlandradio_Logo_2017.svg"
        "funk.net", "funk" -> "/icons/sender/funk.svg"
        "hr" -> "/icons/sender/hr.svg"
        "kika" -> "/icons/sender/kika.svg"
        "mdr" -> "/icons/sender/mdr.svg"
        "ndr" -> "/icons/sender/ndr.svg"
        "one" -> "/icons/sender/one.svg"
        "phoenix" -> "/icons/sender/phoenix.svg"
        "podcastindex", "podcast index" -> "/icons/sender/podcastindex-brand-text.svg"
        "radio bremen tv", "radio bremen", "radiobremen" -> "/icons/sender/radio-bremen.svg"
        "rbb" -> "/icons/sender/rbb.svg"
        "sr" -> "/icons/sender/sr.svg"
        "swr" -> "/icons/sender/swr.svg"
        "tagesschau24" -> "/icons/sender/tagesschau24.svg"
        "wdr" -> "/icons/sender/wdr.svg"
        "zdf" -> "/icons/sender/zdf.svg"
        "zdf-tivi" -> "/icons/sender/ZDFtivi.svg"
        "zdfinfo" -> "/icons/sender/ZDFinfo.svg"
        "zdfneo" -> "/icons/sender/ZDFneo.svg"
        "deutscher bundestag", "parlamentsfernsehen kanal 1", "parlamentsfernsehen kanal 2" ->
            "/icons/sender/Deutscher_Bundestag.svg"

        else -> null
    }

    private fun getPngResource(sender: String): String? = when (sender.lowercase(Locale.ROOT)) {
        "3sat" -> "/mediathek/res/sender/3sat.png"
        "ard", "das erste" -> "/mediathek/res/sender/ard.png"
        "arte.de" -> "/mediathek/res/sender/arte-de.png"
        "arte.en" -> "/mediathek/res/sender/arte-en.png"
        "arte.es" -> "/mediathek/res/sender/arte-es.png"
        "arte.fr" -> "/mediathek/res/sender/arte-fr.png"
        "arte.it" -> "/mediathek/res/sender/arte-it.png"
        "arte.pl" -> "/mediathek/res/sender/arte-pl.png"
        "br" -> "/mediathek/res/sender/br.png"
        "dw" -> "/mediathek/res/sender/dw.png"
        "funk.net", "funk" -> "/mediathek/res/sender/funk_net.png"
        "hr" -> "/mediathek/res/sender/hr.png"
        "kika" -> "/mediathek/res/sender/kika.png"
        "mdr" -> "/mediathek/res/sender/mdr.png"
        "ndr" -> "/mediathek/res/sender/ndr.png"
        "orf" -> "/mediathek/res/sender/orf.png"
        "phoenix" -> "/mediathek/res/sender/phoenix.png"
        "rbb" -> "/mediathek/res/sender/rbb.png"
        "radio bremen tv", "radio bremen" -> "/mediathek/res/sender/rbtv.jpg"
        "sr" -> "/mediathek/res/sender/sr.png"
        "srf" -> "/mediathek/res/sender/srf.png"
        "srf.podcast" -> "/mediathek/res/sender/srf-podcast.png"
        "swr" -> "/mediathek/res/sender/swr.png"
        "wdr" -> "/mediathek/res/sender/wdr.png"
        "zdf" -> "/mediathek/res/sender/zdf.png"
        "zdf-tivi" -> "/mediathek/res/sender/zdf-tivi.png"
        else -> null
    }

    private fun loadResourceIcon(sender: String, resource: String?): ImageIcon? {
        if (resource == null) {
            return null
        }

        val url = SenderIconCacheLoader::class.java.getResource(resource)
        if (url == null) {
            logger.warn("Sender icon resource missing for sender '{}' (resource: {})", sender, resource)
            return null
        }

        return if (resource.endsWith(".svg")) {
            FlatSVGIcon(url)
        } else {
            ImageIcon(url)
        }
    }

    fun load(sender: String): Optional<ImageIcon> {
        val svgResource = getSvgResource(sender)
        val pngResource = getPngResource(sender)

        val icon = if (useLocalIcons.get()) {
            // local mode: prefer PNG sender icons
            loadResourceIcon(sender, pngResource) ?: loadResourceIcon(sender, svgResource)
        } else {
            // wiki mode: prefer SVG sender icons, no network requests
            loadResourceIcon(sender, svgResource) ?: loadResourceIcon(sender, pngResource)
        }

        if (icon == null) {
            logger.trace("No sender icon found for '{}' (localMode={})", sender, useLocalIcons.get())
            return Optional.empty()
        }

        return Optional.of(icon)
    }

    companion object {
        private val logger = LogManager.getLogger()
    }
}
