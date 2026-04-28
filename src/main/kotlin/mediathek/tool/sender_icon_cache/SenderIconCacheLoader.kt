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
    private fun getSvgResource(sender: String): String? = when (normalizeSenderLookupKey(sender)) {
        /*
ARD Kultur
Bayern 1
Bayern 2
Bayern 3
BR Klassik
BR Puls
BR Schlager
BR24
Bremen Eins
Bremen NEXT
Bremen Vier
Bremen Zwei
DASDING
         */
        "1live" -> "/icons/audiothek/1live.svg"
        "3sat" -> "/icons/sender/3sat.svg"
        "antenne brandenburg" -> "/icons/audiothek/antenne-brandenburg.svg"
        "ard", "das erste" -> "/icons/sender/ard.svg"
        "ard-alpha" -> "/icons/sender/ard-alpha.svg"
        "arte.de", "arte.en", "arte.es", "arte.fr", "arte.it", "arte.pl", "arte" -> "/icons/sender/arte.svg"
        "br" -> "/icons/sender/br.svg"
        "br heimat" -> "/icons/audiothek/br-heimat.svg"
        "cosmo" -> "/icons/audiothek/cosmo.svg"
        "deutschlandfunk" -> "/icons/audiothek/deutschlandfunk.svg"
        "deutschlandfunk kultur" -> "/icons/audiothek/deutschlandfunk-kultur.svg"
        "deutschlandfunk nova" -> "/icons/audiothek/deutschlandfunk-nova.svg"
        "deutschlandradio" -> "/icons/sender/Deutschlandradio_Logo_2017.svg"
        "die maus" -> "/icons/audiothek/die-maus.svg"
        "fritz" -> "/icons/audiothek/rbb-fritz.svg"
        "funk.net", "funk" -> "/icons/sender/funk.svg"
        "hr" -> "/icons/sender/hr.svg"
        "hr1" -> "/icons/audiothek/hr1.svg"
        "hr2-kultur" -> "/icons/audiothek/hr2-kultur.svg"
        "hr3" -> "/icons/audiothek/hr3.svg"
        "hr info" -> "/icons/audiothek/hr-info.svg"
        "kika" -> "/icons/sender/kika.svg"
        "mdr" -> "/icons/sender/mdr.svg"
        "mdr aktuell" -> "/icons/audiothek/mdr-aktuell.svg"
        "mdr jump" -> "/icons/audiothek/mdr-jump.svg"
        "mdr klassik" -> "/icons/audiothek/mdr-klassik.svg"
        "mdr kultur" -> "/icons/audiothek/mdr-kultur.svg"
        "mdr sachsen" -> "/icons/audiothek/mdr-sachsen.svg"
        "mdr sachsen-anhalt" -> "/icons/audiothek/mdr-sachsen-anhalt.svg"
        "mdr sputnik" -> "/icons/audiothek/mdr-sputnik.svg"
        "mdr thüringen" -> "/icons/audiothek/mdr-thuringen.svg"
        "mdr tweens" -> "/icons/audiothek/mdr-tweens.svg"
        "ndr" -> "/icons/sender/ndr.svg"
        "ndr 1 niedersachsen" -> "/icons/audiothek/ndr-1-niedersachsen.svg"
        "ndr 1 radio mv" -> "/icons/audiothek/ndr-1-radio-mv.svg"
        "ndr 1 welle nord" -> "/icons/audiothek/ndr-1-welle-nord.svg"
        "ndr 2" -> "/icons/audiothek/ndr-2.svg"
        "ndr 90,3" -> "/icons/audiothek/ndr-90-3.svg"
        "ndr blue" -> "/icons/audiothek/ndr-blue.svg"
        "ndr info" -> "/icons/audiothek/ndr-info.svg"
        "ndr kultur" -> "/icons/audiothek/ndr-kultur.svg"
        "ndr schlager" -> "/icons/audiothek/ndr-schlager.svg"
        "n-joy" -> "/icons/audiothek/njoy.svg"
        "one" -> "/icons/sender/one.svg"
        "phoenix" -> "/icons/sender/phoenix.svg"
        "podcastindex", "podcast index" -> "/icons/sender/podcastindex-brand-text.svg"
        "radio bremen tv", "radio bremen", "radiobremen" -> "/icons/sender/radio-bremen.svg"
        "radio3" -> "/icons/audiothek/radio3.svg"
        "radioeins" -> "/icons/audiothek/radioeins.svg"
        "rbb" -> "/icons/sender/rbb.svg"
        "rbb24 inforadio" -> "/icons/audiothek/rbb24-inforadio.svg"
        "rbb 88.8" -> "/icons/audiothek/rbb-88-8.svg"
        "sr" -> "/icons/sender/sr.svg"
        "sr 1" -> "/icons/audiothek/sr-1.svg"
        "sr 3 saarlandwelle" -> "/icons/audiothek/sr-3.svg"
        "sr kultur" -> "/icons/audiothek/sr-kultur.svg"
        "sr unserding" -> "/icons/audiothek/sr-unserding.svg"
        "swr" -> "/icons/sender/swr.svg"
        "swr aktuell" -> "/icons/audiothek/swr-aktuell.svg"
        "swr kultur" -> "/icons/audiothek/swr-kultur.svg"
        "swr1", "swr1 bw" -> "/icons/audiothek/swr1.svg"
        "swr3" -> "/icons/audiothek/swr3.svg"
        "swr4" -> "/icons/audiothek/swr4.svg"
        "tagesschau" -> "/icons/audiothek/tagesschau.svg"
        "tagesschau24" -> "/icons/sender/tagesschau24.svg"
        "wdr" -> "/icons/sender/wdr.svg"
        "wdr 2" -> "/icons/audiothek/wdr-2.svg"
        "wdr 3" -> "/icons/audiothek/wdr-3.svg"
        "wdr 4" -> "/icons/audiothek/wdr-4.svg"
        "wdr 5" -> "/icons/audiothek/wdr-5.svg"
        "you fm" -> "/icons/audiothek/you-fm.svg"
        "zdf" -> "/icons/sender/zdf.svg"
        "zdf-tivi" -> "/icons/sender/ZDFtivi.svg"
        "zdfinfo" -> "/icons/sender/ZDFinfo.svg"
        "zdfneo" -> "/icons/sender/ZDFneo.svg"
        "deutscher bundestag", "parlamentsfernsehen kanal 1", "parlamentsfernsehen kanal 2" ->
            "/icons/sender/Deutscher_Bundestag.svg"

        else -> null
    }

    private fun getPngResource(sender: String): String? = when (normalizeSenderLookupKey(sender)) {
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
        "sportschau" -> "/icons/audiothek/sportschau.png"
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

    private fun normalizeSenderLookupKey(sender: String): String {
        return sender
            .lowercase(Locale.ROOT)
            .replace(UNICODE_SPACE_SEPARATOR_REGEX, " ")
            .trim()
            .replace(MULTIPLE_WHITESPACE_REGEX, " ")
    }

    companion object {
        private val logger = LogManager.getLogger()
        private val UNICODE_SPACE_SEPARATOR_REGEX = Regex("\\p{Z}+")
        private val MULTIPLE_WHITESPACE_REGEX = Regex("\\s+")
    }
}
