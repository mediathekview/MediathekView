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

package mediathek.tool.sender_icon_cache;

import com.formdev.flatlaf.extras.FlatSVGIcon;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.Locale;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;

class SenderIconCacheLoader {
    private static final Logger logger = LogManager.getLogger();
    private final AtomicBoolean useLocalIcons;

    public SenderIconCacheLoader(@NotNull AtomicBoolean useLocalIcons) {
        this.useLocalIcons = useLocalIcons;
    }

    private @Nullable String getSvgResource(@NotNull String sender) {
        return switch (sender.toLowerCase(Locale.ROOT)) {
            case "3sat" -> "/icons/sender/3sat.svg";
            case "ard", "das erste" -> "/icons/sender/ard.svg";
            case "ard-alpha" -> "/icons/sender/ard-alpha.svg";
            case "arte.de", "arte.en", "arte.es", "arte.fr", "arte.it", "arte.pl", "arte" -> "/icons/sender/arte.svg";
            case "br" -> "/icons/sender/br.svg";
            case "funk.net" -> "/icons/sender/funk.svg";
            case "hr" -> "/icons/sender/hr.svg";
            case "kika" -> "/icons/sender/kika.svg";
            case "mdr" -> "/icons/sender/mdr.svg";
            case "ndr" -> "/icons/sender/ndr.svg";
            case "one" -> "/icons/sender/one.svg";
            case "phoenix" -> "/icons/sender/phoenix.svg";
            case "radio bremen tv", "radio bremen" -> "/icons/sender/radio-bremen.svg";
            case "rbb" -> "/icons/sender/rbb.svg";
            case "sr" -> "/icons/sender/sr.svg";
            case "swr" -> "/icons/sender/swr.svg";
            case "tagesschau24" -> "/icons/sender/tagesschau24.svg";
            case "wdr" -> "/icons/sender/wdr.svg";
            case "zdf" -> "/icons/sender/zdf.svg";
            case "zdf-tivi" -> "/icons/sender/ZDFtivi.svg";
            case "zdfinfo" -> "/icons/sender/ZDFinfo.svg";
            case "zdfneo" -> "/icons/sender/ZDFneo.svg";
            case "deutscher bundestag", "parlamentsfernsehen kanal 1", "parlamentsfernsehen kanal 2" -> "/icons/sender/Deutscher_Bundestag.svg";
            default -> null;
        };
    }

    private @Nullable String getPngResource(@NotNull String sender) {
        return switch (sender.toLowerCase(Locale.ROOT)) {
            case "3sat" -> "/mediathek/res/sender/3sat.png";
            case "ard", "das erste" -> "/mediathek/res/sender/ard.png";
            case "arte.de" -> "/mediathek/res/sender/arte-de.png";
            case "arte.en" -> "/mediathek/res/sender/arte-en.png";
            case "arte.es" -> "/mediathek/res/sender/arte-es.png";
            case "arte.fr" -> "/mediathek/res/sender/arte-fr.png";
            case "arte.it" -> "/mediathek/res/sender/arte-it.png";
            case "arte.pl" -> "/mediathek/res/sender/arte-pl.png";
            case "br" -> "/mediathek/res/sender/br.png";
            case "dw" -> "/mediathek/res/sender/dw.png";
            case "funk.net" -> "/mediathek/res/sender/funk_net.png";
            case "hr" -> "/mediathek/res/sender/hr.png";
            case "kika" -> "/mediathek/res/sender/kika.png";
            case "mdr" -> "/mediathek/res/sender/mdr.png";
            case "ndr" -> "/mediathek/res/sender/ndr.png";
            case "orf" -> "/mediathek/res/sender/orf.png";
            case "phoenix" -> "/mediathek/res/sender/phoenix.png";
            case "rbb" -> "/mediathek/res/sender/rbb.png";
            case "radio bremen tv", "radio bremen" -> "/mediathek/res/sender/rbtv.jpg";
            case "sr" -> "/mediathek/res/sender/sr.png";
            case "srf" -> "/mediathek/res/sender/srf.png";
            case "srf.podcast" -> "/mediathek/res/sender/srf-podcast.png";
            case "swr" -> "/mediathek/res/sender/swr.png";
            case "wdr" -> "/mediathek/res/sender/wdr.png";
            case "zdf" -> "/mediathek/res/sender/zdf.png";
            case "zdf-tivi" -> "/mediathek/res/sender/zdf-tivi.png";
            default -> null;
        };
    }

    private @Nullable ImageIcon loadResourceIcon(@NotNull String sender, @Nullable String resource) {
        if (resource == null) {
            return null;
        }

        var url = SenderIconCacheLoader.class.getResource(resource);
        if (url == null) {
            logger.warn("Sender icon resource missing for sender '{}' (resource: {})", sender, resource);
            return null;
        }

        if (resource.endsWith(".svg")) {
            return new FlatSVGIcon(url);
        }

        return new ImageIcon(url);
    }

    public @NotNull Optional<ImageIcon> load(@NotNull String sender) {
        String svgResource = getSvgResource(sender);
        String pngResource = getPngResource(sender);

        ImageIcon icon;
        if (useLocalIcons.get()) {
            // local mode: prefer PNG sender icons
            icon = loadResourceIcon(sender, pngResource);
            if (icon == null) {
                icon = loadResourceIcon(sender, svgResource);
            }
        } else {
            // wiki mode: prefer SVG sender icons, no network requests
            icon = loadResourceIcon(sender, svgResource);
            if (icon == null) {
                icon = loadResourceIcon(sender, pngResource);
            }
        }

        if (icon == null) {
            logger.trace("No sender icon found for '{}' (localMode={})", sender, useLocalIcons.get());
            return Optional.empty();
        }

        return Optional.of(icon);
    }
}
