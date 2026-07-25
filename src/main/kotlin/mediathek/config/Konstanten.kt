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

package mediathek.config

import mediathek.tool.FileUtils
import mediathek.tool.GetIcon
import mediathek.tool.Version
import okhttp3.HttpUrl
import okhttp3.HttpUrl.Companion.toHttpUrl
import java.awt.Image

object Konstanten {
    val ICON_TRAY: Image = GetIcon.getProgramIcon("tray.png", 256, 256).image

    const val TABLE_DEFAULT_ROW_HEIGHT = 24
    const val TABLE_DEFAULT_LARGE_ICON_ROW_HEIGHT = TABLE_DEFAULT_ROW_HEIGHT + 12
    const val JSOUP_USER_AGENT = "Mozilla/5.0"
    const val ZAPP_API_URL = "https://api.zapp.mediathekview.de/"

    val MVVERSION = Version(15, 0, 1)

    val APPLICATION_TYPE = ApplicationType.PRODUCTION

    const val AUDIOTHEK_SEARCH_TIMEOUT_SECONDS = 45L
    const val ALTER_FILMLISTE_SEKUNDEN_FUER_AUTOUPDATE = 3 * 60 * 60L

    const val PROGRAMMNAME = "MediathekView"
    const val CONFIG_FILE = "mediathek.xml"
    const val CONFIG_FILE_COPY = "mediathek.xml_copy_"

    /**
     * Maximum number of backup files to be stored.
     */
    const val MAX_NUM_BACKUP_FILE_COPIES: Short = 2

    const val MAX_DOWNLOAD_RESTARTS = 2
    const val MAX_EXTERNAL_STARTS = 3

    /**
     * Default time in seconds for automatic continuation of existing downloads.
     */
    const val DOWNLOAD_CONTINUATION_DEFAULT_TIME = 60

    // MediathekView URLs
    val ROUTER_BASE_URL: HttpUrl = "https://liste.mediathekview.de".toHttpUrl()

    val WEBSITE_BASE_URL: HttpUrl = "https://mediathekview.de".toHttpUrl()

    val URL_MEDIATHEKVIEW_RESOURCES: HttpUrl = "https://res.mediathekview.de".toHttpUrl()

    const val HLS_STREAM_INFO_TOKEN_HEADER = "X-HLS-Stream-Info-Token"
    const val HLS_STREAM_INFO_TOKEN = "mv-hls-stream-info-test-2026-04-14"
    val HLS_STREAM_INFO_BASE_URL: HttpUrl = "https://hlssi.mediathekview.de".toHttpUrl()
    val HLS_STREAM_INFO_UPLOAD_URL: HttpUrl = requireNotNull(
        HLS_STREAM_INFO_BASE_URL.resolve("/v1/hls-stream-info")
    )
    val AUDIOTHEK_DB_DOWNLOAD_URL: HttpUrl = requireNotNull(HLS_STREAM_INFO_BASE_URL.resolve("/audio-db"))

    const val PSET_PROGRAM_GROUP_LIST_PATH = "programmgruppen13/programmgruppen.xml"
    const val PROGRAM_VERSION_PATH = "prog-info-13.xml"
    const val ADRESSE_DOWNLOAD = "https://mediathekview.de/download/"
    const val ADRESSE_ONLINE_HELP = "https://mediathekview.de/anleitung/"
    const val ADRESSE_ONLINE_FAQ = "https://mediathekview.de/faq/"
    const val ADRESSE_WEBSITE = "https://mediathekview.de/"
    const val ADRESSE_FORUM = "https://forum.mediathekview.de/"
    const val ADRESSE_DONATION = "https://mediathekview.de/spenden/"
    const val ORF_TUTORIAL_LINK = "https://forum.mediathekview.de/topic/2546/anleitung-einstellungen-für-orf-download"

    // Dateien/Verzeichnisse
    const val VERZEICHNIS_DOWNLOADS = PROGRAMMNAME // Standard wenn nichts angeben, Verzeichnis wird im Homeverzeichnis angelegt
    const val VERZEICHNIS_EINSTELLUNGEN = ".mediathek3" // im Homeverzeichnis
    const val JSON_DATEI_FILME = "filme.json"
    const val SHUTDOWN_LINUX = "shutdown -h now"
    const val JDOWNLOADER_URL = "http://127.0.0.1:9666/flash/add"

    /**
     * Minimum file size which won´t be regarded as an error.
     */
    const val MIN_FILM_FILE_SIZE_KB = 256 * FileUtils.ONE_KB
    const val MAX_PFADE_DIALOG_DOWNLOAD = 15

    /**
     * Standardwert für die Länge des Zieldateinamens
     */
    const val LAENGE_DATEINAME = 25

    /**
     * Standardwert für die Länge des Feldes des Zieldateinamens zB. %T
     */
    const val LAENGE_FELD = 10

    const val PFAD_HILFETEXT_BEENDEN = "/mediathek/file/hilfetext_beenden.txt"
    const val PFAD_HILFETEXT_GEO = "/mediathek/file/hilfetext_geo.txt"
    const val PFAD_HILFETEXT_BLACKLIST = "/mediathek/file/hilfetext_blacklist.txt"
    const val PFAD_HILFETEXT_PRGRAMME = "/mediathek/file/hilfetext_pset.txt"
    const val PFAD_HILFETEXT_STANDARD_PSET = "/mediathek/file/hilfetext_standardPset.txt"
    const val PFAD_HILFETEXT_EDIT_DOWNLOAD_PROG = "/mediathek/file/hilfetext_editDownloadProg.txt"
    const val PFAD_HILFETEXT_RESET = "/mediathek/file/hilfetext_reset.txt"
    const val PFAD_HILFETEXT_RESET_SET = "/mediathek/file/hilfetext_reset_set.txt"
    const val PFAD_HILFETEXT_DIALOG_ADD_ABO = "/mediathek/file/hilfetext_dialog_add_abo.txt"
    const val PFAD_LUCENE_TUTORIAL_MARKDOWN = "/mediathek/file/lucene_tutorial.md"
    const val PFAD_AUDIOTHEK_SUCHE_HILFE_MARKDOWN = "/mediathek/file/hilfetext_audiothek_suche.md"
}
