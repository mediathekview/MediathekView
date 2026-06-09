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
package mediathek.config;

import mediathek.tool.FileUtils;
import mediathek.tool.GetIcon;
import mediathek.tool.Version;
import okhttp3.HttpUrl;

import java.awt.*;
import java.util.concurrent.TimeUnit;


public class Konstanten {
    public static final Image ICON_TRAY = GetIcon.getProgramIcon("tray.png", 256, 256).getImage();
    public static final int TABLE_DEFAULT_ROW_HEIGHT = 24;
    public static final int TABLE_DEFAULT_LARGE_ICON_ROW_HEIGHT = TABLE_DEFAULT_ROW_HEIGHT + 12;
    public static final String JSOUP_USER_AGENT = "Mozilla/5.0";
    public static final String ZAPP_API_URL = "https://api.zapp.mediathekview.de/";
    public static final Version MVVERSION = new Version(15, 0, 0);

    public static final ApplicationType APPLICATION_TYPE = ApplicationType.NIGHTLY;

    public static final long AUDIOTHEK_SEARCH_TIMEOUT_SECONDS = 45;

    public static final long ALTER_FILMLISTE_SEKUNDEN_FUER_AUTOUPDATE = TimeUnit.SECONDS.convert(3, TimeUnit.HOURS);

    public static final String PROGRAMMNAME = "MediathekView";
    public static final String CONFIG_FILE = "mediathek.xml";
    public static final String CONFIG_FILE_COPY = "mediathek.xml_copy_";
    /**
     * Maximum number of backup files to be stored.
     */
    public static final short MAX_NUM_BACKUP_FILE_COPIES = 5;

    public static final byte MAX_DOWNLOAD_RESTARTS = 2;
    public static final byte MAX_EXTERNAL_STARTS = 3;
    /**
     * Default time in seconds for automatic continuation of existing downloads.
     */
    public static final byte DOWNLOAD_CONTINUATION_DEFAULT_TIME = 60;

    // MediathekView URLs
    public static final HttpUrl ROUTER_BASE_URL = HttpUrl.get("https://liste.mediathekview.de");
    public static final HttpUrl WEBSITE_BASE_URL = HttpUrl.get("https://mediathekview.de");
    public static final HttpUrl URL_MEDIATHEKVIEW_RESOURCES = HttpUrl.get("https://res.mediathekview.de");
    public static final String AUDIOTHEK_ONLINE_SEARCH_PROXY_URL = "https://audiothek.crystalpalace.info";
    public static final String AUDIOTHEK_PROXY_CLIENT_TOKEN_HEADER = "X-MV-Client-Token";
    public static final String AUDIOTHEK_PROXY_CLIENT_TOKEN = "mv-audiothek-static-2026-03";
    public static final String HLS_STREAM_INFO_TOKEN_HEADER = "X-HLS-Stream-Info-Token";
    public static final String HLS_STREAM_INFO_TOKEN = "mv-hls-stream-info-test-2026-04-14";
    public static final HttpUrl HLS_STREAM_INFO_BASE_URL = HttpUrl.get("https://hlssi.crystalpalace.info");
    public static final HttpUrl HLS_STREAM_INFO_UPLOAD_URL = HLS_STREAM_INFO_BASE_URL.resolve("/v1/hls-stream-info"); // e.g. HttpUrl.get("https://xx.domain.com/v1/hls-stream-info")
    public static final HttpUrl AUDIOTHEK_DB_DOWNLOAD_URL = HLS_STREAM_INFO_BASE_URL.resolve("/audio-db");
    public static final String PSET_PROGRAM_GROUP_LIST_PATH = "programmgruppen13/programmgruppen.xml";
    public static final String PROGRAM_VERSION_PATH = "prog-info-13.xml";
    public static final String ADRESSE_DOWNLOAD = "https://mediathekview.de/download/";
    public static final String ADRESSE_ONLINE_HELP = "https://mediathekview.de/anleitung/";
    public static final String ADRESSE_ONLINE_FAQ = "https://mediathekview.de/faq/";
    public static final String ADRESSE_WEBSITE = "https://mediathekview.de/";
    public static final String ADRESSE_FORUM = "https://forum.mediathekview.de/";
    public static final String ADRESSE_DONATION = "https://mediathekview.de/spenden/";
    public static final String ORF_TUTORIAL_LINK = "https://forum.mediathekview.de/topic/2546/anleitung-einstellungen-für-orf-download";
    // Dateien/Verzeichnisse
    public static final String VERZEICHNIS_DOWNLOADS = PROGRAMMNAME; // Standard wenn nichts angeben, Verzeichnis wird im Homeverzeichnis angelegt
    public static final String VERZEICHNIS_EINSTELLUNGEN = ".mediathek3"; // im Homeverzeichnis
    public static final String JSON_DATEI_FILME = "filme.json";
    public static final String SHUTDOWN_LINUX = "shutdown -h now";
    public static final String JDOWNLOADER_URL = "http://127.0.0.1:9666/flash/add";

    /**
     * Minimum file size which won´t be regarded as an error.
     */
    public static final long MIN_FILM_FILE_SIZE_KB = 256 * FileUtils.ONE_KB;
    public static final int MAX_PFADE_DIALOG_DOWNLOAD = 15;
    /**
     * Standardwert für die Länge des Zieldateinamens
     */
    public static final int LAENGE_DATEINAME = 25;
    /**
     * Standardwert für die Länge des Feldes des Zieldateinamens zB. %T
     */
    public static final int LAENGE_FELD = 10;

    public static final String PFAD_HILFETEXT_BEENDEN = "/mediathek/file/hilfetext_beenden.txt";
    public static final String PFAD_HILFETEXT_GEO = "/mediathek/file/hilfetext_geo.txt";
    public static final String PFAD_HILFETEXT_BLACKLIST = "/mediathek/file/hilfetext_blacklist.txt";
    public static final String PFAD_HILFETEXT_PRGRAMME = "/mediathek/file/hilfetext_pset.txt";
    public static final String PFAD_HILFETEXT_STANDARD_PSET = "/mediathek/file/hilfetext_standardPset.txt";
    public static final String PFAD_HILFETEXT_EDIT_DOWNLOAD_PROG = "/mediathek/file/hilfetext_editDownloadProg.txt";
    public static final String PFAD_HILFETEXT_RESET = "/mediathek/file/hilfetext_reset.txt";
    public static final String PFAD_HILFETEXT_RESET_SET = "/mediathek/file/hilfetext_reset_set.txt";
    public static final String PFAD_HILFETEXT_DIALOG_ADD_ABO = "/mediathek/file/hilfetext_dialog_add_abo.txt";
    public static final String PFAD_LUCENE_TUTORIAL_MARKDOWN = "/mediathek/file/lucene_tutorial.md";
    public static final String PFAD_AUDIOTHEK_SUCHE_HILFE_MARKDOWN = "/mediathek/file/hilfetext_audiothek_suche.md";
}
