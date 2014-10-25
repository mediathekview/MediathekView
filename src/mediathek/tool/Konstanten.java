/*
 * MediathekView
 * Copyright (C) 2008 W. Xaver
 * W.Xaver[at]googlemail.com
 * http://zdfmediathk.sourceforge.net/
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.tool;

public class Konstanten {

    public static final String VERSION = "8";
    public static final String PROGRAMMNAME = "MediathekView";
    public static final String USER_AGENT_DEFAULT = Funktionen.getProgVersionString();
    public static final String CONFIG_FILE = "mediathek.xml";
    public static final String CONFIG_FILE_COPY = "mediathek.xml_copy_";
    public static final String FILE_ERLEDIGTE_ABOS = "downloadAbos.txt";
    public static final String FILE_HISTORY = "history.txt";

    // MediathekView URLs
    public static final String ADRESSE_FILMLISTEN_SERVER = "http://zdfmediathk.sourceforge.net/update.xml";
    public static final String ADRESSE_PROGRAMM_VERSION = "http://zdfmediathk.sourceforge.net/prog-info.xml"; // ab Version 4, einstellig!
    public static final String ADRESSE_DOWNLAD = "http://sourceforge.net/projects/zdfmediathk/";
    public static final String ADRESSE_ANLEITUNG = "http://sourceforge.net/p/zdfmediathk/wiki/Home/";
    public static final String ADRESSE_VORLAGE_PROGRAMMGRUPPEN = "http://zdfmediathk.sourceforge.net/programmgruppen4/programmgruppen.xml";
    public static final String ADRESSE_WEBSITE = "http://zdfmediathk.sourceforge.net/";
    public static final String ADRESSE_FORUM = "http://zdfmediathk.sourceforge.net/forum/";
    public static final String ADRESSE_DONATION = "http://zdfmediathk.sourceforge.net/index.html#donate";
    // ProgrammUrls
    public static final String ADRESSE_WEBSITE_MPLAYER = "http://sourceforge.net/projects/smplayer";
    public static final String ADRESSE_WEBSITE_VLC = "http://www.videolan.org";
    public static final String ADRESSE_WEBSITE_FLVSTREAMER = "https://savannah.nongnu.org/projects/flvstreamer";
    public static final String ADRESSE_WEBSITE_FFMPEG = "http://ffmpeg.org";
    // Dateien/Verzeichnisse
    public static final String VERZEICHNIS_PROGRAMM_ICONS = "Icons/Programm"; // Unterverzeichnis im Programmverzeichnis in dem die Iconsets liegen
    public static final String VERZEICHNIS_SENDER_ICONS = "Icons/Sender"; // Unterverzeichnis im Programmverzeichnis in dem die Icons (Sender) liegen
    public static final String VERZEICHNIS_DOWNLOADS = PROGRAMMNAME; // Standard wenn nichts angeben, Verzeichnis wird im Homeverzeichnis angelegt
    public static final String VERZEICHNIS_EINSTELLUNGEN = ".mediathek3"; // im Homeverzeichnis
    public static final String JSON_DATEI_FILME = "filme.json";
    // 
    public static final int MIN_DATEI_GROESSE_FILM = 256 * 1024; //minimale Größe (256 KiB) eines Films um nicht als Fehler zu gelten
    public static final String XML_START = "Mediathek";
    public static final int MAX_SENDER_FILME_LADEN = 2; //es können maximal soviele Filme eines Senders/Servers gleichzeitig geladen werden
    public static final int DOWNLOAD_WEITERFUEHREN_IN_SEKUNDEN = 100; //Beim Dialog "Download weiterführen" wird in dieser Zeit der Download weitergeführt

    public static final String ZIELNAMEN_ANPASSEN_NORMAL = "normal";
    public static final String ZIELNAMEN_ANPASSEN_ASCII = "ascii";
    public static final String ZIELNAMEN_ANPASSEN_NIX = "NIX";

    public static final int MAX_PFADE_DIALOG_DOWNLOAD = 15;
    public static final String GUIFILME_DIVIDER_LOCATION = "260";

    public static final int LAENGE_DATEINAME = 25; // Standardwert für die Länge des Zieldateinamens

    public static final int UPDATE_FILME_AUS = 0; // nur manuell + URL manuell wählen
    public static final int UPDATE_FILME_AUTO = 2; // beim Start automatisch + manuell, Url automatisch wählen

}
