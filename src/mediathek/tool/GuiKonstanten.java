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

public class GuiKonstanten {

    public static final String TextUpdateSuchen = "Eimal am Tag nach einer neuen Programmversion suchen";
    public static final String IMPORT_WEBSITE = "import-website";
    public static final String IMPORT_URL = "import-url";
    public static final String IMPORT_DATEI = "import-datei";
    public static final String IMPORT_WEBSITE_TEXT = "Filme laden (Website)";
    public static final String IMPORT_URL_TEXT = "Filme laden (Url)";
    public static final String IMPORT_DATEI_TEXT = "Filme laden (Datei)";
    public static final String DIREKTE_DOWNLOAD_SUFFIX = "mp4,mp3,m4v,flv";
    public static final String DIREKTE_DOWNLOAD_PRAEFIX = "http";
    // für das Anpassen der URL für den flvstreamer
    public static final String RTMP_FLVSTREAMER = "-r ";
    public static final String RTMP_PRTOKOLL = "rtmp";
    // Standardwert für die Länge des Zieldateinamens
    public static final int LAENGE_DATEINAME = 25;
    //
    public static final int UPDATE_FILME_AUS = 0; // nur manuell + URL manuell wählen
    public static final int UPDATE_FILME_AUTO = 2; // beim Start automatisch + manuell, Url automatisch wählen
}
