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
package mediathek.daten;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import mediathek.controller.filmeLaden.FilmeLaden;
import mediathek.tool.Funktionen;
import mediathek.tool.GuiKonstanten;
import mediathek.tool.Konstanten;
import mediathek.tool.Log;
import msearch.daten.ListeFilme;
import msearch.io.MSearchIoXmlFilmlisteSchreiben;

public class Daten {
    // Konstanten, Systemeinstellungen und alles was wichtig ist für
    // alle Versionen: MediathekGui, MediathekAuto, MediathekNoGui

    //alle Programmeinstellungen
    public static String[] system = new String[Konstanten.SYSTEM_MAX_ELEM];
    // flags
    public static boolean debug = false; // Debugmodus
    public static boolean nogui = false; // Version ohne Gui
    public static boolean auto = false; // Version: MediathekAuto
    public static boolean RESET = false; // Programm auf Starteinstellungen zurücksetzen
    // Verzeichnis zum Speichern der Programmeinstellungen
    private static String basisverzeichnis = "";
    // zentrale Klassen
    public static FilmeLaden filmeLaden;
    public static ListeFilme listeFilme = null;
    public static final String LINE_SEPARATOR = System.getProperty("line.separator");
    public static String proxyUrl = "";
    public static int proxyPort = -1;

    public Daten(String pfad) {
        basisverzeichnis = pfad;
        init();
    }

    private void init() {
        for (int i = 0; i < system.length; ++i) {
            system[i] = "";
        }
        //initialisieren
        system[Konstanten.SYSTEM_MAX_DOWNLOAD_NR] = "1";
        system[Konstanten.SYSTEM_WARTEN_NR] = "1";
        system[Konstanten.SYSTEM_USER_AGENT_NR] = Konstanten.USER_AGENT_DEFAULT;
        system[Konstanten.SYSTEM_WARTEN_NR] = "1";
        system[Konstanten.SYSTEM_LOOK_NR] = "0";
        system[Konstanten.SYSTEM_VERSION_NR] = Konstanten.VERSION;
        system[Konstanten.SYSTEM_UPDATE_SUCHEN_NR] = Boolean.TRUE.toString();
        system[Konstanten.SYSTEM_ABOS_SOFORT_SUCHEN_NR] = Boolean.TRUE.toString();
        system[Konstanten.SYSTEM_DOWNLOAD_SOFORT_STARTEN_NR] = Boolean.FALSE.toString();
        system[Konstanten.SYSTEM_UNICODE_AENDERN_NR] = Boolean.TRUE.toString();
        system[Konstanten.SYSTEM_ECHTZEITSUCHE_NR] = Boolean.TRUE.toString();
        system[Konstanten.SYSTEM_ICON_STANDARD_NR] = Boolean.TRUE.toString();
        system[Konstanten.SYSTEM_PANEL_BESCHREIBUNG_ANZEIGEN_NR] = Boolean.TRUE.toString();
        system[Konstanten.SYSTEM_BLACKLIST_FILMLAENGE_NR] = "0";
        system[Konstanten.SYSTEM_ICON_PFAD_NR] = Funktionen.getPathJar() + File.separator + "Icons" + File.separator + "SchwarzWeiss";
        Daten.system[Konstanten.SYSTEM_BANDBREITE_KBYTE_NR] = String.valueOf(0);
        Daten.system[Konstanten.SYSTEM_FILMLISTE_UMBENENNEN_NR] = Boolean.FALSE.toString();
        //        Daten.system[Konstanten.SYSTEM_PANEL_FILTER_ANZEIGEN_NR] = Boolean.TRUE.toString();
        if (Daten.debug) {
            Daten.system[Konstanten.SYSTEM_IMPORT_ART_FILME_NR] = String.valueOf(GuiKonstanten.UPDATE_FILME_AUS);
        }
        listeFilme = new ListeFilme();
        filmeLaden = new FilmeLaden();
    }

    public static void setUserAgentAuto() {
        // Useragent wird vom Programm verwaltet
        system[Konstanten.SYSTEM_USER_AGENT_AUTO_NR] = Boolean.TRUE.toString();
    }

    public static void setUserAgentManuel(String ua) {
        // Useragent den der Benutzer vorgegeben hat
        system[Konstanten.SYSTEM_USER_AGENT_AUTO_NR] = Boolean.FALSE.toString();
        system[Konstanten.SYSTEM_USER_AGENT_NR] = ua;
    }

    public static boolean isUserAgentAuto() {
        if (system[Konstanten.SYSTEM_USER_AGENT_AUTO_NR].equals("")) {
            system[Konstanten.SYSTEM_USER_AGENT_AUTO_NR] = Boolean.TRUE.toString();
            return true;
        } else {
            return Boolean.parseBoolean(system[Konstanten.SYSTEM_USER_AGENT_AUTO_NR]);
        }
    }

    public static String getUserAgent() {
        if (isUserAgentAuto()) {
            return Konstanten.USER_AGENT_DEFAULT;
        } else {
            return system[Konstanten.SYSTEM_USER_AGENT_NR];
        }
    }

    /**
     * Liefert das Verzeichnis der Programmeinstellungen
     *
     * @return Den Verzeichnispfad als String.
     */
    @Deprecated
    public static String getBasisVerzeichnis() {
        // liefert das Verzeichnis der Programmeinstellungen
        return getBasisVerzeichnis(false);
    }

    /**
     * Liefert den Pfad zur Filmliste
     *
     * @return Den Pfad als String
     */
    public static String getDateiFilmliste() {
        return getBasisVerzeichnis(true) + Konstanten.XML_DATEI_FILME;
    }

    /**
     * Liefert das Verzeichnis der Programmeinstellungen
     *
     * @param anlegen Anlegen, oder nicht.
     * @return Den Verzeichnispfad als String.
     */
    @Deprecated
    public static String getBasisVerzeichnis(boolean anlegen) {
        return getBasisVerzeichnis(basisverzeichnis, anlegen);
    }

    /**
     * Liefert das Verzeichnis der Programme
     *
     * @param basis Der Ordner, in dem die Einstellungen liegen
     * @param anlegen Anlegen, oder nicht.
     * @return Den Verzeichnispfad als String
     */
    @Deprecated
    private static String getBasisVerzeichnis(String basis, boolean anlegen) {
        String ret;
        if (basis.equals("")) {
            ret = System.getProperty("user.home") + File.separator + Konstanten.VERZEICHNISS_EINSTELLUNGEN + File.separator;
        } else {
            ret = basis;
        }
        if (anlegen) {
            File basisF = new File(ret);
            if (!basisF.exists()) {
                if (!basisF.mkdirs()) {
                    Log.fehlerMeldung(898736548, Log.FEHLER_ART_PROG, "Daten.getBasisVerzeichnis", new String[]{"Kann den Ordner zum Speichern der Daten nicht anlegen!",
                        "Daten.getBasisVerzeichnis"});
                }

            }
        }
        return ret;
    }

    public void allesSpeichern() {
        new MSearchIoXmlFilmlisteSchreiben().filmeSchreiben(getBasisVerzeichnis(true) + Konstanten.XML_DATEI_FILME, listeFilme);
    }

    /**
     * Return the location of the settings directory.
     * If it does not exist, create one.
     * @return Path to the settings directory
     * @throws IOException
     */
    public static Path getSettingsDirectory() throws IOException {
        final String baseDirectoryString = System.getProperty("user.home") + File.separator + Konstanten.VERZEICHNISS_EINSTELLUNGEN + File.separator;

        Path baseDirectoryPath = Paths.get(baseDirectoryString);

        if (Files.notExists(baseDirectoryPath))
            Files.createDirectory(baseDirectoryPath);

        return baseDirectoryPath;
    }

    /**
     * Return the Path object to the downloadAbo file
     * @return Path object to downloadAbo file
     */
    public static Path getDownloadAboFilePath() {
        Path aboFilePath = null;
        try {
            aboFilePath = getSettingsDirectory().resolve("downloadAbos.txt");
            if (Files.notExists(aboFilePath))
                aboFilePath = Files.createFile(aboFilePath);
        } catch (IOException ex) {
            ex.printStackTrace();
        }

        return aboFilePath;
    }


    /**
     * Return the downloadAbos.txt File location.
     * If the file does not exist, it will be created.
     * @return the DownloadAbo File
     */
    public static File getDownloadAboFile() {
        File aboFile = null;
        try {
            Path aboFilePath = getSettingsDirectory().resolve("downloadAbos.txt");
            if (Files.notExists(aboFilePath))
                aboFilePath = Files.createFile(aboFilePath);

            aboFile = aboFilePath.toFile();
        }
        catch (IOException ex)
        {
            //FIXME assign new error code!
            Log.fehlerMeldung(898736548, Log.FEHLER_ART_PROG, "Daten.getDownloadAboFile", new String[]{"Kann den Ordner zum Speichern der Daten nicht anlegen!",
                    "Daten.getDownloadAboFile"});
        }
        return aboFile;
    }
}
