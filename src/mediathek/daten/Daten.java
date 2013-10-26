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
import javax.swing.JOptionPane;
import mediathek.MediathekGui;
import mediathek.controller.filmeLaden.FilmeLaden;
import mediathek.controller.io.ErledigteAbos;
import mediathek.controller.io.History;
import mediathek.controller.io.IoXmlLesen;
import mediathek.controller.io.IoXmlSchreiben;
import mediathek.controller.io.starter.StarterClass;
import mediathek.gui.GuiAbo;
import mediathek.gui.GuiDownloads;
import mediathek.gui.GuiFilme;
import mediathek.gui.dialog.MVFilmInformation;
import mediathek.tool.DatumZeit;
import mediathek.tool.Funktionen;
import mediathek.tool.GuiKonstanten;
import mediathek.tool.Konstanten;
import mediathek.tool.Log;
import mediathek.tool.MVMessageDialog;
import msearch.daten.ListeFilme;
import msearch.io.MSearchFilmlisteLesen;
import msearch.io.MSearchFilmlisteSchreiben;

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

    public static ListeFilme listeFilmeNachBlackList = null;
    public ListeBlacklist listeBlacklist = null;
    public ListePset listePset = null;
    public ListeAbo listeAbo = null;
    public ListeDownloads listeDownloads = null;
    public History history = null;
    public ErledigteAbos erledigteAbos = null;
    // globale Objekte
    public IoXmlLesen ioXmlLesen = null;
    public IoXmlSchreiben ioXmlSchreiben = null;
    public StarterClass starterClass = null; // Klasse zum Ausführen der Programme: VLC, flvstreamer, ...
    // Panel
    public MediathekGui mediathekGui = null; // JFrame der Gui
    public GuiFilme guiFilme = null; // Tab mit den Filmen
    public GuiDownloads guiDownloads = null; // Tab mit den Downloads
    public GuiAbo guiAbo = null; // Tab mit den Abos
    // für die Tabellen
    public boolean nachDownloadShutDown = false;
    public MVFilmInformation filmInfoHud = null;
    
    
    
    public Daten(String pfad) {
        basisverzeichnis = pfad;
        init();
    }
    public Daten(String basis, MediathekGui gui) {
        basisverzeichnis = basis;
        init();
        mediathekGui = gui;

        updateSplashScreen("Lade Blacklist...");
        listeFilmeNachBlackList = new ListeFilme();
        listeBlacklist = new ListeBlacklist();

        updateSplashScreen("Lade Programmsets...");
        listePset = new ListePset();

        updateSplashScreen("Lade Abos...");
        listeAbo = new ListeAbo(this);

        updateSplashScreen("Lade Downloads...");
        listeDownloads = new ListeDownloads(this);

        updateSplashScreen("Lade erledigte Abos...");
        erledigteAbos = new ErledigteAbos();

        //initialisieren
        updateSplashScreen("Lade Filmliste...");
        ioXmlLesen = new IoXmlLesen();
        ioXmlSchreiben = new IoXmlSchreiben();

        history = new History();

        starterClass = new StarterClass(this);
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
        Daten.system[Konstanten.SYSTEM_NOTIFICATION_NR] = Boolean.TRUE.toString();
        //        Daten.system[Konstanten.SYSTEM_PANEL_FILTER_ANZEIGEN_NR] = Boolean.TRUE.toString();
        if (Daten.debug) {
            Daten.system[Konstanten.SYSTEM_IMPORT_ART_FILME_NR] = String.valueOf(GuiKonstanten.UPDATE_FILME_AUS);
        }
        listeFilme = new ListeFilme();
        filmeLaden = new FilmeLaden();
    }
    /**
     * Update the {@link java.awt.SplashScreen} only if we have a Swing UI.
     *
     * @param text The displayed text on the splash graphics.
     */
    private void updateSplashScreen(String text) {
        if (mediathekGui != null) {
            mediathekGui.updateSplashScreenText(text);
        }
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

    /**
     * Return the location of the settings directory.
     * If it does not exist, create one.
     *
     * @return Path to the settings directory
     * @throws IOException
     */
    public static Path getSettingsDirectory() throws IOException {
        final String baseDirectoryString;
        if (basisverzeichnis.equals("")) {
            baseDirectoryString = System.getProperty("user.home") + File.separator + Konstanten.VERZEICHNISS_EINSTELLUNGEN + File.separator;
        } else {
            baseDirectoryString = basisverzeichnis;
        }

        Path baseDirectoryPath = Paths.get(baseDirectoryString);

        if (Files.notExists(baseDirectoryPath)) {
            Files.createDirectory(baseDirectoryPath);
        }

        return baseDirectoryPath;
    }

    /**
     * Return the path to "mediathek.xml"
     *
     * @return
     */
    public static Path getMediathekXmlFilePath() {
        Path xmlFilePath = null;
        try {
            xmlFilePath = Daten.getSettingsDirectory().resolve("mediathek.xml");
        } catch (IOException ex) {
            ex.printStackTrace();
        }
        return xmlFilePath;
    }
    public void allesLaden() {
        updateSplashScreen("Lade Konfigurationsdaten...");
        ioXmlLesen.datenLesen(this);

        updateSplashScreen("Lade History...");
        history.laden();

        // erst die Systemdaten, dann die Filmliste
        updateSplashScreen("Lade Filmliste...");
        new MSearchFilmlisteLesen().filmlisteLesenJson(Daten.getDateiFilmliste(), "", Daten.listeFilme);
    }

    public static void filmlisteSpeichern() {
        new MSearchFilmlisteSchreiben().filmlisteSchreibenJson(getBasisVerzeichnis(true) + Konstanten.XML_DATEI_FILME, listeFilme);
    }

    public void allesSpeichern() {
//        super.allesSpeichern();
        ioXmlSchreiben.datenSchreiben(this);
        if (Daten.RESET) {
            // das Programm soll beim nächsten Start mit den Standardeinstellungen gestartet werden
            // dazu wird den Ordner mit den Einstellungen umbenannt
            String dir1 = getBasisVerzeichnis();
            if (dir1.endsWith(File.separator)) {
                dir1 = dir1.substring(0, dir1.length() - 1);
            }
            String dir2 = dir1 + "--" + DatumZeit.getJetzt_yyyy_MM_dd__HH_mm_ss();
            try {
                if (new File(dir1).renameTo(new File(dir2))) {
                    // erster Versuch
                    return;
                }
                // für Win weitere Versuche
                if (new File(dir1).delete()) {
                    return;
                }
                Log.systemMeldung("Die Einstellungen konnten nicht zurückgesetzt werden.");
                MVMessageDialog.showMessageDialog(this.mediathekGui, "Die Einstellungen konnten nicht zurückgesetzt werden.\n"
                        + "Es wird versucht die Einstellungen beim Beenden zu löschen.\n"
                        + "Sollte auch das nicht klappen,\n"
                        + "finden Sie im Forum weitere Hilfe.", "Fehler", JOptionPane.ERROR_MESSAGE);
                new File(dir1).deleteOnExit();
            } catch (Exception e) {
                Log.fehlerMeldung(465690123, Log.FEHLER_ART_PROG, Daten.class.getName(), e);
            }
        }
    }
}
