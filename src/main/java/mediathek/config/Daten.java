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
package mediathek.config;

import com.jidesoft.utils.SystemInfo;
import mSearch.daten.ListeFilme;
import mSearch.filmlisten.WriteFilmlistJson;
import mSearch.tool.Listener;
import mSearch.tool.Log;
import mSearch.tool.ReplaceList;
import mSearch.tool.SysMsg;
import mediathek.MediathekGui;
import mediathek.controller.IoXmlLesen;
import mediathek.controller.IoXmlSchreiben;
import mediathek.controller.MVUsedUrls;
import mediathek.controller.starter.StarterClass;
import mediathek.daten.*;
import mediathek.filmlisten.FilmeLaden;
import mediathek.gui.*;
import mediathek.gui.dialog.DialogMediaDB;
import mediathek.gui.filmInformation.IFilmInformation;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.MVFont;
import mediathek.tool.MVMessageDialog;

import javax.swing.*;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileTime;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;

public class Daten
{

    private static Daten instance;

    public static final String LINE_SEPARATOR = System.getProperty("line.separator");
    // flags
    private static boolean debug = false; // Debugmodus
    private static boolean startMaximized = false; // Fenster maximieren
    private static boolean auto = false; // Version: MediathekAuto
    private static boolean reset = false; // Programm auf Starteinstellungen zurücksetzen
    //alle Programmeinstellungen

    // zentrale Klassen
    public static final MVColor mVColor = new MVColor(); // verwendete Farben
    private FilmeLaden filmeLaden; // erledigt das updaten der Filmliste
    private ListeFilme listeFilme = null;
    private ListeFilme listeFilmeNachBlackList = null; // ist DIE Filmliste
    private ListeFilme listeFilmeHistory = null; // für die HEUTIGE HISTORY
    private ListeDownloads listeDownloads = null; // Filme die als "Download: Tab Download" geladen werden sollen
    private ListeDownloads listeDownloadsButton = null; // Filme die über "Tab Filme" als Button/Film abspielen gestartet werden
    private ListeBlacklist listeBlacklist = null;
    private ListeMediaDB listeMediaDB = null;
    private ListeMediaPath listeMediaPath = null;
    private ListeAbo listeAbo = null;
    private DownloadInfos downloadInfos = null;

    // Verzeichnis zum Speichern der Programmeinstellungen
    private static String basisverzeichnis;
    public static ListePset listePset = null;
    public MVUsedUrls history = null; // alle angesehenen Filme
    public MVUsedUrls erledigteAbos = null; // erfolgreich geladenen Abos

    public StarterClass starterClass = null; // Klasse zum Ausführen der Programme (für die Downloads): VLC, flvstreamer, ...

    // Gui
    private MediathekGui mediathekGui; // JFrame der Gui
    public static GuiFilme guiFilme = null; // Tab mit den Filmen
    public static GuiDownloads guiDownloads = null; // Tab mit den Downloads
    public static GuiAbo guiAbo = null; // Tab mit den Abos
    public static GuiDebug guiDebug = null;
    public static GuiMeldungen guiMeldungen = null;

    public static IFilmInformation filmInfo = null; // Infos zum Film
    private DialogMediaDB dialogMediaDB;

    private boolean alreadyMadeBackup = false;

    public static void setDebug(final boolean aIsDebug)
    {
        debug = aIsDebug;
    }

    public static void setStartMaximized(final boolean aIsStartMaximized)
    {
        startMaximized = aIsStartMaximized;
    }

    public static void setAuto(final boolean aIsAuto)
    {
        auto = aIsAuto;
    }

    public static void setReset(final boolean aIsReset)
    {
        reset = aIsReset;
    }

    public static boolean isDebug()
    {
        return debug;
    }

    public static boolean isStartMaximized()
    {
        return startMaximized;
    }

    public static boolean isAuto()
    {
        return auto;
    }

    public static boolean isReset()
    {
        return reset;
    }

    public static final Daten getInstance(String aBasisverzeichnis)
    {
        basisverzeichnis = aBasisverzeichnis;
        return getInstance();
    }

    public static final Daten getInstance(String aBasisverzeichnis, MediathekGui aMediathekGui1)
    {
        basisverzeichnis = aBasisverzeichnis;
        return getInstance(aMediathekGui1);
    }

    private static final Daten getInstance(MediathekGui aMediathekGui)
    {
        return instance == null ? instance = new Daten(aMediathekGui) : instance;
    }

    public static final Daten getInstance()
    {
        return instance == null ? instance =  new Daten() : instance;
    }


    private Daten()
    {
        mediathekGui = null;
        start();
    }

    private Daten(MediathekGui aMediathekGui)
    {
        mediathekGui = aMediathekGui;
        start();
    }

    private void start()
    {
        listeFilme = new ListeFilme();
        filmeLaden = new FilmeLaden(this);
        listeFilmeHistory = new ListeFilme();

        updateSplashScreen("Lade Blacklist...");
        listeFilmeNachBlackList = new ListeFilme();
        listeBlacklist = new ListeBlacklist();

        updateSplashScreen("Lade Programmsets...");
        listePset = new ListePset();

        updateSplashScreen("Lade Abos...");
        listeAbo = new ListeAbo(this);

        updateSplashScreen("Lade Downloads...");
        listeDownloads = new ListeDownloads(this);
        listeDownloadsButton = new ListeDownloads(this);

        updateSplashScreen("Lade erledigte Abos...");
        erledigteAbos = new MVUsedUrls(Konstanten.FILE_ERLEDIGTE_ABOS, getSettingsDirectory_String(), Listener.EREIGNIS_LISTE_ERLEDIGTE_ABOS);

        updateSplashScreen("Lade History...");
        history = new MVUsedUrls(Konstanten.FILE_HISTORY, getSettingsDirectory_String(), Listener.EREIGNIS_LISTE_HISTORY_GEAENDERT);

        listeMediaDB = new ListeMediaDB(this);
        listeMediaPath = new ListeMediaPath();

        downloadInfos = new DownloadInfos(this);
        starterClass = new StarterClass(this);

        Timer timer = new Timer(1000, e ->
        {
            downloadInfos.makeDownloadInfos();
            Listener.notify(Listener.EREIGNIS_TIMER, Daten.class.getName());
        });
        timer.setInitialDelay(4000); // damit auch alles geladen ist
        timer.start();
    }

    public static String getUserAgent()
    {
        return MVConfig.get(MVConfig.Configs.SYSTEM_PARAMETER_USERAGENT);
    }

    /**
     * Liefert den Pfad zur Filmliste
     *
     * @return Den Pfad als String
     */
    public static String getDateiFilmliste()
    {
        String strFile;

        if (SystemInfo.isMacOSX())
        {
            //place filmlist into OS X user cache directory in order not to backup it all the time in TimeMachine...
            strFile = GuiFunktionen.getHomePath() + File.separator + "Library/Caches/MediathekView" + File.separator + Konstanten.JSON_DATEI_FILME;
        } else
        {
            strFile = getSettingsDirectory_String() + File.separator + Konstanten.JSON_DATEI_FILME;
        }

        return strFile;
    }

    /**
     * Return the location of the settings directory.
     * If it does not exist, create one.
     *
     * @return Path to the settings directory
     * @throws IllegalStateException Will be thrown if settings directory don't exist and if there is an error on creating it.
     */
    public static Path getSettingsDirectory() throws IllegalStateException
    {
        final Path baseDirectoryPath;
        if (basisverzeichnis == null || basisverzeichnis.isEmpty())
        {
            baseDirectoryPath = Paths.get(System.getProperty("user.home"), Konstanten.VERZEICHNIS_EINSTELLUNGEN);
        } else
        {
            baseDirectoryPath = Paths.get(basisverzeichnis);
        }


        if (Files.notExists(baseDirectoryPath))
        {
            try
            {
                Files.createDirectories(baseDirectoryPath);
            } catch (IOException ioException)
            {
                Messages.logMessage(Messages.ERROR_CANT_CREATE_FOLDER, ioException, baseDirectoryPath.toString());
                throw new IllegalStateException(Messages.ERROR_CANT_CREATE_FOLDER.getTextFormatted(baseDirectoryPath.toString()), ioException);
            }
        }

        return baseDirectoryPath;
    }

    public static String getSettingsDirectory_String()
    {
        return getSettingsDirectory().toString();
    }

    /**
     * Return the path to "mediathek.xml"
     *
     * @return Path object to mediathek.xml file
     */
    public static Path getMediathekXmlFilePath()
    {
        return Daten.getSettingsDirectory().resolve(Konstanten.CONFIG_FILE);
    }

    /**
     * Return the path to "mediathek.xml_copy_"
     * first copy exists
     *
     * @param xmlFilePath Path to file.
     */
    public static void getMediathekXmlCopyFilePath(ArrayList<Path> xmlFilePath)
    {
        for (int i = 1; i <= MAX_COPY; ++i)
        {
            Path path = Daten.getSettingsDirectory().resolve(Konstanten.CONFIG_FILE_COPY + i);
            if (Files.exists(path))
            {
                xmlFilePath.add(path);
            }
        }
    }

    public void filmlisteSpeichern()
    {
        new WriteFilmlistJson().filmlisteSchreibenJson(getDateiFilmliste(), listeFilme);
    }

    /**
     * Update the {@link java.awt.SplashScreen} only if we have a Swing UI.
     *
     * @param text The displayed text on the splash graphics.
     */
    private void updateSplashScreen(String text)
    {
        if (mediathekGui != null)
        {
            mediathekGui.updateSplashScreenText(text);
        }
    }

    public boolean allesLaden()
    {
        updateSplashScreen("Lade Konfigurationsdaten...");

        if (!load())
        {
            SysMsg.sysMsg("Weder Konfig noch Backup konnte geladen werden!");
            // teils geladene Reste entfernen
            clearKonfig();
            return false;
        }
        SysMsg.sysMsg("Konfig wurde gelesen!");
        mVColor.load(); // Farben einrichten
        MVFont.initFont(); // Fonts einrichten

        // erst die Systemdaten, dann die Filmliste
        updateSplashScreen("Lade Filmliste...");
        return true;
    }

    private void clearKonfig()
    {
        listePset.clear();
        ReplaceList.list.clear();
        listeAbo.clear();
        listeDownloads.clear();
        listeBlacklist.clear();
    }

    private boolean load()
    {
        boolean ret = false;
        Path xmlFilePath = Daten.getMediathekXmlFilePath();

        if (Files.exists(xmlFilePath))
        {
            if (IoXmlLesen.datenLesen(xmlFilePath))
            {
                return true;
            } else
            {
                // dann hat das Laden nicht geklappt
                SysMsg.sysMsg("Konfig konnte nicht gelesen werden!");
            }
        } else
        {
            // dann hat das Laden nicht geklappt
            SysMsg.sysMsg("Konfig existiert nicht!");
        }

        // versuchen das Backup zu laden
        if (loadBackup())
        {
            ret = true;
        }
        return ret;
    }

    private boolean loadBackup()
    {
        boolean ret = false;
        ArrayList<Path> path = new ArrayList<>();
        Daten.getMediathekXmlCopyFilePath(path);
        if (path.isEmpty())
        {
            SysMsg.sysMsg("Es gibt kein Backup");
            return false;
        }

        // dann gibts ein Backup
        SysMsg.sysMsg("Es gibt ein Backup");
        mediathekGui.closeSplashScreen();
        int r = JOptionPane.showConfirmDialog(null, "Die Einstellungen sind beschädigt\n"
                + "und können nicht geladen werden.\n"
                + "Soll versucht werden, mit gesicherten\n"
                + "Einstellungen zu starten?\n\n"
                + "(ansonsten startet das Programm mit\n"
                + "Standardeinstellungen)", "Gesicherte Einstellungen laden?", JOptionPane.YES_NO_OPTION);

        if (r != JOptionPane.OK_OPTION)
        {
            SysMsg.sysMsg("User will kein Backup laden.");
            return false;
        }

        for (Path p : path)
        {
            // teils geladene Reste entfernen
            clearKonfig();
            SysMsg.sysMsg(new String[]{"Versuch Backup zu laden:", p.toString()});
            if (IoXmlLesen.datenLesen(p))
            {
                SysMsg.sysMsg(new String[]{"Backup hat geklappt:", p.toString()});
                ret = true;
                break;
            }

        }
        return ret;
    }

    public void allesSpeichern()
    {
        konfigCopy();
        IoXmlSchreiben.datenSchreiben();
        if (Daten.isReset())
        {
            // das Programm soll beim nächsten Start mit den Standardeinstellungen gestartet werden
            // dazu wird den Ordner mit den Einstellungen umbenannt
            String dir1 = getSettingsDirectory_String();
            if (dir1.endsWith(File.separator))
            {
                dir1 = dir1.substring(0, dir1.length() - 1);
            }

            try
            {
                final Path path1 = Paths.get(dir1);
                final String dir2 = dir1 + "--" + new SimpleDateFormat("yyyy.MM.dd__HH.mm.ss").format(new Date());

                Files.move(path1, Paths.get(dir2), StandardCopyOption.REPLACE_EXISTING);
                Files.deleteIfExists(path1);
            } catch (IOException e)
            {
                SysMsg.sysMsg("Die Einstellungen konnten nicht zurückgesetzt werden.");
                if (mediathekGui != null)
                {
                    MVMessageDialog.showMessageDialog(mediathekGui, "Die Einstellungen konnten nicht zurückgesetzt werden.\n"
                            + "Sie müssen jetzt das Programm beenden und dann den Ordner:\n"
                            + getSettingsDirectory_String() + "\n"
                            + "von Hand löschen und dann das Programm wieder starten.\n\n"
                            + "Im Forum finden Sie weitere Hilfe.", "Fehler", JOptionPane.ERROR_MESSAGE);
                }
                Log.errorLog(465690123, e);
            }
        }
    }

    /**
     * Maximum number of backup files to be stored.
     */
    private final static int MAX_COPY = 5;

    /**
     * Create backup copies of settings file.
     */
    private void konfigCopy()
    {
        if (!alreadyMadeBackup)
        {
            // nur einmal pro Programmstart machen
            SysMsg.sysMsg("-------------------------------------------------------");
            SysMsg.sysMsg("Einstellungen sichern");

            try
            {
                final Path xmlFilePath = Daten.getMediathekXmlFilePath();
                long creatTime = -1;

                Path xmlFilePathCopy_1 = Daten.getSettingsDirectory().resolve(Konstanten.CONFIG_FILE_COPY + 1);
                if (Files.exists(xmlFilePathCopy_1))
                {
                    BasicFileAttributes attrs = Files.readAttributes(xmlFilePathCopy_1, BasicFileAttributes.class);
                    FileTime d = attrs.lastModifiedTime();
                    creatTime = d.toMillis();
                }

                if (creatTime == -1 || creatTime < getHeute_0Uhr())
                {
                    // nur dann ist die letzte Kopie älter als einen Tag
                    for (int i = MAX_COPY; i > 1; --i)
                    {
                        xmlFilePathCopy_1 = Daten.getSettingsDirectory().resolve(Konstanten.CONFIG_FILE_COPY + (i - 1));
                        final Path xmlFilePathCopy_2 = Daten.getSettingsDirectory().resolve(Konstanten.CONFIG_FILE_COPY + i);
                        if (Files.exists(xmlFilePathCopy_1))
                        {
                            Files.move(xmlFilePathCopy_1, xmlFilePathCopy_2, StandardCopyOption.REPLACE_EXISTING);
                        }
                    }
                    if (Files.exists(xmlFilePath))
                    {
                        Files.move(xmlFilePath, Daten.getSettingsDirectory().resolve(Konstanten.CONFIG_FILE_COPY + 1), StandardCopyOption.REPLACE_EXISTING);
                    }
                    SysMsg.sysMsg("Einstellungen wurden gesichert");
                } else
                {
                    SysMsg.sysMsg("Einstellungen wurden heute schon gesichert");
                }
            } catch (IOException e)
            {
                SysMsg.sysMsg("Die Einstellungen konnten nicht komplett gesichert werden!");
                Log.errorLog(795623147, e);
            }

            alreadyMadeBackup = true;
            SysMsg.sysMsg("-------------------------------------------------------");
        }
    }

    /**
     * Return the number of milliseconds from today´s midnight.
     *
     * @return Number of milliseconds from today´s midnight.
     */
    public static long getHeute_0Uhr()
    {
        final Calendar cal = Calendar.getInstance();
        cal.set(Calendar.HOUR_OF_DAY, 0);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MILLISECOND, 0);

        return cal.getTimeInMillis();
    }

    public FilmeLaden getFilmeLaden()
    {
        return filmeLaden;
    }

    public ListeFilme getListeFilme()
    {
        return listeFilme;
    }

    public ListeFilme getListeFilmeNachBlackList()
    {
        return listeFilmeNachBlackList;
    }

    public ListeFilme getListeFilmeHistory()
    {
        return listeFilmeHistory;
    }

    public ListeDownloads getListeDownloads()
    {
        return listeDownloads;
    }

    public ListeDownloads getListeDownloadsButton()
    {
        return listeDownloadsButton;
    }

    public ListeBlacklist getListeBlacklist()
    {
        return listeBlacklist;
    }

    public ListeMediaDB getListeMediaDB()
    {
        return listeMediaDB;
    }

    public ListeMediaPath getListeMediaPath()
    {
        return listeMediaPath;
    }

    public ListeAbo getListeAbo()
    {
        return listeAbo;
    }

    public DownloadInfos getDownloadInfos()
    {
        return downloadInfos;
    }

    public MediathekGui getMediathekGui()
    {
        return mediathekGui;
    }

    public void setListeFilme(final ListeFilme listeFilme)
    {
        this.listeFilme = listeFilme;
    }

    public void setDialogMediaDB(final DialogMediaDB aDialogMediaDB)
    {
        dialogMediaDB = aDialogMediaDB;
    }

    public DialogMediaDB getDialogMediaDB()
    {
        return dialogMediaDB;
    }
}
