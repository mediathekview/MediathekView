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
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import mSearch.daten.ListeFilme;
import mSearch.filmlisten.writer.FilmListWriter;
import mSearch.tool.Listener;
import mSearch.tool.ReplaceList;
import mediathek.MediathekGui;
import mediathek.controller.IoXmlLesen;
import mediathek.controller.IoXmlSchreiben;
import mediathek.controller.MVUsedUrls;
import mediathek.controller.starter.StarterClass;
import mediathek.daten.*;
import mediathek.filmlisten.FilmeLaden;
import mediathek.gui.GuiAbo;
import mediathek.gui.GuiDownloads;
import mediathek.gui.GuiFilme;
import mediathek.gui.GuiMeldungen;
import mediathek.gui.dialog.DialogMediaDB;
import mediathek.gui.filmInformation.InfoDialog;
import mediathek.gui.messages.BaseEvent;
import mediathek.gui.messages.TimerEvent;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.MVFont;
import mediathek.tool.MVMessageDialog;
import net.engio.mbassy.bus.MBassador;
import net.engio.mbassy.bus.config.BusConfiguration;
import net.engio.mbassy.bus.config.Feature;
import net.engio.mbassy.bus.config.IBusConfiguration;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

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
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;

public class Daten
{

    private static Daten instance;

    public static final String LINE_SEPARATOR = System.getProperty("line.separator");
    // flags
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

    private final ObservableList<LiveStreamItem> liveStreamList = FXCollections.observableArrayList();

    public ObservableList<LiveStreamItem> getLivestreamList() {
        return liveStreamList;
    }

    // Verzeichnis zum Speichern der Programmeinstellungen
    private static String basisverzeichnis;
    public static ListePset listePset = null;
    public MVUsedUrls history = null; // alle angesehenen Filme
    public MVUsedUrls erledigteAbos = null; // erfolgreich geladenen Abos

    public StarterClass starterClass = null; // Klasse zum Ausführen der Programme (für die Downloads): VLC, flvstreamer, ...

    // Gui
    private final MediathekGui mediathekGui; // JFrame der Gui
    public static GuiFilme guiFilme = null; // Tab mit den Filmen
    public static GuiDownloads guiDownloads = null; // Tab mit den Downloads
    public static GuiAbo guiAbo = null; // Tab mit den Abos
    public static GuiMeldungen guiMeldungen = null;

    public static InfoDialog filmInfo = null; // Infos zum Film
    private DialogMediaDB dialogMediaDB;

    private boolean alreadyMadeBackup = false;

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

    private MBassador<BaseEvent> messageBus;

    public MBassador<BaseEvent> getMessageBus() {
        return messageBus;
    }

    public static Daten getInstance(String aBasisverzeichnis)
    {
        basisverzeichnis = aBasisverzeichnis;
        return getInstance();
    }

    public static Daten getInstance(String aBasisverzeichnis, MediathekGui aMediathekGui1)
    {
        basisverzeichnis = aBasisverzeichnis;
        return getInstance(aMediathekGui1);
    }

    private static Daten getInstance(MediathekGui aMediathekGui)
    {
        return instance == null ? instance = new Daten(aMediathekGui) : instance;
    }

    public static Daten getInstance()
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

    private static final Logger logger = LogManager.getLogger(Daten.class);

    /**
     * Set up message bus to log errors to our default logger
     */
    private void setupMessageBus() {
        messageBus = new MBassador<>(new BusConfiguration()
                .addFeature(Feature.SyncPubSub.Default())
                .addFeature(Feature.AsynchronousHandlerInvocation.Default())
                .addFeature(Feature.AsynchronousMessageDispatch.Default())
                .addPublicationErrorHandler(error -> logger.error(error.getMessage(), error.getCause()))
                .setProperty(IBusConfiguration.Properties.BusId, "global bus"));
    }

    private void start()
    {
        setupMessageBus();

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
            messageBus.publishAsync(new TimerEvent());
        });
        timer.setInitialDelay(4000); // damit auch alles geladen ist
        timer.start();
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
    private static Path getSettingsDirectory() throws IllegalStateException
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
    private static void getMediathekXmlCopyFilePath(ArrayList<Path> xmlFilePath)
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

    public Optional<CompletableFuture<Void>> getFilmListWriterFuture() {
        return writeFuture;
    }

    private Optional<CompletableFuture<Void>> writeFuture = Optional.empty();

    public void filmlisteSpeichern()
    {
        writeFuture.ifPresent(future -> {
            if (!future.isDone()) {
                try {
                    future.get();
                } catch (InterruptedException | ExecutionException e) {
                    logger.error(e);
                } finally {
                    writeFuture = Optional.empty();
                }
            }
        });

        CompletableFuture<Void> future = CompletableFuture.runAsync(() -> {
            //sleep for 10 seconds to give database a chance to write out changes
            try {
                TimeUnit.SECONDS.sleep(10);
            } catch (InterruptedException ignored) {
            }
            new FilmListWriter().writeFilmList(getDateiFilmliste(), listeFilme);
        });
        writeFuture = Optional.of(future);
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
            logger.info("Weder Konfig noch Backup konnte geladen werden!");
            // teils geladene Reste entfernen
            clearKonfig();
            return false;
        }
        logger.info("Konfig wurde gelesen!");
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
            final IoXmlLesen configReader = new IoXmlLesen();
            if (configReader.datenLesen(xmlFilePath))
            {
                return true;
            } else
            {
                // dann hat das Laden nicht geklappt
                logger.info("Konfig konnte nicht gelesen werden!");
            }
        } else
        {
            // dann hat das Laden nicht geklappt
            logger.info("Konfig existiert nicht!");
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
            logger.info("Es gibt kein Backup");
            return false;
        }

        // dann gibts ein Backup
        logger.info("Es gibt ein Backup");
        mediathekGui.closeSplashScreen();
        int r = JOptionPane.showConfirmDialog(null, "Die Einstellungen sind beschädigt\n"
                + "und können nicht geladen werden.\n"
                + "Soll versucht werden, mit gesicherten\n"
                + "Einstellungen zu starten?\n\n"
                + "(ansonsten startet das Programm mit\n"
                + "Standardeinstellungen)", "Gesicherte Einstellungen laden?", JOptionPane.YES_NO_OPTION);

        if (r != JOptionPane.OK_OPTION)
        {
            logger.info("User will kein Backup laden.");
            return false;
        }

        for (Path p : path)
        {
            // teils geladene Reste entfernen
            clearKonfig();
            logger.info("Versuch Backup zu laden: {}", p.toString());
            final IoXmlLesen configReader = new IoXmlLesen();
            if (configReader.datenLesen(p))
            {
                logger.info("Backup hat geklappt: {}", p.toString());
                ret = true;
                break;
            }

        }
        return ret;
    }

    public void allesSpeichern()
    {
        konfigCopy();

        final IoXmlSchreiben configWriter = new IoXmlSchreiben();
        configWriter.writeConfigurationFile(getMediathekXmlFilePath());

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
                logger.error("Die Einstellungen konnten nicht zurückgesetzt werden.", e);
                if (mediathekGui != null)
                {
                    MVMessageDialog.showMessageDialog(mediathekGui, "Die Einstellungen konnten nicht zurückgesetzt werden.\n"
                            + "Sie müssen jetzt das Programm beenden und dann den Ordner:\n"
                            + getSettingsDirectory_String() + '\n'
                            + "von Hand löschen und dann das Programm wieder starten.\n\n"
                            + "Im Forum finden Sie weitere Hilfe.", "Fehler", JOptionPane.ERROR_MESSAGE);
                }
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
            logger.info("-------------------------------------------------------");
            logger.info("Einstellungen sichern");

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
                    logger.info("Einstellungen wurden gesichert");
                } else
                {
                    logger.info("Einstellungen wurden heute schon gesichert");
                }
            } catch (IOException e)
            {
                logger.error("Die Einstellungen konnten nicht komplett gesichert werden!", e);
            }

            alreadyMadeBackup = true;
            logger.info("-------------------------------------------------------");
        }
    }

    /**
     * Return the number of milliseconds from today´s midnight.
     *
     * @return Number of milliseconds from today´s midnight.
     */
    private static long getHeute_0Uhr()
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

    public void setDialogMediaDB(final DialogMediaDB aDialogMediaDB)
    {
        dialogMediaDB = aDialogMediaDB;
    }

    public DialogMediaDB getDialogMediaDB()
    {
        return dialogMediaDB;
    }
}
