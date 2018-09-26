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
package mediathek;

import javafx.application.Platform;
import javafx.beans.property.IntegerProperty;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.ObservableList;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.stage.Stage;
import jiconfont.icons.FontAwesome;
import jiconfont.swing.IconFontSwing;
import mSearch.daten.DatenFilm;
import mSearch.filmeSuchen.ListenerFilmeLaden;
import mSearch.filmeSuchen.ListenerFilmeLadenEvent;
import mSearch.tool.ApplicationConfiguration;
import mSearch.tool.Functions.OperatingSystemType;
import mSearch.tool.Listener;
import mSearch.tool.Log;
import mSearch.tool.ReplaceList;
import mediathek.config.*;
import mediathek.controller.starter.Start;
import mediathek.daten.DatenDownload;
import mediathek.daten.ListeMediaDB;
import mediathek.filmlisten.FilmeLaden;
import mediathek.gui.*;
import mediathek.gui.actions.CreateProtocolFileAction;
import mediathek.gui.actions.ResetSettingsAction;
import mediathek.gui.actions.ShowFilmInformationAction;
import mediathek.gui.actions.ShowOnlineHelpAction;
import mediathek.gui.actions.export.FilmListExportAction;
import mediathek.gui.bandwidth.BandwidthMonitorController;
import mediathek.gui.dialog.*;
import mediathek.gui.dialogEinstellungen.DialogEinstellungen;
import mediathek.gui.filmInformation.InfoDialog;
import mediathek.gui.messages.*;
import mediathek.javafx.*;
import mediathek.javafx.tool.FXProgressPane;
import mediathek.res.GetIcon;
import mediathek.tool.*;
import mediathek.tool.threads.IndicatorThread;
import mediathek.update.AutomaticFilmlistUpdate;
import mediathek.update.ProgramUpdateCheck;
import mediathek.update.ProgrammUpdateSuchen;
import net.engio.mbassy.listener.Handler;
import org.apache.commons.configuration2.Configuration;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuListener;
import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.HashMap;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static mSearch.tool.Functions.getOs;
import static mediathek.tool.MVFunctionSys.startMeldungen;

@SuppressWarnings("serial")
public class MediathekGui extends JFrame {

    private static final String ICON_NAME = "MediathekView.png";
    private static final String ICON_PATH = "/mediathek/res/";
    private static final int ICON_WIDTH = 58;
    private static final int ICON_HEIGHT = 58;
    private static final String KEY_F10 = "F10";
    private static final String NONE = "none";
    private static final Logger logger = LogManager.getLogger(MediathekGui.class);
    /**
     * "Pointer" to UI
     */
    private static MediathekGui ui = null;
    /**
     * Number of active downloads
     */
    protected final AtomicInteger numDownloadsStarted = new AtomicInteger(0);
    private final Daten daten;
    private final SplashScreenManager splashScreenManager;
    private final JMenu jMenuDatei = new JMenu();
    private final JMenu jMenuFilme = new JMenu();
    private final JMenuBar jMenuBar = new JMenuBar();
    private final JMenu jMenuDownload = new JMenu();
    private final JMenu jMenuAbos = new JMenu();
    private final JMenu jMenuAnsicht = new JMenu();
    private final JMenu jMenuHilfe = new JMenu();
    /**
     * this property keeps track how many items are currently selected in the active table view
     */
    private final IntegerProperty selectedItemsProperty = new SimpleIntegerProperty(0);
    /**
     * Helper to determine what tab is currently active
     */
    private final ObjectProperty<TabPaneIndex> tabPaneIndexProperty = new SimpleObjectProperty<>(TabPaneIndex.NONE);
    private final JTabbedPane tabbedPane = new JTabbedPane();
    private final HashMap<JMenu, MenuTabSwitchListener> menuListeners = new HashMap<>();
    private final JMenuItem miLoadFilmList = new JMenuItem("Neue Filmliste laden...");
    private final JCheckBoxMenuItem cbBandwidthDisplay = new JCheckBoxMenuItem("Bandbreitennutzung");
    private final JCheckBoxMenuItem cbSearchMediaDb = new JCheckBoxMenuItem("Mediensammlung durchsuchen");
    private final JMenuItem miSearchProgramUpdate = new JMenuItem("Nach Update suchen...");
    public GuiFilme tabFilme;
    public GuiDownloads tabDownloads;
    public GuiAbo tabAbos;
    /**
     * Bandwidth monitoring for downloads.
     */
    protected BandwidthMonitorController bandwidthMonitor;
    protected Stage controlsFxWorkaroundStage;
    /**
     * the global configuration for this app.
     */
    protected Configuration config = ApplicationConfiguration.getConfiguration();
    /**
     * Used for implementing shutting down the system.
     */
    protected ShutdownComputerCommand shutdownCommand;
    private MVTray tray;
    private DialogEinstellungen dialogEinstellungen;
    /**
     * Memory display for debugging purposes.
     * Only visible when debug mode is enabled
     */
    private MemoryMonitor memoryMonitor;
    private StatusBarController statusBarController;
    private InfoDialog filmInfo; // Infos zum Film
    private ProgramUpdateCheck programUpdateChecker;
    /**
     * Progress indicator thread for OS X and windows.
     */
    private IndicatorThread progressIndicatorThread;

    public MediathekGui() {
        super();
        getContentPane().setLayout(new BorderLayout());

        ui = this;

        setIconAndWindowImage();

        setupShutdownCommand();

        splashScreenManager = new SplashScreenManager();
        splashScreenManager.initializeSplashScreen();

        splashScreenManager.updateSplashScreenText("Anwendungsdaten laden...");

        createMenuBar();

        fakeInitializeJavaFXRuntime();

        remapF10Key();

        daten = Daten.getInstance();

        startMeldungen();

        loadDaten();

        createStatusBar();

        createFilmInformationHUD();

        setLookAndFeel();

        setupFilmListListener();

        initTabs();

        initMenus();

        initWindowListener();

        setTray();

        setSize();

        initializeSettingsDialog();

        addListener();

        setupSearchKeyForMac();

        //register message bus handler
        daten.getMessageBus().subscribe(this);

        setFocusOnSearchField();

        createMemoryMonitor();

        bandwidthMonitor = new BandwidthMonitorController(this);

        splashScreenManager.closeSplashScreen();

        if (!SystemUtils.IS_OS_WINDOWS)
            workaroundControlsFxNotificationBug();

        /*Platform.runLater(() -> {
            MainWindow window = new MainWindow();
            window.show();
            window.setMaximized(true);
        });*/

        loadFilmlist();
    }

    /**
     * Return the user interface instance
     *
     * @return the class instance or null.
     */
    public static MediathekGui ui() {
        return ui;
    }

    private void setIconAndWindowImage() {
        setWindowTitle();
        setIconImage(GetIcon.getIcon(ICON_NAME, ICON_PATH, ICON_WIDTH, ICON_HEIGHT).getImage());
    }

    public void updateSplashScreenText(final String aSplashScreenText) {
        splashScreenManager.updateSplashScreenText(aSplashScreenText);
    }

    public void closeSplashScreen() {
        splashScreenManager.closeSplashScreen();
    }

    private void remapF10Key() {
        //Hier wird F10 default Funktion unterbunden:
        InputMap im = jMenuBar.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
        im.put(KeyStroke.getKeyStroke(KEY_F10), NONE);
    }

    /**
     * setup the system wide shutdown handler
     */
    protected void setupShutdownCommand() {
        shutdownCommand = new GenericShutdownComputerCommand();
    }

    /**
     * Initialize JavaFX runtime by calling swing interop class.
     * This will start JavaFX thread in case no window has been started yet.
     * Necessary in case no config is found.
     */
    @SuppressWarnings("unused")
    private void fakeInitializeJavaFXRuntime() {
        final JFXPanel dummyPanel = new JFXPanel();
    }

    private void createMenuBar() {
        jMenuDatei.setMnemonic('d');
        jMenuDatei.setText("Datei");
        jMenuBar.add(jMenuDatei);

        jMenuFilme.setMnemonic('F');
        jMenuFilme.setText("Filme");
        jMenuBar.add(jMenuFilme);

        jMenuDownload.setMnemonic('O');
        jMenuDownload.setText("Downloads");
        jMenuBar.add(jMenuDownload);

        jMenuAbos.setMnemonic('b');
        jMenuAbos.setText("Abos");
        jMenuBar.add(jMenuAbos);

        jMenuAnsicht.setMnemonic('a');
        jMenuAnsicht.setText("Ansicht");
        jMenuBar.add(jMenuAnsicht);

        jMenuHilfe.setMnemonic('h');
        jMenuHilfe.setText("Hilfe");
        jMenuBar.add(jMenuHilfe);

        setJMenuBar(jMenuBar);
    }

    /**
     * ControlsFX Notifications expect a stage to be open.
     * Create a utility window hidden and transparent as a stage for them.
     */
    protected void workaroundControlsFxNotificationBug() {
        //does not work on windows and linux
    }

    private void createMemoryMonitor() {
        Platform.runLater(() -> {
            if (Config.isDebuggingEnabled()) {
                memoryMonitor = new MemoryMonitor();
                memoryMonitor.show();
            }
        });
    }

    /**
     * Read a local filmlist or load a new one in auto mode.
     */
    private void loadFilmlist() {
        Platform.runLater(() -> {
            FXProgressPane hb = new FXProgressPane();
            FilmListReaderTask filmListReaderTask = new FilmListReaderTask();
            filmListReaderTask.setOnRunning(e -> {
                getStatusBarController().getStatusBar().getRightItems().add(hb);
                hb.lb.textProperty().bind(filmListReaderTask.messageProperty());
                hb.prog.progressProperty().bind(filmListReaderTask.progressProperty());
            });

            FilmListFilterTask filterTask = new FilmListFilterTask(true);
            filterTask.setOnRunning(e -> {
                hb.lb.textProperty().bind(filterTask.messageProperty());
                hb.prog.progressProperty().bind(filterTask.progressProperty());
            });
            filterTask.setOnSucceeded(e -> getStatusBarController().getStatusBar().getRightItems().remove(hb));
            filterTask.setOnFailed(e -> getStatusBarController().getStatusBar().getRightItems().remove(hb));

            CompletableFuture<Void> loaderTask = CompletableFuture.runAsync(filmListReaderTask);
            loaderTask.thenRun(filterTask);
        });
    }

    /**
     * Setup the keyboard for search field on macOS.
     * Ununsed on other platforms.
     */
    protected void setupSearchKeyForMac() {
    }

    private void initializeSettingsDialog() {
        // Dialog mit den Programmeinstellungen einrichten
        dialogEinstellungen = new DialogEinstellungen(daten);
        daten.setDialogMediaDB(new DialogMediaDB(this));
        daten.getDialogMediaDB().setVis();
    }

    private void loadDaten() {
        if (daten.allesLaden()) {
            // alles geladen
            splashScreenManager.updateSplashScreenText("GUI Initialisieren...");
        } else {
            // erster Start
            ReplaceList.init(); // einmal ein Muster anlegen, für Linux/OS X ist es bereits aktiv!
            new DialogStarteinstellungen(this, daten).setVisible(true);
            MVConfig.loadSystemParameter();

            pack();
        }
    }

    public IntegerProperty getSelectedItemsProperty() {
        return selectedItemsProperty;
    }

    /**
     * Create the status bar item.
     */
    private void createStatusBar() {
        statusBarController = new StatusBarController(daten);

        JFXPanel statusBarPanel = new JFXPanel();
        getContentPane().add(statusBarPanel, BorderLayout.PAGE_END);
        Platform.runLater(() -> {
            statusBarPanel.setScene(new Scene(statusBarController.createStatusBar()));
            installSelectedItemsLabel();
        });
    }

    private void installSelectedItemsLabel() {
        ObservableList<Node> leftItems = statusBarController.getStatusBar().getLeftItems();
        leftItems.add(0, new SelectedItemsLabel(selectedItemsProperty));
        leftItems.add(1, new VerticalSeparator());
    }

    public StatusBarController getStatusBarController() {
        return statusBarController;
    }

    public ObjectProperty<TabPaneIndex> tabPaneIndexProperty() {
        return tabPaneIndexProperty;
    }

    public InfoDialog getFilmInfoDialog() {
        return filmInfo;
    }

    /**
     * Create the film information tool window.
     */
    private void createFilmInformationHUD() {
        filmInfo = new InfoDialog(this);
    }

    @Handler
    private void handleTabVisualSettingsChangedEvent(TabVisualSettingsChangedEvent e) {
        SwingUtilities.invokeLater(() -> {
            configureTabPlacement();
            configureTabIcons();
        });
    }

    private void addListener() {
        Listener.addListener(new Listener(Listener.EREIGNIS_DIALOG_MEDIA_DB, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                cbSearchMediaDb.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_MEDIA_DB_DIALOG_ANZEIGEN)));
            }
        });
    }

    @Handler
    private void handleBandwidthMonitorStateChangedEvent(BandwidthMonitorStateChangedEvent e) {
        SwingUtilities.invokeLater(() -> cbBandwidthDisplay.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BANDWIDTH_MONITOR_VISIBLE))));
    }

    protected void setFocusOnSearchField() {
        Platform.runLater(() -> tabFilme.fap.getSearchField().requestFocus());
    }

    /**
     * This will set the Look&Feel based on Application Preferences. In case of
     * error it will always reset to system LAF.
     */
    private void setLookAndFeel() {
        try {
            String laf = MVConfig.get(MVConfig.Configs.SYSTEM_LOOK);
            //if we have the old values, reset to System LAF
            if (laf.isEmpty() || laf.length() == 1) {
                if (getOs() != OperatingSystemType.LINUX) {
                    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
                }
            } else {
                //otherwise set the requested UI
                laf = MVConfig.get(MVConfig.Configs.SYSTEM_LOOK);
                UIManager.setLookAndFeel(laf);
            }
            SwingUtilities.updateComponentTreeUI(this);
            for (Frame f : Frame.getFrames()) {
                SwingUtilities.updateComponentTreeUI(f);
                for (Window w : f.getOwnedWindows()) {
                    SwingUtilities.updateComponentTreeUI(w);
                }
            }
        } catch (Exception ignored) {
            //update the LAF parameter, just in case we tried to load a non-existing LAF before
            MVConfig.add(MVConfig.Configs.SYSTEM_LOOK, UIManager.getSystemLookAndFeelClassName());
        }
    }

    private void setWindowTitle() {
        setTitle(Konstanten.PROGRAMMNAME + ' ' + Konstanten.MVVERSION);
    }

    private void setSize() {
        if (Daten.isStartMaximized() || Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_FENSTER_MAX))) {
            this.setExtendedState(Frame.MAXIMIZED_BOTH);
        } else {
            GuiFunktionen.setSize(MVConfig.Configs.SYSTEM_GROESSE_GUI, this, null);
        }
    }

    private void setupFilmListListener() {
        daten.getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
            @Override
            public void start(ListenerFilmeLadenEvent event) {
                miLoadFilmList.setEnabled(false);
            }

            @Override
            public void fertig(ListenerFilmeLadenEvent event) {
                miLoadFilmList.setEnabled(true);
                daten.allesSpeichern(); // damit nichts verlorengeht
            }

            @Override
            public void fertigOnlyOne(ListenerFilmeLadenEvent event) {
                setupUpdateCheck();
                setupAutomaticFilmlistReload();
                prepareMediaDb();
            }
        });
    }

    /**
     * Reload filmlist every 24h when in automatic mode.
     */
    private void setupAutomaticFilmlistReload() {
        final AutomaticFilmlistUpdate.IUpdateAction performUpdate = () -> {
            if (GuiFunktionen.getImportArtFilme() == FilmListUpdateType.AUTOMATIC) {
                FilmeLaden filmeLaden = new FilmeLaden(daten);
                filmeLaden.loadFilmlist("");
            }
        };
        AutomaticFilmlistUpdate automaticFilmlistUpdate = new AutomaticFilmlistUpdate(performUpdate);
        automaticFilmlistUpdate.start();
    }

    private void initWindowListener() {
        addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent evt) {
                if (tray != null && !SystemUtils.IS_OS_MAC_OSX && Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_USE_TRAY))) {
                    setVisible(false);
                } else {
                    beenden(false, false);
                }
            }
        });
    }

    private void prepareMediaDb() {
        final ListeMediaDB mediaDb = daten.getListeMediaDB();
        mediaDb.loadSavedList();
        mediaDb.createMediaDB("");
    }

    /**
     * This will setup a repeating update check every 24 hours.
     */
    private void setupUpdateCheck() {
        programUpdateChecker = new ProgramUpdateCheck(daten);
        programUpdateChecker.start();
    }

    public void setTray() {
        if (tray == null && Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_USE_TRAY))) {
            tray = new MVTray().systemTray();
        } else if (tray != null && !Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_USE_TRAY))) {
            tray.beenden();
            tray = null;
        }
    }

    private void initTabs() {
        Container contentPane = getContentPane();
        contentPane.add(tabbedPane, BorderLayout.CENTER);

        tabDownloads = new GuiDownloads(daten, this);
        tabAbos = new GuiAbo(daten, this);
        tabFilme = new GuiFilme(daten, this);

        tabbedPane.addTab(GuiFilme.NAME, tabFilme);
        tabbedPane.addTab(GuiDownloads.NAME, tabDownloads);
        tabbedPane.addTab(GuiAbo.NAME, tabAbos);
        tabbedPane.setSelectedIndex(0);

        configureTabPlacement();
        configureTabIcons();
    }

    /**
     * Enable/Disable the update related menu item.
     *
     * @param enable Shall the menu item be enabled?
     */
    public void enableUpdateMenuItem(boolean enable) {
        miSearchProgramUpdate.setEnabled(enable);
    }

    /**
     * Change placement of tabs based on settings
     */
    private void configureTabPlacement() {
        final boolean top = Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_TABS_TOP));
        if (top)
            tabbedPane.setTabPlacement(JTabbedPane.TOP);
        else
            tabbedPane.setTabPlacement(JTabbedPane.LEFT);
    }

    private void configureTabIcons() {
        final boolean icon = Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_TABS_ICON));

        //no icons...
        if (!icon) {
            setTabIcon(tabFilme, null);
            setTabIcon(tabDownloads, null);
            setTabIcon(tabAbos, null);
        } else {
            //setup icons for each tab here
            setTabIcon(tabFilme, Icons.ICON_TAB_FILM);
            setTabIcon(tabDownloads, Icons.ICON_TAB_DOWNLOAD);
            setTabIcon(tabAbos, Icons.ICON_TAB_ABO);
        }
    }

    private void setTabIcon(Component tab, Icon icon) {
        final int index = tabbedPane.indexOfComponent(tab);
        tabbedPane.setIconAt(index, icon);
    }

    /**
     * Create the platform-specific instance of the progress indicator thread.
     *
     * @return {@link IndicatorThread} instance for the running platform.
     */
    protected IndicatorThread createProgressIndicatorThread() throws Exception {
        throw new Exception("Unsupported Platform");
    }

    /**
     * Message bus handler which gets called when a download is started.
     *
     * @param msg Information about the download
     */
    @Handler
    protected void handleDownloadStart(DownloadStartEvent msg) {
        numDownloadsStarted.incrementAndGet();

        if (progressIndicatorThread == null) {
            try {
                progressIndicatorThread = createProgressIndicatorThread();
                progressIndicatorThread.start();
            } catch (Exception ignored) {
                //ignore if we have an unsupported platform, ie. linux.
            }
        }
    }

    /**
     * Message bus handler which gets called when a download is stopped.
     *
     * @param msg Information about the download
     */
    @Handler
    protected void handleDownloadFinishedEvent(DownloadFinishedEvent msg) {
        final int numDL = numDownloadsStarted.decrementAndGet();

        if (numDL == 0 && progressIndicatorThread != null) {
            progressIndicatorThread.interrupt();
            progressIndicatorThread = null;
        }
    }

    /**
     * Install the listeners which will cause automatic tab switching based on associated Menu item.
     */
    protected void installMenuTabSwitchListener() {
        //initial setup
        menuListeners.put(jMenuFilme, new MenuTabSwitchListener(TABS.TAB_FILME));
        menuListeners.put(jMenuDownload, new MenuTabSwitchListener(TABS.TAB_DOWNLOADS));
        menuListeners.put(jMenuAbos, new MenuTabSwitchListener(TABS.TAB_ABOS));

        //now assign if really necessary
        if (config.getBoolean(ApplicationConfiguration.APPLICATION_INSTALL_TAB_SWITCH_LISTENER, true)) {
            jMenuFilme.addMenuListener(menuListeners.get(jMenuFilme));
            jMenuDownload.addMenuListener(menuListeners.get(jMenuDownload));
            jMenuAbos.addMenuListener(menuListeners.get(jMenuAbos));
        }
    }

    /**
     * Handle the install/or remove event sent from settings dialog
     */
    @Handler
    protected void handleInstallTabSwitchListenerEvent(InstallTabSwitchListenerEvent msg) {
        switch (msg.event) {
            case INSTALL:
                SwingUtilities.invokeLater(() -> {
                    jMenuFilme.addMenuListener(menuListeners.get(jMenuFilme));
                    jMenuDownload.addMenuListener(menuListeners.get(jMenuDownload));
                    jMenuAbos.addMenuListener(menuListeners.get(jMenuAbos));
                });
                break;

            case REMOVE:
                SwingUtilities.invokeLater(() -> {
                    jMenuFilme.removeMenuListener(menuListeners.get(jMenuFilme));
                    jMenuDownload.removeMenuListener(menuListeners.get(jMenuDownload));
                    jMenuAbos.removeMenuListener(menuListeners.get(jMenuAbos));
                });
                break;
        }
    }

    private void createFileMenu() {
        miLoadFilmList.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F5, 0));
        miLoadFilmList.setIcon(IconFontSwing.buildIcon(FontAwesome.CLOUD_DOWNLOAD, 16));
        miLoadFilmList.addActionListener(e -> performFilmListLoadOperation(false));
        jMenuDatei.add(miLoadFilmList);

        jMenuDatei.add(new FilmListExportAction(this));

        //on macOS we will use native handlers instead...
        if (!SystemUtils.IS_OS_MAC_OSX) {
            JMenuItem miSettings = new JMenuItem("Einstellungen...");
            miSettings.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F4, 0));
            miSettings.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/programm/menue-einstellungen.png")));
            miSettings.addActionListener(e -> showSettingsDialog());

            JMenuItem miQuit = new JMenuItem("Beenden");
            miQuit.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Q, KeyEvent.CTRL_DOWN_MASK));
            miQuit.addActionListener(e -> beenden(false, false));

            jMenuDatei.add(miSettings);
            jMenuDatei.addSeparator();
            jMenuDatei.add(miQuit);
        }
    }

    private void createViewMenu() {
        JCheckBoxMenuItem cbVideoplayer = new JCheckBoxMenuItem("Buttons anzeigen");
        if (!SystemUtils.IS_OS_MAC_OSX)
            cbVideoplayer.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F11, 0));
        cbVideoplayer.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN)));
        cbVideoplayer.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN, String.valueOf(cbVideoplayer.isSelected()));
            Listener.notify(Listener.EREIGNIS_LISTE_PSET, MediathekGui.class.getSimpleName());
        });

        Listener.addListener(new Listener(Listener.EREIGNIS_LISTE_PSET, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                cbVideoplayer.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN)));
            }
        });

        JMenuItem miShowMemoryMonitor = new JMenuItem("Speicherverbrauch anzeigen");
        miShowMemoryMonitor.addActionListener(e -> showMemoryMonitor());

        cbBandwidthDisplay.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BANDWIDTH_MONITOR_VISIBLE)));
        cbBandwidthDisplay.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_BANDWIDTH_MONITOR_VISIBLE, Boolean.toString(cbBandwidthDisplay.isSelected()));
            daten.getMessageBus().publishAsync(new BandwidthMonitorStateChangedEvent());
        });

        cbSearchMediaDb.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_MEDIA_DB_DIALOG_ANZEIGEN)));
        cbSearchMediaDb.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_MEDIA_DB_DIALOG_ANZEIGEN, String.valueOf(cbSearchMediaDb.isSelected()));
            daten.getDialogMediaDB().setVis();
        });

        jMenuAnsicht.add(cbVideoplayer);
        jMenuAnsicht.addSeparator();
        jMenuAnsicht.add(miShowMemoryMonitor);
        jMenuAnsicht.add(cbBandwidthDisplay);
        jMenuAnsicht.addSeparator();
        jMenuAnsicht.add(new ShowFilmInformationAction());
        jMenuAnsicht.addSeparator();
        jMenuAnsicht.add(cbSearchMediaDb);
    }

    private void createHelpMenu() {
        JMenuItem miShowOnlineHelp = new JMenuItem("Online-Hilfe anzeigen");
        miShowOnlineHelp.setAction(new ShowOnlineHelpAction());

        JMenuItem miCreateProtocolFile = new JMenuItem("Protokolldatei erstellen...");
        miCreateProtocolFile.setAction(new CreateProtocolFileAction());

        JMenuItem miResetSettings = new JMenuItem("Einstellungen zurücksetzen...");
        miResetSettings.setAction(new ResetSettingsAction(this, daten));

        miSearchProgramUpdate.addActionListener(e -> searchForUpdateOrShowProgramInfos(false));

        JMenuItem miShowProgramInfo = new JMenuItem("Programminfos anzeigen...");
        miShowProgramInfo.addActionListener(e -> searchForUpdateOrShowProgramInfos(true));

        JMenuItem miShowAboutDialog = new JMenuItem("Über dieses Programm...");
        miShowAboutDialog.addActionListener(e -> showAboutDialog());

        jMenuHilfe.add(miShowOnlineHelp);
        jMenuHilfe.addSeparator();
        jMenuHilfe.add(miCreateProtocolFile);
        jMenuHilfe.add(miResetSettings);
        jMenuHilfe.addSeparator();
        jMenuHilfe.add(miSearchProgramUpdate);
        jMenuHilfe.add(miShowProgramInfo);
        jMenuHilfe.addSeparator();
        jMenuHilfe.add(miShowAboutDialog);
    }

    protected void initMenus() {
        installMenuTabSwitchListener();

        createFileMenu();
        tabFilme.installMenuEntries(jMenuFilme);
        tabDownloads.installMenuEntries(jMenuDownload);
        tabAbos.installMenuEntries(jMenuAbos);
        createViewMenu();

        createHelpMenu();
    }

    private void showMemoryMonitor() {
        Platform.runLater(() -> {
            if (memoryMonitor == null) {
                memoryMonitor = new MemoryMonitor();
            }

            memoryMonitor.show();
        });
    }

    /**
     * Display the About Box
     */
    protected void showAboutDialog() {
        AboutDialog aboutDialog = new AboutDialog(this);
        GuiFunktionen.centerOnScreen(aboutDialog, false);
        aboutDialog.setVisible(true);
        aboutDialog.dispose();
    }

    public void performFilmListLoadOperation(boolean manualMode) {
        if (manualMode || GuiFunktionen.getImportArtFilme() == FilmListUpdateType.MANUAL) {
            // Dialog zum Laden der Filme anzeigen
            LoadFilmListDialog dlg = new LoadFilmListDialog(this);
            dlg.setVisible(true);
        } else {
            // Filme werden automatisch geladen
            FilmeLaden filmeLaden = new FilmeLaden(daten);
            filmeLaden.loadFilmlist("");
        }
    }

    public void showSettingsDialog() {
        dialogEinstellungen.setVisible(true);
    }

    private void closeMemoryMonitor() {
        if (memoryMonitor != null)
            Platform.runLater(() -> memoryMonitor.close());
    }

    private void writeOldConfiguration() {
        if (getExtendedState() == JFrame.MAXIMIZED_BOTH) {
            MVConfig.add(MVConfig.Configs.SYSTEM_FENSTER_MAX, Boolean.TRUE.toString());
        } else {
            MVConfig.add(MVConfig.Configs.SYSTEM_FENSTER_MAX, Boolean.FALSE.toString());
        }

        // Hauptfenster
        GuiFunktionen.getSize(MVConfig.Configs.SYSTEM_GROESSE_GUI, this);
        // Dialog Einstellungen
        GuiFunktionen.getSize(MVConfig.Configs.SYSTEM_GROESSE_EINSTELLUNGEN, dialogEinstellungen);
        // Infodialog/Bandwidth
        bandwidthMonitor.writeConfig();
        // MediaDB
        GuiFunktionen.getSize(MVConfig.Configs.SYSTEM_MEDIA_DB_DIALOG_GROESSE, daten.getDialogMediaDB());
    }

    private void closeControlsFxWorkaroundStage() {
        Platform.runLater(() -> {
            if (controlsFxWorkaroundStage != null)
                controlsFxWorkaroundStage.close();
        });
    }

    public boolean beenden(boolean showOptionTerminate, boolean shutDown) {
        //write all settings if not done already...
        ApplicationConfiguration.getInstance().writeConfiguration();

        if (daten.getListeDownloads().nochNichtFertigeDownloads() > 0) {
            // erst mal prüfen ob noch Downloads laufen
            DialogBeenden dialogBeenden = new DialogBeenden(this);
            if (showOptionTerminate) {
                dialogBeenden.setComboWaitAndTerminate();
            }
            dialogBeenden.setModal(true);
            dialogBeenden.setVisible(true);
            if (!dialogBeenden.applicationCanTerminate()) {
                return false;
            }
            shutDown = dialogBeenden.isShutdownRequested();
        }

        closeMemoryMonitor();

        closeControlsFxWorkaroundStage();

        programUpdateChecker.close();

        ShutdownDialog dialog = new ShutdownDialog(this, 10);
        dialog.show();

        dialog.setStatusText(1, "Warte auf commonPool()");
        waitForCommonPoolToComplete();

        // Tabelleneinstellungen merken
        dialog.setStatusText(2, "Film-Daten sichern");
        tabFilme.tabelleSpeichern();

        dialog.setStatusText(3, "Download-Daten sichern");
        tabDownloads.tabelleSpeichern();

        dialog.setStatusText(4, "Abo-Daten sichern");
        tabAbos.tabelleSpeichern();

        dialog.setStatusText(5, "MediaDB sichern");
        daten.getDialogMediaDB().tabelleSpeichern();

        dialog.setStatusText(6, "Downloads anhalten");
        stopDownloads();

        dialog.setStatusText(7, "Programmkonfiguration schreiben");
        writeOldConfiguration();

        dialog.setStatusText(8, "Datenbank schließen");
        DatenFilm.Database.closeDatabase();

        dialog.setStatusText(9, "Programmdaten sichern");
        daten.allesSpeichern();

        dialog.setStatusText(10, "Fertig.");
        dialog.hide();

        tabFilme.fap.filterDialog.dispose();

        bandwidthMonitor.close();

        Log.endMsg();

        if (shutDown) {
            shutdownComputer();
        }

        dispose();

        //shutdown JavaFX
        Platform.runLater(Platform::exit);

        System.exit(0);

        return false;
    }

    private void stopDownloads() {
        if (daten.getListeDownloads() != null) {
            // alle laufenden Downloads/Programme stoppen
            for (DatenDownload download : daten.getListeDownloads()) {
                Start s = download.start;
                if (s != null) {
                    s.stoppen = true;
                }
            }
        }
    }

    private void waitForCommonPoolToComplete() {
        while (ForkJoinPool.commonPool().hasQueuedSubmissions()) {
            try {
                logger.debug("POOL SUBMISSIONS: {}", ForkJoinPool.commonPool().getQueuedSubmissionCount());
                TimeUnit.MILLISECONDS.sleep(500);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

    }

    /**
     * Shutdown the computer depending on Operating System.
     */
    private void shutdownComputer() {
        shutdownCommand.execute();
    }

    private void searchForUpdateOrShowProgramInfos(boolean infos) {
        new ProgrammUpdateSuchen().checkVersion(!infos, infos, true);
    }

    private class MenuTabSwitchListener implements MenuListener {

        private final TABS tabs;

        MenuTabSwitchListener(TABS tabs) {
            this.tabs = tabs;
        }

        @Override
        public void menuSelected(MenuEvent e) {
            findTab(tabs);
        }

        @Override
        public void menuDeselected(MenuEvent e) {
        }

        @Override
        public void menuCanceled(MenuEvent e) {
        }

        private void findTab(TABS state) {
            switch (state) {
                case TAB_FILME:
                    setTabIfContain(tabFilme);
                    break;
                case TAB_DOWNLOADS:
                    setTabIfContain(tabDownloads);
                    break;
                case TAB_ABOS:
                    setTabIfContain(tabAbos);
                    break;

                default:
                    break;
            }
        }

        private void setTabIfContain(Component check) {
            for (int i = 0; i < tabbedPane.getTabCount(); ++i) {
                Component c = tabbedPane.getComponentAt(i);
                if (c.equals(check)) {
                    tabbedPane.setSelectedIndex(i);
                    return;
                }
            }
        }
    }
}
