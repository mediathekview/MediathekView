package mediathek.gui.tabs.tab_downloads;

import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.control.Alert;
import javafx.scene.control.TabPane;
import jiconfont.icons.FontAwesome;
import jiconfont.swing.IconFontSwing;
import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.controller.history.MVUsedUrl;
import mediathek.controller.starter.Start;
import mediathek.daten.DatenAbo;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenFilm;
import mediathek.daten.DatenPset;
import mediathek.filmeSuchen.ListenerFilmeLaden;
import mediathek.filmeSuchen.ListenerFilmeLadenEvent;
import mediathek.gui.TabPaneIndex;
import mediathek.gui.actions.ShowFilmInformationAction;
import mediathek.gui.dialog.DialogBeendenZeit;
import mediathek.gui.dialog.DialogEditAbo;
import mediathek.gui.dialog.DialogEditDownload;
import mediathek.gui.messages.*;
import mediathek.gui.tabs.AGuiTabPanel;
import mediathek.gui.toolbar.FXDownloadToolBar;
import mediathek.javafx.descriptionPanel.DescriptionPanelController;
import mediathek.javafx.downloadtab.DownloadTabInformationLabel;
import mediathek.javafx.tool.JavaFxUtils;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.*;
import mediathek.tool.cellrenderer.CellRendererDownloads;
import mediathek.tool.listener.BeobTableHeader;
import mediathek.tool.models.TModelDownload;
import mediathek.tool.table.MVDownloadsTable;
import net.engio.mbassy.listener.Handler;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.apache.commons.configuration2.Configuration;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import java.awt.*;
import java.awt.event.*;
import java.io.File;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;

public class GuiDownloads extends AGuiTabPanel {
    public static final String NAME = "Downloads";
    private static final String COMBO_DISPLAY_ALL = "alle";
    private static final String COMBO_DISPLAY_DOWNLOADS_ONLY = "nur Downloads";
    private static final String COMBO_DISPLAY_ABOS_ONLY = "nur Abos";
    private static final int INDEX_COMBO_DISPLAY_ALL = 0;
    private static final int INDEX_COMBO_DISPLAY_DOWNLOADS_ONLY = 1;
    private static final int INDEX_COMBO_DISPLAY_ABOS_ONLY = 2;

    private static final String COMBO_VIEW_ALL = "alle";
    private static final String COMBO_VIEW_NOT_STARTED = "nicht gestartet";
    private static final String COMBO_VIEW_STARTED = "gestartet";
    private static final String COMBO_VIEW_WAITING = "nur wartende";
    private static final String COMBO_VIEW_RUN_ONLY = "nur laufende";
    private static final String COMBO_VIEW_FINISHED_ONLY = "nur abgeschlossene";
    private static final int INDEX_COMBO_VIEW_ALL = 0;
    private static final int INDEX_COMBO_VIEW_NOT_STARTED = 1;
    private static final int INDEX_COMBO_VIEW_STARTED = 2;
    private static final int INDEX_COMBO_VIEW_WAITING = 3;
    private static final int INDEX_COMBO_VIEW_RUN_ONLY = 4;
    private static final int INDEX_COMBO_VIEW_FINISHED_ONLY = 5;


    private static final String MENU_ITEM_TEXT_CLEANUP_DOWNLOADS = "Liste säubern";
    private static final String ACTION_MAP_KEY_EDIT_DOWNLOAD = "dl_aendern";
    private static final String ACTION_MAP_KEY_DELETE_DOWNLOAD = "dl_delete";
    private static final String ACTION_MAP_KEY_MARK_AS_SEEN = "seen";
    private static final String ACTION_MAP_KEY_MAERK_AS_UNSEEN = "unseen";
    private static final String ACTION_MAP_KEY_START_DOWNLOAD = "dl_start";
    private static final String ACTION_MAP_KEY_SEARCH_MEDIADB = "search_in_mediadb";
    private final static int[] COLUMNS_DISABLED = new int[]{DatenDownload.DOWNLOAD_BUTTON_START,
            DatenDownload.DOWNLOAD_BUTTON_DEL,
            DatenDownload.DOWNLOAD_REF,
            DatenDownload.DOWNLOAD_URL_RTMP};
    private static final Logger logger = LogManager.getLogger(GuiDownloads.class);
    private static final String HEAD = "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n"
            + "<head><style type=\"text/css\"> .sans { font-family: Verdana, Geneva, sans-serif; }</style></head>"
            + "<body>";
    private static final String END = "</body></html>";
    private final AtomicLong _lastUpdate = new AtomicLong(0);
    private final AtomicBoolean tabVisible = new AtomicBoolean(false);
    private final JCheckBoxMenuItem cbShowDownloadDescription = new JCheckBoxMenuItem("Filmbeschreibung anzeigen");
    private final Configuration config = ApplicationConfiguration.getConfiguration();

    private boolean onlyAbos;
    private boolean onlyDownloads;

    private boolean onlyWaiting;
    private boolean onlyNotStarted;
    private boolean onlyStarted;
    private boolean onlyFinished;
    private boolean onlyRun;
    private boolean loadFilmlist;
    /**
     * The internally used model.
     */
    private TModelDownload model;
    private DownloadTabInformationLabel filmInfoLabel;

    public GuiDownloads(Daten aDaten, MediathekGui mediathekGui) {
        super();
        daten = aDaten;
        this.mediathekGui = mediathekGui;

        initComponents();

        setupF4Key(mediathekGui);

        setupDownloadListTable();
        setupDescriptionPanel();

        showDescriptionPanel();

        init();

        installTabInfoStatusBarControl();

        setupFilmSelectionPropertyListener(mediathekGui);

        initTable();

        addListenerMediathekView();
        setupDisplayCategories();

        setupCheckboxView();

        setupToolBar();

        setupDownloadRateLimitSpinner();

        setupFilterPanel();

        setupComponentListener();

        if (Taskbar.isTaskbarSupported())
            setupTaskbarMenu();

        initializeTouchBar();
    }

    private void setupToolBar() {
        JavaFxUtils.invokeInFxThreadAndWait(() -> {
            var toolBar = new FXDownloadToolBar();
            toolBar.btnFilmInfo.setOnAction(e -> SwingUtilities.invokeLater(() -> MediathekGui.ui().getFilmInfoDialog().showInfo()));
            toolBar.btnUpdateDownloads.setOnAction(e -> SwingUtilities.invokeLater(this::updateDownloads));
            toolBar.btnStartAllDownloads.setOnAction(e -> SwingUtilities.invokeLater(() -> starten(true)));
            toolBar.btnPlayFilm.setOnAction(e -> SwingUtilities.invokeLater(this::filmAbspielen));
            toolBar.btnZurueckstellen.setOnAction(e -> SwingUtilities.invokeLater(() -> downloadLoeschen(false)));
            toolBar.btnRemoveDownload.setOnAction(e -> SwingUtilities.invokeLater(() -> downloadLoeschen(true)));
            toolBar.btnCleanup.setOnAction(e -> SwingUtilities.invokeLater(this::cleanupDownloads));
            toolBar.btnFilter.setOnAction(e -> SwingUtilities.invokeLater(() -> Daten.getInstance().getMessageBus().publishAsync(new DownloadFilterVisibilityChangedEvent())));

            Daten.getInstance().getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
                @Override
                public void start(ListenerFilmeLadenEvent event) {
                    Platform.runLater(() -> toolBar.btnUpdateDownloads.setDisable(true));
                }

                @Override
                public void fertig(ListenerFilmeLadenEvent event) {
                    Platform.runLater(() -> toolBar.btnUpdateDownloads.setDisable(false));
                }
            });

            toolBarPanel.setScene(new Scene(toolBar));
        });
    }

    private void setupF4Key(MediathekGui mediathekGui) {
        if (SystemUtils.IS_OS_WINDOWS) {
            // zum Abfangen der Win-F4 für comboboxen
            InputMap im = cbDisplayCategories.getInputMap();
            im.put(KeyStroke.getKeyStroke(KeyEvent.VK_F4, 0), "einstellungen");
            ActionMap am = cbDisplayCategories.getActionMap();
            am.put("einstellungen", new AbstractAction() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    mediathekGui.getSettingsDialog().setVisible(true);
                }
            });
        }
    }

    /**
     * Update the property with the current number of selected entries from the JTable.
     */
    private void setupFilmSelectionPropertyListener(MediathekGui mediathekGui) {
        tabelle.getSelectionModel().addListSelectionListener(e -> {
            if (!e.getValueIsAdjusting()) {
                final int sel = tabelle.getSelectedRowCount();
                Platform.runLater(() -> mediathekGui.getSelectedItemsProperty().setValue(sel));
            }
        });
        addComponentListener(new ComponentAdapter() {
            @Override
            public void componentShown(ComponentEvent e) {
                final int sel = tabelle.getSelectedRowCount();
                Platform.runLater(() -> mediathekGui.getSelectedItemsProperty().setValue(sel));
                onComponentShown();
            }
        });
    }

    @Override
    protected void installTabInfoStatusBarControl() {
        final var leftItems = mediathekGui.getStatusBarController().getStatusBar().getLeftItems();

        Platform.runLater(() -> {
            filmInfoLabel = new DownloadTabInformationLabel(daten);
            if (isVisible())
                leftItems.add(filmInfoLabel);
        });

        addComponentListener(new ComponentAdapter() {
            @Override
            public void componentShown(ComponentEvent e) {
                Platform.runLater(() -> {
                    filmInfoLabel.setVisible(true);
                    leftItems.add(filmInfoLabel);
                });
            }

            @Override
            public void componentHidden(ComponentEvent e) {
                Platform.runLater(() -> {
                    filmInfoLabel.setVisible(false);
                    leftItems.remove(filmInfoLabel);
                });
            }
        });
    }

    private void setupDownloadListTable() {
        tabelle = new MVDownloadsTable();
        downloadListScrollPane.setViewportView(tabelle);
    }

    private void setupDisplayCategories() {
        cbDisplayCategories.setModel(getDisplaySelectionModel());
        cbDisplayCategories.addActionListener(new DisplayCategoryListener());
    }

    private void setupCheckboxView() {
        cbView.setModel(getViewModel());
        cbView.addActionListener(new ViewCategoryListener());
    }

    private void initTable() {
        tabelle.initTabelle();
        tabelle.setSpalten();
        if (tabelle.getRowCount() > 0) {
            tabelle.setRowSelectionInterval(0, 0);
        }
    }

    private void setupComponentListener() {
        addComponentListener(new ComponentAdapter() {
            @Override
            public void componentShown(ComponentEvent e) {
                tabVisible.set(true);
            }

            @Override
            public void componentHidden(ComponentEvent e) {
                tabVisible.set(false);
            }
        });
    }

    private void setupFilterPanel() {
        final boolean visible = MVConfig.getBool(MVConfig.Configs.SYSTEM_TAB_DOWNLOAD_FILTER_VIS);
        updateFilterVisibility(visible);

        var config = ApplicationConfiguration.getConfiguration();

        final int location = config.getInt(ApplicationConfiguration.APPLICATION_UI_DOWNLOAD_TAB_DIVIDER_LOCATION, Konstanten.GUIDOWNLOAD_DIVIDER_LOCATION);
        jSplitPane1.setDividerLocation(location);
        jSplitPane1.addPropertyChangeListener(JSplitPane.DIVIDER_LOCATION_PROPERTY, pce -> {
            if (jPanelFilterExtern.isVisible()) {
                config.setProperty(ApplicationConfiguration.APPLICATION_UI_DOWNLOAD_TAB_DIVIDER_LOCATION,jSplitPane1.getDividerLocation());
            }
        });
    }

    @Handler
    private void handleParallelDownloadNumberChange(ParallelDownloadNumberChangedEvent e) {
        SwingUtilities.invokeLater(() -> {
            final int maxNumDownloads = ApplicationConfiguration.getConfiguration().getInt(ApplicationConfiguration.DOWNLOAD_MAX_SIMULTANEOUS_NUM,1);
            jSpinnerAnzahlDownloads.setValue(maxNumDownloads);
        });
    }

    @Handler
    private void handleDownloadFilterVisibilityChanged(DownloadFilterVisibilityChangedEvent e) {
        SwingUtilities.invokeLater(() -> {
            boolean visibility = !jPanelFilterExtern.isVisible();
            updateFilterVisibility(visibility);
            MVConfig.add(MVConfig.Configs.SYSTEM_TAB_DOWNLOAD_FILTER_VIS, Boolean.toString(visibility));
        });
    }

    private void updateFilterVisibility(boolean visible) {
        jPanelFilterExtern.setVisible(visible);
        if (visible) {
            final int location = config.getInt(ApplicationConfiguration.APPLICATION_UI_DOWNLOAD_TAB_DIVIDER_LOCATION, Konstanten.GUIDOWNLOAD_DIVIDER_LOCATION);
            jSplitPane1.setDividerLocation(location);
        }
    }

    private void setupTaskbarMenu() {
        var taskbar = Taskbar.getTaskbar();
        if (taskbar.isSupported(Taskbar.Feature.MENU)) {
            PopupMenu popupMenu = taskbar.getMenu();
            if (popupMenu == null)
                popupMenu = new PopupMenu();

            MenuItem miStartAllDownloads = new MenuItem("Alle Downloads starten");
            miStartAllDownloads.addActionListener(e -> starten(true));
            MenuItem miStopAllDownloads = new MenuItem("Alle Downloads stoppen");
            miStopAllDownloads.addActionListener(e -> stoppen(true));
            popupMenu.add(miStartAllDownloads);
            popupMenu.add(miStopAllDownloads);

            taskbar.setMenu(popupMenu);
        }
    }

    @Handler
    private void handleDownloadInfoUpdate(DownloadInfoUpdateAvailableEvent e) {
        if (tabVisible.get()) {
            SwingUtilities.invokeLater(() -> {
                if (txtDownload.isShowing())
                    setInfoText();
            });
        }
    }

    private void setupDownloadRateLimitSpinner() {
        //restore spinner setting from config
        final int oldDownloadLimit = ApplicationConfiguration.getConfiguration().getInt(ApplicationConfiguration.DOWNLOAD_RATE_LIMIT, 0);
        jSpinner1.setValue(oldDownloadLimit);

        jSpinner1.addChangeListener(e -> {
            final int downloadLimit = (int) jSpinner1.getValue();
            logger.info("Saving download rate limit {} to config", downloadLimit);
            ApplicationConfiguration.getConfiguration().setProperty(ApplicationConfiguration.DOWNLOAD_RATE_LIMIT, downloadLimit);
            DownloadRateLimitChangedEvent evt = new DownloadRateLimitChangedEvent();
            evt.newLimit = downloadLimit;
            daten.getMessageBus().publishAsync(evt);
        });
    }

    @Override
    public void installMenuEntries(JMenu menu) {
        JMenuItem miDownloadsStartAll = new JMenuItem("Alle Downloads starten");
        miDownloadsStartAll.setIcon(IconFontSwing.buildIcon(FontAwesome.ANGLE_DOUBLE_DOWN, 16));
        miDownloadsStartAll.addActionListener(e -> starten(true));

        JMenuItem miDownloadStartTimed = new JMenuItem("Alle Downloads um xx:yy Uhr starten");
        miDownloadStartTimed.addActionListener(e -> startAtTime());

        JMenuItem miStopAllDownloads = new JMenuItem("Alle Downloads stoppen");
        miStopAllDownloads.addActionListener(e -> stoppen(true));

        JMenuItem miStopWaitingDownloads = new JMenuItem("Wartende Downloads stoppen");
        miStopWaitingDownloads.addActionListener(e -> stopAllWaitingDownloads());

        JMenuItem miUpdateDownloads = new JMenuItem("Liste der Downloads aktualisieren");
        miUpdateDownloads.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_W, KeyEvent.CTRL_DOWN_MASK));
        miUpdateDownloads.setIcon(IconFontSwing.buildIcon(FontAwesome.REFRESH, 16));
        miUpdateDownloads.addActionListener(e -> updateDownloads());
        daten.getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
            @Override
            public void start(ListenerFilmeLadenEvent event) {
                miUpdateDownloads.setEnabled(false);
            }

            @Override
            public void fertig(ListenerFilmeLadenEvent event) {
                miUpdateDownloads.setEnabled(true);
            }
        });

        JMenuItem miCleanupDownloads = new JMenuItem(MENU_ITEM_TEXT_CLEANUP_DOWNLOADS);
        miCleanupDownloads.setIcon(IconFontSwing.buildIcon(FontAwesome.ERASER, 16));
        miCleanupDownloads.addActionListener(e -> cleanupDownloads());

        JMenuItem miStartDownloads = new JMenuItem("Ausgewählte Downloads starten");
        miStartDownloads.setIcon(IconFontSwing.buildIcon(FontAwesome.CARET_DOWN, 16));
        miStartDownloads.addActionListener(e -> starten(false));

        JMenuItem miStopDownloads = new JMenuItem("Ausgewählte Downloads stoppen");
        miStopDownloads.addActionListener(e -> stoppen(false));

        JMenuItem miDownloadsVorziehen = new JMenuItem("Downloads vorziehen");
        miDownloadsVorziehen.setIcon(Icons.ICON_MENUE_VORZIEHEN);
        miDownloadsVorziehen.addActionListener(e -> downloadsVorziehen());

        JMenuItem miDownloadsZurueckstellen = new JMenuItem("Downloads zurückstellen");
        miDownloadsZurueckstellen.setIcon(IconFontSwing.buildIcon(FontAwesome.CLOCK_O, 16));
        miDownloadsZurueckstellen.addActionListener(e -> downloadLoeschen(false));

        JMenuItem miDownloadsLoeschen = new JMenuItem("Downloads aus Liste entfernen");
        miDownloadsLoeschen.setIcon(IconFontSwing.buildIcon(FontAwesome.TRASH_O, 16));
        miDownloadsLoeschen.addActionListener(e -> downloadLoeschen(true));

        JMenuItem miEditDownload = new JMenuItem("Download ändern");
        miEditDownload.setIcon(IconFontSwing.buildIcon(FontAwesome.PENCIL_SQUARE_O, 16));
        miEditDownload.addActionListener(e -> editDownload());

        JMenuItem miMarkFilmAsSeen = new JMenuItem("Filme als gesehen markieren");
        miMarkFilmAsSeen.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_G, KeyEvent.CTRL_DOWN_MASK));
        miMarkFilmAsSeen.addActionListener(markFilmAsSeenAction);

        JMenuItem miMarkFilmAsUnseen = new JMenuItem("Filme als ungesehen markieren");
        miMarkFilmAsUnseen.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N, KeyEvent.CTRL_DOWN_MASK));
        miMarkFilmAsUnseen.addActionListener(markFilmAsUnseenAction);

        JMenuItem miPlayDownload = new JMenuItem("Gespeicherten Film abspielen");
        miPlayDownload.setIcon(IconFontSwing.buildIcon(FontAwesome.PLAY, 16));
        miPlayDownload.addActionListener(e -> filmAbspielen());

        JMenuItem miSearchMediaDb = new JMenuItem("Titel in der Mediensammlung suchen");
        miSearchMediaDb.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_M, KeyEvent.CTRL_DOWN_MASK));
        miSearchMediaDb.addActionListener(e -> searchInMediaDb());

        JMenuItem miInvertSelection = new JMenuItem("Auswahl umkehren");
        miInvertSelection.addActionListener(e -> tabelle.invertSelection());

        JMenuItem miShutdownAfterDownload = new JMenuItem("Aktion nach abgeschlossenen Downloads...");
        miShutdownAfterDownload.setIcon(IconFontSwing.buildIcon(FontAwesome.POWER_OFF, 16));
        miShutdownAfterDownload.addActionListener(e -> {
            if (daten.getListeDownloads().unfinishedDownloads() > 0) {
                // ansonsten gibts keine laufenden Downloads auf die man warten sollte
                mediathekGui.beenden(true, false);
            } else {
                Platform.runLater(() -> {
                    Alert alert = new Alert(Alert.AlertType.ERROR);
                    alert.setTitle(Konstanten.PROGRAMMNAME);
                    alert.setHeaderText("Keine laufenden Downloads!");
                    alert.setContentText("Die Downloads müssen zuerst gestartet werden.");
                    alert.showAndWait();
                });
            }
        });

        menu.add(miDownloadsStartAll);
        menu.add(miDownloadStartTimed);
        menu.add(miStopAllDownloads);
        menu.add(miStopWaitingDownloads);
        menu.add(miUpdateDownloads);
        menu.add(miCleanupDownloads);
        menu.addSeparator();
        menu.add(miStartDownloads);
        menu.add(miStopDownloads);
        menu.add(miDownloadsVorziehen);
        menu.add(miDownloadsZurueckstellen);
        menu.add(miDownloadsLoeschen);
        menu.add(miEditDownload);
        menu.addSeparator();
        menu.add(cbShowDownloadDescription);
        menu.addSeparator();
        menu.add(miMarkFilmAsSeen);
        menu.add(miMarkFilmAsUnseen);
        menu.add(miPlayDownload);
        menu.addSeparator();
        menu.add(miSearchMediaDb);
        menu.addSeparator();
        menu.add(miInvertSelection);
        menu.addSeparator();
        menu.add(miShutdownAfterDownload);
    }

    private void setupDescriptionPanel() {
        Platform.runLater(() -> {
            try {
                FXMLLoader loader = new FXMLLoader();
                loader.setLocation(Konstanten.FXML_FILM_DESCRIPTION_PANEL_URL);

                TabPane descriptionPane = loader.load();
                final DescriptionPanelController descriptionPanelController = loader.getController();
                descriptionPanelController.setOnCloseRequest(e -> {
                    SwingUtilities.invokeLater(() -> fxDescriptionPanel.setVisible(false));
                    e.consume();
                });

                fxDescriptionPanel.setScene(new Scene(descriptionPane));
                SwingUtilities.invokeLater(() -> tabelle.getSelectionModel().addListSelectionListener(e -> {
                    Optional<DatenFilm> optFilm = getCurrentlySelectedFilm();
                    Platform.runLater(() -> descriptionPanelController.showFilmDescription(optFilm));
                }));
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        });
    }

    public void onComponentShown() {
        mediathekGui.tabPaneIndexProperty().setValue(TabPaneIndex.DOWNLOAD);
        updateFilmData();
    }

    public void starten(boolean alle) {
        filmStartenWiederholenStoppen(alle, true);
    }

    public void startAtTime() {
        filmStartAtTime();
    }

    public void stoppen(boolean alle) {
        filmStartenWiederholenStoppen(alle, false);
    }

    private final MarkFilmAsSeenAction markFilmAsSeenAction = new MarkFilmAsSeenAction();

    private final MarkFilmAsUnseenAction markFilmAsUnseenAction = new MarkFilmAsUnseenAction();


    private void searchInMediaDb(DatenDownload datenDownload) {
        final var mediaDB = mediathekGui.getMediaDatabaseDialog();
        mediaDB.setVis();
        if (datenDownload != null)
            mediaDB.setFilter(datenDownload.arr[DatenDownload.DOWNLOAD_TITEL]);
    }

    private void setupKeyMappings() {
        final InputMap im = tabelle.getInputMap();
        im.put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0), ACTION_MAP_KEY_EDIT_DOWNLOAD);
        im.put(KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, 0), ACTION_MAP_KEY_DELETE_DOWNLOAD);
        im.put(KeyStroke.getKeyStroke(KeyEvent.VK_G, 0), ACTION_MAP_KEY_MARK_AS_SEEN);
        im.put(KeyStroke.getKeyStroke(KeyEvent.VK_U, 0), ACTION_MAP_KEY_MAERK_AS_UNSEEN);
        im.put(KeyStroke.getKeyStroke(KeyEvent.VK_D, 0), ACTION_MAP_KEY_START_DOWNLOAD);
        im.put(KeyStroke.getKeyStroke(KeyEvent.VK_M, 0), ACTION_MAP_KEY_SEARCH_MEDIADB);

        final ActionMap am = tabelle.getActionMap();
        am.put(ACTION_MAP_KEY_EDIT_DOWNLOAD, new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                editDownload();
            }
        });
        am.put(ACTION_MAP_KEY_DELETE_DOWNLOAD, new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                downloadLoeschen(true);
            }
        });
        am.put(ACTION_MAP_KEY_MARK_AS_SEEN, markFilmAsSeenAction);
        am.put(ACTION_MAP_KEY_MAERK_AS_UNSEEN, markFilmAsUnseenAction);
        am.put(ACTION_MAP_KEY_START_DOWNLOAD, new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                filmStartenWiederholenStoppen(false, true);
            }
        });
        am.put(ACTION_MAP_KEY_SEARCH_MEDIADB, new SearchInMediaDbAction());
    }

    private void init() {
        setupKeyMappings();
        //Tabelle einrichten

        final CellRendererDownloads cellRenderer = new CellRendererDownloads(daten.getSenderIconCache());
        tabelle.setDefaultRenderer(Object.class, cellRenderer);
        tabelle.setDefaultRenderer(Datum.class, cellRenderer);
        tabelle.setDefaultRenderer(MVFilmSize.class, cellRenderer);
        tabelle.setDefaultRenderer(Integer.class, cellRenderer);

        model = new TModelDownload();
        tabelle.setModel(model);
        tabelle.addMouseListener(new BeobMausTabelle());
        tabelle.getSelectionModel().addListSelectionListener(event -> {
            if (!event.getValueIsAdjusting()) {
                updateFilmData();
            }
        });

        tabelle.setLineBreak(MVConfig.getBool(MVConfig.Configs.SYSTEM_TAB_DOWNLOAD_LINEBREAK));
        tabelle.getTableHeader().addMouseListener(new BeobTableHeader(tabelle,
                DatenDownload.spaltenAnzeigen,
                COLUMNS_DISABLED,
                new int[]{DatenDownload.DOWNLOAD_BUTTON_START, DatenDownload.DOWNLOAD_BUTTON_DEL},
                true, MVConfig.Configs.SYSTEM_TAB_DOWNLOAD_LINEBREAK));

        btnClear.setIcon(Icons.ICON_BUTTON_CLEAR);
        btnClear.addActionListener(l -> {
            cbDisplayCategories.setSelectedIndex(0);
            cbView.setSelectedIndex(0);
        });

        jSpinnerAnzahlDownloads.setModel(new SpinnerNumberModel(1, 1, 9, 1));
        jSpinnerAnzahlDownloads.setValue(config.getInt(ApplicationConfiguration.DOWNLOAD_MAX_SIMULTANEOUS_NUM,1));
        jSpinnerAnzahlDownloads.addChangeListener(l -> {
            final int maxNumDownloads = ((Number)jSpinnerAnzahlDownloads.getModel().getValue()).intValue();
            config.setProperty(ApplicationConfiguration.DOWNLOAD_MAX_SIMULTANEOUS_NUM, maxNumDownloads);
            daten.getMessageBus().publishAsync(new ParallelDownloadNumberChangedEvent());
        });

        final int location = config.getInt(ApplicationConfiguration.APPLICATION_UI_DOWNLOAD_TAB_DIVIDER_LOCATION, Konstanten.GUIDOWNLOAD_DIVIDER_LOCATION);
        jSplitPane1.setDividerLocation(location);

        setupInfoPanel();
        daten.getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
            @Override
            public void start(ListenerFilmeLadenEvent event) {
                loadFilmlist = true;
            }

            @Override
            public void fertig(ListenerFilmeLadenEvent event) {
                loadFilmlist = false;
                daten.getListeDownloads().filmEintragen();
                if (Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_ABOS_SOFORT_SUCHEN))) {
                    updateDownloads();
                } else {
                    reloadTable(); // damit die Filmnummern richtig angezeigt werden
                }
            }
        });
    }

    private void setupInfoPanel() {
        txtDownload.setText("");
        txtDownload.setEditable(false);
        txtDownload.setFocusable(false);
        txtDownload.setContentType("text/html");
    }

    private void setInfoText() {
        if (daten.getListeDownloads().getStarts().total_starts == 0) {
            txtDownload.setText("");
            return;
        }

        String info = HEAD;

        // Downloads
        info += getInfoText();

        final var downloadInfos = daten.getDownloadInfos();
        // Größe
        final long byteAlleDownloads = downloadInfos.getByteAlleDownloads();
        final long byteAktDownloads = downloadInfos.getByteAktDownloads();
        if (byteAlleDownloads > 0 || byteAktDownloads > 0) {
            info += "<br />";
            info += "<span class=\"sans\"><b>Größe:</b><br />";
            if (byteAktDownloads > 0) {
                info += FileSize.convertSize(byteAktDownloads) + " von "
                        + FileSize.convertSize(byteAlleDownloads) + " MByte" + "</span>";
            } else {
                info += FileSize.convertSize(byteAlleDownloads) + " MByte" + "</span>";
            }
        }
        // Restzeit
        final long timeRestAktDownloads = downloadInfos.getTimeRestAktDownloads();
        final long timeRestAllDownloads = downloadInfos.getTimeRestAllDownloads();
        if (timeRestAktDownloads > 0 && timeRestAllDownloads > 0) {
            info += "<br />";
            info += "<span class=\"sans\"><b>Restzeit:</b><br />" + "laufende: "
                    + downloadInfos.getRestzeit() + ",<br />alle: " + downloadInfos.getGesamtRestzeit() + "</span>";
        } else if (timeRestAktDownloads > 0) {
            info += "<br />";
            info += "<span class=\"sans\"><b>Restzeit:</b><br />laufende: " + downloadInfos.getRestzeit() + "</span>";
        } else if (timeRestAllDownloads > 0) {
            info += "<br />";
            info += "<span class=\"sans\"><b>Restzeit:</b><br />alle: " + downloadInfos.getGesamtRestzeit() + "</span>";
        }

        info += END;

        txtDownload.setText(info);
    }

    private String getInfoText() {
        String textLinks;
        final var info = daten.getListeDownloads().getStarts();
        textLinks = "<span class=\"sans\"><b>Downloads:  </b>" + info.total_starts + "<br />";

        if (info.hasValues()) {
            textLinks += "( ";
            textLinks += (info.running == 1) ? "1 läuft" : info.running + " laufen";
            textLinks += (info.initialized == 1) ? ", 1 wartet" : ", " + info.initialized + " warten";
            if (info.finished > 0)
                textLinks += (info.finished == 1) ? ", 1 fertig" : ", " + info.finished + " fertig";

            if (info.error > 0)
                textLinks += (info.error == 1) ? ", 1 fehlerhaft" : ", " + info.error + " fehlerhaft";

            textLinks += " )";
        }
        textLinks += "<br /></span>";
        return textLinks;
    }

    @Handler
    private void handleRestartDownloadEvent(RestartDownloadEvent e) {
        reloadAndSave();
    }

    @Handler
    private void handleDownloadQueueRankChanged(DownloadQueueRankChangedEvent e) {
        reloadAndSave();
    }

    private void reloadAndSave() {
        SwingUtilities.invokeLater(() -> {
            reloadTable();
            daten.allesSpeichern();
        });
    }

    @Handler
    private void handleAboListChanged(AboListChangedEvent e) {
        SwingUtilities.invokeLater(() -> {
            if (Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_ABOS_SOFORT_SUCHEN)))
                updateDownloads();
        });
    }

    @Handler
    private void handleDownloadListChange(DownloadListChangedEvent e) {
        SwingUtilities.invokeLater(() -> {
            reloadTable();
            daten.allesSpeichern();
        });
    }

    @Handler
    private void handleBlacklistChangedEvent(BlacklistChangedEvent e) {
        SwingUtilities.invokeLater(() -> {
            if (Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_ABOS_SOFORT_SUCHEN))
                    && Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_AUCH_ABO))) {
                // nur auf Blacklist reagieren, wenn auch für Abos eingeschaltet
                updateDownloads();
            }
        });
    }

    private void addListenerMediathekView() {
        //register message bus handler
        daten.getMessageBus().subscribe(this);

        Listener.addListener(new Listener(Listener.EREIGNIS_BLACKLIST_AUCH_FUER_ABOS, GuiDownloads.class.getSimpleName()) {
            @Override
            public void ping() {
                if (Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_ABOS_SOFORT_SUCHEN))) {
                    updateDownloads();
                }
            }
        });

        setupShowFilmDescriptionMenuItem();
    }

    @Handler
    private void handleDownloadProgressChanged(DownloadProgressChangedEvent e) {
        final long now = System.currentTimeMillis();
        // nur alle 500ms aufrufen
        if (now - _lastUpdate.get() >= 500) {
            _lastUpdate.set(now);
            SwingUtilities.invokeLater(() -> daten.getListeDownloads().setModelProgress(model));
        }
    }

    @Handler
    private void handleGeoStateChangedEvent(GeoStateChangedEvent e) {
        SwingUtilities.invokeLater(() -> {
            tabelle.fireTableDataChanged(true);
            setInfo();
        });
    }

    /**
     * Setup and show film description panel.
     * Most of the setup is done in {@link GuiDownloads} function.
     * Here we just display the panel
     */
    private void setupShowFilmDescriptionMenuItem() {
        cbShowDownloadDescription.setSelected(ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.DOWNLOAD_SHOW_DESCRIPTION, true));
        cbShowDownloadDescription.addActionListener(l -> fxDescriptionPanel.setVisible(cbShowDownloadDescription.isSelected()));
        cbShowDownloadDescription.addItemListener(e -> ApplicationConfiguration.getConfiguration().setProperty(ApplicationConfiguration.DOWNLOAD_SHOW_DESCRIPTION, cbShowDownloadDescription.isSelected()));
        fxDescriptionPanel.addComponentListener(new ComponentAdapter() {
            @Override
            public void componentShown(ComponentEvent e) {
                cbShowDownloadDescription.setSelected(true);
            }

            @Override
            public void componentHidden(ComponentEvent e) {
                cbShowDownloadDescription.setSelected(false);
            }
        });
    }

    /**
     * Show description panel based on settings.
     */
    private void showDescriptionPanel() {
        fxDescriptionPanel.setVisible(ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.DOWNLOAD_SHOW_DESCRIPTION, true));
    }

    private synchronized void reloadTable() {
        // nur Downloads die schon in der Liste sind werden geladen
        tabelle.getSpalten();

        daten.getListeDownloads().getModel(model, onlyAbos, onlyDownloads, onlyNotStarted, onlyStarted, onlyWaiting, onlyRun, onlyFinished);
        tabelle.setSpalten();
        updateFilmData();
        setInfo();
    }

    @Handler
    private void handleStartEvent(StartEvent msg) {
        SwingUtilities.invokeLater(this::reloadTable);
    }

    private void searchInMediaDb() {
        final DatenDownload datenDownload = getSelDownload();
        MVConfig.add(MVConfig.Configs.SYSTEM_MEDIA_DB_DIALOG_ANZEIGEN, Boolean.TRUE.toString());
        searchInMediaDb(datenDownload);
    }

    public synchronized void updateDownloads() {
        if (loadFilmlist) {
            // wird danach automatisch gemacht
            return;
        }
        // erledigte entfernen, nicht gestartete Abos entfernen und neu nach Abos suchen
        var listeDownloads = daten.getListeDownloads();
        listeDownloads.abosAuffrischen();
        listeDownloads.abosSuchen(mediathekGui);
        reloadTable();

        if (Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_DOWNLOAD_SOFORT_STARTEN))) {
            // und wenn gewollt auch gleich starten
            filmStartenWiederholenStoppen(true /*alle*/, true /*starten*/, false /*fertige wieder starten*/);
        }
    }

    public synchronized void cleanupDownloads() {
        // abgeschlossene Downloads werden aus der Tabelle/Liste entfernt
        // die Starts dafür werden auch gelöscht
        daten.getListeDownloads().listePutzen();
    }

    private synchronized void downloadsAufraeumen(DatenDownload datenDownload) {
        // abgeschlossene Downloads werden aus der Tabelle/Liste entfernt
        // die Starts dafür werden auch gelöscht
        daten.getListeDownloads().listePutzen(datenDownload);
    }

    private ArrayList<DatenDownload> getSelDownloads() {
        ArrayList<DatenDownload> arrayDownloads = new ArrayList<>();
        final int[] rows = tabelle.getSelectedRows();
        if (rows.length > 0) {
            for (int row : rows) {
                DatenDownload datenDownload = (DatenDownload) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(row), DatenDownload.DOWNLOAD_REF);
                arrayDownloads.add(datenDownload);
            }
        } else {
            NoSelectionErrorDialog.show();
        }
        return arrayDownloads;
    }

    @Override
    protected Optional<DatenFilm> getCurrentlySelectedFilm() {
        final int selectedTableRow = tabelle.getSelectedRow();
        if (selectedTableRow != -1) {
            Optional<DatenFilm> optRet;
            final int modelIndex = tabelle.convertRowIndexToModel(selectedTableRow);
            final DatenDownload download = (DatenDownload) tabelle.getModel().getValueAt(modelIndex, DatenDownload.DOWNLOAD_REF);
            if (download.film == null)
                optRet = Optional.empty();
            else
                optRet = Optional.of(download.film);
            return optRet;
        } else {
            return Optional.empty();
        }
    }

    private DatenDownload getSelDownload() {
        DatenDownload datenDownload = null;
        final int row = tabelle.getSelectedRow();
        if (row != -1) {
            datenDownload = (DatenDownload) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(row), DatenDownload.DOWNLOAD_REF);
        } else {
            NoSelectionErrorDialog.show();
        }
        return datenDownload;
    }

    public synchronized void editDownload() {
        DatenDownload datenDownload = getSelDownload();
        if (datenDownload == null) {
            return;
        }
        boolean gestartet = false;
        if (datenDownload.start != null) {
            if (datenDownload.start.status >= Start.STATUS_RUN) {
                gestartet = true;
            }
        }
        DatenDownload datenDownloadKopy = datenDownload.getCopy();
        DialogEditDownload dialog = new DialogEditDownload(mediathekGui, true, datenDownloadKopy, gestartet, tabelle.getColumnModel());
        dialog.setVisible(true);
        if (dialog.ok) {
            datenDownload.aufMichKopieren(datenDownloadKopy);
            reloadTable();
        }
    }

    private void downloadsVorziehen() {
        ArrayList<DatenDownload> arrayDownloads = getSelDownloads();
        if (arrayDownloads.isEmpty()) {
            return;
        }
        daten.getListeDownloads().downloadsVorziehen(arrayDownloads);
    }

    private void zielordnerOeffnen() {
        DatenDownload datenDownload = getSelDownload();
        if (datenDownload == null) {
            return;
        }
        String s = datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD];
        DirOpenAction.zielordnerOeffnen(mediathekGui, s);
    }

    public void filmAbspielen() {
        DatenDownload datenDownload = getSelDownload();
        if (datenDownload == null) {
            return;
        }
        String s = datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME];
        OpenPlayerAction.filmAbspielen(mediathekGui, s);
    }

    private void filmLoeschen_() {
        DatenDownload datenDownload = getSelDownload();
        if (datenDownload == null) {
            return;
        }
        // Download nur löschen wenn er nicht läuft
        if (datenDownload.start != null) {
            if (datenDownload.start.status < Start.STATUS_FERTIG) {
                MVMessageDialog.showMessageDialog(mediathekGui, "Download erst stoppen!", "Film löschen", JOptionPane.ERROR_MESSAGE);
                return;
            }
        }
        try {
            File file = new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
            if (!file.exists()) {
                MVMessageDialog.showMessageDialog(mediathekGui, "Die Datei existiert nicht!", "Film löschen", JOptionPane.ERROR_MESSAGE);
                return;
            }
            int ret = JOptionPane.showConfirmDialog(mediathekGui,
                    datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME], "Film Löschen?", JOptionPane.YES_NO_OPTION);
            if (ret == JOptionPane.OK_OPTION) {

                // und jetzt die Datei löschen
                logger.info(new String[]{"Datei löschen: ", file.getAbsolutePath()});
                if (!file.delete()) {
                    throw new Exception();
                }
            }
        } catch (Exception ex) {
            MVMessageDialog.showMessageDialog(mediathekGui, "Konnte die Datei nicht löschen!", "Film löschen", JOptionPane.ERROR_MESSAGE);
            logger.error("Fehler beim löschen: " + datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
        }
    }

    /**
     * @param dauerhaft false werden Downloads zurück gestellt. true löscht permanent.
     */
    public void downloadLoeschen(boolean dauerhaft) {
        try {
            ArrayList<DatenDownload> arrayDownloads = getSelDownloads();
            if (arrayDownloads.isEmpty()) {
                return;
            }

            var zeit = DateTimeFormatter.ofPattern("dd.MM.yyyy").format(LocalDateTime.ofInstant(Instant.now(), ZoneId.systemDefault()));

            ArrayList<DatenDownload> arrayDownloadsLoeschen = new ArrayList<>();
            LinkedList<MVUsedUrl> urlAboList = new LinkedList<>();

            for (DatenDownload datenDownload : arrayDownloads) {
                if (dauerhaft) {
                    arrayDownloadsLoeschen.add(datenDownload);
                    if (datenDownload.isFromAbo()) {
                        // ein Abo wird zusätzlich ins Logfile geschrieben
                        urlAboList.add(new MVUsedUrl(zeit,
                                datenDownload.arr[DatenDownload.DOWNLOAD_THEMA],
                                datenDownload.arr[DatenDownload.DOWNLOAD_TITEL],
                                datenDownload.arr[DatenDownload.DOWNLOAD_HISTORY_URL]));
                    }
                } else {
                    // wenn nicht dauerhaft
                    datenDownload.zurueckstellen();
                }
            }
            if (!urlAboList.isEmpty()) {
                daten.getAboHistoryController().createLineWriterThread(urlAboList);
            }
            daten.getListeDownloads().downloadLoeschen(arrayDownloadsLoeschen);
            reloadTable();
        } catch (Exception ex) {
            logger.error("downloadLoeschen()", ex);
        }
    }

    private void filmStartAtTime() {
        // bezieht sich immer auf "alle"
        // Film der noch keinen Starts hat wird gestartet
        // Film dessen Start schon auf fertig/fehler steht wird wieder gestartet
        // wird immer vom Benutzer aufgerufen
        ArrayList<DatenDownload> listeAllDownloads = new ArrayList<>();
        ArrayList<DatenDownload> listeUrlsDownloadsAbbrechen = new ArrayList<>();
        ArrayList<DatenDownload> listeDownloadsStarten = new ArrayList<>();
        // ==========================
        // erst mal die Liste nach der Tabelle sortieren
        if (tabelle.getRowCount() == 0) {
            return;
        }
        for (int i = 0; i < tabelle.getRowCount(); ++i) {
            // um in der Reihenfolge zu starten
            DatenDownload datenDownload = (DatenDownload) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(i), DatenDownload.DOWNLOAD_REF);
            listeAllDownloads.add(datenDownload);
            daten.getListeDownloads().remove(datenDownload);
            daten.getListeDownloads().add(datenDownload);
        }
        // ========================
        // und jetzt abarbeiten
        for (DatenDownload download : listeAllDownloads) {
            // ==========================================
            // starten
            if (download.start != null) {
                if (download.start.status == Start.STATUS_RUN) {
                    // dann läuft er schon
                    continue;
                }
                if (download.start.status > Start.STATUS_RUN) {
                    // wenn er noch läuft gibts nix
                    // wenn er schon fertig ist, erst mal fragen vor dem erneuten Starten
                    //TODO in auto dialog umwandeln!
                    int a = JOptionPane.showConfirmDialog(mediathekGui, "Film nochmal starten?  ==> " + download.arr[DatenDownload.DOWNLOAD_TITEL],
                            "Fertiger Download", JOptionPane.YES_NO_OPTION);
                    if (a != JOptionPane.YES_OPTION) {
                        // weiter mit der nächsten URL
                        continue;
                    }
                    listeUrlsDownloadsAbbrechen.add(download);
                    if (download.isFromAbo()) {
                        // wenn er schon feritg ist und ein Abos ist, Url auch aus dem Logfile löschen, der Film ist damit wieder auf "Anfang"
                        daten.getAboHistoryController().urlAusLogfileLoeschen(download.arr[DatenDownload.DOWNLOAD_HISTORY_URL]);
                    }
                }
            }
            listeDownloadsStarten.add(download);
        }
        // ========================
        // jetzt noch die Starts stoppen
        daten.getListeDownloads().downloadAbbrechen(listeUrlsDownloadsAbbrechen);

        // und die Downloads starten oder stoppen
        //alle Downloads starten/wiederstarten
        DialogBeendenZeit dialogBeenden = new DialogBeendenZeit(mediathekGui, daten, listeDownloadsStarten);
        dialogBeenden.setVisible(true);
        if (dialogBeenden.applicationCanTerminate()) {
            // fertig und beenden
            mediathekGui.beenden(false /*Dialog auf "sofort beenden" einstellen*/, dialogBeenden.isShutdownRequested());
        }

        reloadTable();
    }

    private void filmStartenWiederholenStoppen(boolean alle, boolean starten /* starten/wiederstarten oder stoppen */) {
        filmStartenWiederholenStoppen(alle, starten, true /*auch fertige wieder starten*/);
    }

    private void filmStartenWiederholenStoppen(boolean alle, boolean starten /* starten/wiederstarten oder stoppen */, boolean fertige /*auch fertige wieder starten*/) {
        // bezieht sich immer auf "alle" oder nur die markierten
        // Film der noch keinen Starts hat wird gestartet
        // Film dessen Start schon auf fertig/fehler steht wird wieder gestartet
        // bei !starten wird der Film gestoppt
        // wird immer vom Benutzer aufgerufen
        ArrayList<DatenDownload> listeDownloadsLoeschen = new ArrayList<>();
        ArrayList<DatenDownload> listeDownloadsStarten = new ArrayList<>();
        ArrayList<DatenDownload> listeDownloadsMarkiert = new ArrayList<>();

        if (tabelle.getRowCount() == 0) {
            return;
        }

        // ==========================
        // erst mal die Liste nach der Tabelle sortieren
        if (starten && alle) {
            //Liste in der Reihenfolge wie in der Tabelle sortieren
            for (int i = 0; i < tabelle.getRowCount(); ++i) {
                DatenDownload datenDownload = (DatenDownload) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(i), DatenDownload.DOWNLOAD_REF);
                daten.getListeDownloads().remove(datenDownload);
                daten.getListeDownloads().add(datenDownload);
            }
        }

        // ==========================
        // die URLs sammeln
        if (alle) {
            for (int i = 0; i < tabelle.getRowCount(); ++i) {
                DatenDownload datenDownload = (DatenDownload) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(i), DatenDownload.DOWNLOAD_REF);
                listeDownloadsMarkiert.add(datenDownload);
            }
        } else {
            listeDownloadsMarkiert = getSelDownloads();
        }
        if (!starten) {
            // dann das Starten von neuen Downloads etwas Pausieren
            daten.starterClass.pause();
        }
        // ========================
        // und jetzt abarbeiten
        int antwort = -1;
        for (DatenDownload download : listeDownloadsMarkiert) {
            if (starten) {
                // ==========================================
                // starten
                if (download.start != null) {
                    if (download.start.status == Start.STATUS_RUN
                            || !fertige && download.start.status > Start.STATUS_RUN) {
                        // wenn er noch läuft gibts nix
                        // fertige bleiben auch unverändert
                        continue;
                    }
                    if (download.start.status > Start.STATUS_RUN) {
                        // wenn er schon fertig ist, erst mal fragen vor dem erneuten Starten
                        //TODO in auto dialog umwandeln!
                        if (antwort == -1) {
                            // nur einmal fragen
                            String text;
                            if (listeDownloadsMarkiert.size() > 1) {
                                text = "Es sind bereits fertige Filme dabei,\n"
                                        + "diese nochmal starten?";
                            } else {
                                text = "Film nochmal starten?  ==> " + download.arr[DatenDownload.DOWNLOAD_TITEL];
                            }
                            antwort = JOptionPane.showConfirmDialog(mediathekGui, text,
                                    "Fertiger Download", JOptionPane.YES_NO_CANCEL_OPTION);
                        }
                        if (antwort == JOptionPane.CANCEL_OPTION) {
                            //=============================
                            //dann wars das
                            return;
                        }
                        if (antwort == JOptionPane.NO_OPTION) {
                            // weiter mit der nächsten URL
                            continue;
                        }
                        listeDownloadsLoeschen.add(download);
                        if (download.isFromAbo()) {
                            // wenn er schon feritg ist und ein Abos ist, Url auch aus dem Logfile löschen, der Film ist damit wieder auf "Anfang"
                            daten.getAboHistoryController().urlAusLogfileLoeschen(download.arr[DatenDownload.DOWNLOAD_HISTORY_URL]);
                        }
                    }
                }
                listeDownloadsStarten.add(download);
            } else if (download.start != null) {
                // ==========================================
                // stoppen
                // wenn kein s -> dann gibts auch nichts zum stoppen oder wieder-starten
                if (download.start.status <= Start.STATUS_RUN) {
                    // löschen -> nur wenn noch läuft, sonst gibts nichts mehr zum löschen
                    listeDownloadsLoeschen.add(download);
                }
            }
        }
        // ========================
        // jetzt noch die Starts stoppen
        daten.getListeDownloads().downloadAbbrechen(listeDownloadsLoeschen);
        // und die Downloads starten oder stoppen
        if (starten) {
            //alle Downloads starten/wiederstarten
            DatenDownload.startenDownloads(listeDownloadsStarten);
        }
        reloadTable();
    }

    public void stopAllWaitingDownloads() {
        // es werden alle noch nicht gestarteten Downloads gelöscht
        ArrayList<DatenDownload> listeStopDownload = new ArrayList<>();
        for (int i = 0; i < tabelle.getRowCount(); ++i) {
            DatenDownload datenDownload = (DatenDownload) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(i), DatenDownload.DOWNLOAD_REF);
            if (datenDownload.start != null) {
                if (datenDownload.start.status < Start.STATUS_RUN) {
                    listeStopDownload.add(datenDownload);
                }
            }
        }
        daten.getListeDownloads().downloadAbbrechen(listeStopDownload);
    }

    private void setInfo() {
        daten.getMessageBus().publishAsync(new UpdateStatusBarLeftDisplayEvent());
    }

    /**
     * Return the model used for the display categories {@link javax.swing.JComboBox}.
     *
     * @return The selection model.
     */
    private DefaultComboBoxModel<String> getDisplaySelectionModel() {
        return new DefaultComboBoxModel<>(new String[]{COMBO_DISPLAY_ALL, COMBO_DISPLAY_DOWNLOADS_ONLY, COMBO_DISPLAY_ABOS_ONLY});
    }

    /**
     * Return the model used for the view categories {@link javax.swing.JComboBox}.
     *
     * @return The selection model.
     */private DefaultComboBoxModel<String> getViewModel() {
        return new DefaultComboBoxModel<>(new String[]{COMBO_VIEW_ALL, COMBO_VIEW_NOT_STARTED, COMBO_VIEW_STARTED, COMBO_VIEW_WAITING, COMBO_VIEW_RUN_ONLY, COMBO_VIEW_FINISHED_ONLY});
    }

    private void updateFilmData() {
        if (isShowing()) {
            DatenFilm aktFilm = null;
            final int selectedTableRow = tabelle.getSelectedRow();
            if (selectedTableRow >= 0) {
                final DatenDownload datenDownload = (DatenDownload) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(selectedTableRow), DatenDownload.DOWNLOAD_REF);
                if (datenDownload != null) {
                    aktFilm = datenDownload.film;
                }
            }
            mediathekGui.getFilmInfoDialog().updateCurrentFilm(aktFilm);
        }
    }

    @Override
    public void showTouchBar() {
        touchBar.show(MediathekGui.ui());
    }

    @Override
    public void hideTouchBar() {
        touchBar.hide(MediathekGui.ui());
    }

    @Override
    protected List<DatenFilm> getSelFilme() {
        ArrayList<DatenFilm> arrayFilme = new ArrayList<>();
        final int[] rows = tabelle.getSelectedRows();
        if (rows.length > 0) {
            for (int row : rows) {
                DatenDownload datenDownload = (DatenDownload) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(row), DatenDownload.DOWNLOAD_REF);
                if (datenDownload.film != null) {
                    arrayFilme.add(datenDownload.film);
                }
            }
        } else {
            NoSelectionErrorDialog.show();
        }
        return arrayFilme;
    }

    private class SearchInMediaDbAction extends AbstractAction {

        @Override
        public void actionPerformed(ActionEvent e) {
            final int row = tabelle.getSelectedRow();
            if (row != -1) {
                MVConfig.add(MVConfig.Configs.SYSTEM_MEDIA_DB_DIALOG_ANZEIGEN, Boolean.TRUE.toString());
                final DatenDownload datenDownload = (DatenDownload) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(row), DatenDownload.DOWNLOAD_REF);
                searchInMediaDb(datenDownload);
            }
        }
    }

    public class BeobMausTabelle extends MouseAdapter {

        private final ShowFilmInformationAction showFilmInformationAction = new ShowFilmInformationAction(false);
        DatenDownload datenDownload;
        private Point p;

        @Override
        public void mouseClicked(MouseEvent arg0) {
            if (arg0.getButton() == MouseEvent.BUTTON1) {
                if (arg0.getClickCount() == 1) {
                    p = arg0.getPoint();
                    int row = tabelle.rowAtPoint(p);
                    int column = tabelle.columnAtPoint(p);
                    if (row >= 0) {
                        buttonTable(row, column);
                    }
                } else if (arg0.getClickCount() > 1) {
                    editDownload();
                }
            }
        }

        @Override
        public void mousePressed(MouseEvent arg0) {
            p = arg0.getPoint();
            int row = tabelle.rowAtPoint(p);
            if (row >= 0) {
                datenDownload = (DatenDownload) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(row), DatenDownload.DOWNLOAD_REF);
            }
            if (arg0.isPopupTrigger()) {
                showMenu(arg0);
            }
        }

        @Override
        public void mouseReleased(MouseEvent arg0) {
            p = arg0.getPoint();
            int row = tabelle.rowAtPoint(p);
            if (row >= 0) {
                datenDownload = (DatenDownload) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(row), DatenDownload.DOWNLOAD_REF);
            }
            if (arg0.isPopupTrigger()) {
                showMenu(arg0);
            }
        }

        private void buttonTable(int row, int column) {
            if (row != -1) {
                datenDownload = (DatenDownload) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(row), DatenDownload.DOWNLOAD_REF);
                if (tabelle.convertColumnIndexToModel(column) == DatenDownload.DOWNLOAD_BUTTON_START) {
                    // filmStartenWiederholenStoppen(boolean alle, boolean starten /* starten/wiederstarten oder stoppen */)
                    if (datenDownload.start != null && !datenDownload.isDownloadManager()) {
                        if (datenDownload.start.status == Start.STATUS_FERTIG) {
                            filmAbspielen();
                        } else if (datenDownload.start.status == Start.STATUS_ERR) {
                            // Download starten
                            filmStartenWiederholenStoppen(false, true /*starten*/);
                        } else {
                            // Download stoppen
                            filmStartenWiederholenStoppen(false, false /*starten*/);
                        }
                    } else {
                        // Download starten
                        filmStartenWiederholenStoppen(false, true /*starten*/);
                    }
                } else if (tabelle.convertColumnIndexToModel(column) == DatenDownload.DOWNLOAD_BUTTON_DEL) {
                    if (datenDownload.start != null) {
                        if (datenDownload.start.status >= Start.STATUS_FERTIG) {
                            downloadsAufraeumen(datenDownload);
                        } else {
                            // Download dauerhaft löschen
                            downloadLoeschen(true);
                        }
                    } else {
                        // Download dauerhaft löschen
                        downloadLoeschen(true);
                    }
                }
            }
        }

        private void showMenu(MouseEvent evt) {
            p = evt.getPoint();
            final int nr = tabelle.rowAtPoint(p);
            if (nr != -1) {
                tabelle.setRowSelectionInterval(nr, nr);
            }
            JPopupMenu jPopupMenu = new JPopupMenu();

            //Film vorziehen
            boolean wartenOderLaufen = false;
            final int row = tabelle.getSelectedRow();
            if (row != -1) {
                DatenDownload download = (DatenDownload) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(row), DatenDownload.DOWNLOAD_REF);
                if (download.start != null) {
                    if (download.start.status <= Start.STATUS_RUN) {
                        wartenOderLaufen = true;
                    }
                }
            }
            // Download starten
            JMenuItem itemStarten = new JMenuItem("Download starten");
            itemStarten.setIcon(IconFontSwing.buildIcon(FontAwesome.CARET_DOWN, 16));
            itemStarten.setEnabled(!wartenOderLaufen);
            jPopupMenu.add(itemStarten);
            itemStarten.addActionListener(arg0 -> filmStartenWiederholenStoppen(false /* alle */, true /* starten */));

            // Download stoppen
            JMenuItem itemStoppen = new JMenuItem("Download stoppen");
            itemStoppen.setEnabled(wartenOderLaufen);
            jPopupMenu.add(itemStoppen);
            itemStoppen.addActionListener(arg0 -> filmStartenWiederholenStoppen(false /* alle */, false /* starten */));

            jPopupMenu.addSeparator();

            JMenuItem itemVorziehen = new JMenuItem("Download vorziehen");
            itemVorziehen.setIcon(Icons.ICON_MENUE_VORZIEHEN);
            jPopupMenu.add(itemVorziehen);
            itemVorziehen.addActionListener(arg0 -> downloadsVorziehen());

            JMenuItem itemLoeschen = new JMenuItem("Download zurückstellen");
            itemLoeschen.setIcon(IconFontSwing.buildIcon(FontAwesome.CLOCK_O, 16));
            jPopupMenu.add(itemLoeschen);
            itemLoeschen.addActionListener(arg0 -> downloadLoeschen(false /* dauerhaft */));
            //dauerhaft löschen
            JMenuItem itemDauerhaftLoeschen = new JMenuItem("Download aus Liste entfernen");
            itemDauerhaftLoeschen.setIcon(IconFontSwing.buildIcon(FontAwesome.TRASH_O, 16));
            jPopupMenu.add(itemDauerhaftLoeschen);
            itemDauerhaftLoeschen.addActionListener(arg0 -> downloadLoeschen(true /* dauerhaft */));
            //Download ändern
            JMenuItem itemAendern = new JMenuItem("Download ändern");
            itemAendern.setIcon(IconFontSwing.buildIcon(FontAwesome.PENCIL_SQUARE_O, 16));
            jPopupMenu.add(itemAendern);
            itemAendern.addActionListener(arg0 -> editDownload());

            jPopupMenu.addSeparator();

            JMenuItem itemAlleStarten = new JMenuItem("alle Downloads starten");
            itemAlleStarten.setIcon(IconFontSwing.buildIcon(FontAwesome.ANGLE_DOUBLE_DOWN, 16));
            jPopupMenu.add(itemAlleStarten);
            itemAlleStarten.addActionListener(arg0 -> filmStartenWiederholenStoppen(true /* alle */, true /* starten */));
            JMenuItem itemAlleStoppen = new JMenuItem("alle Downloads stoppen");
            jPopupMenu.add(itemAlleStoppen);
            itemAlleStoppen.addActionListener(arg0 -> filmStartenWiederholenStoppen(true /* alle */, false /* starten */));

            JMenuItem itemWartendeStoppen = new JMenuItem("wartende Downloads stoppen");
            jPopupMenu.add(itemWartendeStoppen);
            itemWartendeStoppen.addActionListener(arg0 -> stopAllWaitingDownloads());

            JMenuItem itemAktualisieren = new JMenuItem("Liste der Downloads aktualisieren");
            itemAktualisieren.setIcon(IconFontSwing.buildIcon(FontAwesome.REFRESH, 16));
            jPopupMenu.add(itemAktualisieren);
            itemAktualisieren.addActionListener(arg0 -> updateDownloads());

            JMenuItem itemAufraeumen = new JMenuItem(MENU_ITEM_TEXT_CLEANUP_DOWNLOADS);
            itemAufraeumen.setIcon(IconFontSwing.buildIcon(FontAwesome.ERASER, 16));
            jPopupMenu.add(itemAufraeumen);
            itemAufraeumen.addActionListener(arg0 -> cleanupDownloads());

            jPopupMenu.addSeparator();

            // Film abspielen
            JMenuItem itemPlayerDownload = new JMenuItem("gespeicherten Film (Datei) abspielen");
            itemPlayerDownload.setIcon(IconFontSwing.buildIcon(FontAwesome.PLAY, 16));

            itemPlayerDownload.addActionListener(e -> filmAbspielen());
            jPopupMenu.add(itemPlayerDownload);
            // Film löschen
            JMenuItem itemDeleteDownload = new JMenuItem("gespeicherten Film (Datei) löschen");
            itemDeleteDownload.setIcon(Icons.ICON_MENUE_DOWNLOAD_LOESCHEN);

            itemDeleteDownload.addActionListener(e -> filmLoeschen_());
            jPopupMenu.add(itemDeleteDownload);
            // Zielordner öffnen
            JMenuItem itemOeffnen = new JMenuItem("Zielordner öffnen");
            itemOeffnen.setIcon(Icons.ICON_MENUE_FILE_OPEN);
            jPopupMenu.add(itemOeffnen);
            itemOeffnen.addActionListener(e -> zielordnerOeffnen());

            jPopupMenu.addSeparator();

            //Abo ändern
            JMenu submenueAbo = new JMenu("Abo");
            JMenuItem itemChangeAbo = new JMenuItem("Abo ändern");
            JMenuItem itemDelAbo = new JMenuItem("Abo löschen");
            if (datenDownload == null) {
                submenueAbo.setEnabled(false);
                itemChangeAbo.setEnabled(false);
                itemDelAbo.setEnabled(false);
            } else if (datenDownload.film == null) {
                submenueAbo.setEnabled(false);
                itemChangeAbo.setEnabled(false);
                itemDelAbo.setEnabled(false);
            } else {
                final DatenAbo datenAbo = daten.getListeAbo().getAboFuerFilm_schnell(datenDownload.film, false /*die Länge nicht prüfen*/);
                if (datenAbo == null) {
                    submenueAbo.setEnabled(false);
                    itemChangeAbo.setEnabled(false);
                    itemDelAbo.setEnabled(false);
                } else {
                    // dann können wir auch ändern
                    itemDelAbo.addActionListener(e -> daten.getListeAbo().aboLoeschen(datenAbo));
                    itemChangeAbo.addActionListener(e -> {
                        DialogEditAbo dialog = new DialogEditAbo(mediathekGui, true, datenAbo, false/*onlyOne*/);
                        dialog.setVisible(true);
                        if (dialog.ok) {
                            daten.getListeAbo().aenderungMelden();
                        }
                    });
                }
            }
            submenueAbo.add(itemDelAbo);
            submenueAbo.add(itemChangeAbo);
            jPopupMenu.add(submenueAbo);

            jPopupMenu.addSeparator();

            // Film in der MediaDB suchen
            JMenuItem itemDb = new JMenuItem("Titel in der Mediensammlung suchen");
            itemDb.addActionListener(e -> searchInMediaDb());
            jPopupMenu.add(itemDb);

            // URL abspielen
            JMenuItem itemPlayer = new JMenuItem("Film (URL) abspielen");
            itemPlayer.addActionListener(e -> {
                final int nr1 = tabelle.rowAtPoint(p);
                if (nr1 != -1) {
                    final Optional<DatenPset> optPSetPlay = Optional.ofNullable(Daten.listePset.getPsetAbspielen());
                    optPSetPlay.ifPresentOrElse(gruppe -> {
                        Optional<DatenDownload> optDL = Optional.ofNullable((DatenDownload) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(nr1), DatenDownload.DOWNLOAD_REF));
                        optDL.ifPresent(dl -> {
                            if (dl.film != null) {
                                try {
                                    DatenFilm filmDownload = (DatenFilm) dl.film.clone();
                                    // und jetzt die tatsächlichen URLs des Downloads eintragen
                                    filmDownload.setUrl(dl.arr[DatenDownload.DOWNLOAD_URL]);
                                    filmDownload.setUrlKlein("");
                                    // und starten
                                    daten.starterClass.urlMitProgrammStarten(gruppe, filmDownload, "");
                                }
                                catch (CloneNotSupportedException ex) {
                                    logger.error("Cloning is not supported", ex);
                                }
                            }
                        });
                    }, () -> {
                        final String menuPath;
                        if (SystemUtils.IS_OS_MAC_OSX) {
                            menuPath = "MediathekView->Einstellungen…->Aufzeichnen und Abspielen->Set bearbeiten";
                        } else {
                            menuPath = "Datei->Einstellungen->Set bearbeiten";
                        }
                        MVMessageDialog.showMessageDialog(mediathekGui, "Bitte legen Sie im Menü \"" + menuPath + "\" ein Programm zum Abspielen fest.",
                                "Kein Videoplayer!", JOptionPane.INFORMATION_MESSAGE);
                    });
                }
            });
            jPopupMenu.add(itemPlayer);

            // URL kopieren
            JMenuItem itemUrl = new JMenuItem("URL kopieren");
            itemUrl.addActionListener(e -> {
                int nr1 = tabelle.rowAtPoint(p);
                if (nr1 != -1) {
                    GuiFunktionen.copyToClipboard(
                            tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(nr1),
                                    DatenDownload.DOWNLOAD_URL).toString());
                }
            });
            jPopupMenu.add(itemUrl);

            jPopupMenu.add(showFilmInformationAction);

            jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
        }
    }

    /**
     * This class filters the shown table items based on the made selection.
     */
    private final class ViewCategoryListener implements ActionListener {
        @Override
        public void actionPerformed(ActionEvent e) {
            JComboBox<?> source = (JComboBox<?>) e.getSource();

            switch (source.getSelectedIndex()) {
                case INDEX_COMBO_VIEW_ALL -> {
                    onlyNotStarted = false;
                    onlyStarted = false;
                    onlyWaiting = false;
                    onlyFinished = false;
                    onlyRun = false;
                }
                case INDEX_COMBO_VIEW_NOT_STARTED -> {
                    onlyNotStarted = true;
                    onlyStarted = false;
                    onlyWaiting = false;
                    onlyFinished = false;
                    onlyRun = false;
                }
                case INDEX_COMBO_VIEW_STARTED -> {
                    onlyNotStarted = false;
                    onlyStarted = true;
                    onlyWaiting = false;
                    onlyFinished = false;
                    onlyRun = false;
                }
                case INDEX_COMBO_VIEW_WAITING -> {
                    onlyNotStarted = false;
                    onlyStarted = false;
                    onlyWaiting = true;
                    onlyFinished = false;
                    onlyRun = false;
                }
                case INDEX_COMBO_VIEW_FINISHED_ONLY -> {
                    onlyNotStarted = false;
                    onlyStarted = false;
                    onlyWaiting = false;
                    onlyFinished = true;
                    onlyRun = false;
                }
                case INDEX_COMBO_VIEW_RUN_ONLY -> {
                    onlyNotStarted = false;
                    onlyStarted = false;
                    onlyWaiting = false;
                    onlyFinished = false;
                    onlyRun = true;
                }
            }

            reloadTable();
        }
    }

    /**
     * This class filters the shown table items based on the made selection.
     */
    private final class DisplayCategoryListener implements ActionListener {
        @Override
        public void actionPerformed(ActionEvent e) {
            JComboBox<?> source = (JComboBox<?>) e.getSource();

            switch (source.getSelectedIndex()) {
                case INDEX_COMBO_DISPLAY_ALL -> {
                    onlyAbos = false;
                    onlyDownloads = false;
                }
                case INDEX_COMBO_DISPLAY_DOWNLOADS_ONLY -> {
                    onlyAbos = false;
                    onlyDownloads = true;
                }
                case INDEX_COMBO_DISPLAY_ABOS_ONLY -> {
                    onlyAbos = true;
                    onlyDownloads = false;
                }
            }

            reloadTable();
        }
    }

    /**
     * This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    // Generated using JFormDesigner non-commercial license
    private void initComponents() {
        jSplitPane1 = new JSplitPane();
        jPanelFilterExtern = new JPanel();
        var panel3 = new JPanel();
        var label1 = new JLabel();
        cbDisplayCategories = new JComboBox<>();
        var label2 = new JLabel();
        cbView = new JComboBox<>();
        btnClear = new JButton();
        var panel2 = new JPanel();
        var jLabel3 = new JLabel();
        jSpinnerAnzahlDownloads = new JSpinner();
        var lblBandwidth = new JLabel();
        var jLabel1 = new JLabel();
        jSpinner1 = new JSpinner();
        var spDownload = new JScrollPane();
        txtDownload = new JEditorPane();
        var downloadListArea = new JPanel();
        downloadListScrollPane = new JScrollPane();
        fxDescriptionPanel = new JFXPanel();
        toolBarPanel = new JFXPanel();

        //======== this ========
        setLayout(new BorderLayout());

        //======== jSplitPane1 ========
        {
            jSplitPane1.setDividerLocation(330);

            //======== jPanelFilterExtern ========
            {
                jPanelFilterExtern.setPreferredSize(new Dimension(200, 644));
                jPanelFilterExtern.setLayout(new MigLayout(
                    new LC().insets("0").hideMode(3).gridGap("0", "0"), //NON-NLS
                    // columns
                    new AC()
                        .grow().fill(),
                    // rows
                    new AC()
                        .gap()
                        .fill().gap()
                        .grow().fill()));

                //======== panel3 ========
                {
                    panel3.setBorder(new TitledBorder("Anzeige")); //NON-NLS
                    panel3.setLayout(new MigLayout(
                        new LC().insets("5").hideMode(3).gridGap("5", "5"), //NON-NLS
                        // columns
                        new AC()
                            .fill().gap()
                            .grow().fill(),
                        // rows
                        new AC()
                            .fill().gap()
                            .fill().gap()
                            .fill()));

                    //---- label1 ----
                    label1.setText("Typ:"); //NON-NLS
                    panel3.add(label1, new CC().cell(0, 0));
                    panel3.add(cbDisplayCategories, new CC().cell(1, 0));

                    //---- label2 ----
                    label2.setText("Status:"); //NON-NLS
                    panel3.add(label2, new CC().cell(0, 1));
                    panel3.add(cbView, new CC().cell(1, 1));

                    //---- btnClear ----
                    btnClear.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-clear.png"))); //NON-NLS
                    btnClear.setToolTipText("Filter zur\u00fccksetzen"); //NON-NLS
                    panel3.add(btnClear, new CC().cell(0, 2, 2, 1).alignX("right").growX(0).width("32:32:32").height("32:32:32")); //NON-NLS
                }
                jPanelFilterExtern.add(panel3, new CC().cell(0, 0));

                //======== panel2 ========
                {
                    panel2.setBorder(new TitledBorder("Downloads")); //NON-NLS
                    panel2.setLayout(new MigLayout(
                        new LC().insets("5").hideMode(3).gridGap("5", "5"), //NON-NLS
                        // columns
                        new AC()
                            .fill().gap()
                            .fill(),
                        // rows
                        new AC()
                            .gap()
                            .fill()));

                    //---- jLabel3 ----
                    jLabel3.setText("gleichzeitig:"); //NON-NLS
                    panel2.add(jLabel3, new CC().cell(0, 0));
                    panel2.add(jSpinnerAnzahlDownloads, new CC().cell(1, 0));

                    //---- lblBandwidth ----
                    lblBandwidth.setText("max. Bandbreite:"); //NON-NLS
                    panel2.add(lblBandwidth, new CC().cell(0, 1));

                    //---- jLabel1 ----
                    jLabel1.setText("KiB/s"); //NON-NLS
                    panel2.add(jLabel1, new CC().cell(2, 1));

                    //---- jSpinner1 ----
                    jSpinner1.setModel(new SpinnerNumberModel(0, 0, 1048576, 1));
                    jSpinner1.setToolTipText("<html>\nBandbreitenbegrenzung eines Downloads in XX Kilobytes pro Sekunde.\n<b><br><u>WICHTIG:</u><br>ENTWEDER<br>den Wert \u00fcber die Pfeiltasten \u00e4ndern<br>ODER<br>Zahlen eingeben UND ENTER-Taste dr\u00fccken!</b>\n</html>"); //NON-NLS
                    panel2.add(jSpinner1, new CC().cell(1, 1));
                }
                jPanelFilterExtern.add(panel2, new CC().cell(0, 1));

                //======== spDownload ========
                {
                    spDownload.setPreferredSize(new Dimension(14, 150));

                    //---- txtDownload ----
                    txtDownload.setEditable(false);
                    txtDownload.setOpaque(false);
                    txtDownload.setPreferredSize(new Dimension(10, 500));
                    spDownload.setViewportView(txtDownload);
                }
                jPanelFilterExtern.add(spDownload, new CC().cell(0, 2));
            }
            jSplitPane1.setLeftComponent(jPanelFilterExtern);

            //======== downloadListArea ========
            {
                downloadListArea.setLayout(new BorderLayout());
                downloadListArea.add(downloadListScrollPane, BorderLayout.CENTER);
                downloadListArea.add(fxDescriptionPanel, BorderLayout.SOUTH);
            }
            jSplitPane1.setRightComponent(downloadListArea);
        }
        add(jSplitPane1, BorderLayout.CENTER);
        add(toolBarPanel, BorderLayout.NORTH);
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    private JSplitPane jSplitPane1;
    private JPanel jPanelFilterExtern;
    private JComboBox<String> cbDisplayCategories;
    private JComboBox<String> cbView;
    private JButton btnClear;
    private JSpinner jSpinnerAnzahlDownloads;
    private JSpinner jSpinner1;
    private JEditorPane txtDownload;
    private JScrollPane downloadListScrollPane;
    private JFXPanel fxDescriptionPanel;
    private JFXPanel toolBarPanel;
    // End of variables declaration//GEN-END:variables
}
