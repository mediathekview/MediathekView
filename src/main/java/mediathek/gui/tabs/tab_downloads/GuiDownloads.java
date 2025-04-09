package mediathek.gui.tabs.tab_downloads;

import ca.odell.glazedlists.EventList;
import ca.odell.glazedlists.GlazedLists;
import ca.odell.glazedlists.swing.GlazedListsSwing;
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.controller.history.MVUsedUrl;
import mediathek.controller.starter.Start;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenFilm;
import mediathek.daten.DatenPset;
import mediathek.daten.abo.DatenAbo;
import mediathek.filmeSuchen.ListenerFilmeLaden;
import mediathek.filmeSuchen.ListenerFilmeLadenEvent;
import mediathek.gui.actions.*;
import mediathek.gui.dialog.DialogBeendenZeit;
import mediathek.gui.dialog.DialogEditAbo;
import mediathek.gui.dialog.DialogEditDownload;
import mediathek.gui.messages.*;
import mediathek.gui.tabs.AGuiTabPanel;
import mediathek.gui.tabs.tab_film.FilmDescriptionPanel;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.*;
import mediathek.tool.cellrenderer.CellRendererDownloads;
import mediathek.tool.datum.Datum;
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
import org.jdesktop.swingx.JXStatusBar;
import org.jetbrains.annotations.NotNull;

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
import java.util.List;
import java.util.Optional;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;

public class GuiDownloads extends AGuiTabPanel {
    public static final String NAME = "Downloads";
    public static final int DIVIDER_LOCATION = -1;
    private static final String COMBO_DISPLAY_ALL = "alle";
    private static final String COMBO_DISPLAY_DOWNLOADS_ONLY = "nur Downloads";
    private static final String COMBO_DISPLAY_ABOS_ONLY = "nur Abos";
    private static final String COMBO_VIEW_ALL = "alle";
    private static final String COMBO_VIEW_NOT_STARTED = "nicht gestartet";
    private static final String COMBO_VIEW_STARTED = "gestartet";
    private static final String COMBO_VIEW_WAITING = "nur wartende";
    private static final String COMBO_VIEW_RUN_ONLY = "nur laufende";
    private static final String COMBO_VIEW_FINISHED_ONLY = "nur abgeschlossene";
    private static final String ACTION_MAP_KEY_EDIT_DOWNLOAD = "dl_aendern";
    private static final String ACTION_MAP_KEY_DELETE_DOWNLOAD = "dl_delete";
    private static final String ACTION_MAP_KEY_MARK_AS_SEEN = "seen";
    private static final String ACTION_MAP_KEY_MAERK_AS_UNSEEN = "unseen";
    private static final String ACTION_MAP_KEY_START_DOWNLOAD = "dl_start";
    private final static int[] COLUMNS_DISABLED = {DatenDownload.DOWNLOAD_BUTTON_START, DatenDownload.DOWNLOAD_BUTTON_DEL,
            DatenDownload.DOWNLOAD_REF, DatenDownload.DOWNLOAD_URL_RTMP};
    private static final Logger logger = LogManager.getLogger(GuiDownloads.class);
    private final AtomicLong _lastUpdate = new AtomicLong(0);
    private final JCheckBoxMenuItem cbShowDownloadDescription = new JCheckBoxMenuItem("Filmbeschreibung anzeigen");
    private final Configuration config = ApplicationConfiguration.getConfiguration();
    private final MarkFilmAsSeenAction markFilmAsSeenAction = new MarkFilmAsSeenAction();
    private final MarkFilmAsUnseenAction markFilmAsUnseenAction = new MarkFilmAsUnseenAction();
    private final JXStatusBar statusBar = new JXStatusBar();
    private final DownloadStartInfoProperty startInfoProperty = new DownloadStartInfoProperty();
    private final AboLabel lblAbos = new AboLabel(startInfoProperty);
    private final TotalDownloadsLabel totalDownloadsLabel = new TotalDownloadsLabel(startInfoProperty);
    private final ManualDownloadsInfoLabel manualDownloadsInfoLabel = new ManualDownloadsInfoLabel(startInfoProperty);
    private final WaitingDownloadsInfoLabel waitingDownloadsInfoLabel = new WaitingDownloadsInfoLabel(startInfoProperty);
    private final ActiveDownloadsInfoLabel activeDownloadsInfoLabel = new ActiveDownloadsInfoLabel(startInfoProperty);
    private final FinishedDownloadsInfoLabel finishedDownloadsInfoLabel = new FinishedDownloadsInfoLabel(startInfoProperty);
    private final FailedDownloadsInfoLabel failedDownloadsInfoLabel = new FailedDownloadsInfoLabel(startInfoProperty);
    private final DownloadsConfigPanel dlConfigPanel = new DownloadsConfigPanel();
    protected final StartAllDownloadsAction startAllDownloadsAction = new StartAllDownloadsAction(this);
    protected final StartAllDownloadsTimedAction startAllDownloadsTimedAction = new StartAllDownloadsTimedAction(this);
    protected final StopAllDownloadsAction stopAllDownloadsAction = new StopAllDownloadsAction(this);
    protected final StopAllWaitingDownloadsAction stopAllWaitingDownloadsAction = new StopAllWaitingDownloadsAction(this);
    protected final RefreshDownloadListAction refreshDownloadListAction = new RefreshDownloadListAction(this);
    protected final CleanupDownloadListAction cleanupDownloadListAction = new CleanupDownloadListAction(this);
    protected final PlayDownloadAction playDownloadAction = new PlayDownloadAction(this);
    protected final StopDownloadsAction stopDownloadsAction = new StopDownloadsAction(this);
    protected final StartDownloadsAction startDownloadsAction = new StartDownloadsAction(this);
    protected final DeferDownloadsAction deferDownloadsAction = new DeferDownloadsAction(this);
    protected final AdvanceDownloadsAction advanceDownloadsAction = new AdvanceDownloadsAction(this);
    protected final DeleteDownloadsAction deleteDownloadsAction = new DeleteDownloadsAction(this);
    protected final EditDownloadAction editDownloadAction = new EditDownloadAction(this);
    protected final DeleteDownloadAction deleteDownloadAction = new DeleteDownloadAction(this);
    protected final OpenTargetFolderAction openTargetFolderAction = new OpenTargetFolderAction(this);
    protected final ToggleFilterPanelAction toggleFilterPanelAction = new ToggleFilterPanelAction();
    protected final MergeSubtitleWithVideoAction mergeSubtitleWithVideoAction = new MergeSubtitleWithVideoAction(MediathekGui.ui());
    protected final JToolBar swingToolBar = new JToolBar();
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
    private MVDownloadsTable tabelle;
    private JSplitPane jSplitPane1;
    private JPanel jPanelFilterExtern;
    private JComboBox<String> cbDisplayCategories;
    private JComboBox<String> cbView;
    private JButton btnClear;
    private JScrollPane downloadListScrollPane;

    public GuiDownloads(Daten aDaten, MediathekGui mediathekGui) {
        super();
        daten = aDaten;
        this.mediathekGui = mediathekGui;
        descriptionPanel = new FilmDescriptionPanel();


        initComponents();
        // use rounded combo boxes
        cbDisplayCategories.putClientProperty("JComponent.roundRect", true);
        cbView.putClientProperty("JComponent.roundRect", true);

        setupDownloadListStatusBar();

        setupDownloadListTable();

        setupDescriptionTab(tabelle, cbShowDownloadDescription, ApplicationConfiguration.DOWNLOAD_SHOW_DESCRIPTION, this::getCurrentlySelectedFilm);

        init();

        setupFilmSelectionPropertyListener();
        setupDownloadSizeSelectionUpdater();

        initTable();

        addListenerMediathekView();
        setupDisplayCategories();

        setupCheckboxView();

        setupFilterPanel();

        if (Taskbar.isTaskbarSupported())
            setupTaskbarMenu();

        tabelle.getTableHeader().setReorderingAllowed(false);
    }

    private void setupDownloadListStatusBar() {
        statusBar.add(totalDownloadsLabel);
        statusBar.add(lblAbos);
        statusBar.add(manualDownloadsInfoLabel);
        statusBar.add(activeDownloadsInfoLabel);
        statusBar.add(waitingDownloadsInfoLabel);
        statusBar.add(finishedDownloadsInfoLabel);
        statusBar.add(failedDownloadsInfoLabel);
    }

    @Override
    public void tabelleSpeichern() {
        if (tabelle != null) {
            tabelle.writeTableConfigurationData();
        }
    }

    private void updateFilmSizes(int[] rows) {
        boolean updateNeeded = false;

        for (var row : rows) {
            try {
                var indexRow = tabelle.convertRowIndexToModel(row);
                var listeDownloads = daten.getListeDownloads();
                var dlInfo = listeDownloads.get(indexRow);
                if (dlInfo != null) {
                    if (dlInfo.mVFilmSize.getSize() != 0)
                        continue;

                    if (dlInfo.film != null) {
                        var oldSize = dlInfo.mVFilmSize.getSize();
                        dlInfo.queryLiveSize();
                        if (dlInfo.mVFilmSize.getSize() != oldSize)
                            updateNeeded = true;
                    }
                } else
                    logger.error("Could not get download object");
            } catch (Exception ignored) {
            }
        }

        if (updateNeeded)
            reloadTable();
    }

    private void setupDownloadSizeSelectionUpdater() {
        tabelle.getSelectionModel().addListSelectionListener(l -> {
            if (!l.getValueIsAdjusting()) {
                var rows = tabelle.getSelectedRows();
                updateFilmSizes(rows);
            }
        });
    }

    /**
     * Update the property with the current number of selected entries from the JTable.
     */
    private void setupFilmSelectionPropertyListener() {
        tabelle.getSelectionModel().addListSelectionListener(e -> {
            if (!e.getValueIsAdjusting()) {
                updateSelectedListItemsCount(tabelle);
            }
        });
        addComponentListener(new ComponentAdapter() {
            @Override
            public void componentShown(ComponentEvent e) {
                updateSelectedListItemsCount(tabelle);
                onComponentShown();
            }
        });
    }

    private void setupDownloadListTable() {
        tabelle = new MVDownloadsTable();
        downloadListScrollPane.setViewportView(tabelle);
    }

    private void setupDisplayCategories() {
        final EventList<String> displaySelectionList = GlazedLists.eventListOf(COMBO_DISPLAY_ALL, COMBO_DISPLAY_DOWNLOADS_ONLY, COMBO_DISPLAY_ABOS_ONLY);
        cbDisplayCategories.setModel(GlazedListsSwing.eventComboBoxModelWithThreadProxyList(displaySelectionList));
        cbDisplayCategories.getModel().setSelectedItem(COMBO_DISPLAY_ALL);
        cbDisplayCategories.addActionListener(new DisplayCategoryListener());
    }

    private void setupCheckboxView() {
        EventList<String> viewSelectionList = GlazedLists.eventListOf(COMBO_VIEW_ALL, COMBO_VIEW_NOT_STARTED,
                COMBO_VIEW_STARTED, COMBO_VIEW_WAITING, COMBO_VIEW_RUN_ONLY, COMBO_VIEW_FINISHED_ONLY);
        cbView.setModel(GlazedListsSwing.eventComboBoxModelWithThreadProxyList(viewSelectionList));
        cbView.getModel().setSelectedItem(COMBO_VIEW_ALL);
        cbView.addActionListener(new ViewCategoryListener());
    }

    private void initTable() {
        tabelle.readColumnConfigurationData();
        tabelle.setSpalten();
        if (tabelle.getRowCount() > 0) {
            tabelle.setRowSelectionInterval(0, 0);
        }
    }

    private void setupFilterPanel() {
        final boolean visible = MVConfig.getBool(MVConfig.Configs.SYSTEM_TAB_DOWNLOAD_FILTER_VIS);
        updateFilterVisibility(visible);

        setSplitDividerLocation();
        jSplitPane1.addPropertyChangeListener(JSplitPane.DIVIDER_LOCATION_PROPERTY, e -> {
            if (jPanelFilterExtern.isVisible()) {
                config.setProperty(ApplicationConfiguration.APPLICATION_UI_DOWNLOAD_TAB_DIVIDER_LOCATION, jSplitPane1.getDividerLocation());
            }
        });
    }

    protected void toggleDownloadFilterPanel() {
        boolean visibility = !jPanelFilterExtern.isVisible();
        updateFilterVisibility(visibility);
        MVConfig.add(MVConfig.Configs.SYSTEM_TAB_DOWNLOAD_FILTER_VIS, Boolean.toString(visibility));
    }

    private void updateFilterVisibility(boolean visible) {
        jPanelFilterExtern.setVisible(visible);
        if (visible) {
            setSplitDividerLocation();
        }
    }

    private void setSplitDividerLocation() {
        var location = config.getInt(ApplicationConfiguration.APPLICATION_UI_DOWNLOAD_TAB_DIVIDER_LOCATION, DIVIDER_LOCATION);
        if (location == DIVIDER_LOCATION) {
            jSplitPane1.resetToPreferredSizes();
        } else {
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

    @Override
    public void installMenuEntries(JMenu menu) {
        JMenuItem miMarkFilmAsSeen = new JMenuItem("Filme als gesehen markieren");
        miMarkFilmAsSeen.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_G, KeyEvent.CTRL_DOWN_MASK));
        miMarkFilmAsSeen.addActionListener(markFilmAsSeenAction);

        JMenuItem miMarkFilmAsUnseen = new JMenuItem("Filme als ungesehen markieren");
        miMarkFilmAsUnseen.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N, KeyEvent.CTRL_DOWN_MASK));
        miMarkFilmAsUnseen.addActionListener(markFilmAsUnseenAction);

        menu.add(startAllDownloadsAction);
        menu.add(startAllDownloadsTimedAction);
        menu.add(stopAllDownloadsAction);
        menu.add(stopAllWaitingDownloadsAction);
        menu.add(refreshDownloadListAction);
        menu.add(cleanupDownloadListAction);
        menu.addSeparator();
        menu.add(startDownloadsAction);
        menu.add(stopDownloadsAction);
        menu.add(advanceDownloadsAction);
        menu.add(deferDownloadsAction);
        menu.add(deleteDownloadsAction);
        menu.add(editDownloadAction);
        menu.addSeparator();
        menu.add(mergeSubtitleWithVideoAction);
        menu.addSeparator();
        menu.add(cbShowDownloadDescription);
        menu.addSeparator();
        menu.add(miMarkFilmAsSeen);
        menu.add(miMarkFilmAsUnseen);
        menu.add(playDownloadAction);
    }

    public void onComponentShown() {
        updateFilmData();
    }

    public void starten(boolean alle) {
        filmStartenWiederholenStoppen(alle, true, true, false);
    }

    public void stoppen(boolean alle) {
        filmStartenWiederholenStoppen(alle, false, true, false);
    }

    private void setupKeyMappings() {
        final InputMap im = tabelle.getInputMap();
        im.put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0), ACTION_MAP_KEY_EDIT_DOWNLOAD);
        im.put(KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, 0), ACTION_MAP_KEY_DELETE_DOWNLOAD);
        im.put(KeyStroke.getKeyStroke(KeyEvent.VK_G, 0), ACTION_MAP_KEY_MARK_AS_SEEN);
        im.put(KeyStroke.getKeyStroke(KeyEvent.VK_U, 0), ACTION_MAP_KEY_MAERK_AS_UNSEEN);
        im.put(KeyStroke.getKeyStroke(KeyEvent.VK_D, 0), ACTION_MAP_KEY_START_DOWNLOAD);

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
                filmStartenWiederholenStoppen(false, true, true, false);
            }
        });
    }

    private void init() {
        setupKeyMappings();
        //Tabelle einrichten

        final CellRendererDownloads cellRenderer = new CellRendererDownloads();
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

        btnClear.addActionListener(e -> {
            cbDisplayCategories.setSelectedIndex(0);
            cbView.setSelectedIndex(0);
        });

        setSplitDividerLocation();
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
        MessageBus.getMessageBus().subscribe(this);

        Listener.addListener(new Listener(Listener.EREIGNIS_BLACKLIST_AUCH_FUER_ABOS, GuiDownloads.class.getSimpleName()) {
            @Override
            public void ping() {
                if (Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_ABOS_SOFORT_SUCHEN))) {
                    updateDownloads();
                }
            }
        });
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
            updateStartInfoProperty();
        });
    }

    /**
     * Setup and show film description panel.
     * Most of the setup is done in {@link GuiDownloads} function.
     * Here we just display the panel
     */
    @Override
    protected void setupShowFilmDescriptionMenuItem() {
        cbShowDownloadDescription.setSelected(ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.DOWNLOAD_SHOW_DESCRIPTION, true));
        cbShowDownloadDescription.addActionListener(e -> {
            boolean visible = cbShowDownloadDescription.isSelected();
            makeDescriptionTabVisible(visible);
            config.setProperty(ApplicationConfiguration.DOWNLOAD_SHOW_DESCRIPTION, visible);
        });
    }

    private synchronized void reloadTable() {
        // nur Downloads die schon in der Liste sind werden geladen
        tabelle.getSpalten();

        daten.getListeDownloads().getModel(model, onlyAbos, onlyDownloads, onlyNotStarted, onlyStarted, onlyWaiting, onlyRun, onlyFinished);
        tabelle.setSpalten();
        updateFilmData();
        updateStartInfoProperty();
    }

    @Handler
    private void handleStartEvent(StartEvent msg) {
        SwingUtilities.invokeLater(this::reloadTable);
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
            // Auto DL should NOT start manually created downloads
            filmStartenWiederholenStoppen(true, true, false, true);
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
        final var model = tabelle.getModel();
        if (rows.length > 0) {
            for (int row : rows) {
                DatenDownload datenDownload = (DatenDownload) model.getValueAt(tabelle.convertRowIndexToModel(row), DatenDownload.DOWNLOAD_REF);
                arrayDownloads.add(datenDownload);
            }
        } else {
            NoSelectionErrorDialog.show(this);
        }
        return arrayDownloads;
    }

    @Override
    public Optional<DatenFilm> getCurrentlySelectedFilm() {
        try {
            final int selectedTableRow = tabelle.getSelectedRow();
            if (selectedTableRow != -1) {
                Optional<DatenFilm> optRet;
                int modelIndex = tabelle.convertRowIndexToModel(selectedTableRow);
                final DatenDownload download = (DatenDownload) tabelle.getModel().getValueAt(modelIndex, DatenDownload.DOWNLOAD_REF);
                if (download.film == null)
                    optRet = Optional.empty();
                else
                    optRet = Optional.of(download.film);
                return optRet;
            } else {
                return Optional.empty();
            }
        } catch (Exception e) {
            return Optional.empty();
        }
    }

    private DatenDownload getSelDownload() {
        DatenDownload datenDownload = null;
        final int row = tabelle.getSelectedRow();
        if (row != -1) {
            datenDownload = (DatenDownload) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(row), DatenDownload.DOWNLOAD_REF);
        } else {
            NoSelectionErrorDialog.show(this);
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

    public void downloadsVorziehen() {
        ArrayList<DatenDownload> arrayDownloads = getSelDownloads();
        if (arrayDownloads.isEmpty()) {
            return;
        }
        daten.getListeDownloads().downloadsVorziehen(arrayDownloads);
    }

    public void zielordnerOeffnen() {
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

    public void filmLoeschen_() {
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
            logger.error("Fehler beim löschen: {}", datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
        }
    }

    /**
     * @param permanentDeletion false werden Downloads zurück gestellt. true löscht permanent.
     */
    public void downloadLoeschen(boolean permanentDeletion) {
        try {
            ArrayList<DatenDownload> arrayDownloads = getSelDownloads();
            if (arrayDownloads.isEmpty()) {
                return;
            }

            var zeit = DateTimeFormatter.ofPattern("dd.MM.yyyy").format(LocalDateTime.ofInstant(Instant.now(), ZoneId.systemDefault()));

            ArrayList<DatenDownload> arrayDownloadsLoeschen = new ArrayList<>();
            List<MVUsedUrl> urlAboList = new ArrayList<>();

            for (DatenDownload datenDownload : arrayDownloads) {
                if (permanentDeletion) {
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
                daten.getAboHistoryController().add(urlAboList);
            }

            daten.getListeDownloads().downloadLoeschen(arrayDownloadsLoeschen);
            reloadTable();
        } catch (Exception ex) {
            logger.error("downloadLoeschen()", ex);
        }
    }

    private @NotNull List<DatenDownload> addAllDownloadsToList() {
        final var rowCount = tabelle.getRowCount();
        final var tableModel = tabelle.getModel();
        List<DatenDownload> destList = new ArrayList<>();

        for (int i = 0; i < rowCount; ++i) {
            DatenDownload datenDownload = (DatenDownload) tableModel.getValueAt(tabelle.convertRowIndexToModel(i), DatenDownload.DOWNLOAD_REF);
            destList.add(datenDownload);
        }
        return destList;
    }

    /**
     * starts all downloads at a specific time.
     */
    public void startAllDownloadsAtSpecificTime() {
        // bezieht sich immer auf "alle"
        // Film der noch keinen Starts hat wird gestartet
        // Film dessen Start schon auf fertig/fehler steht wird wieder gestartet
        // wird immer vom Benutzer aufgerufen
        if (tabelle.getRowCount() == 0) {
            JOptionPane.showMessageDialog(this,
                    "Es sind keine Downloads in der Liste zum Starten vorhanden.",
                    Konstanten.PROGRAMMNAME,
                    JOptionPane.INFORMATION_MESSAGE);
            return;
        }

        // ==========================
        // erst mal die Liste nach der Tabelle sortieren
        tabelle.sortDownloadListByTableRows();
        final var allDownloadsList = addAllDownloadsToList();

        // ========================
        // und jetzt abarbeiten
        ArrayList<DatenDownload> listeUrlsDownloadsAbbrechen = new ArrayList<>();
        ArrayList<DatenDownload> listeDownloadsStarten = new ArrayList<>();

        for (DatenDownload download : allDownloadsList) {
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
                    int reply = GuiFunktionen.createDismissableMessageDialog(mediathekGui, "Fertiger Download",
                            "Film nochmal starten?  ==> " + download.arr[DatenDownload.DOWNLOAD_TITEL],
                            JOptionPane.YES_NO_OPTION, JOptionPane.NO_OPTION, 10, TimeUnit.SECONDS,
                            JOptionPane.QUESTION_MESSAGE);
                    if (reply != JOptionPane.YES_OPTION) {
                        // weiter mit der nächsten URL
                        continue;
                    }
                    listeUrlsDownloadsAbbrechen.add(download);
                    if (download.isFromAbo()) {
                        // wenn er schon fertig ist und ein Abo ist, Url auch aus dem Logfile löschen, der Film ist damit wieder auf "Anfang"
                        daten.getAboHistoryController().removeUrl(download.arr[DatenDownload.DOWNLOAD_HISTORY_URL]);
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
        DialogBeendenZeit dialogBeenden = new DialogBeendenZeit(mediathekGui, listeDownloadsStarten);
        dialogBeenden.setVisible(true);
        if (dialogBeenden.applicationCanTerminate()) {
            // fertig und beenden
            mediathekGui.setShutdownRequested(dialogBeenden.isShutdownRequested());
            mediathekGui.quitApplication();
        }

        reloadTable();
    }

    private void filmStartenWiederholenStoppen(boolean processAllDownloads, boolean starten /* starten/wiederstarten oder stoppen */,
                                               boolean restartFinishedDownloads /*auch fertige wieder starten*/,
                                               boolean skipManualDownloads) {
        // bezieht sich immer auf "alle" oder nur die markierten
        // Film der noch keinen Starts hat wird gestartet
        // Film dessen Start schon auf fertig/fehler steht wird wieder gestartet
        // bei !starten wird der Film gestoppt
        // wird immer vom Benutzer aufgerufen
        ArrayList<DatenDownload> listeDownloadsLoeschen = new ArrayList<>();
        ArrayList<DatenDownload> listeDownloadsStarten = new ArrayList<>();

        if (tabelle.getRowCount() == 0) {
            return;
        }

        // ==========================
        // erst mal die Liste nach der Tabelle sortieren
        if (starten && processAllDownloads) {
            tabelle.sortDownloadListByTableRows();
        }

        // ==========================
        // die URLs sammeln
        final var selectedDownloadsList = processAllDownloads ? addAllDownloadsToList() : getSelDownloads();

        if (!starten) {
            // dann das Starten von neuen Downloads etwas Pausieren
            daten.getStarterClass().delayNewStarts();
        }

        // ========================
        // und jetzt abarbeiten
        int antwort = -1;
        for (DatenDownload download : selectedDownloadsList) {
            if (starten) {
                // ==========================================
                // starten
                if (download.start != null) {
                    if (download.start.status == Start.STATUS_RUN
                            || !restartFinishedDownloads && download.start.status > Start.STATUS_RUN) {
                        // wenn er noch läuft gibts nix
                        // fertige bleiben auch unverändert
                        continue;
                    }
                    if (download.start.status > Start.STATUS_RUN) {
                        // wenn er schon fertig ist, erst mal fragen vor dem erneuten Starten
                        if (antwort == -1) {
                            // nur einmal fragen
                            String text;
                            if (selectedDownloadsList.size() > 1) {
                                text = "Es sind bereits fertige Filme dabei,\n"
                                        + "diese nochmal starten?";
                            } else {
                                text = "Film nochmal starten?  ==> " + download.arr[DatenDownload.DOWNLOAD_TITEL];
                            }
                            antwort = GuiFunktionen.createDismissableMessageDialog(mediathekGui, "Fertiger Download",
                                    text,
                                    JOptionPane.YES_NO_CANCEL_OPTION, JOptionPane.NO_OPTION, 10, TimeUnit.SECONDS,
                                    JOptionPane.QUESTION_MESSAGE);
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
                            // wenn er schon fertig ist und ein Abos ist, Url auch aus dem Logfile löschen, der Film ist damit wieder auf "Anfang"
                            daten.getAboHistoryController().removeUrl(download.arr[DatenDownload.DOWNLOAD_HISTORY_URL]);
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

        //do not start manual downloads, only downloads which were created from abos
        if (skipManualDownloads)
            listeDownloadsStarten.removeIf(item -> !item.isFromAbo());

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

    private void updateFilmData() {
        if (!isShowing())
            return;

        var infoDialog = mediathekGui.getFilmInfoDialog();
        if (infoDialog != null) {
            infoDialog.updateCurrentFilm(getCurrentlySelectedFilm().orElse(null));
        }
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
            NoSelectionErrorDialog.show(this);
        }
        return arrayFilme;
    }

    private void initComponents() {
        jSplitPane1 = new JSplitPane();
        jPanelFilterExtern = new JPanel();
        var panel3 = new JPanel();
        var label1 = new JLabel();
        cbDisplayCategories = new JComboBox<>();
        var label2 = new JLabel();
        cbView = new JComboBox<>();
        btnClear = new JButton();
        var downloadListArea = new JPanel();
        downloadListScrollPane = new JScrollPane();

        setLayout(new BorderLayout());

        jSplitPane1.setDividerLocation(330);

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

        label1.setText("Typ:"); //NON-NLS
        panel3.add(label1, new CC().cell(0, 0));
        panel3.add(cbDisplayCategories, new CC().cell(1, 0));

        label2.setText("Status:"); //NON-NLS
        panel3.add(label2, new CC().cell(0, 1));
        panel3.add(cbView, new CC().cell(1, 1));

        btnClear.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/broom.svg")); //NON-NLS
        btnClear.setToolTipText("Filter zurücksetzen"); //NON-NLS
        panel3.add(btnClear, new CC().cell(0, 2, 2, 1).alignX("right").growX(0).width("32:32:32").height("32:32:32")); //NON-NLS
        jPanelFilterExtern.add(panel3, new CC().cell(0, 0));
        jPanelFilterExtern.add(dlConfigPanel, new CC().cell(0, 1));
        jSplitPane1.setLeftComponent(jPanelFilterExtern);

        downloadListArea.setLayout(new BorderLayout());
        JPanel tempPanel = new JPanel();
        tempPanel.setLayout(new BorderLayout());
        tempPanel.add(downloadListScrollPane, BorderLayout.CENTER);
        tempPanel.add(statusBar, BorderLayout.SOUTH);
        downloadListArea.add(tempPanel, BorderLayout.CENTER);
        downloadListArea.add(descriptionTab, BorderLayout.SOUTH);
        jSplitPane1.setRightComponent(downloadListArea);
        add(jSplitPane1, BorderLayout.CENTER);
        add(swingToolBar, BorderLayout.NORTH);

        createSwingToolBar();

        daten.getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
            @Override
            public void start(ListenerFilmeLadenEvent event) {
                loadFilmlist = true;
                SwingUtilities.invokeLater(() -> refreshDownloadListAction.setEnabled(false));
            }

            @Override
            public void fertig(ListenerFilmeLadenEvent event) {
                loadFilmlist = false;
                SwingUtilities.invokeLater(() -> refreshDownloadListAction.setEnabled(true));
                daten.getListeDownloads().filmEintragen();
                if (Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_ABOS_SOFORT_SUCHEN))) {
                    updateDownloads();
                } else {
                    reloadTable(); // damit die Filmnummern richtig angezeigt werden
                }
            }
        });
    }

    protected void createSwingToolBar() {
        swingToolBar.setFloatable(true);
        swingToolBar.setName("Downloads");

        swingToolBar.add(refreshDownloadListAction);
        swingToolBar.add(startAllDownloadsAction);
        swingToolBar.add(playDownloadAction);
        swingToolBar.add(deferDownloadsAction);
        swingToolBar.add(deleteDownloadsAction);
        swingToolBar.add(cleanupDownloadListAction);
        swingToolBar.addSeparator();
        swingToolBar.add(toggleFilterPanelAction);
    }

    public class ToggleFilterPanelAction extends AbstractAction {
        public ToggleFilterPanelAction() {
            putValue(Action.NAME, "Filter anzeigen/ausblenden");
            putValue(Action.SHORT_DESCRIPTION, "Filter anzeigen/ausblenden");
            putValue(Action.SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/filter.svg"));
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            toggleDownloadFilterPanel();
        }
    }

    public class BeobMausTabelle extends MouseAdapter {
        private DatenDownload datenDownload;
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
                    if (datenDownload.start != null && !datenDownload.isDownloadManager()) {
                        if (datenDownload.start.status == Start.STATUS_FERTIG) {
                            filmAbspielen();
                        } else
                            filmStartenWiederholenStoppen(false, datenDownload.start.status == Start.STATUS_ERR, true, false);
                    } else {
                        // Download starten
                        filmStartenWiederholenStoppen(false, true, true, false);
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
            itemStarten.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/caret-down.svg"));
            itemStarten.setEnabled(!wartenOderLaufen);
            jPopupMenu.add(itemStarten);
            itemStarten.addActionListener(e -> filmStartenWiederholenStoppen(false, true, true, false));

            // Download stoppen
            JMenuItem itemStoppen = new JMenuItem("Download stoppen");
            itemStoppen.setEnabled(wartenOderLaufen);
            jPopupMenu.add(itemStoppen);
            itemStoppen.addActionListener(e -> filmStartenWiederholenStoppen(false, false, true, false));

            jPopupMenu.addSeparator();
            jPopupMenu.add(advanceDownloadsAction);
            jPopupMenu.add(deferDownloadsAction);
            jPopupMenu.add(deleteDownloadsAction);
            jPopupMenu.add(editDownloadAction);

            jPopupMenu.addSeparator();
            jPopupMenu.add(startAllDownloadsAction);
            jPopupMenu.add(stopAllDownloadsAction);

            JMenuItem itemWartendeStoppen = new JMenuItem("wartende Downloads stoppen");
            jPopupMenu.add(itemWartendeStoppen);
            itemWartendeStoppen.addActionListener(e -> stopAllWaitingDownloads());

            jPopupMenu.add(refreshDownloadListAction);
            jPopupMenu.add(cleanupDownloadListAction);
            jPopupMenu.addSeparator();
            jPopupMenu.add(playDownloadAction);
            jPopupMenu.add(deleteDownloadAction);
            jPopupMenu.add(openTargetFolderAction);
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
                        DialogEditAbo dialog = new DialogEditAbo(mediathekGui, datenAbo, false/*onlyOne*/);
                        dialog.setVisible(true);
                        if (dialog.successful()) {
                            daten.getListeAbo().aenderungMelden();
                        }
                    });
                }
            }
            submenueAbo.add(itemDelAbo);
            submenueAbo.add(itemChangeAbo);
            jPopupMenu.add(submenueAbo);

            jPopupMenu.addSeparator();

            JMenuItem itemPlayer = new JMenuItem("Film (URL) abspielen");
            itemPlayer.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/circle-play.svg"));
            itemPlayer.addActionListener(e -> {
                final int nr1 = tabelle.rowAtPoint(p);
                if (nr1 != -1) {
                    final Optional<DatenPset> optPSetPlay = Optional.ofNullable(Daten.listePset.getPsetAbspielen());
                    optPSetPlay.ifPresentOrElse(gruppe -> {
                        Optional<DatenDownload> optDL = Optional.ofNullable((DatenDownload) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(nr1), DatenDownload.DOWNLOAD_REF));
                        optDL.ifPresent(dl -> {
                            if (dl.film != null) {
                                DatenFilm filmClone = new DatenFilm(dl.film);
                                // und jetzt die tatsächlichen URLs des Downloads eintragen
                                filmClone.setNormalQualityUrl(dl.arr[DatenDownload.DOWNLOAD_URL]);
                                filmClone.setLowQualityUrl("");
                                // und starten
                                daten.getStarterClass().urlMitProgrammStarten(gruppe, filmClone, "");
                            }
                        });
                    }, () -> {
                        final String menuPath;
                        if (SystemUtils.IS_OS_MAC_OSX) {
                            menuPath = "MediathekView->Einstellungen…->Aufzeichnen und Abspielen->Set bearbeiten";
                        } else {
                            menuPath = "Datei->Einstellungen->Set bearbeiten";
                        }
                        JOptionPane.showMessageDialog(mediathekGui, "Bitte legen Sie im Menü \"" + menuPath + "\" ein Programm zum Abspielen fest.",
                                "Kein Videoplayer!", JOptionPane.INFORMATION_MESSAGE);
                    });
                }
            });
            jPopupMenu.add(itemPlayer);

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

            jPopupMenu.add(mediathekGui.showFilmInformationAction);

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

            switch ((String) source.getModel().getSelectedItem()) {
                case COMBO_VIEW_ALL -> {
                    onlyNotStarted = false;
                    onlyStarted = false;
                    onlyWaiting = false;
                    onlyFinished = false;
                    onlyRun = false;
                }
                case COMBO_VIEW_NOT_STARTED -> {
                    onlyNotStarted = true;
                    onlyStarted = false;
                    onlyWaiting = false;
                    onlyFinished = false;
                    onlyRun = false;
                }
                case COMBO_VIEW_STARTED -> {
                    onlyNotStarted = false;
                    onlyStarted = true;
                    onlyWaiting = false;
                    onlyFinished = false;
                    onlyRun = false;
                }
                case COMBO_VIEW_WAITING -> {
                    onlyNotStarted = false;
                    onlyStarted = false;
                    onlyWaiting = true;
                    onlyFinished = false;
                    onlyRun = false;
                }
                case COMBO_VIEW_FINISHED_ONLY -> {
                    onlyNotStarted = false;
                    onlyStarted = false;
                    onlyWaiting = false;
                    onlyFinished = true;
                    onlyRun = false;
                }
                case COMBO_VIEW_RUN_ONLY -> {
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
            switch ((String) source.getModel().getSelectedItem()) {
                case COMBO_DISPLAY_ALL -> {
                    onlyAbos = false;
                    onlyDownloads = false;
                }
                case COMBO_DISPLAY_DOWNLOADS_ONLY -> {
                    onlyAbos = false;
                    onlyDownloads = true;
                }
                case COMBO_DISPLAY_ABOS_ONLY -> {
                    onlyAbos = true;
                    onlyDownloads = false;
                }
            }

            reloadTable();
        }
    }
}
