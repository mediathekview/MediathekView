/*
 * Copyright (c) 2026 derreisende77.
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

package mediathek.gui.tabs.tab_downloads;

import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.controller.history.MVUsedUrl;
import mediathek.controller.starter.DirectDownloadPartFiles;
import mediathek.controller.starter.Start;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenFilm;
import mediathek.filmeSuchen.ListenerFilmeLaden;
import mediathek.filmeSuchen.ListenerFilmeLadenEvent;
import mediathek.gui.actions.*;
import mediathek.gui.dialog.DialogBeendenZeit;
import mediathek.gui.dialog.edit_download.DialogEditDownload;
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
import org.apache.commons.configuration2.Configuration;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jspecify.annotations.NonNull;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.KeyEvent;
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
    private static final String ACTION_MAP_KEY_EDIT_DOWNLOAD = "dl_aendern";
    private static final String ACTION_MAP_KEY_DELETE_DOWNLOAD = "dl_delete";
    private static final String ACTION_MAP_KEY_MARK_AS_SEEN = "seen";
    private static final String ACTION_MAP_KEY_MAERK_AS_UNSEEN = "unseen";
    private static final String ACTION_MAP_KEY_START_DOWNLOAD = "dl_start";
    private final static int[] COLUMNS_DISABLED = {DatenDownload.DOWNLOAD_BUTTON_START, DatenDownload.DOWNLOAD_BUTTON_DEL,
            DatenDownload.DOWNLOAD_REF, DatenDownload.DOWNLOAD_URL_RTMP};
    private static final Logger logger = LogManager.getLogger(GuiDownloads.class);
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
    protected final MergeSubtitleWithVideoAction mergeSubtitleWithVideoAction = new MergeSubtitleWithVideoAction(MediathekGui.ui());
    protected final JToolBar swingToolBar = new DownloadsToolBar(
            refreshDownloadListAction,
            startAllDownloadsAction,
            playDownloadAction,
            deferDownloadsAction,
            deleteDownloadsAction,
            cleanupDownloadListAction);
    private final JToolBar configToolBar = new DownloadsConfigToolBar();
    private final DownloadsDisplayFilterToolBar displayFilterToolBar = new DownloadsDisplayFilterToolBar();
    private final JPanel toolBarRow = new DownloadsToolBarRow(swingToolBar, displayFilterToolBar, configToolBar);
    private final AtomicLong _lastUpdate = new AtomicLong(0);
    private final JCheckBoxMenuItem cbShowDownloadDescription = new JCheckBoxMenuItem("Filmbeschreibung anzeigen");
    private final Configuration config = ApplicationConfiguration.getConfiguration();
    private final MarkFilmAsSeenAction markFilmAsSeenAction = new MarkFilmAsSeenAction();
    private final MarkFilmAsUnseenAction markFilmAsUnseenAction = new MarkFilmAsUnseenAction();
    private final DownloadsFilterController filterController =
            new DownloadsFilterController(displayFilterToolBar, config, this::reloadTable);
    private final DownloadStartInfoProperty startInfoProperty = new DownloadStartInfoProperty();
    private final DownloadsStatusBar statusBar = new DownloadsStatusBar(startInfoProperty);
    private final DownloadSizeLookupService downloadSizeLookupService = new DownloadSizeLookupService(this::reloadTable);
    private boolean loadFilmlist;
    /**
     * The internally used model.
     */
    private TModelDownload model;
    private MVDownloadsTable tabelle;
    private DownloadsTableSelection tableSelection;
    private JScrollPane downloadListScrollPane;

    public GuiDownloads(Daten aDaten, MediathekGui mediathekGui) {
        super();
        daten = aDaten;
        this.mediathekGui = mediathekGui;
        descriptionPanel = new FilmDescriptionPanel();


        initComponents();

        setupDownloadListTable();

        setupDescriptionTab(tabelle, cbShowDownloadDescription, ApplicationConfiguration.DOWNLOAD_SHOW_DESCRIPTION, this::getCurrentlySelectedFilm);

        init();

        setupFilmSelectionPropertyListener();
        setupDownloadSizeSelectionUpdater();

        initTable();

        addListenerMediathekView();
        filterController.install();

        if (Taskbar.isTaskbarSupported())
            setupTaskbarMenu();

        tabelle.getTableHeader().setReorderingAllowed(false);
    }

    @Override
    public void tabelleSpeichern() {
        if (tabelle != null) {
            tabelle.writeTableConfigurationData();
        }
    }

    private List<DatenDownload> getSelectedDownloadsFromTable() {
        return tableSelection.selectedDownloadsForLookup();
    }

    private void setupDownloadSizeSelectionUpdater() {
        tabelle.getSelectionModel().addListSelectionListener(l -> {
            if (!l.getValueIsAdjusting()) {
                downloadSizeLookupService.updateFilmSizes(getSelectedDownloadsFromTable());
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
        tableSelection = new DownloadsTableSelection(tabelle, this);
        downloadListScrollPane.setViewportView(tabelle);
    }

    private void initTable() {
        tabelle.readColumnConfigurationData();
        tabelle.setSpalten();
        if (tabelle.getRowCount() > 0) {
            tabelle.setRowSelectionInterval(0, 0);
        }
    }

    private void setupTaskbarMenu() {
        var taskbar = Taskbar.getTaskbar();
        if (taskbar.isSupported(Taskbar.Feature.MENU)) {
            PopupMenu popupMenu = taskbar.getMenu();
            if (popupMenu == null)
                popupMenu = new PopupMenu();

            MenuItem miStartAllDownloads = new MenuItem("Alle Downloads starten");
            miStartAllDownloads.addActionListener(_ -> starten(true));
            MenuItem miStopAllDownloads = new MenuItem("Alle Downloads stoppen");
            miStopAllDownloads.addActionListener(_ -> stoppen(true));
            popupMenu.add(miStartAllDownloads);
            popupMenu.add(miStopAllDownloads);

            taskbar.setMenu(popupMenu);
        }
    }

    @Override
    public void installMenuEntries(JMenu menu) {
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
        menu.add(markFilmAsSeenAction);
        menu.add(markFilmAsUnseenAction);
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
        tabelle.addMouseListener(new DownloadsTableMouseHandler(this, tabelle, daten, mediathekGui));
        tabelle.getSelectionModel().addListSelectionListener(event -> {
            if (!event.getValueIsAdjusting()) {
                updateFilmData();
            }
        });

        tabelle.setLineBreak(MVConfig.getBool(MVConfig.Configs.SYSTEM_TAB_DOWNLOAD_LINEBREAK));
        tabelle.getTableHeader().addMouseListener(new BeobTableHeader(tabelle,
                DatenDownload.getColumnVisibilityStore(),
                COLUMNS_DISABLED,
                new int[]{DatenDownload.DOWNLOAD_BUTTON_START, DatenDownload.DOWNLOAD_BUTTON_DEL},
                true, MVConfig.Configs.SYSTEM_TAB_DOWNLOAD_LINEBREAK));
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
        cbShowDownloadDescription.addActionListener(_ -> {
            boolean visible = cbShowDownloadDescription.isSelected();
            makeDescriptionTabVisible(visible);
            config.setProperty(ApplicationConfiguration.DOWNLOAD_SHOW_DESCRIPTION, visible);
        });
    }

    private synchronized void reloadTable() {
        // nur Downloads die schon in der Liste sind werden geladen
        tabelle.getSpalten();

        var displayFilter = filterController.getDisplayFilter();
        var viewFilter = filterController.getViewFilter();
        daten.getListeDownloads().getModel(model,
                displayFilter.onlyAbos(),
                displayFilter.onlyDownloads(),
                viewFilter.onlyNotStarted(),
                viewFilter.onlyStarted(),
                viewFilter.onlyWaiting(),
                viewFilter.onlyRun(),
                viewFilter.onlyFinished());
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

    synchronized void downloadsAufraeumen(DatenDownload datenDownload) {
        // abgeschlossene Downloads werden aus der Tabelle/Liste entfernt
        // die Starts dafür werden auch gelöscht
        daten.getListeDownloads().listePutzen(datenDownload);
    }

    private ArrayList<DatenDownload> getSelDownloads() {
        return tableSelection.selectedDownloadsOrShowError();
    }

    @Override
    public Optional<DatenFilm> getCurrentlySelectedFilm() {
        return tableSelection.currentlySelectedFilm();
    }

    private DatenDownload getSelDownload() {
        return tableSelection.selectedDownloadOrShowError();
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
        DialogEditDownload dialog = new DialogEditDownload(mediathekGui, datenDownloadKopy, gestartet);
        dialog.setVisible(true);
        if (dialog.isConfirmed()) {
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
                JOptionPane.showMessageDialog(mediathekGui, "Download erst stoppen!", "Film löschen", JOptionPane.ERROR_MESSAGE);
                return;
            }
        }
        try {
            File file = getExistingDownloadFile(datenDownload);
            if (!file.exists()) {
                JOptionPane.showMessageDialog(mediathekGui, "Die Datei existiert nicht!", "Film löschen", JOptionPane.ERROR_MESSAGE);
                return;
            }
            int ret = JOptionPane.showConfirmDialog(mediathekGui,
                    file.getAbsolutePath(), "Film Löschen?", JOptionPane.YES_NO_OPTION);
            if (ret == JOptionPane.OK_OPTION) {

                // und jetzt die Datei löschen
                logger.info(new String[]{"Datei löschen: ", file.getAbsolutePath()});
                if (!file.delete()) {
                    throw new Exception();
                }
            }
        }
        catch (Exception ex) {
            JOptionPane.showMessageDialog(mediathekGui, "Konnte die Datei nicht löschen!", "Film löschen", JOptionPane.ERROR_MESSAGE);
            logger.error("Fehler beim löschen: {}", datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
        }
    }

    private File getExistingDownloadFile(DatenDownload datenDownload) {
        File finalFile = new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
        if (finalFile.exists()) {
            return finalFile;
        }

        File partFile = DirectDownloadPartFiles.INSTANCE.partFileFor(finalFile);
        if (partFile.exists()) {
            return partFile;
        }

        return finalFile;
    }

    /**
     * @param permanentDeletion false werden Downloads zurück gestellt. true löscht permanent.
     */
    public void downloadLoeschen(boolean permanentDeletion) {
        try {
            int rowToSelectAfterDeletion = tabelle.getSelectedRow();
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
                }
                else {
                    // wenn nicht dauerhaft
                    datenDownload.zurueckstellen();
                }
            }

            if (!urlAboList.isEmpty()) {
                daten.getAboHistoryController().add(urlAboList);
            }

            daten.getListeDownloads().downloadLoeschen(arrayDownloadsLoeschen);
            reloadTable();
            selectSingleRowAfterDeletion(rowToSelectAfterDeletion);
        }
        catch (Exception ex) {
            logger.error("downloadLoeschen()", ex);
        }
    }

    private void selectSingleRowAfterDeletion(int rowToSelect) {
        int rowCount = tabelle.getRowCount();
        if (rowCount == 0) {
            tabelle.clearSelection();
            return;
        }

        int validRow = Math.max(0, Math.min(rowToSelect, rowCount - 1));
        tabelle.setRowSelectionInterval(validRow, validRow);
    }

    private @NonNull List<DatenDownload> addAllDownloadsToList() {
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
            mediathekGui.quitApplication(dialogBeenden.isShutdownRequested());
        }

        reloadTable();
    }

    void filmStartenWiederholenStoppen(boolean processAllDownloads, boolean starten /* starten/wiederstarten oder stoppen */,
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
                            }
                            else {
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
            }
            else if (download.start != null) {
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
            listeDownloadsStarten.removeIf(item -> !item.isFromAbo() || item.isAutomaticStartBlockedByAbo());

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
        return tableSelection.selectedFilmsOrShowError();
    }

    private void initComponents() {
        var downloadListArea = new JPanel();
        downloadListScrollPane = new JScrollPane();

        setLayout(new BorderLayout());

        downloadListArea.setLayout(new BorderLayout());
        JPanel tempPanel = new JPanel();
        tempPanel.setLayout(new BorderLayout());
        tempPanel.add(downloadListScrollPane, BorderLayout.CENTER);
        tempPanel.add(statusBar, BorderLayout.SOUTH);
        downloadListArea.add(tempPanel, BorderLayout.CENTER);
        downloadListArea.add(descriptionTab, BorderLayout.SOUTH);

        add(downloadListArea, BorderLayout.CENTER);
        add(toolBarRow, BorderLayout.NORTH);

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
                }
                else {
                    reloadTable(); // damit die Filmnummern richtig angezeigt werden
                }
            }
        });
    }

}
