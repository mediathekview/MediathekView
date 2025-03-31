package mediathek.gui.tabs.tab_film;

import ca.odell.glazedlists.BasicEventList;
import ca.odell.glazedlists.EventList;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.formdev.flatlaf.FlatClientProperties;
import com.formdev.flatlaf.extras.FlatSVGIcon;
import com.formdev.flatlaf.icons.FlatSearchWithHistoryIcon;
import com.google.common.util.concurrent.FutureCallback;
import com.google.common.util.concurrent.Futures;
import com.google.common.util.concurrent.ListenableFuture;
import javafx.application.Platform;
import javafx.beans.value.ChangeListener;
import mediathek.config.*;
import mediathek.controller.history.SeenHistoryController;
import mediathek.controller.starter.Start;
import mediathek.daten.*;
import mediathek.daten.abo.DatenAbo;
import mediathek.daten.blacklist.BlacklistRule;
import mediathek.filmeSuchen.ListenerFilmeLaden;
import mediathek.filmeSuchen.ListenerFilmeLadenEvent;
import mediathek.filmlisten.writer.FilmListWriter;
import mediathek.gui.FilterSelectionComboBoxModel;
import mediathek.gui.actions.ManageBookmarkAction;
import mediathek.gui.actions.PlayFilmAction;
import mediathek.gui.actions.UrlHyperlinkAction;
import mediathek.gui.dialog.DialogAboNoSet;
import mediathek.gui.dialog.DialogAddDownload;
import mediathek.gui.dialog.DialogAddMoreDownload;
import mediathek.gui.duplicates.details.DuplicateFilmDetailsDialog;
import mediathek.gui.messages.*;
import mediathek.gui.messages.history.DownloadHistoryChangedEvent;
import mediathek.gui.tabs.AGuiTabPanel;
import mediathek.gui.tabs.tab_film.helpers.GuiFilmeModelHelper;
import mediathek.gui.tabs.tab_film.helpers.GuiModelHelper;
import mediathek.gui.tabs.tab_film.helpers.LuceneGuiFilmeModelHelper;
import mediathek.javafx.bookmark.BookmarkWindowController;
import mediathek.javafx.filterpanel.FilterActionPanel;
import mediathek.javafx.filterpanel.SearchControlFieldMode;
import mediathek.javafx.tool.JavaFxUtils;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.*;
import mediathek.tool.cellrenderer.CellRendererFilme;
import mediathek.tool.datum.DatumFilm;
import mediathek.tool.listener.BeobTableHeader;
import mediathek.tool.models.TModelFilm;
import mediathek.tool.table.MVFilmTable;
import net.engio.mbassy.listener.Handler;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jdesktop.swingx.VerticalLayout;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.table.TableModel;
import javax.swing.text.JTextComponent;
import java.awt.*;
import java.awt.event.*;
import java.awt.print.PrinterException;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.IntConsumer;

public class GuiFilme extends AGuiTabPanel {

    public static final String NAME = "Filme";
    private static final String ACTION_MAP_KEY_PLAY_FILM = "film_abspielen";
    private static final String ACTION_MAP_KEY_SAVE_FILM = "download_film";
    private static final String ACTION_MAP_KEY_BOOKMARK_FILM = "bookmark_film";
    private static final String ACTION_MAP_KEY_COPY_NORMAL_URL = "copy_url";
    private static final String ACTION_MAP_KEY_COPY_HD_URL = "copy_url_hd";
    private static final String ACTION_MAP_KEY_COPY_KLEIN_URL = "copy_url_klein";
    private static final String ACTION_MAP_KEY_MARK_SEEN = "seen";
    private static final String ACTION_MAP_KEY_MARK_UNSEEN = "unseen";
    private static final int[] HIDDEN_COLUMNS = {DatenFilm.FILM_ABSPIELEN, DatenFilm.FILM_AUFZEICHNEN,
            DatenFilm.FILM_MERKEN};
    private static final Logger logger = LogManager.getLogger();
    private static final int[] BUTTON_COLUMNS = {DatenFilm.FILM_ABSPIELEN, DatenFilm.FILM_AUFZEICHNEN,
            DatenFilm.FILM_MERKEN};
    public static boolean[] VISIBLE_COLUMNS = new boolean[DatenFilm.MAX_ELEM];
    public final PlayFilmAction playFilmAction = new PlayFilmAction(this);
    public final SaveFilmAction saveFilmAction = new SaveFilmAction();
    public final CopyUrlToClipboardAction copyHqUrlToClipboardAction = new CopyUrlToClipboardAction(FilmResolution.Enum.HIGH_QUALITY);
    public final CopyUrlToClipboardAction copyNormalUrlToClipboardAction = new CopyUrlToClipboardAction(FilmResolution.Enum.NORMAL);
    protected final JTabbedPane psetButtonsTab = new JTabbedPane();
    private final BookmarkAddFilmAction bookmarkAddFilmAction = new BookmarkAddFilmAction();
    private final BookmarkRemoveFilmAction bookmarkRemoveFilmAction = new BookmarkRemoveFilmAction();
    private final BookmarkClearListAction bookmarkClearListAction = new BookmarkClearListAction();
    private final ManageBookmarkAction manageBookmarkAction = new ManageBookmarkAction(MediathekGui.ui());
    private final MarkFilmAsSeenAction markFilmAsSeenAction = new MarkFilmAsSeenAction();
    private final MarkFilmAsUnseenAction markFilmAsUnseenAction = new MarkFilmAsUnseenAction();
    private final JScrollPane filmListScrollPane = new JScrollPane();
    private final JCheckBoxMenuItem cbkShowDescription = new JCheckBoxMenuItem("Beschreibung anzeigen");
    private final SeenHistoryController historyController = new SeenHistoryController();
    private final JCheckBoxMenuItem cbShowButtons = new JCheckBoxMenuItem("Buttons anzeigen");
    private final NonRepeatingTimer zeitraumTimer;
    private final NonRepeatingTimer reloadTableDataTimer;
    /**
     * The JavaFx Film action popup panel.
     */
    private final FilterActionPanel filterActionPanel;
    private final FilterConfiguration filterConfiguration = new FilterConfiguration();
    public ToggleFilterDialogVisibilityAction toggleFilterDialogVisibilityAction = new ToggleFilterDialogVisibilityAction();
    protected SearchField searchField;
    protected JComboBox<FilterDTO> filterSelectionComboBox = new JComboBox<>(new FilterSelectionComboBoxModel());
    protected FilterVisibilityToggleButton btnToggleFilterDialogVisibility = new FilterVisibilityToggleButton(toggleFilterDialogVisibilityAction);
    protected PsetButtonsPanel psetButtonsPanel;
    private Optional<BookmarkWindowController> bookmarkWindowController = Optional.empty();
    private boolean stopBeob;
    private MVFilmTable tabelle;
    /**
     * We perform model filtering in the background the keep UI thread alive.
     */
    private ListenableFuture<TableModel> modelFuture;
    public GuiFilme(Daten aDaten, MediathekGui mediathekGui) {
        daten = aDaten;
        this.mediathekGui = mediathekGui;
        descriptionPanel = new FilmDescriptionPanel(this);

        setLayout(new BorderLayout());
        add(filmListScrollPane, BorderLayout.CENTER);
        var extensionArea = new JPanel(new VerticalLayout());
        add(extensionArea, BorderLayout.SOUTH);

        if (daten.getListeFilmeNachBlackList() instanceof IndexedFilmList)
            searchField = new LuceneSearchField();
        else
            searchField = new RegularSearchField();

        // add film description panel
        extensionArea.add(descriptionTab);
        extensionArea.add(psetButtonsTab);

        setupFilmListTable();
        setupFilmSelectionPropertyListener();
        setupDescriptionTab(tabelle, cbkShowDescription, ApplicationConfiguration.FILM_SHOW_DESCRIPTION);
        setupPsetButtonsTab();

        filterActionPanel = new FilterActionPanel(btnToggleFilterDialogVisibility, filterConfiguration);
        add(new FilmToolBar(), BorderLayout.NORTH);

        start_init();

        zeitraumTimer = new NonRepeatingTimer(e -> {
            // reset sender filter first
            JavaFxUtils.invokeInFxThreadAndWait(() -> filterActionPanel.getViewSettingsPane().senderCheckList.getCheckModel().clearChecks());
            MessageBus.getMessageBus().publish(new FilterZeitraumEvent());
        });

        reloadTableDataTimer = new NonRepeatingTimer(e -> loadTable());

        // register message bus handler
        MessageBus.getMessageBus().subscribe(this);

        setupActionListeners();
    }

    public FilterConfiguration getFilterConfiguration() {
        return filterConfiguration;
    }

    public FilterActionPanel getFilterActionPanel() {
        return filterActionPanel;
    }

    /**
     * Convenience function to update bookmark list and refresh UI.
     * @param filmList the data list
     */
    private void updateBookmarkListAndRefresh(List<DatenFilm> filmList)
    {
        var bookmarkList = Daten.getInstance().getListeBookmarkList();
        bookmarkList.checkAndBookmarkMovies(filmList);
        bookmarkList.saveToFile();
        repaint();
    }

    @Handler
    public void handleTableModelChange(TableModelChangeEvent e) {
        final Consumer<Boolean> function = (Boolean flag) -> {
            playFilmAction.setEnabled(flag);
            saveFilmAction.setEnabled(flag);
            bookmarkAddFilmAction.setEnabled(flag);
            bookmarkRemoveFilmAction.setEnabled(flag);
            bookmarkClearListAction.setEnabled(flag);
            manageBookmarkAction.setEnabled(flag);
            toggleFilterDialogVisibilityAction.setEnabled(flag);
            searchField.setEnabled(flag);
            filterSelectionComboBox.setEnabled(flag);
        };
        if (e.active) {
            SwingUtilities.invokeLater(() -> function.accept(false));
        } else {
            SwingUtilities.invokeLater(() -> {
                function.accept(true);
                if (e.fromSearchField)
                    searchField.requestFocusInWindow();
            });
        }
    }

    @Override
    public void tabelleSpeichern() {
        if (tabelle != null) {
            tabelle.writeTableConfigurationData();
        }
    }

    private void setupFilmListTable() {
        tabelle = new MVFilmTable();
        filmListScrollPane.setViewportView(tabelle);
    }

    /**
     * Update the property with the current number of selected entries from the JTable.
     */
    private void setupFilmSelectionPropertyListener() {
        tabelle.getSelectionModel().addListSelectionListener(e -> {
            if (!e.getValueIsAdjusting()) {
                updateSelectedListItemsCount(tabelle);
                final int sel = tabelle.getSelectedRowCount();
                playFilmAction.setEnabled(sel <= 1);
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

    public void installViewMenuEntry(JMenu jMenuAnsicht) {
        jMenuAnsicht.add(cbShowButtons, 0);
    }

    /**
     * Show description panel based on settings.
     */
    protected void makeButtonsTabVisible(boolean visible) {
        if (visible) {
            if (psetButtonsTab.indexOfComponent(psetButtonsPanel) == -1) {
                psetButtonsTab.add(psetButtonsPanel, 0);
                psetButtonsTab.setTitleAt(0, "Buttons");
            }
        } else {
            if (psetButtonsTab.indexOfComponent(psetButtonsPanel) != -1) {
                psetButtonsTab.remove(psetButtonsPanel);
            }
        }
    }

    @Override
    public void installMenuEntries(JMenu menu) {
        JMenuItem miMarkFilmAsSeen = new JMenuItem("Filme als gesehen markieren");
        miMarkFilmAsSeen.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_G, KeyEvent.CTRL_DOWN_MASK));
        miMarkFilmAsSeen.addActionListener(markFilmAsSeenAction);

        JMenuItem miMarkFilmAsUnseen = new JMenuItem("Filme als ungesehen markieren");
        miMarkFilmAsUnseen.setAccelerator(
                KeyStroke.getKeyStroke(KeyEvent.VK_N, KeyEvent.CTRL_DOWN_MASK));
        miMarkFilmAsUnseen.addActionListener(markFilmAsUnseenAction);

        menu.add(playFilmAction);
        menu.add(saveFilmAction);
        menu.add(bookmarkAddFilmAction);
        menu.addSeparator();
        menu.add(miMarkFilmAsSeen);
        menu.add(miMarkFilmAsUnseen);
        menu.addSeparator();
        menu.add(mediathekGui.toggleBlacklistAction);
        menu.add(mediathekGui.editBlacklistAction);
        menu.addSeparator();
        menu.add(cbkShowDescription);
    }

    private void setupPsetButtonsTab() {
        var initialVisibility = ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.APPLICATION_BUTTONS_PANEL_VISIBLE, false);
        setupButtonsMenuItem(initialVisibility);

        psetButtonsPanel = new PsetButtonsPanel(this);
        psetButtonsPanel.putClientProperty("JTabbedPane.tabClosable", true);
        psetButtonsPanel.putClientProperty("JTabbedPane.tabCloseCallback", (IntConsumer) c -> cbShowButtons.doClick());
        psetButtonsPanel.install(psetButtonsTab);

        makeButtonsTabVisible(initialVisibility);
    }

    private void setupButtonsMenuItem(boolean initialVisibility) {
        var config = ApplicationConfiguration.getConfiguration();

        if (!SystemUtils.IS_OS_MAC_OSX)
            cbShowButtons.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F11, 0));
        cbShowButtons.setSelected(initialVisibility);
        cbShowButtons.addActionListener(e -> {
            boolean visible = cbShowButtons.isSelected();
            makeButtonsTabVisible(visible);
            config.setProperty(ApplicationConfiguration.APPLICATION_BUTTONS_PANEL_VISIBLE, visible);
        });
    }

    private void onComponentShown() {
        updateFilmData();
        updateStartInfoProperty();
    }

    public int getTableRowCount() {
        if (tabelle != null) {
            return tabelle.getModel().getRowCount();
        } else {
            return 0;
        }
    }

    private void setupKeyMapping() {
        final InputMap focusedWindowMap = tabelle.getInputMap();

        focusedWindowMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_P, 0), ACTION_MAP_KEY_PLAY_FILM);
        focusedWindowMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0), ACTION_MAP_KEY_PLAY_FILM);
        focusedWindowMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_D, 0), ACTION_MAP_KEY_SAVE_FILM);
        focusedWindowMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_B, 0), ACTION_MAP_KEY_BOOKMARK_FILM);
        focusedWindowMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_H, 0), ACTION_MAP_KEY_COPY_HD_URL);
        focusedWindowMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_N, 0), ACTION_MAP_KEY_COPY_NORMAL_URL);
        focusedWindowMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_K, 0), ACTION_MAP_KEY_COPY_KLEIN_URL);
        focusedWindowMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_G, 0), ACTION_MAP_KEY_MARK_SEEN);
        focusedWindowMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_U, 0), ACTION_MAP_KEY_MARK_UNSEEN);

        final ActionMap actionMap = tabelle.getActionMap();
        actionMap.put(ACTION_MAP_KEY_PLAY_FILM, playFilmAction);
        actionMap.put(ACTION_MAP_KEY_SAVE_FILM, saveFilmAction);
        actionMap.put(ACTION_MAP_KEY_BOOKMARK_FILM, bookmarkAddFilmAction);
        actionMap.put(ACTION_MAP_KEY_COPY_NORMAL_URL, copyNormalUrlToClipboardAction);
        actionMap.put(ACTION_MAP_KEY_COPY_HD_URL, copyHqUrlToClipboardAction);
        actionMap.put(ACTION_MAP_KEY_COPY_KLEIN_URL, new CopyUrlToClipboardAction(FilmResolution.Enum.LOW));
        actionMap.put(ACTION_MAP_KEY_MARK_SEEN, markFilmAsSeenAction);
        actionMap.put(ACTION_MAP_KEY_MARK_UNSEEN, markFilmAsUnseenAction);
    }

    private void setupCellRenderer() {
        CellRendererFilme cellRenderer = new CellRendererFilme();
        tabelle.setDefaultRenderer(Object.class, cellRenderer);
        tabelle.setDefaultRenderer(DatumFilm.class, cellRenderer);
        tabelle.setDefaultRenderer(Integer.class, cellRenderer);
    }

    private void start_init() {
        daten.getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
            @Override
            public void fertig(ListenerFilmeLadenEvent event) {
                Platform.runLater(filterActionPanel::updateThemaComboBox);
            }
        });

        setupKeyMapping();

        tabelle.setModel(new TModelFilm());
        tabelle.addMouseListener(new TableContextMenuHandler());
        tabelle.getSelectionModel().addListSelectionListener(event -> {
            final ListSelectionModel m = (ListSelectionModel) event.getSource();
            if (!m.isSelectionEmpty() && !m.getValueIsAdjusting() && !stopBeob) {
                updateFilmData();
            }
        });

        setupCellRenderer();

        tabelle.setLineBreak(MVConfig.getBool(MVConfig.Configs.SYSTEM_TAB_FILME_LINEBREAK));

        setupHeaderPopupMenu();

        tabelle.readColumnConfigurationData();
        if (tabelle.getRowCount() > 0) {
            tabelle.setRowSelectionInterval(0, 0);
        }
    }

    private void setupHeaderPopupMenu() {
        final var headerListener =
                new BeobTableHeader(
                        tabelle,
                        VISIBLE_COLUMNS,
                        HIDDEN_COLUMNS,
                        BUTTON_COLUMNS,
                        true,
                        MVConfig.Configs.SYSTEM_TAB_FILME_LINEBREAK);

        tabelle.getTableHeader().addMouseListener(headerListener);
    }

    @Handler
    private void handleDownloadHistoryChangedEvent(DownloadHistoryChangedEvent e) {
        SwingUtilities.invokeLater(() -> {
            if (filterConfiguration.isShowUnseenOnly()) {
                MessageBus.getMessageBus().publish(new ReloadTableDataEvent());
            } else {
                tabelle.fireTableDataChanged(true);
            }
        });
    }

    @Handler
    private void handleButtonStart(ButtonStartEvent e) {
        SwingUtilities.invokeLater(() -> {
            tabelle.fireTableDataChanged(true);
            updateStartInfoProperty();
        });
    }

    @Handler
    private void handleStartEvent(StartEvent msg) {
        SwingUtilities.invokeLater(this::updateStartInfoProperty);
    }

    private synchronized void saveFilm(@Nullable DatenPset pSet) {
        if (Daten.listePset.getListeSpeichern().isEmpty()) {
            new DialogAboNoSet(mediathekGui).setVisible(true);
            // Satz mit x, war wohl nix
            return;
        }

        if (pSet == null) {
            pSet = Daten.listePset.getListeSpeichern().getFirst();
        }

        List<DatenFilm> liste = getSelFilme();
        boolean addAllWithDefaults = false;
        String pfad = "";
        boolean info = false;
        boolean subtitle = false;

        if (liste.size() > 1) {
            DialogAddMoreDownload damd = new DialogAddMoreDownload(mediathekGui, pSet);
            var result = damd.showDialog();
            if (damd.wasCancelled()) {
                return;
            }
            else {
                addAllWithDefaults = result.addAll();
                pfad = result.path();
                info = result.info();
                subtitle = result.subtitle();
            }
        }

        for (var film : liste) {
            // erst mal schauen obs den schon gibt
            if (daten.getListeDownloads().getDownloadUrlFilm(film.getUrlNormalQuality()) != null) {
                int ret = JOptionPane.showConfirmDialog(mediathekGui,
                        "Download für den Film existiert bereits.\n" + "Nochmal anlegen?",
                        "Anlegen?",
                        JOptionPane.YES_NO_OPTION);
                if (ret != JOptionPane.YES_OPTION) {
                    continue;
                }
            }

            if (addAllWithDefaults) {
                var datenDownload = new DatenDownload(pSet, film, DatenDownload.QUELLE_DOWNLOAD, null, "",
                        pfad, "", info, subtitle);
                registerDownload(datenDownload);
            } else {
                saveFilm(film, pSet);
            }
        }
    }

    /**
     * Register a download object in queue.
     * @param datenDownload the object that should be downloaded.
     */
    private void registerDownload(@NotNull DatenDownload datenDownload) {
        Daten.getInstance().getListeDownloads().addMitNummer(datenDownload);
        MessageBus.getMessageBus().publishAsync(new DownloadListChangedEvent());
        if (Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_DIALOG_DOWNLOAD_D_STARTEN))) {
            // und evtl. auch gleich starten
            datenDownload.startDownload();
        }
    }

    /**
     * Download a single film via add download dialog.
     * @param datenFilm film of interest
     * @param pSet the program set, can be null.
     */
    private void saveFilm(@NotNull DatenFilm datenFilm, @NotNull DatenPset pSet) {
        if (Daten.listePset.getListeSpeichern().isEmpty()) {
            //TODO should be impossible to reach this code block as psets are checked before...investigate.
            MVMessageDialog.showMessageDialog(this,
                    "Ohne Programm-Sets können keine Downloads gestartet werden.",
                    Konstanten.PROGRAMMNAME, JOptionPane.ERROR_MESSAGE);
        } else {
            // dann alle Downloads im Dialog abfragen
            Optional<FilmResolution.Enum> res =
                    filterConfiguration.isShowHighQualityOnly() ? Optional.of(FilmResolution.Enum.HIGH_QUALITY) : Optional.empty();
            DialogAddDownload dialog = new DialogAddDownload(mediathekGui, datenFilm, pSet, res);
            dialog.setVisible(true);
        }
    }

    /**
     * If necessary instantiate and show the bookmark window
     */
    public void showManageBookmarkWindow() {
        Platform.runLater(() -> {
            if (bookmarkWindowController.isEmpty()) {
                bookmarkWindowController = Optional.of(new BookmarkWindowController());
                bookmarkWindowController.get().setPartner(this);
            }
            bookmarkWindowController.get().show();
        });
    }

    public void playerStarten(DatenPset pSet) {
        // Url mit Prognr. starten
        if (tabelle.getSelectedRow() == -1) {
            NoSelectionErrorDialog.show(this);
        } else if (pSet.istSpeichern()) {
            // wenn das pSet zum Speichern (über die Button) gewählt wurde,
            // weiter mit dem Dialog "Speichern"
            saveFilm(pSet);
        } else {
            // mit dem flvstreamer immer nur einen Filme starten
            final String aufloesung;
            if (filterConfiguration.isShowHighQualityOnly()) {
                aufloesung = FilmResolution.Enum.HIGH_QUALITY.toString();
            } else aufloesung = "";

            Optional<DatenFilm> filmSelection = getCurrentlySelectedFilm();
            filmSelection.ifPresent(
                    film -> daten.getStarterClass().urlMitProgrammStarten(pSet, film, aufloesung));
        }
    }

    /**
     * Cleanup during shutdown
     */
    public void saveSettings() {
        bookmarkWindowController.ifPresent(BookmarkWindowController::saveSettings);
    }

    /**
     * Return the film object from a table row. As this can also be null we will return an Optional to
     * prevent NPEs inside the caller.
     *
     * @param zeileTabelle table row.
     * @return Optional object to a film object.
     */
    private Optional<DatenFilm> getFilm(final int zeileTabelle) {
        if (zeileTabelle >= 0 && zeileTabelle < tabelle.getRowCount()) {
            return Optional.of((DatenFilm) tabelle.getModel()
                    .getValueAt(tabelle.convertRowIndexToModel(zeileTabelle), DatenFilm.FILM_REF));
        } else {
            return Optional.empty();
        }
    }

    @Override
    public Optional<DatenFilm> getCurrentlySelectedFilm() {
        final int selectedTableRow = tabelle.getSelectedRow();
        if (selectedTableRow != -1) {
            try {
                final int modelIndex = tabelle.convertRowIndexToModel(selectedTableRow);
                return Optional.of((DatenFilm) tabelle.getModel().getValueAt(modelIndex, DatenFilm.FILM_REF));
            }
            catch (Exception e) {
                return Optional.empty();
            }
        } else {
            return Optional.empty();
        }
    }

    @Override
    protected List<DatenFilm> getSelFilme() {
        ArrayList<DatenFilm> arrayFilme = new ArrayList<>();
        int[] rows = tabelle.getSelectedRows();
        if (rows.length > 0) {
            for (int row : rows) {
                DatenFilm datenFilm = (DatenFilm) tabelle.getModel()
                        .getValueAt(tabelle.convertRowIndexToModel(row), DatenFilm.FILM_REF);
                arrayFilme.add(datenFilm);
            }
        } else {
            NoSelectionErrorDialog.show(this);
        }
        return arrayFilme;
    }

    /**
     * Update Film Information and description panel with updated film...
     */
    private void updateFilmData() {
        var infoDialog = mediathekGui.getFilmInfoDialog();
        if (infoDialog != null) {
            final Optional<DatenFilm> filmSelection = getCurrentlySelectedFilm();
            filmSelection.ifPresent(infoDialog::updateCurrentFilm);
        }
    }

    /**
     * Update table data when receiving ReloadTableDataEvent or subclasses of it.
     * @param e event
     */
    @Handler
    private void handleReloadTableDataEvent(ReloadTableDataEvent e) {
        SwingUtilities.invokeLater(() -> {
            if (!reloadTableDataTimer.isRunning())
                reloadTableDataTimer.start();
            else
                reloadTableDataTimer.restart();
        });
    }

    @Handler
    private void handleFilterZeitraumEvent(FilterZeitraumEvent e) {
        SwingUtilities.invokeLater(() -> {
            daten.getListeBlacklist().filterListe();
            MessageBus.getMessageBus().publish(new ReloadTableDataEvent());
        });
    }

    private void setupActionListeners() {
        Platform.runLater(() -> {
            final ChangeListener<Boolean> reloadTableListener = (ov, oV, nV) -> MessageBus.getMessageBus().publish(new ReloadTableDataEvent());

            filterActionPanel.showOnlyHighQualityProperty().addListener(reloadTableListener);
            filterActionPanel.showSubtitlesOnlyProperty().addListener(reloadTableListener);
            filterActionPanel.showNewOnlyProperty().addListener(reloadTableListener);
            filterActionPanel.showBookMarkedOnlyProperty().addListener(reloadTableListener);
            filterActionPanel.showUnseenOnlyProperty().addListener(reloadTableListener);
            filterActionPanel.dontShowAbosProperty().addListener(reloadTableListener);
            filterActionPanel.dontShowTrailersProperty().addListener(reloadTableListener);
            filterActionPanel.dontShowSignLanguageProperty().addListener(reloadTableListener);
            filterActionPanel.dontShowAudioVersionsProperty().addListener(reloadTableListener);
            filterActionPanel.dontShowDuplicatesProperty().addListener(reloadTableListener);
            filterActionPanel.showLivestreamsOnlyProperty().addListener(reloadTableListener);

            filterActionPanel.addFilmLengthSliderListeners((v1, v2, newValue) -> {
                if (!newValue) {
                    MessageBus.getMessageBus().publish(new ReloadTableDataEvent());
                }
            });

            filterActionPanel.getViewSettingsPane().zeitraumSpinner.valueProperty()
                    .addListener((ov, oV, nV) -> SwingUtilities.invokeLater(() -> {
                        if (!zeitraumTimer.isRunning())
                            zeitraumTimer.start();
                        else
                            zeitraumTimer.restart();
                    }));

            filterActionPanel.getViewSettingsPane().themaComboBox.setOnAction(e -> {
                if (!filterActionPanel.getViewSettingsPane().themaComboBox.getItems().isEmpty()) {
                    MessageBus.getMessageBus().publish(new ReloadTableDataEvent());
                }
            });
        });
    }

    @Override
    protected void setupShowFilmDescriptionMenuItem() {
        var config = ApplicationConfiguration.getConfiguration();

        cbkShowDescription.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F10, 0));
        cbkShowDescription.setSelected(config.getBoolean(ApplicationConfiguration.FILM_SHOW_DESCRIPTION, true));
        cbkShowDescription.addActionListener(e -> {
            boolean visible = cbkShowDescription.isSelected();
            makeDescriptionTabVisible(visible);
            config.setProperty(ApplicationConfiguration.FILM_SHOW_DESCRIPTION, visible);
        });
    }

    private void loadTable() {
        loadTable(false);
    }

    private void loadTable(boolean from_search_field) {
        if (modelFuture != null) {
            if (!modelFuture.isDone()) {
                return;
            }
        }

        final var messageBus = MessageBus.getMessageBus();
        messageBus.publish(new TableModelChangeEvent(true, from_search_field));

        stopBeob = true;
        tabelle.getSpalten();
        tabelle.setEnabled(false);

        var decoratedPool = daten.getDecoratedPool();
        modelFuture = decoratedPool.submit(() -> {
            var searchFieldData = new SearchFieldData(searchField.getText(), searchField.getSearchMode());
            GuiModelHelper helper = GuiModelHelperFactory.createGuiModelHelper(
                    historyController, searchFieldData, filterConfiguration);
            return helper.getFilteredTableModel();
        });
        Futures.addCallback(modelFuture,
                new FutureCallback<>() {
                    public void onSuccess(TableModel model) {
                        SwingUtilities.invokeLater(() -> {
                            tabelle.setModel(model);
                            tabelle.setEnabled(true);
                            updateStartInfoProperty();
                            tabelle.setSpalten();
                            updateFilmData();
                            stopBeob = false;
                            tabelle.scrollToSelection();
                            messageBus.publish(new TableModelChangeEvent(false, from_search_field));
                        });
                    }

                    public void onFailure(@NotNull Throwable thrown) {
                        logger.error("Model filtering failed!", thrown);
                        SwingUtilities.invokeLater(() -> {
                            tabelle.setEnabled(true);
                            updateStartInfoProperty();
                            tabelle.setSpalten();
                            updateFilmData();
                            stopBeob = false;
                            messageBus.publish(new TableModelChangeEvent(false, from_search_field));
                        });
                    }
                },
                decoratedPool);
    }

    static class GuiModelHelperFactory {
        public static GuiModelHelper createGuiModelHelper(@NotNull SeenHistoryController historyController,
                                                          @NotNull SearchFieldData searchFieldData,
                                                          @NotNull FilterConfiguration filterConfig) {
            GuiModelHelper helper;
            if (Daten.getInstance().getListeFilmeNachBlackList() instanceof IndexedFilmList) {
                helper = new LuceneGuiFilmeModelHelper(historyController, searchFieldData, filterConfig);
            } else {
                helper = new GuiFilmeModelHelper(historyController, searchFieldData, filterConfig);
            }
            return helper;
        }
    }

    static class NonRepeatingTimer extends Timer {
        public NonRepeatingTimer(ActionListener listener) {
            super(250, listener);

            setRepeats(false);
            setCoalesce(true);
        }
    }

    private static class FilterZeitraumEvent extends BaseEvent {}

    static public class FilterVisibilityToggleButton extends JToggleButton {
        public FilterVisibilityToggleButton(Action a) {
            super(a);
            setText("");
            final boolean visible = ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.FilterDialog.VISIBLE, false);
            setSelected(visible);
        }
    }

    private class FilmToolBar extends JToolBar {
        public FilmToolBar() {
            add(playFilmAction);
            add(saveFilmAction);
            addSeparator();

            filterSelectionComboBox.setMaximumSize(new Dimension(150, 100));
            filterSelectionComboBox.setEditable(false);
            filterSelectionComboBox.setToolTipText("Aktiver Filter");
            add(filterSelectionComboBox);
            addSeparator();

            add(new JLabel("Suche:"));
            add(searchField);
            addSeparator();

            add(btnToggleFilterDialogVisibility);

            addSeparator();
            add(bookmarkAddFilmAction);
            add(bookmarkRemoveFilmAction);
            addSeparator();
            add(bookmarkClearListAction);
            addSeparator();
            add(manageBookmarkAction);
        }
    }

    private class BookmarkClearListAction extends AbstractAction {
        public BookmarkClearListAction() {
            putValue(Action.SHORT_DESCRIPTION, "Merkliste vollständig löschen");
            putValue(Action.NAME, "Merkliste vollständig löschen");
            putValue(Action.SMALL_ICON, SVGIconUtilities.createToolBarIcon("icons/fontawesome/file-circle-xmark.svg"));
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            var res = JOptionPane.showConfirmDialog(mediathekGui,
                    "Möchten Sie wirklich die Merkliste vollständig löschen?", Konstanten.PROGRAMMNAME, JOptionPane.YES_NO_OPTION);
            if (res == JOptionPane.YES_OPTION) {
                var daten = Daten.getInstance();
                var list = daten.getListeFilmeNachBlackList().parallelStream()
                        .filter(DatenFilm::isBookmarked).toList();
                if (!list.isEmpty()) {
                    updateBookmarkListAndRefresh(list);
                }
                //delete leftover items which have no corresponding DatenFilm objects anymore -> outdated
                daten.getListeBookmarkList().clear();
                JOptionPane.showMessageDialog(mediathekGui, "Merkliste wurde gelöscht.", Konstanten.PROGRAMMNAME, JOptionPane.INFORMATION_MESSAGE);
            }        }
    }

    public class ToggleFilterDialogVisibilityAction extends AbstractAction {
        public ToggleFilterDialogVisibilityAction() {
            putValue(Action.NAME, "Filterdialog anzeigen");
            putValue(Action.SHORT_DESCRIPTION, "Filter anzeigen");
            putValue(Action.SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/filter.svg"));
            putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_F12, 0));
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            var dlg = filterActionPanel.getFilterDialog();
            if (dlg != null) {
                var visible = dlg.isVisible();
                visible = !visible;

                dlg.setVisible(visible);
            }
        }
    }

    public abstract class SearchField extends JTextField {
        private static final Dimension DEFAULT_DIMENSION = new Dimension(500, 100);
        private static final String SEARCHMODE_PROPERTY_STRING = "searchMode";
        protected final PropertyChangeSupport pcs = new PropertyChangeSupport(this);
        protected SearchControlFieldMode searchMode;

        public SearchField() {
            super("", 40);
            setMaximumSize(DEFAULT_DIMENSION);

            //show clear icon when text is entered
            putClientProperty(FlatClientProperties.TEXT_FIELD_SHOW_CLEAR_BUTTON, true);
            putClientProperty("JTextField.clearCallback", (Consumer<JTextComponent>) textField -> clearSearchField());

            addKeyListener(new EscapeKeyAdapter());
            addActionListener(e -> performSearch());

            createTrailingComponents();

            setupContextMenu();
        }

        private void setupContextMenu() {
            var handler = new TextCopyPasteHandler<>(this);
            setComponentPopupMenu(handler.getPopupMenu());
        }

        protected abstract void createTrailingComponents();

        protected abstract void performSearch();

        protected void clearSearchField() {
            setText("");
            fireActionPerformed();
        }

        public void addSearchModeChangeListener(PropertyChangeListener listener) {
            this.pcs.addPropertyChangeListener(SEARCHMODE_PROPERTY_STRING, listener);
        }

        public SearchControlFieldMode getSearchMode() {
            return searchMode;
        }

        public void setSearchMode(SearchControlFieldMode mode) {
            var oldValue = searchMode;
            searchMode = mode;
            pcs.firePropertyChange(SEARCHMODE_PROPERTY_STRING, oldValue, mode);
        }

        /**
         * Clear searchfield on escape key press.
         */
        class EscapeKeyAdapter extends KeyAdapter {
            @Override
            public void keyPressed(KeyEvent e) {
                if (e.getKeyChar() == KeyEvent.VK_ESCAPE) {
                    clearSearchField();
                }
            }
        }

        public class SearchHistoryButton extends JButton {
            private static final Logger logger = LogManager.getLogger();
            private final EventList<String> historyList = new BasicEventList<>();
            private final JMenuItem miClearHistory = new JMenuItem("Alles löschen");
            private final JMenuItem miEditHistory = new JMenuItem("Einträge bearbeiten");
            private String SEARCH_HISTORY_CONFIG = "search.history.items";

            public SearchHistoryButton(@Nullable SearchControlFieldMode mode) {
                super(new FlatSearchWithHistoryIcon(true));
                setToolTipText("Vorherige Suchen");

                if (mode != null) {
                    if (mode == SearchControlFieldMode.LUCENE) {
                        SEARCH_HISTORY_CONFIG += "_lucene";
                    }
                }

                miClearHistory.addActionListener(_e -> {
                    historyList.clear();
                    saveHistory();
                });

                miEditHistory.addActionListener(e -> {
                    EditHistoryDialog dlg = new EditHistoryDialog(mediathekGui , miEditHistory, historyList);
                    dlg.setVisible(true);
                });

                addActionListener(e -> {
                    JPopupMenu popupMenu = new JPopupMenu();
                    popupMenu.add(miClearHistory);
                    popupMenu.add(miEditHistory);
                    historyList.getReadWriteLock().readLock().lock();
                    try {
                        if (!historyList.isEmpty()) {
                            popupMenu.addSeparator();
                            for (var item : historyList) {
                                JMenuItem historyItem = new JMenuItem(item);
                                historyItem.addActionListener(evt -> {
                                    searchField.setText(item);
                                    searchField.fireActionPerformed();
                                });
                                popupMenu.add(historyItem);
                            }
                        }
                    }
                    finally {
                        historyList.getReadWriteLock().readLock().unlock();
                    }
                    popupMenu.show(this, 0, this.getHeight());
                });

                loadHistory();
                historyList.addListEventListener(le -> saveHistory());
            }

            public void addHistoryEntry(String text) {
                if (!historyList.contains(text)) {
                    historyList.addFirst(text);
                }
            }

            private void loadHistory() {
                try {
                    ObjectMapper mapper = new ObjectMapper();
                    var json = ApplicationConfiguration.getConfiguration().getString(SEARCH_HISTORY_CONFIG, "");
                    if (!json.isEmpty()) {
                        List<String> entries = mapper.readValue(json, new TypeReference<>() {
                        });
                        if (!entries.isEmpty()) {
                            historyList.getReadWriteLock().writeLock().lock();
                            try {
                                historyList.addAll(entries);
                            }
                            finally {
                                historyList.getReadWriteLock().writeLock().unlock();
                            }
                        }
                    }
                } catch (JsonProcessingException ex) {
                    logger.error("Failed to load search history", ex);
                }
            }

            private void saveHistory() {
                ObjectMapper mapper = new ObjectMapper();
                try {
                    historyList.getReadWriteLock().readLock().lock();
                    try {
                        var json = mapper.writeValueAsString(historyList);
                        ApplicationConfiguration.getConfiguration().setProperty(SEARCH_HISTORY_CONFIG, json);
                    }
                    finally {
                        historyList.getReadWriteLock().readLock().unlock();
                    }
                } catch (JsonProcessingException e) {
                    logger.error("Failed to write search history", e);
                }
            }
        }
    }

    public class LuceneSearchField extends SearchField {
        private static final Dimension LUCENE_DEFAULT_DIMENSION = new Dimension(700, 100);
        private final SearchHistoryButton luceneSearchHistoryButton = new SearchHistoryButton(SearchControlFieldMode.LUCENE);

        public LuceneSearchField() {
            setMaximumSize(LUCENE_DEFAULT_DIMENSION);
            setSearchMode(SearchControlFieldMode.LUCENE);

            putClientProperty(FlatClientProperties.PLACEHOLDER_TEXT, "Lucene Search Query");
            putClientProperty(FlatClientProperties.TEXT_FIELD_LEADING_COMPONENT, luceneSearchHistoryButton);
        }

        @Override
        protected void createTrailingComponents() {
            JToolBar searchToolbar = new JToolBar();
            searchToolbar.addSeparator();

            var luceneBtn = new JButton();
            luceneBtn.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/circle-question.svg"));
            luceneBtn.setToolTipText("Lucene Query Syntax Hilfe");
            luceneBtn.addActionListener(evt -> {
                if (Desktop.isDesktopSupported()) {
                    var desktop = Desktop.getDesktop();
                    if (desktop.isSupported(Desktop.Action.BROWSE)) {
                        try {
                            desktop.browse(Konstanten.LUCENE_CLIENT_HELP_URL.uri());
                        } catch (Exception e) {
                            throw new RuntimeException(e);
                        }
                    } else {
                        showError();
                    }
                } else {
                    showError();
                }
            });
            searchToolbar.add(luceneBtn);
            putClientProperty(FlatClientProperties.TEXT_FIELD_TRAILING_COMPONENT, searchToolbar);
        }

        private void showError() {
            JOptionPane.showMessageDialog(this, "Es konnte kein Browser geöffnet werden.",
                    Konstanten.PROGRAMMNAME, JOptionPane.ERROR_MESSAGE);
        }

        @Override
        protected void performSearch() {
            String searchText = getText();
            if (!searchText.isEmpty()) {
                luceneSearchHistoryButton.addHistoryEntry(searchText);
            }

            loadTable(true);
        }
    }

    public class RegularSearchField extends SearchField {
        private final SearchHistoryButton regularSearchHistoryButton = new SearchHistoryButton(null);

        public RegularSearchField() {
            addSearchModeChangeListener(e -> setupHelperTexts());
            setupPlaceholderText();

            putClientProperty(FlatClientProperties.TEXT_FIELD_LEADING_COMPONENT, regularSearchHistoryButton);

            installDocumentListener();
        }

        protected void setupPlaceholderText() {
            //put placeholder text
            boolean bSearchThroughDescription = ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.SEARCH_USE_FILM_DESCRIPTIONS, false);
            if (bSearchThroughDescription)
                setSearchMode(SearchControlFieldMode.IRGENDWO);
            else
                setSearchMode(SearchControlFieldMode.THEMA_TITEL);
        }

        @Override
        protected void performSearch() {
            String searchText = getText();
            if (!searchText.isEmpty()) {
                regularSearchHistoryButton.addHistoryEntry(searchText);
            }

            loadTable(true);
        }

        private void installDocumentListener() {
            getDocument().addDocumentListener(new DocumentListener() {
                @Override
                public void insertUpdate(DocumentEvent e) {
                    doCheck();
                }

                @Override
                public void removeUpdate(DocumentEvent e) {
                    doCheck();
                }

                @Override
                public void changedUpdate(DocumentEvent e) {
                    doCheck();
                }

                private void doCheck() {
                    var searchText = getText();
                    checkPatternValidity(searchText);
                    setForegroundTextColor(searchText);
                }
            });
        }

        private void setForegroundTextColor(String text) {
            if (Filter.isPattern(text))
                setForeground(MVColor.getRegExPatternColor());
            else
                setForeground(UIManager.getColor("TextField.foreground"));
        }

        private boolean isPatternValid(String text) {
            return Filter.makePatternNoCache(text) != null;
        }

        private void checkPatternValidity(String text) {
            if (Filter.isPattern(text))
                GuiFunktionen.showErrorIndication(this, !isPatternValid(text));
            else
                GuiFunktionen.showErrorIndication(this, false);
        }

        /**
         * Sets tooltip and placeholder texts according to {@link RegularSearchField#searchMode}.
         */
        private void setupHelperTexts() {
            String text;
            switch (searchMode) {
                case IRGENDWO -> text = "Thema/Titel/Beschreibung";
                case THEMA_TITEL -> text = "Thema/Titel";
                case LUCENE -> text = "Lucene Query";
                default -> {
                    logger.error("Illegal search mode");
                    text = "";
                }

            }
            putClientProperty(FlatClientProperties.PLACEHOLDER_TEXT, text);

            if (searchMode == SearchControlFieldMode.IRGENDWO || searchMode == SearchControlFieldMode.THEMA_TITEL) {
                setToolTipText(text + " durchsuchen");
            } else {
                setToolTipText("Lucene Query Syntax für die Suche");
            }
        }

        @Override
        protected void createTrailingComponents() {
            JToolBar searchToolbar = new JToolBar();
            searchToolbar.addSeparator();
            searchToolbar.add(new ToggleSearchFieldToggleButton());
            putClientProperty(FlatClientProperties.TEXT_FIELD_TRAILING_COMPONENT, searchToolbar);
        }

        class ToggleSearchFieldToggleButton extends JToggleButton {
            public ToggleSearchFieldToggleButton() {
                FlatSVGIcon selectedIcon = SVGIconUtilities.createSVGIcon("icons/fontawesome/envelope-open-text.svg");
                selectedIcon.setColorFilter(new FlatSVGIcon.ColorFilter(c -> MVColor.getSelectedColor()));
                FlatSVGIcon normalIcon = SVGIconUtilities.createSVGIcon("icons/fontawesome/envelope-open-text.svg");
                normalIcon.setColorFilter(new FlatSVGIcon.ColorFilter(c -> Color.GRAY));
                setIcon(normalIcon);
                setSelectedIcon(selectedIcon);

                boolean bSearchThroughDescription = ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.SEARCH_USE_FILM_DESCRIPTIONS, false);
                setSelected(bSearchThroughDescription);
                setupToolTip(bSearchThroughDescription);

                addActionListener(e -> {
                    switch (getSearchMode()) {
                        case IRGENDWO -> {
                            setSearchMode(SearchControlFieldMode.THEMA_TITEL);
                            setupToolTip(false);
                        }
                        case THEMA_TITEL -> {
                            setSearchMode(SearchControlFieldMode.IRGENDWO);
                            setupToolTip(true);
                        }
                    }
                    //update config
                    ApplicationConfiguration.getConfiguration().setProperty(ApplicationConfiguration.SEARCH_USE_FILM_DESCRIPTIONS, getSearchMode() == SearchControlFieldMode.IRGENDWO);

                    loadTable();
                });
            }

            private void setupToolTip(boolean active) {
                if (active)
                    setToolTipText("Suche in Beschreibung aktiviert");
                else
                    setToolTipText("Suche in Beschreibung deaktiviert");
            }
        }
    }

    public class SaveFilmAction extends AbstractAction {
        public SaveFilmAction() {
            putValue(Action.SHORT_DESCRIPTION, "Film downloaden");
            putValue(Action.NAME, "Film downloaden");
            putValue(Action.SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/download.svg"));
            KeyStroke keyStroke;
            if (SystemUtils.IS_OS_MAC_OSX) {
                keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_F7, GuiFunktionen.getPlatformControlKey());
            } else
                keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_D, GuiFunktionen.getPlatformControlKey());
            putValue(Action.ACCELERATOR_KEY, keyStroke);
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            saveFilm(null);
        }
    }

    private class BookmarkAddFilmAction extends AbstractAction {
        public BookmarkAddFilmAction() {
            KeyStroke keyStroke;
            if (SystemUtils.IS_OS_MAC_OSX) {
                keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_F8, GuiFunktionen.getPlatformControlKey());
            } else {
                keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_B, GuiFunktionen.getPlatformControlKey());
            }
            putValue(Action.ACCELERATOR_KEY, keyStroke);
            putValue(Action.SHORT_DESCRIPTION, "Ausgewählte Filme in der Merkliste speichern");
            putValue(Action.NAME, "Ausgewählte Filme merken");
            putValue(Action.SMALL_ICON, SVGIconUtilities.createToolBarIcon("icons/fontawesome/file-circle-plus.svg"));
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            var selectedFilms = getSelFilme();
            if (!selectedFilms.isEmpty()) {
                var tbdFilms = selectedFilms.parallelStream()
                        .filter(f -> !f.isBookmarked())
                        .filter(f -> !f.isLivestream())
                        .toList();
                if (!tbdFilms.isEmpty()) {
                    updateBookmarkListAndRefresh(tbdFilms);
                }
            }
        }
    }

    private class BookmarkRemoveFilmAction extends AbstractAction {
        public BookmarkRemoveFilmAction() {
            putValue(Action.SHORT_DESCRIPTION, "Ausgewählte Filme aus der Merkliste löschen");
            putValue(Action.NAME, "Ausgewählte Filme aus der Merkliste löschen");
            putValue(Action.SMALL_ICON, SVGIconUtilities.createToolBarIcon("icons/fontawesome/file-circle-minus.svg"));
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            var selectedFilms = getSelFilme();
            if (!selectedFilms.isEmpty()) {
                var tbdFilms = selectedFilms.parallelStream()
                        .filter(DatenFilm::isBookmarked)
                        .toList();
                if (!tbdFilms.isEmpty()) {
                    updateBookmarkListAndRefresh(tbdFilms);
                }
            }
        }
    }

    public class CopyUrlToClipboardAction extends AbstractAction {
        private final FilmResolution.Enum resolution;

        CopyUrlToClipboardAction(FilmResolution.Enum resolution) {
            this.resolution = resolution;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            Optional<DatenFilm> filmSelection = getCurrentlySelectedFilm();
            filmSelection.ifPresent(
                    film -> GuiFunktionen.copyToClipboard(film.getUrlFuerAufloesung(resolution)));
        }
    }

    /**
     * Implements the context menu for tab film.
     */
    class TableContextMenuHandler extends MouseAdapter {
        private final BeobPrint beobPrint = new BeobPrint();
        private final BeobAbo beobAbo = new BeobAbo(false);
        private final BeobAbo beobAboMitTitel = new BeobAbo(true);
        private final BeobBlacklist beobBlacklistSender = new BeobBlacklist(true, false);
        private final BeobBlacklist beobBlacklistSenderThema = new BeobBlacklist(true, true);
        private final BeobBlacklist beobBlacklistThema = new BeobBlacklist(false, true);
        private final ActionListener unseenActionListener = new BeobHistory(false);
        private final ActionListener seenActionListener = new BeobHistory(true);
        private final JDownloadHelper jDownloadHelper = new JDownloadHelper();
        private final DownloadSubtitleAction downloadSubtitleAction = new DownloadSubtitleAction(GuiFilme.this);
        private Point p;

        TableContextMenuHandler() {
        }

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
                    var infoDialog = mediathekGui.getFilmInfoDialog();
                    if (infoDialog != null) {
                        if (!infoDialog.isVisible()) {
                            infoDialog.showInfo();
                        }

                    }
                }
            }
        }

        @Override
        public void mousePressed(MouseEvent arg0) {
            if (arg0.isPopupTrigger()) {
                showMenu(arg0);
            }
        }

        @Override
        public void mouseReleased(MouseEvent arg0) {
            if (arg0.isPopupTrigger()) {
                showMenu(arg0);
            }
        }

        private void buttonTable(int row, int column) {
            if (row != -1) {
                switch (tabelle.convertColumnIndexToModel(column)) {
                    case DatenFilm.FILM_ABSPIELEN -> {
                        Optional<DatenFilm> filmSelection = getCurrentlySelectedFilm();
                        filmSelection.ifPresent(datenFilm -> {
                            boolean stop = false;
                            final DatenDownload datenDownload =
                                    daten.getListeDownloadsButton().getDownloadUrlFilm(datenFilm.getUrlNormalQuality());
                            if (datenDownload != null) {
                                if (datenDownload.start != null) {
                                    if (datenDownload.start.status == Start.STATUS_RUN) {
                                        stop = true;
                                        daten.getListeDownloadsButton().delDownloadButton(datenFilm.getUrlNormalQuality());
                                    }
                                }
                            }
                            if (!stop) {
                                playFilmAction.actionPerformed(null);
                            }
                        });
                    }
                    case DatenFilm.FILM_AUFZEICHNEN -> saveFilm(null);
                    case DatenFilm.FILM_MERKEN -> getCurrentlySelectedFilm().ifPresent(film -> {
                        if (!film.isLivestream()) {
                            if (film.isBookmarked())
                                bookmarkRemoveFilmAction.actionPerformed(null);
                            else
                                bookmarkAddFilmAction.actionPerformed(null);
                        }
                    });
                }
            }
        }

        private void showMenu(MouseEvent evt) {
            p = evt.getPoint();
            final int nr = tabelle.rowAtPoint(p);
            if (nr >= 0) {
                tabelle.setRowSelectionInterval(nr, nr);
            }

            JPopupMenu jPopupMenu = new JPopupMenu();

            jPopupMenu.add(playFilmAction);
            jPopupMenu.add(saveFilmAction);

            JMenuItem miBookmark = new JMenuItem(bookmarkAddFilmAction);
            jPopupMenu.add(miBookmark);
            jPopupMenu.addSeparator();

            JMenu submenueAbo = new JMenu("Abo");
            jPopupMenu.add(submenueAbo);
            // Abo anlegen
            JMenuItem itemAbo = new JMenuItem("Abo mit Sender und Thema anlegen");
            JMenuItem itemAboMitTitel = new JMenuItem("Abo mit Sender und Thema und Titel anlegen");

            Optional<DatenFilm> res = getFilm(nr);
            res.ifPresent(film -> {
                if ((daten.getListeAbo().getAboFuerFilm_schnell(film, false)) != null) {
                    // gibts schon -> deaktivieren...
                    itemAbo.setEnabled(false);
                    itemAboMitTitel.setEnabled(false);
                } else {
                    // neues Abo anlegen möglich...
                    itemAbo.addActionListener(beobAbo);
                    itemAboMitTitel.addActionListener(beobAboMitTitel);
                }
                // update Bookmark state
                if (film.isLivestream()) {
                    jPopupMenu.remove(miBookmark);
                } else {
                    miBookmark.setText(film.isBookmarked() ? "Film aus Merkliste entfernen" : "Film merken");
                }
            });

            submenueAbo.add(itemAbo);
            submenueAbo.add(itemAboMitTitel);

            // Programme einblenden
            JMenu submenue = new JMenu("Film mit Set starten");
            jPopupMenu.add(submenue);
            ListePset liste = Daten.listePset.getListeButton();
            for (DatenPset pset : liste) {
                if (pset.getListeProg().isEmpty() && pset.getName().isEmpty()) {
                    // ein "leeres" Pset, Platzhalter
                    continue;
                }

                JMenuItem item = new JMenuItem(pset.getName());
                pset.getForegroundColor().ifPresent(item::setForeground);
                if (!pset.getListeProg().isEmpty()) {
                    item.addActionListener(e -> playerStarten(pset));
                }
                submenue.add(item);
            }

            JMenu submenueBlack = new JMenu("Blacklist");
            jPopupMenu.add(submenueBlack);
            // anlegen
            var itemBlackSender = new JMenuItem("Sender in die Blacklist einfügen");
            itemBlackSender.addActionListener(beobBlacklistSender);

            var itemBlackThema = new JMenuItem("Thema in die Blacklist einfügen");
            itemBlackThema.addActionListener(beobBlacklistThema);

            var itemBlackSenderThema = new JMenuItem("Sender und Thema in die Blacklist einfügen");
            itemBlackSenderThema.addActionListener(beobBlacklistSenderThema);
            submenueBlack.add(itemBlackSender);
            submenueBlack.add(itemBlackThema);
            submenueBlack.add(itemBlackSenderThema);

            res.ifPresent(film -> {
                jDownloadHelper.installContextMenu(film, jPopupMenu);
                jPopupMenu.addSeparator();
                setupCopytoClipboardContextMenu(film, jPopupMenu);
                jPopupMenu.addSeparator();
                setupSearchEntries(jPopupMenu, film);
            });

            res.ifPresent(film -> {
                if (film.hasSubtitle()) {
                    jPopupMenu.add(downloadSubtitleAction);
                    jPopupMenu.addSeparator();
                }
            });

            // Drucken
            var miPrintTable = new JMenuItem("Tabelle drucken");
            miPrintTable.addActionListener(beobPrint);
            jPopupMenu.add(miPrintTable);

            jPopupMenu.add(mediathekGui.showFilmInformationAction);
            // History
            res.ifPresent(film -> setupHistoryContextActions(jPopupMenu, film));

            res.ifPresent(film -> {
                jPopupMenu.addSeparator();
                var miCreateInfoFile = new JMenuItem("Infodatei erzeugen...");
                miCreateInfoFile.addActionListener(ae -> {
                    var file = FileDialogs.chooseSaveFileLocation(MediathekGui.ui(), "Infodatei speichern", "");
                    if (file != null) {
                        MVInfoFile infoFile = new MVInfoFile();
                        try {
                            infoFile.writeManualInfoFile(film, file.toPath());
                        } catch (IOException e) {
                            throw new RuntimeException(e);
                        }
                    }
                });
                jPopupMenu.add(miCreateInfoFile);
            });

            res.ifPresent(film -> {
                if (film.isDuplicate()) {
                    jPopupMenu.addSeparator();
                    var mi = new JMenuItem("Zusammengehörige Filme anzeigen...");
                    mi.addActionListener(e -> {
                        DuplicateFilmDetailsDialog dlg = new DuplicateFilmDetailsDialog(MediathekGui.ui(), film);
                        dlg.setVisible(true);
                    });
                    jPopupMenu.add(mi);
                }

                if (!film.isLivestream()) {
                    jPopupMenu.addSeparator();
                    var mi = new JMenuItem("Duplikate entfernen...");
                    mi.addActionListener(e -> performDuplicateRemoval(film));
                    jPopupMenu.add(mi);
                }
            });

            // anzeigen
            jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
        }

        /**
         * Perform duplicate detection and removal of a given film.
         * This will NOT trigger a reevaluation of duplicates while the filmlist is loaded.
         * @param film The source film for duplicates
         */
        private void performDuplicateRemoval(@NotNull DatenFilm film) {
            var daten = Daten.getInstance();
            var completeFilmList = daten.getListeFilme();
            var filteredFilmList = daten.getListeBlacklist();

            var duplicateList = new ArrayList<>(completeFilmList.parallelStream()
                    .filter(f -> f.getSender().equalsIgnoreCase(film.getSender()))
                    .filter(f -> f.getThema().equalsIgnoreCase(film.getThema()))
                    .filter(f -> f.getTitle().equalsIgnoreCase(film.getTitle()))
                    .filter(f -> f.getUrlNormalQuality().equalsIgnoreCase(film.getUrlNormalQuality()))
                    .toList());
            var filmCount = duplicateList.size();
            if (filmCount > 1) {
                filmCount--; // decrement to show only duplicates
                var duplicateString = filmCount == 1 ? "Duplikat" : "Duplikate";
                var message = String.format("Es wurden %d %s gefunden.\nMöchten Sie diese entfernen?", filmCount, duplicateString);
                var result = JOptionPane.showConfirmDialog(mediathekGui, message,
                        Konstanten.PROGRAMMNAME, JOptionPane.YES_NO_OPTION);
                if (result == JOptionPane.YES_OPTION) {
                    // selected film will survive
                    duplicateList.remove(film);
                    //remove from original filmlist and update balcklist filtering
                    completeFilmList.removeAll(duplicateList);

                    // we must manually write the modified filmlist
                    var writer = new FilmListWriter(false);
                    writer.writeFilmList(StandardLocations.getFilmlistFilePathString(),
                            completeFilmList, null);

                    // filtered only after write otherwise race will occur
                    filteredFilmList.filterListAndNotifyListeners();
                    JOptionPane.showMessageDialog(mediathekGui, "Duplikate wurden entfernt.",
                            Konstanten.PROGRAMMNAME, JOptionPane.INFORMATION_MESSAGE);
                }
            }
            else {
                JOptionPane.showMessageDialog(mediathekGui, "Es wurden keine Duplikate gefunden.",
                        Konstanten.PROGRAMMNAME, JOptionPane.INFORMATION_MESSAGE);
            }
        }

        private void setupHistoryContextActions(@NotNull JPopupMenu popupMenu, @NotNull DatenFilm film) {
            if (!film.isLivestream()) {
                JMenuItem miHistory;
                try (var history = new SeenHistoryController()) {
                    if (history.hasBeenSeen(film)) {
                        miHistory = new JMenuItem("Film als ungesehen markieren");
                        miHistory.addActionListener(unseenActionListener);
                    } else {
                        miHistory = new JMenuItem("Film als gesehen markieren");
                        miHistory.addActionListener(seenActionListener);
                    }
                    popupMenu.add(miHistory);
                }
            }
        }

        private void setupCopytoClipboardContextMenu(@NotNull DatenFilm film, @NotNull JPopupMenu popupMenu) {
            var mCopyToClipboard = new JMenu("In Zwischenablage kopieren");
            var miCopyClipboardTitle = new JMenuItem("Titel");
            miCopyClipboardTitle.addActionListener(e -> GuiFunktionen.copyToClipboard(film.getTitle()));
            mCopyToClipboard.add(miCopyClipboardTitle);

            var miCopyClipboardThema = new JMenuItem("Thema");
            miCopyClipboardThema.addActionListener(e -> GuiFunktionen.copyToClipboard(film.getThema()));
            mCopyToClipboard.add(miCopyClipboardThema);

            var miCopyTitleThemaToClipboard = new JMenuItem("Thema - Titel");
            miCopyTitleThemaToClipboard.addActionListener(e -> {
                var text = film.getThema() + " - " + film.getTitle();
                GuiFunktionen.copyToClipboard(text);
            });
            mCopyToClipboard.add(miCopyTitleThemaToClipboard);

            var miCopySenderThemaTitelToClipboard = new JMenuItem("Sender - Thema - Titel");
            miCopySenderThemaTitelToClipboard.addActionListener(e -> {
                var t = String.format("%s - %s - %s", film.getSender(), film.getThema(), film.getTitle());
                GuiFunktionen.copyToClipboard(t);
            });
            mCopyToClipboard.add(miCopySenderThemaTitelToClipboard);

            var miCopyDescriptionToClipboard = new JMenuItem("Beschreibung");
            miCopyDescriptionToClipboard.addActionListener(e -> GuiFunktionen.copyToClipboard(film.getDescription()));
            mCopyToClipboard.add(miCopyDescriptionToClipboard);

            setupFilmUrlCopyToClipboardEntries(mCopyToClipboard, film);

            popupMenu.add(mCopyToClipboard);
        }

        private void setupFilmUrlCopyToClipboardEntries(@NotNull JMenu parentMenu, @NotNull DatenFilm film) {
            parentMenu.addSeparator();

            JMenuItem item;
            final String uNormal = film.getUrlFuerAufloesung(FilmResolution.Enum.NORMAL);
            String uHd = film.getUrlFuerAufloesung(FilmResolution.Enum.HIGH_QUALITY);
            String uLow = film.getUrlFuerAufloesung(FilmResolution.Enum.LOW);
            if (uHd.equals(uNormal)) {
                uHd = ""; // dann gibts keine
            }
            if (uLow.equals(uNormal)) {
                uLow = ""; // dann gibts keine
            }
            if (!uNormal.isEmpty()) {
                final ActionListener copyNormalUrlListener = e -> GuiFunktionen.copyToClipboard(uNormal);
                if (!uHd.isEmpty() || !uLow.isEmpty()) {
                    JMenu submenueURL = new JMenu("Film-URL");
                    // HD
                    if (!uHd.isEmpty()) {
                        item = new JMenuItem("höchste/hohe Qualität");
                        item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_H, GuiFunktionen.getPlatformControlKey() |
                                KeyEvent.SHIFT_DOWN_MASK | KeyEvent.ALT_DOWN_MASK));
                        item.addActionListener(
                                e -> GuiFunktionen.copyToClipboard(film.getUrlFuerAufloesung(FilmResolution.Enum.HIGH_QUALITY)));
                        submenueURL.add(item);
                    }

                    // normale Auflösung, gibts immer
                    item = new JMenuItem("mittlere Qualität");
                    item.addActionListener(copyNormalUrlListener);
                    item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N, GuiFunktionen.getPlatformControlKey() |
                            KeyEvent.SHIFT_DOWN_MASK | KeyEvent.ALT_DOWN_MASK));

                    submenueURL.add(item);

                    // kleine Auflösung
                    if (!uLow.isEmpty()) {
                        item = new JMenuItem("niedrige Qualität");
                        item.addActionListener(
                                e -> GuiFunktionen.copyToClipboard(film.getUrlFuerAufloesung(FilmResolution.Enum.LOW)));
                        submenueURL.add(item);
                    }
                    parentMenu.add(submenueURL);
                } else {
                    item = new JMenuItem("Verfügbare URL");
                    item.addActionListener(copyNormalUrlListener);
                    parentMenu.add(item);
                }
            }

            if (!film.getSubtitleUrl().isEmpty()) {

                item = new JMenuItem("Untertitel-URL");
                item.addActionListener(e -> GuiFunktionen.copyToClipboard(film.getSubtitleUrl()));
                parentMenu.add(item);
            }
        }

        private void setupSearchEntries(@NotNull JPopupMenu popupMenu, @NotNull DatenFilm film) {
            var mOnlineSearch = new JMenu("Online-Suche nach");
            var mThema = new JMenu("Thema");
            var mTitel = new JMenu("Titel");

            var set = EnumSet.allOf(OnlineSearchProviders.class);

            for (var item : set) {
                var miThema = new JMenuItem(item.toString());
                miThema.addActionListener(e -> {
                    var url = item.getQueryUrl() + URLEncoder.encode(film.getThema(), StandardCharsets.UTF_8);
                    tryLaunchBrowser(url);
                });
                mThema.add(miThema);

                var miTitel = new JMenuItem(item.toString());
                miTitel.addActionListener(e -> {
                    var url = item.getQueryUrl() + URLEncoder.encode(film.getTitle(), StandardCharsets.UTF_8);
                    tryLaunchBrowser(url);
                });
                mTitel.add(miTitel);
            }

            mOnlineSearch.add(mThema);
            mOnlineSearch.add(mTitel);
            popupMenu.add(mOnlineSearch);
            popupMenu.addSeparator();
        }

        private void tryLaunchBrowser(String url) {
            try {
                UrlHyperlinkAction.openURL(url);
            } catch (URISyntaxException ex) {
                logger.error("Failed to launch online search for url {}", url);
            }
        }

        private class BeobHistory implements ActionListener {

            private final boolean seen;

            BeobHistory(boolean seen) {
                this.seen = seen;
            }

            private void updateHistory(DatenFilm film) {
                try (var history = new SeenHistoryController()) {
                    if (seen) {
                        history.markSeen(film);
                    } else {
                        history.markUnseen(film);
                    }
                }
            }

            @Override
            public void actionPerformed(ActionEvent e) {
                final int nr = tabelle.rowAtPoint(p);
                if (nr != -1) {
                    Optional<DatenFilm> res = getFilm(nr);
                    res.ifPresent(this::updateHistory);
                }
            }
        }

        private class BeobPrint implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                try {
                    tabelle.print();
                } catch (PrinterException ex) {
                    logger.error(ex);
                }
            }
        }

        private class BeobAbo implements ActionListener {

            private final boolean mitTitel;

            BeobAbo(boolean mmitTitel) {
                mitTitel = mmitTitel;
            }

            @Override
            public void actionPerformed(ActionEvent e) {
                if (Daten.listePset.getListeAbo().isEmpty()) {
                    new DialogAboNoSet(mediathekGui).setVisible(true);
                } else {
                    final int nr = tabelle.rowAtPoint(p);
                    if (nr >= 0) {
                        stopBeob = true;
                        Optional<DatenFilm> res = getFilm(nr);
                        res.ifPresent(film -> {
                            DatenAbo datenAbo;
                            if ((datenAbo =
                                    daten.getListeAbo().getAboFuerFilm_schnell(film, false /*ohne Länge*/))
                                    != null) {
                                // gibts schon, dann löschen
                                daten.getListeAbo().aboLoeschen(datenAbo);
                            } else // neues Abo anlegen
                            {
                                if (mitTitel) {
                                    daten.getListeAbo().addAbo(film.getThema() /*aboname*/, film.getSender(),
                                            film.getThema(), film.getTitle());
                                } else {
                                    daten.getListeAbo().addAbo(film.getThema() /*aboname*/, film.getSender(),
                                            film.getThema(), "");
                                }
                            }
                        });
                        stopBeob = false;
                    }
                }
            }
        }

        private final class BeobBlacklist implements ActionListener {

            private final boolean sender;
            private final boolean thema;

            BeobBlacklist(boolean sender, boolean thema) {
                this.sender = sender;
                this.thema = thema;
            }

            @Override
            public void actionPerformed(ActionEvent e) {
                final int nr = tabelle.rowAtPoint(p);
                if (nr >= 0) {
                    Optional<DatenFilm> res = getFilm(nr);
                    res.ifPresent(
                            film -> {
                                final String thema = film.getThema();
                                final String sender = film.getSender();
                                // Blackliste für alle Fälle einschalten, notify kommt beim add()
                                ApplicationConfiguration.getConfiguration().setProperty(ApplicationConfiguration.BLACKLIST_IS_ON, true);
                                var listeBlacklist = daten.getListeBlacklist();
                                if (!this.sender) {
                                    listeBlacklist.add(new BlacklistRule("", thema, "", ""));
                                } else if (!this.thema) {
                                    listeBlacklist.add(new BlacklistRule(sender, "", "", ""));
                                } else {
                                    listeBlacklist.add(new BlacklistRule(sender, thema, "", ""));
                                }
                            });
                }
            }
        }
    }

}
