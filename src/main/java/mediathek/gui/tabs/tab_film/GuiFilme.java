package mediathek.gui.tabs.tab_film;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.formdev.flatlaf.FlatClientProperties;
import com.formdev.flatlaf.extras.FlatSVGIcon;
import com.formdev.flatlaf.icons.FlatSearchWithHistoryIcon;
import com.google.common.util.concurrent.FutureCallback;
import com.google.common.util.concurrent.Futures;
import com.google.common.util.concurrent.ListenableFuture;
import javafx.animation.PauseTransition;
import javafx.application.Platform;
import javafx.util.Duration;
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.config.MVColor;
import mediathek.config.MVConfig;
import mediathek.controller.history.SeenHistoryController;
import mediathek.controller.starter.Start;
import mediathek.daten.*;
import mediathek.daten.abo.DatenAbo;
import mediathek.daten.blacklist.BlacklistRule;
import mediathek.filmeSuchen.ListenerFilmeLaden;
import mediathek.filmeSuchen.ListenerFilmeLadenEvent;
import mediathek.gui.FilterSelectionComboBoxModel;
import mediathek.gui.actions.PlayFilmAction;
import mediathek.gui.actions.UrlHyperlinkAction;
import mediathek.gui.dialog.DialogAboNoSet;
import mediathek.gui.dialog.DialogAddDownload;
import mediathek.gui.dialog.DialogAddMoreDownload;
import mediathek.gui.messages.*;
import mediathek.gui.messages.history.DownloadHistoryChangedEvent;
import mediathek.gui.tabs.AGuiTabPanel;
import mediathek.gui.tabs.tab_film.helpers.GuiFilmeModelHelper;
import mediathek.gui.tabs.tab_film.helpers.GuiModelHelper;
import mediathek.gui.tabs.tab_film.helpers.LuceneGuiFilmeModelHelper;
import mediathek.javafx.bookmark.BookmarkWindowController;
import mediathek.javaswing.filterpanel.FilmActionPanelSwing;
import mediathek.javaswing.filterpanel.SearchControlFieldModeSwing;
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
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.table.TableModel;
import javax.swing.text.JTextComponent;
import java.awt.*;
import java.awt.event.*;
import java.awt.print.PrinterException;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.lang.reflect.InvocationTargetException;
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
    public final BookmarkFilmAction bookmarkFilmAction = new BookmarkFilmAction();
    public final CopyUrlToClipboardAction copyHqUrlToClipboardAction = new CopyUrlToClipboardAction(FilmResolution.Enum.HIGH_QUALITY);
    public final CopyUrlToClipboardAction copyNormalUrlToClipboardAction = new CopyUrlToClipboardAction(FilmResolution.Enum.NORMAL);
    protected final JTabbedPane psetButtonsTab = new JTabbedPane();
    private final PauseTransition reloadTableDataTransition = new PauseTransition(Duration.millis(250d));
    private final MarkFilmAsSeenAction markFilmAsSeenAction = new MarkFilmAsSeenAction();
    private final MarkFilmAsUnseenAction markFilmAsUnseenAction = new MarkFilmAsUnseenAction();
    private final JScrollPane filmListScrollPane = new JScrollPane();
    private final JPanel extensionArea = new JPanel();
    private final JCheckBoxMenuItem cbkShowDescription =
            new JCheckBoxMenuItem("Beschreibung anzeigen");
    private final SeenHistoryController historyController = new SeenHistoryController();
    private final JToolBar toolBar = new JToolBar();
    private final JCheckBoxMenuItem cbShowButtons = new JCheckBoxMenuItem("Buttons anzeigen");
    private final PauseTransition zeitraumTransition = new PauseTransition(Duration.millis(250));
    /**
     * The JavaFx Film action popup panel.
     */
    public FilmActionPanelSwing filmActionPanelSwing;
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
        super();
        daten = aDaten;
        this.mediathekGui = mediathekGui;
        descriptionPanel = new FilmDescriptionPanel(this);

        if (daten.getListeFilmeNachBlackList() instanceof IndexedFilmList)
            searchField = new LuceneSearchField();
        else
            searchField = new RegularSearchField();

        setLayout(new BorderLayout());

        add(filmListScrollPane, BorderLayout.CENTER);
        createExtensionArea();
        createToolBar();

        // add film description panel
        extensionArea.add(descriptionTab);
        extensionArea.add(psetButtonsTab);

        setupFilmListTable();
        setupFilmSelectionPropertyListener();
        setupDescriptionTab(tabelle, cbkShowDescription, ApplicationConfiguration.FILM_SHOW_DESCRIPTION);
        setupPsetButtonsTab();
        setupFilmActionPanel();

        start_init();
        // register message bus handler
        MessageBus.getMessageBus().subscribe(this);

        setupActionListeners();
    }

    private void createToolBar() {
        add(toolBar, BorderLayout.NORTH);

        toolBar.setFloatable(true);
        toolBar.setName("Filme");

        toolBar.add(playFilmAction);
        toolBar.add(saveFilmAction);
        toolBar.add(bookmarkFilmAction);
        toolBar.addSeparator();

        filterSelectionComboBox.setMaximumSize(new Dimension(150, 100));
        filterSelectionComboBox.setEditable(false);
        filterSelectionComboBox.setToolTipText("Aktiver Filter");
        toolBar.add(filterSelectionComboBox);
        toolBar.addSeparator();

        toolBar.add(new JLabel("Suche:"));
        toolBar.add(searchField);
        toolBar.addSeparator();

        toolBar.add(btnToggleFilterDialogVisibility);
    }

    @Handler
    public void handleTableModelChange(TableModelChangeEvent e) {
        if (e.active) {
            SwingUtilities.invokeLater(() -> {
                playFilmAction.setEnabled(false);
                saveFilmAction.setEnabled(false);
                bookmarkFilmAction.setEnabled(false);
                toggleFilterDialogVisibilityAction.setEnabled(false);
                searchField.setEnabled(false);
                filterSelectionComboBox.setEnabled(false);
            });
        } else {
            SwingUtilities.invokeLater(() -> {
                playFilmAction.setEnabled(true);
                saveFilmAction.setEnabled(true);
                bookmarkFilmAction.setEnabled(true);
                toggleFilterDialogVisibilityAction.setEnabled(true);
                searchField.setEnabled(true);
                if (e.fromSearchField)
                    searchField.requestFocusInWindow();
                filterSelectionComboBox.setEnabled(true);
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

    private void createExtensionArea() {
        extensionArea.setLayout(new VerticalLayout());
        add(extensionArea, BorderLayout.SOUTH);
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
        menu.add(bookmarkFilmAction);
        menu.addSeparator();
        menu.add(miMarkFilmAsSeen);
        menu.add(miMarkFilmAsUnseen);
        menu.addSeparator();
        menu.add(mediathekGui.toggleBlacklistAction);
        menu.add(mediathekGui.editBlacklistAction);
        menu.addSeparator();
        menu.add(cbkShowDescription);
    }

    private void setupFilmActionPanel() {
        filmActionPanelSwing = new FilmActionPanelSwing(btnToggleFilterDialogVisibility);
    }

    private void setupPsetButtonsTab() {
        var initialVisibility = ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.APPLICATION_BUTTONS_PANEL_VISIBLE, false);
        setupButtonsMenuItem(initialVisibility);

        psetButtonsPanel = new PsetButtonsPanel(this);
        psetButtonsPanel.putClientProperty("JTabbedPane.tabClosable", true);
        psetButtonsPanel.putClientProperty("JTabbedPane.tabCloseCallback", (IntConsumer) tabIndex -> cbShowButtons.doClick());
        psetButtonsPanel.install(psetButtonsTab);

        makeButtonsTabVisible(initialVisibility);
    }

    private void setupButtonsMenuItem(boolean initialVisibility) {
        var config = ApplicationConfiguration.getConfiguration();

        if (!SystemUtils.IS_OS_MAC_OSX)
            cbShowButtons.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F11, 0));
        cbShowButtons.setSelected(initialVisibility);
        cbShowButtons.addActionListener(l -> {
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
        actionMap.put(ACTION_MAP_KEY_BOOKMARK_FILM, bookmarkFilmAction);
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
                Platform.runLater(() -> filmActionPanelSwing.updateThemaComboBox());
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
            if (filmActionPanelSwing.isShowUnseenOnly()) {
                Platform.runLater(reloadTableDataTransition::playFromStart);
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
    private void handleAboListChanged(AboListChangedEvent e) {
        Platform.runLater(reloadTableDataTransition::playFromStart);
    }

    @Handler
    private void handleBlacklistChangedEvent(BlacklistChangedEvent e) {
        Platform.runLater(reloadTableDataTransition::playFromStart);
    }

    @Handler
    private void handleStartEvent(StartEvent msg) {
        SwingUtilities.invokeLater(this::updateStartInfoProperty);
    }

    private synchronized void saveFilm(DatenPset pSet) {
        if (Daten.listePset.getListeSpeichern().isEmpty()) {
            new DialogAboNoSet(mediathekGui).setVisible(true);
            // Satz mit x, war wohl nix
            return;
        }

        List<DatenFilm> liste = getSelFilme();
        boolean standard = false;
        String pfad = "";
        boolean info = false;
        boolean subtitle = false;

        if (liste.size() > 1) {
            if (pSet == null) {
                pSet = Daten.listePset.getListeSpeichern().get(0);
            }
            DialogAddMoreDownload damd = new DialogAddMoreDownload(mediathekGui, pSet);
            damd.setVisible(true);
            standard = damd.addAll;
            pfad = damd.getPath();
            info = damd.info;
            subtitle = damd.subtitle;
            if (damd.cancel) {
                return;
            }
        }

        for (DatenFilm datenFilm : liste) {
            // erst mal schauen obs den schon gibt
            DatenDownload datenDownload =
                    daten.getListeDownloads().getDownloadUrlFilm(datenFilm.getUrlNormalQuality());
            if (datenDownload != null) {
                int ret = JOptionPane.showConfirmDialog(mediathekGui,
                        "Download für den Film existiert bereits.\n" + "Nochmal anlegen?",
                        "Anlegen?",
                        JOptionPane.YES_NO_OPTION);
                if (ret != JOptionPane.OK_OPTION) {
                    continue;
                }
            }

            if (standard) {
                if (pSet == null) {
                    pSet = Daten.listePset.getListeSpeichern().get(0);
                }
                datenDownload = new DatenDownload(
                        pSet, datenFilm, DatenDownload.QUELLE_DOWNLOAD, null, "",
                        pfad, "");
                datenDownload.arr[DatenDownload.DOWNLOAD_INFODATEI] = Boolean.toString(info);
                datenDownload.arr[DatenDownload.DOWNLOAD_SUBTITLE] = Boolean.toString(subtitle);

                daten.getListeDownloads().addMitNummer(datenDownload);
                MessageBus.getMessageBus().publishAsync(new DownloadListChangedEvent());
                if (Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_DIALOG_DOWNLOAD_D_STARTEN))) {
                    // und evtl. auch gleich starten
                    datenDownload.startDownload();
                }
            } else {
                saveFilm(datenFilm, pSet);
            }
        }
    }

    private void saveFilm(DatenFilm datenFilm, DatenPset pSet) {
        if (Daten.listePset.getListeSpeichern().isEmpty()) {
            MVMessageDialog.showMessageDialog(this,
                    "Ohne Programm-Sets können keine Downloads gestartet werden.",
                    Konstanten.PROGRAMMNAME, JOptionPane.ERROR_MESSAGE);
        } else {
            // dann alle Downloads im Dialog abfragen
            Optional<FilmResolution.Enum> res =
                    filmActionPanelSwing.isShowOnlyHighQuality() ? Optional.of(FilmResolution.Enum.HIGH_QUALITY) : Optional.empty();
            DialogAddDownload dialog = new DialogAddDownload(mediathekGui, datenFilm, pSet, res);
            dialog.setVisible(true);
        }
    }

    private void bookmarkFilm() {
        var movies = getSelFilme();
        final long size = movies.size();
        if (size > 250) {
            var reply = JOptionPane.showConfirmDialog(this,
                    String.format("Möchten Sie wirklich %d Einträge der Merkliste bearbeiten?%nDas Programm könnte während der Operation nicht reagieren.", size),
                    Konstanten.PROGRAMMNAME,
                    JOptionPane.YES_NO_OPTION);
            if (reply == JOptionPane.YES_OPTION) {
                daten.getListeBookmarkList().checkAndBookmarkMovies(movies);
                repaint();
            }
        } else {
            daten.getListeBookmarkList().checkAndBookmarkMovies(movies);
            repaint();
        }
    }

    /**
     * If necessary instantiate and show the bookmark window
     */
    public void showBookmarkWindow() {
        if (bookmarkWindowController.isEmpty()) {
            bookmarkWindowController = Optional.of(new BookmarkWindowController());
            bookmarkWindowController.get().setPartner(this);
        }
        bookmarkWindowController.get().show();
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
            if (filmActionPanelSwing.isShowOnlyHighQuality()) {
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

    private void setupDataTransitions() {
        //execute on JavaFX thread!
        reloadTableDataTransition.setOnFinished(e -> {
            try {
                SwingUtilities.invokeAndWait(this::loadTable);
            } catch (InterruptedException | InvocationTargetException ex) {
                logger.error("Table reload failed", ex);
            }
        });

        zeitraumTransition.setOnFinished(evt -> {
            // reset sender filter first
            filmActionPanelSwing.getViewSettingsPane().senderCheckList.getSelectionModel().clearSelection();
            try {
                SwingUtilities.invokeAndWait(() -> daten.getListeBlacklist().filterListe());
            } catch (InterruptedException | InvocationTargetException e) {
                logger.error("Failed to filter list", e);
            }
            reloadTableDataTransition.playFromStart();
        });
    }

    private void setupActionListeners() {
        Platform.runLater(() -> {
            setupDataTransitions();


            final ChangeListener reloadTableListener = new ChangeListener() {
                @Override
                public void stateChanged(ChangeEvent e) {
                    reloadTableDataTransition.playFromStart();
                }
            };

            final ChangeListener reloadTableListener2 = new ChangeListener() {
                @Override
                public void stateChanged(ChangeEvent e) {
                    // Hier muss überprüft werden, ob der Wert false ist.
                    // Da ChangeListener kein direktes "newValue" übergibt, muss die Quelle des Events geprüft werden.

                    // Beispielhafte Annahme: Die Quelle ist ein JToggleButton oder JSlider.
                    Object source = e.getSource();
                    if (source instanceof javax.swing.JToggleButton) {
                        javax.swing.JToggleButton toggleButton = (javax.swing.JToggleButton) source;
                        if (!toggleButton.isSelected()) {
                            reloadTableDataTransition.playFromStart();
                        }
                    }
                }
            };

            filmActionPanelSwing.showOnlyHighQualityProperty().addChangeListener(reloadTableListener);
            filmActionPanelSwing.showSubtitlesOnlyProperty().addChangeListener(reloadTableListener);
            filmActionPanelSwing.showNewOnlyProperty().addChangeListener(reloadTableListener);
            filmActionPanelSwing.showBookMarkedOnlyProperty().addChangeListener(reloadTableListener);
            filmActionPanelSwing.showUnseenOnlyProperty().addChangeListener(reloadTableListener);
            filmActionPanelSwing.dontShowAbosProperty().addChangeListener(reloadTableListener);
            filmActionPanelSwing.dontShowTrailersProperty().addChangeListener(reloadTableListener);
            filmActionPanelSwing.dontShowSignLanguageProperty().addChangeListener(reloadTableListener);
            filmActionPanelSwing.dontShowAudioVersionsProperty().addChangeListener(reloadTableListener);
            filmActionPanelSwing.showLivestreamsOnlyProperty().addChangeListener(reloadTableListener);
            var filmLengthSlider = filmActionPanelSwing.getFilmLengthSlider();
            filmLengthSlider.addChangeListener(reloadTableListener2);
            filmLengthSlider.addChangeListener(reloadTableListener2);

            filmActionPanelSwing.zeitraumProperty().addChangeListener(e  -> zeitraumTransition.playFromStart());

            filmActionPanelSwing.getViewSettingsPane().themaComboBox.addActionListener(evt -> {
                if (filmActionPanelSwing.getViewSettingsPane().themaComboBox.getItemCount() != 0) {
                    reloadTableDataTransition.playFromStart();
                }
            });
        });
    }

    @Override
    protected void setupShowFilmDescriptionMenuItem() {
        var config = ApplicationConfiguration.getConfiguration();

        cbkShowDescription.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F10, 0));
        cbkShowDescription.setSelected(config.getBoolean(ApplicationConfiguration.FILM_SHOW_DESCRIPTION, true));
        cbkShowDescription.addActionListener(l -> {
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
            GuiModelHelper helper;
            var searchFieldData = new SearchFieldData(searchField.getText(),
                    searchField.getSearchMode());
            if (Daten.getInstance().getListeFilmeNachBlackList() instanceof IndexedFilmList) {
                helper = new LuceneGuiFilmeModelHelper(filmActionPanelSwing, historyController, searchFieldData);
            } else {
                helper = new GuiFilmeModelHelper(filmActionPanelSwing, historyController, searchFieldData);
            }
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

    static public class FilterVisibilityToggleButton extends JToggleButton {
        public FilterVisibilityToggleButton(Action a) {
            super(a);
            setText("");
            final boolean visible = ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.FilterDialog.VISIBLE, false);
            setSelected(visible);
        }
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
            var dlg = filmActionPanelSwing.getFilterDialog();
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
        protected SearchControlFieldModeSwing searchMode;

        public SearchField() {
            super("", 20);
            setMaximumSize(DEFAULT_DIMENSION);

            //show clear icon when text is entered
            putClientProperty(FlatClientProperties.TEXT_FIELD_SHOW_CLEAR_BUTTON, true);
            putClientProperty("JTextField.clearCallback", (Consumer<JTextComponent>) textField -> clearSearchField());

            addKeyListener(new EscapeKeyAdapter());
            addActionListener(l -> performSearch());

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

        public SearchControlFieldModeSwing getSearchMode() {
            return searchMode;
        }

        public void setSearchMode(SearchControlFieldModeSwing mode) {
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
            private final List<String> historyList = new ArrayList<>();
            private final JMenuItem miClearHistory = new JMenuItem("Historie löschen");
            private String SEARCH_HISTORY_CONFIG = "search.history.items";

            public SearchHistoryButton(@Nullable SearchControlFieldModeSwing mode) {
                super(new FlatSearchWithHistoryIcon(true));
                setToolTipText("Vorherige Suchen");

                if (mode != null) {
                    if (mode == SearchControlFieldModeSwing.LUCENE) {
                        SEARCH_HISTORY_CONFIG += "_lucene";
                    }
                }

                miClearHistory.addActionListener(l -> {
                    historyList.clear();
                    saveHistory();
                });
                addActionListener(l -> {
                    JPopupMenu popupMenu = new JPopupMenu();
                    popupMenu.add(miClearHistory);
                    if (!historyList.isEmpty()) {
                        popupMenu.addSeparator();
                        for (var item : historyList) {
                            JMenuItem historyItem = new JMenuItem(item);
                            historyItem.addActionListener(li -> {
                                searchField.setText(item);
                                searchField.fireActionPerformed();
                            });
                            popupMenu.add(historyItem);
                        }
                    }
                    popupMenu.show(this, 0, this.getHeight());
                });

                loadHistory();
            }

            public void addHistoryEntry(String text) {
                if (!historyList.contains(text)) {
                    historyList.add(0, text);
                    saveHistory();
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
                            historyList.addAll(entries);
                        }
                    }
                } catch (JsonProcessingException ex) {
                    logger.error("Failed to load search history", ex);
                }
            }

            private void saveHistory() {
                ObjectMapper mapper = new ObjectMapper();
                try {
                    var json = mapper.writeValueAsString(historyList);
                    ApplicationConfiguration.getConfiguration().setProperty(SEARCH_HISTORY_CONFIG, json);
                } catch (JsonProcessingException e) {
                    logger.error("Failed to write search history", e);
                }
            }
        }
    }

    public class LuceneSearchField extends SearchField {
        private static final Dimension LUCENE_DEFAULT_DIMENSION = new Dimension(700, 100);
        private final SearchHistoryButton luceneSearchHistoryButton = new SearchHistoryButton(SearchControlFieldModeSwing.LUCENE);

        public LuceneSearchField() {
            setMaximumSize(LUCENE_DEFAULT_DIMENSION);
            setSearchMode(SearchControlFieldModeSwing.LUCENE);

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
            luceneBtn.addActionListener(l -> {
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
            addSearchModeChangeListener(evt -> setupHelperTexts());
            setupPlaceholderText();

            putClientProperty(FlatClientProperties.TEXT_FIELD_LEADING_COMPONENT, regularSearchHistoryButton);

            installDocumentListener();
        }

        protected void setupPlaceholderText() {
            //put placeholder text
            boolean bSearchThroughDescription = ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.SEARCH_USE_FILM_DESCRIPTIONS, false);
            if (bSearchThroughDescription)
                setSearchMode(SearchControlFieldModeSwing.IRGENDWO);
            else
                setSearchMode(SearchControlFieldModeSwing.THEMA_TITEL);
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

            if (searchMode == SearchControlFieldModeSwing.IRGENDWO || searchMode == SearchControlFieldModeSwing.THEMA_TITEL) {
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
                selectedIcon.setColorFilter(new FlatSVGIcon.ColorFilter(color -> MVColor.getSelectedColor()));
                FlatSVGIcon normalIcon = SVGIconUtilities.createSVGIcon("icons/fontawesome/envelope-open-text.svg");
                normalIcon.setColorFilter(new FlatSVGIcon.ColorFilter(color -> Color.GRAY));
                setIcon(normalIcon);
                setSelectedIcon(selectedIcon);

                boolean bSearchThroughDescription = ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.SEARCH_USE_FILM_DESCRIPTIONS, false);
                setSelected(bSearchThroughDescription);
                setupToolTip(bSearchThroughDescription);

                addActionListener(l -> {
                    switch (getSearchMode()) {
                        case IRGENDWO -> {
                            setSearchMode(SearchControlFieldModeSwing.THEMA_TITEL);
                            setupToolTip(false);
                        }
                        case THEMA_TITEL -> {
                            setSearchMode(SearchControlFieldModeSwing.IRGENDWO);
                            setupToolTip(true);
                        }
                    }
                    //update config
                    ApplicationConfiguration.getConfiguration().setProperty(ApplicationConfiguration.SEARCH_USE_FILM_DESCRIPTIONS, getSearchMode() == SearchControlFieldModeSwing.IRGENDWO);

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

    public class BookmarkFilmAction extends AbstractAction {
        public BookmarkFilmAction() {
            KeyStroke keyStroke;
            if (SystemUtils.IS_OS_MAC_OSX) {
                keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_F8, GuiFunktionen.getPlatformControlKey());
            } else {
                keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_B, GuiFunktionen.getPlatformControlKey());
            }
            putValue(Action.ACCELERATOR_KEY, keyStroke);
            putValue(Action.SHORT_DESCRIPTION, "Film merken");
            putValue(Action.NAME, "Film merken");
            putValue(Action.SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/bookmark.svg"));
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            bookmarkFilm();
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
                        if (!film.isLivestream())
                            bookmarkFilm();
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

            JMenuItem miBookmark = new JMenuItem(bookmarkFilmAction);
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
                    item.addActionListener(l -> playerStarten(pset));
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
            // anzeigen
            jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
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
                miThema.addActionListener(l -> {
                    var url = item.getQueryUrl() + URLEncoder.encode(film.getThema(), StandardCharsets.UTF_8);
                    try {
                        UrlHyperlinkAction.openURL(MediathekGui.ui(), url);
                    } catch (URISyntaxException ex) {
                        logger.error("Failed to launch online search for url {}", url);
                    }
                });
                mThema.add(miThema);

                var miTitel = new JMenuItem(item.toString());
                miTitel.addActionListener(l -> {
                    var url = item.getQueryUrl() + URLEncoder.encode(film.getTitle(), StandardCharsets.UTF_8);
                    try {
                        UrlHyperlinkAction.openURL(MediathekGui.ui(), url);
                    } catch (URISyntaxException ex) {
                        logger.error("Failed to launch online search for url {}", url);
                    }
                });
                mTitel.add(miTitel);
            }

            mOnlineSearch.add(mThema);
            mOnlineSearch.add(mTitel);
            popupMenu.add(mOnlineSearch);
            popupMenu.addSeparator();
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
                                MVConfig.add(MVConfig.Configs.SYSTEM_BLACKLIST_ON, Boolean.TRUE.toString());
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
