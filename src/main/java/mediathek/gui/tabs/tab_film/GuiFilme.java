package mediathek.gui.tabs.tab_film;

import javafx.animation.PauseTransition;
import javafx.application.Platform;
import javafx.beans.value.ChangeListener;
import javafx.collections.ListChangeListener;
import javafx.embed.swing.JFXPanel;
import javafx.scene.control.Alert;
import javafx.scene.control.ButtonBar;
import javafx.scene.control.ButtonType;
import javafx.util.Duration;
import jiconfont.icons.FontAwesome;
import jiconfont.swing.IconFontSwing;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.controller.history.SeenHistoryController;
import mediathek.controller.starter.Start;
import mediathek.daten.*;
import mediathek.daten.blacklist.BlacklistRule;
import mediathek.filmeSuchen.ListenerFilmeLaden;
import mediathek.filmeSuchen.ListenerFilmeLadenEvent;
import mediathek.gui.TabPaneIndex;
import mediathek.gui.actions.ShowBlacklistDialogAction;
import mediathek.gui.actions.ShowFilmInformationAction;
import mediathek.gui.dialog.DialogAboNoSet;
import mediathek.gui.dialog.DialogAddDownload;
import mediathek.gui.dialog.DialogAddMoreDownload;
import mediathek.gui.dialog.DialogEditAbo;
import mediathek.gui.messages.*;
import mediathek.gui.messages.history.DownloadHistoryChangedEvent;
import mediathek.gui.tabs.AGuiTabPanel;
import mediathek.javafx.bookmark.BookmarkWindowController;
import mediathek.javafx.buttonsPanel.ButtonsPanelController;
import mediathek.javafx.descriptionPanel.DescriptionPanelController;
import mediathek.javafx.filmtab.FilmTabInfoPane;
import mediathek.javafx.filterpanel.FilmActionPanel;
import mediathek.javafx.tool.JavaFxUtils;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.*;
import mediathek.tool.cellrenderer.CellRendererFilme;
import mediathek.tool.listener.BeobTableHeader;
import mediathek.tool.models.TModelFilm;
import mediathek.tool.table.MVFilmTable;
import net.engio.mbassy.listener.Handler;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jdesktop.swingx.VerticalLayout;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.awt.print.PrinterException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class GuiFilme extends AGuiTabPanel {

    public static final String NAME = "Filme";
    private static final String ACTION_MAP_KEY_PLAY_FILM = "film_abspielen";
    private static final String ACTION_MAP_KEY_SAVE_FILM = "download_film";
    private static final String ACTION_MAP_KEY_BOOKMARK_FILM = "bookmark_film";
    private static final String ACTION_MAP_KEY_COPY_NORMAL_URL = "copy_url";
    private static final String ACTION_MAP_KEY_COPY_HD_URL = "copy_url_hd";
    private static final String ACTION_MAP_KEY_COPY_KLEIN_URL = "copy_url_klein";
    private static final String ACTION_MAP_KEY_MEDIA_DB = "mediadb";
    private static final String ACTION_MAP_KEY_MARK_SEEN = "seen";
    private static final String ACTION_MAP_KEY_MARK_UNSEEN = "unseen";
    private static final int[] HIDDEN_COLUMNS =
            new int[]{DatenFilm.FILM_ABSPIELEN, DatenFilm.FILM_AUFZEICHNEN, DatenFilm.FILM_MERKEN};
    private static final Logger logger = LogManager.getLogger(GuiFilme.class);
    private static final int[] BUTTON_COLUMNS =
            new int[]{DatenFilm.FILM_ABSPIELEN, DatenFilm.FILM_AUFZEICHNEN, DatenFilm.FILM_MERKEN};
    public static boolean[] VISIBLE_COLUMNS = new boolean[DatenFilm.MAX_ELEM];
    public final FilterFilmAction filterFilmAction = new FilterFilmAction();
    public final PlayFilmAction playAction = new PlayFilmAction();
    public final SaveFilmAction saveFilmAction = new SaveFilmAction();
    public final BookmarkFilmAction bookmarkFilmAction = new BookmarkFilmAction();
    public final BookmarkManageListAction bookmarkManageListAction = new BookmarkManageListAction();
    private final MarkFilmAsSeenAction markFilmAsSeenAction = new MarkFilmAsSeenAction();
    private final MarkFilmAsUnseenAction markFilmAsUnseenAction = new MarkFilmAsUnseenAction();
    private final JScrollPane filmListScrollPane = new JScrollPane();
    private final JPanel extensionArea = new JPanel();
    private final JCheckBoxMenuItem cbkShowDescription =
            new JCheckBoxMenuItem("Beschreibung anzeigen");
    private final MediensammlungAction mediensammlungAction = new MediensammlungAction();
    private final JFXPanel fxDescriptionPanel = new JFXPanel();
    private final JFXPanel fxPsetButtonsPanel = new JFXPanel();
    private final SeenHistoryController historyController = new SeenHistoryController();
    /**
     * The JavaFx Film action popup panel.
     */
    public FilmActionPanel fap;
    private Optional<BookmarkWindowController> bookmarkWindowController = Optional.empty();
    /**
     * The swing helper panel FilmAction bar.
     */
    private JFXPanel fxFilmActionPanel;
    private boolean stopBeob;
    private FilmTabInfoPane filmInfoLabel;
    private JCheckBoxMenuItem cbShowButtons;
    /**
     * We need a strong reference here for message bus to work properly. Otherwise the buttons
     * panel controller will not receive change messages.
     */
    private ButtonsPanelController psetController;

    public GuiFilme(Daten aDaten, MediathekGui mediathekGui) {
        super();
        daten = aDaten;
        this.mediathekGui = mediathekGui;

        setLayout(new BorderLayout());

        createFilmListArea();
        createExtensionArea();
        createFilmActionPanel();

        // add film description panel
        extensionArea.add(fxDescriptionPanel);
        extensionArea.add(fxPsetButtonsPanel);

        setupFilmListTable();

        installTabInfoStatusBarControl();

        setupFilmSelectionPropertyListener(mediathekGui);

        setupDescriptionPanel();
        setupPsetButtonsPanel();
        setupFilmActionPanel();

        start_init();
        // register message bus handler
        daten.getMessageBus().subscribe(this);

        setupActionListeners();

        initializeTouchBar();
    }

    private void setupFilmListTable() {
        tabelle = new MVFilmTable();
        filmListScrollPane.setViewportView(tabelle);
    }

    private void createFilmActionPanel() {
        fxFilmActionPanel = new JFXPanel();
        add(fxFilmActionPanel, BorderLayout.NORTH);
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
            filmInfoLabel = new FilmTabInfoPane(daten, this);
            if (isVisible()) leftItems.add(filmInfoLabel);
        });

        addComponentListener(new ComponentAdapter() {
            @Override
            public void componentShown(ComponentEvent e) {
                Platform.runLater(
                        () -> {
                            filmInfoLabel.setVisible(true);
                            leftItems.add(filmInfoLabel);
                        });
            }

            @Override
            public void componentHidden(ComponentEvent e) {
                Platform.runLater(
                        () -> {
                            filmInfoLabel.setVisible(false);
                            leftItems.remove(filmInfoLabel);
                        });
            }
        });
    }

    private void createFilmListArea() {
        add(filmListScrollPane, BorderLayout.CENTER);
    }

    private void createExtensionArea() {
        extensionArea.setLayout(new VerticalLayout());
        add(extensionArea, BorderLayout.SOUTH);
    }

    @Handler
    private void handleButtonsPanelVisibilityChanged(ButtonsPanelVisibilityChangedEvent evt) {
        SwingUtilities.invokeLater(() -> cbShowButtons.setSelected(evt.visible));
        SwingUtilities.invokeLater(() -> fxPsetButtonsPanel.setVisible(evt.visible));
    }

    public void installViewMenuEntry(JMenu jMenuAnsicht) {
        cbShowButtons = new JCheckBoxMenuItem("Buttons anzeigen");
        if (!SystemUtils.IS_OS_MAC_OSX)
            cbShowButtons.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F11, 0));
        cbShowButtons.setSelected(ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.APPLICATION_BUTTONS_PANEL_VISIBLE, false));
        cbShowButtons.addActionListener(e -> daten.getMessageBus().publishAsync(new ButtonsPanelVisibilityChangedEvent(cbShowButtons.isSelected())));

        jMenuAnsicht.add(cbShowButtons, 0);
    }

    @Override
    public void installMenuEntries(JMenu menu) {
        JMenuItem miPlayFilm = new JMenuItem("Film abspielen");
        if (SystemUtils.IS_OS_MAC_OSX)
            miPlayFilm.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F6, InputEvent.META_DOWN_MASK));
        else miPlayFilm.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_P, KeyEvent.CTRL_DOWN_MASK));
        miPlayFilm.setIcon(IconFontSwing.buildIcon(FontAwesome.PLAY, 16));
        miPlayFilm.addActionListener(playAction);

        JMenuItem miRecordFilm = new JMenuItem("Film aufzeichnen");
        if (SystemUtils.IS_OS_MAC_OSX)
            miRecordFilm.setAccelerator(
                    KeyStroke.getKeyStroke(KeyEvent.VK_F7, InputEvent.META_DOWN_MASK));
        else
            miRecordFilm.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_D, KeyEvent.CTRL_DOWN_MASK));
        miRecordFilm.setIcon(IconFontSwing.buildIcon(FontAwesome.DOWNLOAD, 16));
        miRecordFilm.addActionListener(saveFilmAction);

        JMenuItem miBookmarkFilm = new JMenuItem("Film merken");
        if (SystemUtils.IS_OS_MAC_OSX) {
            miBookmarkFilm.setAccelerator(
                    KeyStroke.getKeyStroke(KeyEvent.VK_F8, InputEvent.META_DOWN_MASK));
        } else {
            miBookmarkFilm.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_B, KeyEvent.CTRL_DOWN_MASK));
        }
        miBookmarkFilm.setIcon(IconFontSwing.buildIcon(FontAwesome.BOOKMARK_O, 16));
        miBookmarkFilm.addActionListener(bookmarkFilmAction);

        JMenuItem miOpenBlacklist = new JMenuItem("Blacklist öffnen");
        if (SystemUtils.IS_OS_MAC_OSX)
            miOpenBlacklist.setAccelerator(
                    KeyStroke.getKeyStroke(KeyEvent.VK_F9, InputEvent.META_DOWN_MASK));
        else
            miOpenBlacklist.setAccelerator(
                    KeyStroke.getKeyStroke(KeyEvent.VK_B, KeyEvent.CTRL_DOWN_MASK));
        miOpenBlacklist.setAction(new ShowBlacklistDialogAction(mediathekGui, daten));

        if (!SystemUtils.IS_OS_MAC_OSX)
            cbkShowDescription.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F10, 0));

        JMenuItem miMarkFilmAsSeen = new JMenuItem("Filme als gesehen markieren");
        miMarkFilmAsSeen.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_G, KeyEvent.CTRL_DOWN_MASK));
        miMarkFilmAsSeen.addActionListener(markFilmAsSeenAction);

        JMenuItem miMarkFilmAsUnseen = new JMenuItem("Filme als ungesehen markieren");
        miMarkFilmAsUnseen.setAccelerator(
                KeyStroke.getKeyStroke(KeyEvent.VK_N, KeyEvent.CTRL_DOWN_MASK));
        miMarkFilmAsUnseen.addActionListener(markFilmAsUnseenAction);

        JMenuItem miSearchMediaCollection = new JMenuItem("Titel in der Mediensammlung suchen");
        miSearchMediaCollection.setAccelerator(
                KeyStroke.getKeyStroke(KeyEvent.VK_M, KeyEvent.CTRL_DOWN_MASK));
        miSearchMediaCollection.addActionListener(mediensammlungAction);

        menu.add(miPlayFilm);
        menu.add(miRecordFilm);
        menu.add(miBookmarkFilm);
        menu.addSeparator();
        menu.add(miMarkFilmAsSeen);
        menu.add(miMarkFilmAsUnseen);
        menu.addSeparator();
        menu.add(miOpenBlacklist);
        menu.addSeparator();
        menu.add(cbkShowDescription);
        menu.addSeparator();
        menu.add(miSearchMediaCollection);
    }

    private void setupFilmActionPanel() {
        fap = new FilmActionPanel(daten);
        JavaFxUtils.invokeInFxThreadAndWait(
                () -> fxFilmActionPanel.setScene(fap.getFilmActionPanelScene()));
    }

    private void setupPsetButtonsPanel() {
        JavaFxUtils.invokeInFxThreadAndWait(() -> {
            try {
                psetController = ButtonsPanelController.install(fxPsetButtonsPanel,this);
                psetController.setOnCloseRequest(e -> {
                    daten.getMessageBus().publishAsync(new ButtonsPanelVisibilityChangedEvent(false));
                    e.consume();
                });
                psetController.setupButtonLayout();
            } catch (Exception ex) {
                logger.error("setupPsetButtonsPanel", ex);
            }
        });

        var config = ApplicationConfiguration.getConfiguration();
        SwingUtilities.invokeLater(() -> fxPsetButtonsPanel.addComponentListener(new ComponentAdapter() {
            @Override
            public void componentShown(ComponentEvent e) {
                config.setProperty(ApplicationConfiguration.APPLICATION_BUTTONS_PANEL_VISIBLE, true);
            }

            @Override
            public void componentHidden(ComponentEvent e) {
                config.setProperty(ApplicationConfiguration.APPLICATION_BUTTONS_PANEL_VISIBLE, false);
            }
        }));

        final var visible = config.getBoolean(ApplicationConfiguration.APPLICATION_BUTTONS_PANEL_VISIBLE, false);
        fxPsetButtonsPanel.setVisible(visible);
    }

    private void setupDescriptionPanel() {
        JavaFxUtils.invokeInFxThreadAndWait(() -> {
            try {
                var descriptionPanelController = DescriptionPanelController.install(fxDescriptionPanel);
                SwingUtilities.invokeLater(() -> tabelle.getSelectionModel().addListSelectionListener(e -> {
                    Optional<DatenFilm> optFilm = getCurrentlySelectedFilm();
                    Platform.runLater(() -> descriptionPanelController.showFilmDescription(optFilm));
                }));
            } catch (Exception ex) {
                logger.error("setupDescriptionPanel", ex);
            }
        });
    }

    /**
     * Show description panel based on settings.
     */
    private void showDescriptionPanel() {
        fxDescriptionPanel.setVisible(ApplicationConfiguration.getConfiguration()
                .getBoolean(ApplicationConfiguration.FILM_SHOW_DESCRIPTION, true));
    }

    private void onComponentShown() {
        mediathekGui.tabPaneIndexProperty().setValue(TabPaneIndex.FILME);

        updateFilmData();
        setInfoStatusbar();
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
        focusedWindowMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_M, 0), ACTION_MAP_KEY_MEDIA_DB);
        focusedWindowMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_G, 0), ACTION_MAP_KEY_MARK_SEEN);
        focusedWindowMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_U, 0), ACTION_MAP_KEY_MARK_UNSEEN);

        final ActionMap actionMap = tabelle.getActionMap();
        actionMap.put(ACTION_MAP_KEY_PLAY_FILM, playAction);
        actionMap.put(ACTION_MAP_KEY_SAVE_FILM, saveFilmAction);
        actionMap.put(ACTION_MAP_KEY_BOOKMARK_FILM, bookmarkFilmAction);
        actionMap.put(
                ACTION_MAP_KEY_COPY_NORMAL_URL,
                new CopyUrlToClipboardAction(FilmResolution.Enum.NORMAL));
        actionMap.put(
                ACTION_MAP_KEY_COPY_HD_URL, new CopyUrlToClipboardAction(FilmResolution.Enum.HIGH_QUALITY));
        actionMap.put(
                ACTION_MAP_KEY_COPY_KLEIN_URL,
                new CopyUrlToClipboardAction(FilmResolution.Enum.LOW));
        actionMap.put(ACTION_MAP_KEY_MEDIA_DB, mediensammlungAction);
        actionMap.put(ACTION_MAP_KEY_MARK_SEEN, markFilmAsSeenAction);
        actionMap.put(ACTION_MAP_KEY_MARK_UNSEEN, markFilmAsUnseenAction);
    }

    private void setupCellRenderer() {
        final CellRendererFilme cellRenderer = new CellRendererFilme(daten);
        tabelle.setDefaultRenderer(Object.class, cellRenderer);
        tabelle.setDefaultRenderer(DatumFilm.class, cellRenderer);
        tabelle.setDefaultRenderer(Integer.class, cellRenderer);
    }

    private void start_init() {
        showDescriptionPanel();
        daten.getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
            @Override
            public void start(ListenerFilmeLadenEvent event) {
                loadTable();
            }

            @Override
            public void fertig(ListenerFilmeLadenEvent event) {
                loadTable();
                Platform.runLater(() -> fap.updateThemaBox());
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

        setVisFilterPanelAndLoad();
        tabelle.initTabelle();
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
        headerListener.setFontSizeChangeCapable(true);
        tabelle.getTableHeader().addMouseListener(headerListener);
    }

    @Handler
    private void handleDownloadHistoryChangedEvent(DownloadHistoryChangedEvent e) {
        SwingUtilities.invokeLater(() -> {
            if (fap.showUnseenOnly.getValue()) {
                loadTable();
            } else {
                tabelle.fireTableDataChanged(true);
            }
        });
    }

    @Handler
    private void handleButtonStart(ButtonStartEvent e) {
        SwingUtilities.invokeLater(() -> {
            tabelle.fireTableDataChanged(true);
            setInfoStatusbar();
        });
    }

    @Handler
    private void handleAboListChanged(AboListChangedEvent e) {
        SwingUtilities.invokeLater(this::loadTable);
    }

    @Handler
    private void handleBlacklistChangedEvent(BlacklistChangedEvent e) {
        SwingUtilities.invokeLater(this::loadTable);
    }

    @Handler
    private void handleStartEvent(StartEvent msg) {
        SwingUtilities.invokeLater(this::setInfoStatusbar);
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
                pSet = Daten.listePset.getListeSpeichern().getFirst();
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
                    daten.getListeDownloads().getDownloadUrlFilm(datenFilm.getUrl());
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
                    pSet = Daten.listePset.getListeSpeichern().getFirst();
                }
                datenDownload = new DatenDownload(
                        pSet, datenFilm, DatenDownload.QUELLE_DOWNLOAD, null, "",
                        pfad, "");
                datenDownload.arr[DatenDownload.DOWNLOAD_INFODATEI] = Boolean.toString(info);
                datenDownload.arr[DatenDownload.DOWNLOAD_SUBTITLE] = Boolean.toString(subtitle);

                daten.getListeDownloads().addMitNummer(datenDownload);
                daten.getMessageBus().publishAsync(new DownloadListChangedEvent());
                if (Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_DIALOG_DOWNLOAD_D_STARTEN))) {
                    // und evtl. auch gleich starten
                    datenDownload.startDownload();
                }
            } else {
                // dann alle Downloads im Dialog abfragen
                String aufloesung = "";
                if (fap.showOnlyHd.getValue()) {
                    aufloesung = FilmResolution.Enum.HIGH_QUALITY.toString();
                }
                DialogAddDownload dialog =
                        new DialogAddDownload(mediathekGui, datenFilm, pSet, aufloesung);
                dialog.setVisible(true);
            }
        }
    }

    private synchronized void bookmarkFilm() {
        var movies = getSelFilme();
        final long size = movies.size();
        if (size > 250) {
            JavaFxUtils.invokeInFxThreadAndWait(() -> {
                ButtonType yes = new ButtonType("Ja", ButtonBar.ButtonData.YES);
                ButtonType no = new ButtonType("Nein", ButtonBar.ButtonData.NO);
                Alert alert = new Alert(Alert.AlertType.WARNING,
                        String.format(
                                "Möchten Sie wirklich %d Einträge bearbeiten?\nDas Programm könnte während der Operation nicht reagieren.",
                                size),
                        yes,
                        no);
                alert.setTitle("Merkliste");
                alert.showAndWait().filter(r -> r == yes).ifPresent(response ->
                        SwingUtilities.invokeLater(() -> {
                            daten.getListeBookmarkList().checkAndBookmarkMovies(movies);
                            repaint();
                        }));
            });
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
            NoSelectionErrorDialog.show();
        } else if (pSet.istSpeichern()) {
            // wenn das pSet zum Speichern (über die Button) gewählt wurde,
            // weiter mit dem Dialog "Speichern"
            saveFilm(pSet);
        } else {
            // mit dem flvstreamer immer nur einen Filme starten
            final String aufloesung;
            if (fap.showOnlyHd.getValue()) {
                aufloesung = FilmResolution.Enum.HIGH_QUALITY.toString();
            } else aufloesung = "";

            Optional<DatenFilm> filmSelection = getCurrentlySelectedFilm();
            filmSelection.ifPresent(
                    film -> daten.starterClass.urlMitProgrammStarten(pSet, film, aufloesung));
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
    protected Optional<DatenFilm> getCurrentlySelectedFilm() {
        final int selectedTableRow = tabelle.getSelectedRow();
        if (selectedTableRow != -1) {
            final int modelIndex = tabelle.convertRowIndexToModel(selectedTableRow);
            return Optional.of((DatenFilm) tabelle.getModel().getValueAt(modelIndex, DatenFilm.FILM_REF));
        } else {
            return Optional.empty();
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
        int[] rows = tabelle.getSelectedRows();
        if (rows.length > 0) {
            for (int row : rows) {
                DatenFilm datenFilm = (DatenFilm) tabelle.getModel()
                        .getValueAt(tabelle.convertRowIndexToModel(row), DatenFilm.FILM_REF);
                arrayFilme.add(datenFilm);
            }
        } else {
            NoSelectionErrorDialog.show();
        }
        return arrayFilme;
    }

    /**
     * Update Film Information and description panel with updated film...
     */
    private void updateFilmData() {
        final Optional<DatenFilm> filmSelection = getCurrentlySelectedFilm();
        filmSelection.ifPresent(mediathekGui.getFilmInfoDialog()::updateCurrentFilm);
    }

    private void setInfoStatusbar() {
        daten.getMessageBus().publishAsync(new UpdateStatusBarLeftDisplayEvent());
    }

    private void reloadTable() {
        if (!stopBeob) {
            loadTable();
        }
    }

    private void setVisFilterPanelAndLoad() {
        // =======================================
        // und jezt die Anzeige
        this.updateUI();
        loadTable();
    }

    private void setupActionListeners() {
        Platform.runLater(() -> {
            final ChangeListener<Boolean> reloadTableListener =
                    (observable, oldValue, newValue) -> SwingUtilities.invokeLater(this::reloadTable);
            fap.showOnlyHd.addListener(reloadTableListener);
            fap.showSubtitlesOnly.addListener(reloadTableListener);
            fap.showNewOnly.addListener(reloadTableListener);
            fap.showBookMarkedOnly.addListener(reloadTableListener);
            fap.showUnseenOnly.addListener(reloadTableListener);
            fap.dontShowAbos.addListener(reloadTableListener);
            fap.dontShowTrailers.addListener(reloadTableListener);
            fap.dontShowSignLanguage.addListener(reloadTableListener);
            fap.dontShowAudioVersions.addListener(reloadTableListener);
            fap.showLivestreamsOnly.addListener(reloadTableListener);
            fap.filmLengthSlider.lowValueChangingProperty().addListener((observable, oldValue, newValue) -> {
                if (!newValue) {
                    SwingUtilities.invokeLater(this::reloadTable);
                }
            });
            fap.filmLengthSlider.highValueChangingProperty().addListener((observable, oldValue, newValue) -> {
                if (!newValue) {
                    SwingUtilities.invokeLater(this::reloadTable);
                }
            });
            fap.searchThroughDescription.addListener((os, o, n) -> {
                if (!fap.roSearchStringProperty.getReadOnlyProperty().isEmpty().get())
                    SwingUtilities.invokeLater(this::reloadTable);
            });

            setupSenderListListeners();

            setupZeitraumListener();

            fap.themaBox.setOnAction(evt -> {
                if (!fap.themaBox.getItems().isEmpty()) {
                    SwingUtilities.invokeLater(this::reloadTable);
                }
            });
        });

        setupShowFilmDescriptionMenuItem();
    }

    /**
     * Setup and show film description panel. Most of the setup is done in {@link GuiFilme} function.
     * Here we just display the panel
     */
    private void setupShowFilmDescriptionMenuItem() {
        cbkShowDescription.setSelected(
                ApplicationConfiguration.getConfiguration()
                        .getBoolean(ApplicationConfiguration.FILM_SHOW_DESCRIPTION, true));
        cbkShowDescription.addActionListener(
                l -> fxDescriptionPanel.setVisible(cbkShowDescription.isSelected()));
        cbkShowDescription.addItemListener(e -> ApplicationConfiguration.getConfiguration().setProperty(
                ApplicationConfiguration.FILM_SHOW_DESCRIPTION,
                cbkShowDescription.isSelected()));
        fxDescriptionPanel.addComponentListener(new ComponentAdapter() {
            @Override
            public void componentShown(ComponentEvent e) {
                cbkShowDescription.setSelected(true);
            }

            @Override
            public void componentHidden(ComponentEvent e) {
                cbkShowDescription.setSelected(false);
            }
        });
    }

    private void setupZeitraumListener() {
        PauseTransition trans = new PauseTransition(Duration.millis(250));
        trans.setOnFinished(evt -> {
            // reset sender filter first
            fap.senderList.getCheckModel().clearChecks();
            SwingUtilities.invokeLater(() -> {
                daten.getListeBlacklist().filterListe();
                loadTable();
            });
        });
        fap.zeitraumProperty.addListener((observable, oldValue, newValue) -> trans.playFromStart());
    }

    private void setupSenderListListeners() {
        PauseTransition filterSenderDelay = new PauseTransition(Duration.millis(750d));
        filterSenderDelay.setOnFinished(e -> SwingUtilities.invokeLater(this::reloadTable));
        fap.senderList.getCheckModel().getCheckedItems()
                .addListener((ListChangeListener<String>) c -> filterSenderDelay.playFromStart());
    }

    private synchronized void loadTable() {
        final var messageBus = Daten.getInstance().getMessageBus();
        messageBus.publishAsync(new TableModelChangeEvent(true));
        try {
            stopBeob = true;
            tabelle.getSpalten();
            tabelle.setEnabled(false);

            final GuiFilmeModelHelper helper = new GuiFilmeModelHelper(fap, daten.getListeFilmeNachBlackList(), historyController);
            final var model = helper.getFilteredTableModel();
            tabelle.setModel(model);

            tabelle.setEnabled(true);
            setInfoStatusbar();
            tabelle.setSpalten();
            updateFilmData();
            stopBeob = false;
        } catch (Exception ex) {
            logger.error("loadTable", ex);
        }

        tabelle.scrollToSelection();

        messageBus.publishAsync(new TableModelChangeEvent(false));
    }

    public class FilterFilmAction extends AbstractAction {

        @Override
        public void actionPerformed(ActionEvent e) {
            loadTable();
        }
    }

    public class PlayFilmAction extends AbstractAction {

        @Override
        public synchronized void actionPerformed(ActionEvent e) {
            DatenPset pset = Daten.listePset.getPsetAbspielen();
            if (pset != null) {
                playerStarten(pset);
            } else {
                MVMessageDialog.showMessageDialog(mediathekGui,
                        "Im Menü unter \"Datei->Einstellungen->Set bearbeiten\" ein Programm zum Abspielen festlegen.",
                        "kein Videoplayer!",
                        JOptionPane.INFORMATION_MESSAGE);
            }
        }
    }

    public class SaveFilmAction extends AbstractAction {

        @Override
        public void actionPerformed(ActionEvent e) {
            saveFilm(null);
        }
    }

    public class BookmarkFilmAction extends AbstractAction {

        @Override
        public void actionPerformed(ActionEvent e) {
            bookmarkFilm();
        }
    }

    public class BookmarkManageListAction extends AbstractAction {
        @Override
        public void actionPerformed(ActionEvent ae) {
            showBookmarkWindow();
        }
    }

    private class CopyUrlToClipboardAction extends AbstractAction {
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

    private class MediensammlungAction extends AbstractAction {

        @Override
        public void actionPerformed(ActionEvent e) {
            final Optional<DatenFilm> filmSelection = getCurrentlySelectedFilm();
            filmSelection.ifPresent(film -> {
                MVConfig.add(MVConfig.Configs.SYSTEM_MEDIA_DB_DIALOG_ANZEIGEN, Boolean.TRUE.toString());
                final var mediaDB = mediathekGui.getMediaDatabaseDialog();
                mediaDB.setVis();
                mediaDB.setFilter(film.getTitle());
            });
        }
    }

    /**
     * Implements the context menu for tab film.
     */
    class TableContextMenuHandler extends MouseAdapter {
        private static final String COPY_URL_STR = "URL kopieren";
        private final BeobPrint beobPrint = new BeobPrint();
        private final BeobAbo beobAbo = new BeobAbo(false);
        private final BeobAbo beobAboMitTitel = new BeobAbo(true);
        private final BeobBlacklist beobBlacklistSender = new BeobBlacklist(true, false);
        private final BeobBlacklist beobBlacklistSenderThema = new BeobBlacklist(true, true);
        private final BeobBlacklist beobBlacklistThema = new BeobBlacklist(false, true);
        private final JMenuItem miPlay = createPlayItem();
        private final JMenuItem miSave = createSaveFilmItem();
        private final JMenuItem miBookmark = createBookmarkFilmItem();
        private final ShowFilmInformationAction showFilmInformationAction =
                new ShowFilmInformationAction(false);
        private final ActionListener unseenActionListener = new BeobHistory(false);
        private final ActionListener seenActionListener = new BeobHistory(true);
        private final JDownloadHelper jDownloadHelper = new JDownloadHelper();
        private Point p;
        private JMenuItem miPrintTable;

        TableContextMenuHandler() {
            createStaticMenuEntries();
        }

        private void createStaticMenuEntries() {
            miPrintTable = new JMenuItem("Tabelle drucken");
            miPrintTable.addActionListener(beobPrint);
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
                    // filmAbspielen_();
                    if (!mediathekGui.getFilmInfoDialog().isVisible()) {
                        mediathekGui.getFilmInfoDialog().showInfo();
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
                    case DatenFilm.FILM_ABSPIELEN:
                        Optional<DatenFilm> filmSelection = getCurrentlySelectedFilm();
                        filmSelection.ifPresent(datenFilm -> {
                            boolean stop = false;
                            final DatenDownload datenDownload =
                                    daten.getListeDownloadsButton().getDownloadUrlFilm(datenFilm.getUrl());
                            if (datenDownload != null) {
                                if (datenDownload.start != null) {
                                    if (datenDownload.start.status == Start.STATUS_RUN) {
                                        stop = true;
                                        daten.getListeDownloadsButton().delDownloadButton(datenFilm.getUrl());
                                    }
                                }
                            }
                            if (!stop) {
                                playAction.actionPerformed(null);
                            }
                        });
                        break;

                    case DatenFilm.FILM_AUFZEICHNEN:
                        saveFilm(null);
                        break;

                    case DatenFilm.FILM_MERKEN:
                        bookmarkFilm();
                        break;

                    default:
                        break;
                }
            }
        }

        private JMenuItem createPlayItem() {
            JMenuItem item = new JMenuItem("Film abspielen");
            item.setIcon(IconFontSwing.buildIcon(FontAwesome.PLAY, 16));
            item.addActionListener(playAction);
            return item;
        }

        private JMenuItem createSaveFilmItem() {
            JMenuItem item = new JMenuItem("Film aufzeichnen");
            item.setIcon(IconFontSwing.buildIcon(FontAwesome.DOWNLOAD, 16));
            item.addActionListener(saveFilmAction);
            return item;
        }

        private JMenuItem createBookmarkFilmItem() {
            JMenuItem item = new JMenuItem("Film merken");
            item.setIcon(IconFontSwing.buildIcon(FontAwesome.BOOKMARK_O, 16));
            item.addActionListener(bookmarkFilmAction);
            return item;
        }


        private void showMenu(MouseEvent evt) {
            p = evt.getPoint();
            final int nr = tabelle.rowAtPoint(p);
            if (nr >= 0) {
                tabelle.setRowSelectionInterval(nr, nr);
            }

            JPopupMenu jPopupMenu = new JPopupMenu();

            jPopupMenu.add(miPlay);
            jPopupMenu.add(miSave);
            jPopupMenu.add(miBookmark);
            jPopupMenu.addSeparator();

            JMenu submenueAbo = new JMenu("Abo");
            jPopupMenu.add(submenueAbo);
            // Abo anlegen
            JMenuItem itemAboLoeschen = new JMenuItem("Abo Löschen");
            JMenuItem itemAbo = new JMenuItem("Abo mit Sender und Thema anlegen");
            JMenuItem itemAboMitTitel = new JMenuItem("Abo mit Sender und Thema und Titel anlegen");
            JMenuItem itemChangeAboFilter = new JMenuItem("Abo ändern");

            Optional<DatenFilm> res = getFilm(nr);
            res.ifPresent(film -> {
                if ((daten.getListeAbo().getAboFuerFilm_schnell(film, false /*die Länge nicht prüfen*/))
                        != null) {
                    // gibts schon, dann löschen
                    itemAbo.setEnabled(false);
                    itemAboMitTitel.setEnabled(false);
                    itemAboLoeschen.addActionListener(beobAbo);

                    // dann können wir auch ändern
                    itemChangeAboFilter.addActionListener(new BeobChangeAbo());
                } else {
                    itemAboLoeschen.setEnabled(false);
                    itemChangeAboFilter.setEnabled(false);
                    // neues Abo anlegen
                    itemAbo.addActionListener(beobAbo);
                    itemAboMitTitel.addActionListener(beobAboMitTitel);
                }
                // update Bookmark state
                miBookmark.setText(
                        film.isBookmarked() ? "Film aus Merkliste entfernen" : "Film merken");
            });

            submenueAbo.add(itemAboLoeschen);
            submenueAbo.add(itemChangeAboFilter);
            submenueAbo.add(itemAbo);
            submenueAbo.add(itemAboMitTitel);

            // Programme einblenden
            JMenu submenue = new JMenu("Film mit Set starten");
            jPopupMenu.add(submenue);
            ListePset liste = Daten.listePset.getListeButton();
            for (DatenPset pset : liste) {
                if (pset.getListeProg().isEmpty() && pset.arr[DatenPset.PROGRAMMSET_NAME].isEmpty()) {
                    // ein "leeres" Pset, Platzhalter
                    continue;
                }
                Color col = pset.getFarbe();
                JMenuItem item = new JMenuItem(pset.arr[DatenPset.PROGRAMMSET_NAME]);
                if (pset.getListeProg().isEmpty()) {
                    if (col != null) {
                        item.setForeground(col);
                    }
                } else {
                    item.addActionListener(l -> playerStarten(pset));
                    if (col != null) {
                        item.setBackground(col);
                    }
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

            res.ifPresent(film -> jDownloadHelper.installContextMenu(film, jPopupMenu));
            // Url
            res.ifPresent(film -> {
                if (film.isPlayList()) {
                    var miCopyPlayListUrl = new JMenuItem(COPY_URL_STR);
                    miCopyPlayListUrl.addActionListener(l -> GuiFunktionen.copyToClipboard(film.getUrl()));
                    jPopupMenu.addSeparator();
                    jPopupMenu.add(miCopyPlayListUrl);
                } else {
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
                        jPopupMenu.addSeparator();

                        final ActionListener copyNormalUrlListener = e -> GuiFunktionen.copyToClipboard(uNormal);
                        if (!uHd.isEmpty() || !uLow.isEmpty()) {
                            JMenu submenueURL = new JMenu(COPY_URL_STR);
                            // HD
                            if (!uHd.isEmpty()) {
                                item = new JMenuItem("in höchster/hoher Qualität");
                                item.addActionListener(
                                        e -> GuiFunktionen.copyToClipboard(film.getUrlFuerAufloesung(FilmResolution.Enum.HIGH_QUALITY)));
                                submenueURL.add(item);
                            }

                            // normale Auflösung, gibts immer
                            item = new JMenuItem("in mittlerer Qualität");
                            item.addActionListener(copyNormalUrlListener);
                            submenueURL.add(item);

                            // kleine Auflösung
                            if (!uLow.isEmpty()) {
                                item = new JMenuItem("in niedriger Qualität");
                                item.addActionListener(
                                        e -> GuiFunktionen.copyToClipboard(film.getUrlFuerAufloesung(FilmResolution.Enum.LOW)));
                                submenueURL.add(item);
                            }
                            jPopupMenu.add(submenueURL);
                        } else {
                            item = new JMenuItem("Film-URL kopieren");
                            item.addActionListener(copyNormalUrlListener);
                            jPopupMenu.add(item);
                        }
                    }
                    if (!film.getUrlSubtitle().isEmpty()) {
                        item = new JMenuItem("Untertitel-URL kopieren");
                        item.addActionListener(e -> GuiFunktionen.copyToClipboard(film.getUrlSubtitle()));
                        jPopupMenu.add(item);
                    }
                }
            });

            jPopupMenu.addSeparator();

            // Film in der MediaDB suchen
            res.ifPresent(film -> {
                JMenuItem itemDb = new JMenuItem("Titel in der Mediensammlung suchen");
                itemDb.addActionListener(mediensammlungAction);
                jPopupMenu.add(itemDb);
            });

            // Drucken
            jPopupMenu.add(miPrintTable);

            jPopupMenu.add(showFilmInformationAction);

            // History
            res.ifPresent(film -> {
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
                        jPopupMenu.add(miHistory);
                    }
                }
            });
            // anzeigen
            jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
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

        private class BeobChangeAbo implements ActionListener {

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
                                DialogEditAbo dialog =
                                        new DialogEditAbo(mediathekGui, true, datenAbo, false /*onlyOne*/);
                                dialog.setVisible(true);
                                if (dialog.ok) {
                                    daten.getListeAbo().aenderungMelden();
                                }
                            }
                        });
                        stopBeob = false;
                    }
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
