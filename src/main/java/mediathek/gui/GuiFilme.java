package mediathek.gui;

import com.google.common.collect.Lists;
import javafx.animation.PauseTransition;
import javafx.application.Platform;
import javafx.beans.value.ChangeListener;
import javafx.collections.ListChangeListener;
import javafx.embed.swing.JFXPanel;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.control.TabPane;
import javafx.util.Duration;
import jiconfont.icons.FontAwesome;
import jiconfont.swing.IconFontSwing;
import mediathek.MediathekGui;
import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.controller.starter.Start;
import mediathek.daten.*;
import mediathek.filmeSuchen.ListenerFilmeLaden;
import mediathek.filmeSuchen.ListenerFilmeLadenEvent;
import mediathek.gui.actions.ShowBlacklistDialogAction;
import mediathek.gui.actions.ShowFilmInformationAction;
import mediathek.gui.dialog.DialogAboNoSet;
import mediathek.gui.dialog.DialogAddDownload;
import mediathek.gui.dialog.DialogAddMoreDownload;
import mediathek.gui.dialog.DialogEditAbo;
import mediathek.gui.messages.*;
import mediathek.gui.messages.history.DownloadHistoryChangedEvent;
import mediathek.gui.tab_film.BeobOpenPlayer;
import mediathek.gui.tab_film.IButtonPanelController;
import mediathek.gui.tab_film.SwingButtonPanelController;
import mediathek.javafx.descriptionPanel.DescriptionPanelController;
import mediathek.javafx.filmtab.FilmTabInfoPane;
import mediathek.javafx.filterpanel.FilmActionPanel;
import mediathek.tool.*;
import mediathek.tool.cellrenderer.CellRendererFilme;
import mediathek.tool.listener.BeobTableHeader;
import mediathek.tool.models.TModelFilm;
import mediathek.tool.table.MVFilmTable;
import net.engio.mbassy.listener.Handler;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.apache.commons.lang3.SystemUtils;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.awt.print.PrinterException;
import java.util.ArrayList;
import java.util.Optional;

@SuppressWarnings("serial")
public class GuiFilme extends AGuiTabPanel {

    public static final String NAME = "Filme";
    private static final String ACTION_MAP_KEY_PLAY_FILM = "film_abspielen";
    private static final String ACTION_MAP_KEY_SAVE_FILM = "download_film";
    private static final String ACTION_MAP_KEY_COPY_NORMAL_URL = "copy_url";
    private static final String ACTION_MAP_KEY_COPY_HD_URL = "copy_url_hd";
    private static final String ACTION_MAP_KEY_COPY_KLEIN_URL = "copy_url_klein";
    private static final String ACTION_MAP_KEY_MEDIA_DB = "mediadb";
    private static final String ACTION_MAP_KEY_MARK_SEEN = "seen";
    private static final String ACTION_MAP_KEY_MARK_UNSEEN = "unseen";
    private static final int[] HIDDEN_COLUMNS = new int[]{
            DatenFilm.FILM_ABSPIELEN,
            DatenFilm.FILM_AUFZEICHNEN
    };
    public final FilterFilmAction filterFilmAction = new FilterFilmAction();
    public final PlayFilmAction playAction = new PlayFilmAction();
    public final SaveFilmAction saveFilmAction = new SaveFilmAction();
    public final MarkFilmAsSeenAction markFilmAsSeenAction = new MarkFilmAsSeenAction();
    public final MarkFilmAsUnseenAction markFilmAsUnseenAction = new MarkFilmAsUnseenAction();
    private final JScrollPane filmListScrollPane = new JScrollPane();
    private final JPanel descriptionPanel = new JPanel();
    private final JPanel extensionArea = new JPanel();
    private final JCheckBoxMenuItem cbkShowDescription = new JCheckBoxMenuItem("Beschreibung anzeigen");
    /**
     * The JavaFx Film action popup panel.
     */
    public FilmActionPanel fap;
    public MediensammlungAction mediensammlungAction = new MediensammlungAction();
    /**
     * The swing helper panel FilmAction bar.
     */
    private JFXPanel fxFilmActionPanel;
    private boolean stopBeob = false;
    private FilmTabInfoPane filmInfoLabel;
    private JFXPanel fxDescriptionPanel;
    private IButtonPanelController buttonPanelController;

    public GuiFilme(Daten aDaten, MediathekGui mediathekGui) {
        super();
        daten = aDaten;
        this.mediathekGui = mediathekGui;

        setLayout(new BorderLayout());

        createFilmListArea();
        createExtensionArea();
        createFilmActionPanel();
        createDescriptionPanel();

        setupFilmListTable();

        installTabInfoStatusBarControl();

        setupFilmSelectionPropertyListener(mediathekGui);
        setupButtonPanel();
        setupDescriptionPanel();
        setupFilmActionPanel();

        start_init();
        start_addListener();

        setupActionListeners();
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

    private void installTabInfoStatusBarControl() {
        final var leftItems = mediathekGui.getStatusBarController().getStatusBar().getLeftItems();

        Platform.runLater(() -> {
            filmInfoLabel = new FilmTabInfoPane(daten, this);
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

    private void createFilmListArea() {
        add(filmListScrollPane, BorderLayout.CENTER);
    }

    private void createDescriptionPanel() {
        descriptionPanel.setLayout(new BorderLayout());
        extensionArea.add(descriptionPanel, new CC().cell(0, 0));

        fxDescriptionPanel = new JFXPanel();
        descriptionPanel.add(fxDescriptionPanel, BorderLayout.CENTER);
    }

    private void createExtensionArea() {
        extensionArea.setLayout(new MigLayout(
                new LC().insets("0").hideMode(3).gridGap("0", "0"), //NON-NLS
                // columns
                new AC()
                        .grow().fill(),
                // rows
                new AC()
                        .grow().fill().gap()
                        .grow().fill()));

        add(extensionArea, BorderLayout.SOUTH);
    }

    public void installMenuEntries(JMenu menu) {
        JMenuItem miPlayFilm = new JMenuItem("Film abspielen");
        if (SystemUtils.IS_OS_MAC_OSX)
            miPlayFilm.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F6, InputEvent.META_DOWN_MASK));
        else
            miPlayFilm.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_P, KeyEvent.CTRL_DOWN_MASK));
        miPlayFilm.setIcon(IconFontSwing.buildIcon(FontAwesome.PLAY, 16));
        miPlayFilm.addActionListener(playAction);

        JMenuItem miRecordFilm = new JMenuItem("Film aufzeichnen");
        if (SystemUtils.IS_OS_MAC_OSX)
            miRecordFilm.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F7, InputEvent.META_DOWN_MASK));
        else
            miRecordFilm.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_D, KeyEvent.CTRL_DOWN_MASK));
        miRecordFilm.setIcon(IconFontSwing.buildIcon(FontAwesome.DOWNLOAD, 16));
        miRecordFilm.addActionListener(saveFilmAction);

        JMenuItem miOpenBlacklist = new JMenuItem("Blacklist öffnen");
        if (SystemUtils.IS_OS_MAC_OSX)
            miOpenBlacklist.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F9, InputEvent.META_DOWN_MASK));
        else
            miOpenBlacklist.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_B, KeyEvent.CTRL_DOWN_MASK));
        miOpenBlacklist.setAction(new ShowBlacklistDialogAction(mediathekGui, daten));

        if (!SystemUtils.IS_OS_MAC_OSX)
            cbkShowDescription.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F10, 0));

        JMenuItem miMarkFilmAsSeen = new JMenuItem("Filme als gesehen markieren");
        miMarkFilmAsSeen.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_G, KeyEvent.CTRL_DOWN_MASK));
        miMarkFilmAsSeen.setIcon(Icons.ICON_MENUE_HISTORY_ADD);
        miMarkFilmAsSeen.addActionListener(markFilmAsSeenAction);

        JMenuItem miMarkFilmAsUnseen = new JMenuItem("Filme als ungesehen markieren");
        miMarkFilmAsUnseen.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N, KeyEvent.CTRL_DOWN_MASK));
        miMarkFilmAsUnseen.setIcon(Icons.ICON_MENUE_HISTORY_REMOVE);
        miMarkFilmAsUnseen.addActionListener(markFilmAsUnseenAction);

        JMenuItem miSearchMediaCollection = new JMenuItem("Titel in der Mediensammlung suchen");
        miSearchMediaCollection.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_M, KeyEvent.CTRL_DOWN_MASK));
        miSearchMediaCollection.addActionListener(mediensammlungAction);

        menu.add(miPlayFilm);
        menu.add(miRecordFilm);
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
        Platform.runLater(() -> fxFilmActionPanel.setScene(fap.getFilmActionPanelScene()));
    }

    private void setupDescriptionPanel() {
        Platform.runLater(() -> {
            try {
                FXMLLoader loader = new FXMLLoader();
                loader.setLocation(Konstanten.FXML_FILM_DESCRIPTION_PANEL_URL);

                TabPane descriptionPane = loader.load();
                final DescriptionPanelController descriptionPanelController = loader.getController();
                descriptionPanelController.setOnCloseRequest(e -> {
                    SwingUtilities.invokeLater(() -> descriptionPanel.setVisible(false));
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

    /**
     * Show description panel based on settings.
     */
    private void showDescriptionPanel() {
        descriptionPanel.setVisible(ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.FILM_SHOW_DESCRIPTION, true));
    }

    public void onComponentShown() {
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
        focusedWindowMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_H, 0), ACTION_MAP_KEY_COPY_HD_URL);
        focusedWindowMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_N, 0), ACTION_MAP_KEY_COPY_NORMAL_URL);
        focusedWindowMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_K, 0), ACTION_MAP_KEY_COPY_KLEIN_URL);
        focusedWindowMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_M, 0), ACTION_MAP_KEY_MEDIA_DB);
        focusedWindowMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_G, 0), ACTION_MAP_KEY_MARK_SEEN);
        focusedWindowMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_U, 0), ACTION_MAP_KEY_MARK_UNSEEN);

        final ActionMap actionMap = tabelle.getActionMap();
        actionMap.put(ACTION_MAP_KEY_PLAY_FILM, playAction);
        actionMap.put(ACTION_MAP_KEY_SAVE_FILM, saveFilmAction);
        actionMap.put(ACTION_MAP_KEY_COPY_NORMAL_URL, new CopyUrlToClipboardAction(FilmResolution.AUFLOESUNG_NORMAL));
        actionMap.put(ACTION_MAP_KEY_COPY_HD_URL, new CopyUrlToClipboardAction(FilmResolution.AUFLOESUNG_HD));
        actionMap.put(ACTION_MAP_KEY_COPY_KLEIN_URL, new CopyUrlToClipboardAction(FilmResolution.AUFLOESUNG_KLEIN));
        actionMap.put(ACTION_MAP_KEY_MEDIA_DB, mediensammlungAction);
        actionMap.put(ACTION_MAP_KEY_MARK_SEEN, markFilmAsSeenAction);
        actionMap.put(ACTION_MAP_KEY_MARK_UNSEEN, markFilmAsUnseenAction);
    }

    private void setupCellRenderer() {
        final CellRendererFilme cellRenderer = new CellRendererFilme(daten);
        tabelle.setDefaultRenderer(Object.class, cellRenderer);
        tabelle.setDefaultRenderer(Datum.class, cellRenderer);
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
        tabelle.addMouseListener(new BeobMausTabelle());
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
        final var headerListener = new BeobTableHeader(tabelle,
                Daten.spaltenAnzeigenFilme,
                HIDDEN_COLUMNS,
                new int[]{DatenFilm.FILM_ABSPIELEN, DatenFilm.FILM_AUFZEICHNEN},
                true, MVConfig.Configs.SYSTEM_TAB_FILME_LINEBREAK);
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

    private void start_addListener() {
        //register message bus handler
        daten.getMessageBus().subscribe(this);

        Listener.addListener(new Listener(Listener.EREIGNIS_LISTE_PSET, GuiFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                buttonPanelController.setupButtons();
                buttonPanelController.setVisible(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN)));
            }
        });

        Listener.addListener(new Listener(Listener.EREIGNIS_BLACKLIST_GEAENDERT, GuiFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                loadTable();
            }
        });
    }

    @Handler
    private void handleStartEvent(StartEvent msg) {
        SwingUtilities.invokeLater(this::setInfoStatusbar);
    }

    private synchronized void saveFilm(DatenPset pSet) {
        if (Daten.listePset.getListeSpeichern().isEmpty()) {
            new DialogAboNoSet(mediathekGui, daten).setVisible(true);
            // Satz mit x, war wohl nix
            return;
        }

        ArrayList<DatenFilm> liste = getSelFilme();
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
            DatenDownload datenDownload = daten.getListeDownloads().getDownloadUrlFilm(datenFilm.getUrl());
            if (datenDownload != null) {
                int ret = JOptionPane.showConfirmDialog(mediathekGui, "Download für den Film existiert bereits.\n"
                        + "Nochmal anlegen?", "Anlegen?", JOptionPane.YES_NO_OPTION);
                if (ret != JOptionPane.OK_OPTION) {
                    continue;
                }
            }

            if (standard) {
                if (pSet == null) {
                    pSet = Daten.listePset.getListeSpeichern().getFirst();
                }
                datenDownload = new DatenDownload(pSet, datenFilm, DatenDownload.QUELLE_DOWNLOAD, null, "", pfad, ""/*Auflösung*/);
                datenDownload.arr[DatenDownload.DOWNLOAD_INFODATEI] = Boolean.toString(info);
                datenDownload.arr[DatenDownload.DOWNLOAD_SUBTITLE] = Boolean.toString(subtitle);

                daten.getListeDownloads().addMitNummer(datenDownload);
                daten.getMessageBus().publishAsync(new DownloadListChangedEvent());
                if (Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_DIALOG_DOWNLOAD_D_STARTEN))) {
                    // und evtl. auch gleich starten
                    datenDownload.startDownload(daten);
                }
            } else {
                //dann alle Downloads im Dialog abfragen
                String aufloesung = "";
                if (fap.showOnlyHd.getValue()) {
                    aufloesung = FilmResolution.AUFLOESUNG_HD;
                }
                DialogAddDownload dialog = new DialogAddDownload(mediathekGui, daten, datenFilm, pSet, aufloesung);
                dialog.setVisible(true);
            }
        }
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
                aufloesung = FilmResolution.AUFLOESUNG_HD;
            } else
                aufloesung = "";

            Optional<DatenFilm> filmSelection = getCurrentlySelectedFilm();
            filmSelection.ifPresent(film -> daten.starterClass.urlMitProgrammStarten(pSet, film, aufloesung));
        }
    }

    /**
     * Return the film object from a table row.
     * As this can also be null we will return an Optional to prevent NPEs inside the caller.
     *
     * @param zeileTabelle table row.
     * @return Optional object to a film object.
     */
    private Optional<DatenFilm> getFilm(final int zeileTabelle) {
        if (zeileTabelle >= 0 && zeileTabelle < tabelle.getRowCount()) {
            return Optional.of((DatenFilm) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(zeileTabelle), DatenFilm.FILM_REF));
        } else {
            return Optional.empty();
        }
    }

    private Optional<DatenFilm> getCurrentlySelectedFilm() {
        final int selectedTableRow = tabelle.getSelectedRow();
        if (selectedTableRow >= 0) {
            return Optional.of((DatenFilm) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(selectedTableRow), DatenFilm.FILM_REF));
        } else {
            return Optional.empty();
        }
    }

    private ArrayList<DatenFilm> getSelFilme() {
        ArrayList<DatenFilm> arrayFilme = new ArrayList<>();
        int[] rows = tabelle.getSelectedRows();
        if (rows.length > 0) {
            for (int row : rows) {
                DatenFilm datenFilm = (DatenFilm) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(row), DatenFilm.FILM_REF);
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

    @Handler
    private void handlePsetButtonChangedEvent(PsetNumberOfButtonsChangedEvent e) {
        SwingUtilities.invokeLater(() -> buttonPanelController.setupButtons());
    }

    public IButtonPanelController getButtonPanelController() {
        return buttonPanelController;
    }

    private void setupButtonPanel() {
        buttonPanelController = new SwingButtonPanelController(this, extensionArea);
        buttonPanelController.setupButtons();
        // und jetzt noch anzeigen
        buttonPanelController.setVisible(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN)));
    }

    private void reloadTable() {
        if (!stopBeob) {
            loadTable();
        }
    }

    private void setVisFilterPanelAndLoad() {
        //=======================================
        // und jezt die Anzeige
        this.updateUI();
        loadTable();
    }

    private void setupActionListeners() {
        Platform.runLater(() -> {
            final ChangeListener<Boolean> reloadTableListener = (observable, oldValue, newValue) -> SwingUtilities.invokeLater(this::reloadTable);
            fap.showOnlyHd.addListener(reloadTableListener);
            fap.showSubtitlesOnly.addListener(reloadTableListener);
            fap.showNewOnly.addListener(reloadTableListener);
            fap.showUnseenOnly.addListener(reloadTableListener);
            fap.dontShowAbos.addListener(reloadTableListener);
            fap.dontShowTrailers.addListener(reloadTableListener);
            fap.dontShowSignLanguage.addListener(reloadTableListener);
            fap.dontShowAudioVersions.addListener(reloadTableListener);
            fap.showLivestreamsOnly.addListener(reloadTableListener);
            fap.filmLengthSlider.lowValueChangingProperty().addListener((observable, oldValue, newValue) -> {
                if (!newValue)
                    SwingUtilities.invokeLater(this::reloadTable);
            });
            fap.filmLengthSlider.highValueChangingProperty().addListener((observable, oldValue, newValue) -> {
                if (!newValue)
                    SwingUtilities.invokeLater(this::reloadTable);
            });
            fap.searchThroughDescription.addListener((os, o, n) -> {
                if (!fap.roSearchStringProperty.getReadOnlyProperty().isEmpty().get())
                    SwingUtilities.invokeLater(this::reloadTable);
            });

            setupSenderListListeners();

            setupZeitraumListener();

            fap.themaBox.setOnAction(evt -> SwingUtilities.invokeLater(this::reloadTable));
        });

        setupShowFilmDescriptionMenuItem();
    }

    /**
     * Setup and show film description panel.
     * Most of the setup is done in {@link GuiFilme} function.
     * Here we just display the panel
     */
    private void setupShowFilmDescriptionMenuItem() {
        cbkShowDescription.setSelected(ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.FILM_SHOW_DESCRIPTION, true));
        cbkShowDescription.addActionListener(l -> descriptionPanel.setVisible(cbkShowDescription.isSelected()));
        cbkShowDescription.addItemListener(e -> ApplicationConfiguration.getConfiguration().setProperty(ApplicationConfiguration.FILM_SHOW_DESCRIPTION, cbkShowDescription.isSelected()));
        descriptionPanel.addComponentListener(new ComponentAdapter() {
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
                    //reset sender filter first
                    fap.senderList.getCheckModel().clearChecks();
                    SwingUtilities.invokeLater(() -> {
                                daten.getListeBlacklist().filterListe();
                                loadTable();
                            }
                    );
                }
        );
        fap.zeitraumProperty.addListener((observable, oldValue, newValue) -> trans.playFromStart());
    }

    private void setupSenderListListeners() {
        PauseTransition filterSenderDelay = new PauseTransition(Duration.millis(750d));
        filterSenderDelay.setOnFinished(e -> SwingUtilities.invokeLater(this::reloadTable));
        fap.senderList.getCheckModel()
                .getCheckedItems().
                addListener((ListChangeListener<String>) c -> filterSenderDelay.playFromStart());
    }

    private synchronized void loadTable() {
        try {
            stopBeob = true;
            tabelle.getSpalten();

            GuiFilmeModelHelper helper = new GuiFilmeModelHelper(fap, daten, tabelle);
            helper.prepareTableModel();

            setInfoStatusbar();
            tabelle.setSpalten();
            updateFilmData();
            stopBeob = false;
        } catch (Exception ex) {
            Log.errorLog(558965421, ex);
        }

        tabelle.scrollToSelection();

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
                MVMessageDialog.showMessageDialog(mediathekGui, "Im Menü unter \"Datei->Einstellungen->Set bearbeiten\" ein Programm zum Abspielen festlegen.",
                        "kein Videoplayer!", JOptionPane.INFORMATION_MESSAGE);
            }
        }
    }

    public class SaveFilmAction extends AbstractAction {

        @Override
        public void actionPerformed(ActionEvent e) {
            saveFilm(null);
        }
    }

    private class CopyUrlToClipboardAction extends AbstractAction {
        private final String resolution;

        public CopyUrlToClipboardAction(String resolution) {
            this.resolution = resolution;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            Optional<DatenFilm> filmSelection = getCurrentlySelectedFilm();
            filmSelection.ifPresent(film -> GuiFunktionen.copyToClipboard(film.getUrlFuerAufloesung(resolution)));
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

    private class MarkFilmAsSeenAction extends AbstractAction {

        @Override
        public void actionPerformed(ActionEvent e) {
            daten.getSeenHistoryController().markAsSeen(getSelFilme());
        }
    }

    private class MarkFilmAsUnseenAction extends AbstractAction {

        @Override
        public void actionPerformed(ActionEvent e) {
            daten.getSeenHistoryController().markAsUnseen(getSelFilme());
        }
    }

    public class BeobMausTabelle extends MouseAdapter {
        //rechhte Maustaste in der Tabelle

        private final BeobPrint beobPrint = new BeobPrint();
        private final BeobAbo beobAbo = new BeobAbo(false /* mit Titel */);
        private final BeobAbo beobAboMitTitel = new BeobAbo(true /* mit Titel */);
        private final BeobBlacklist beobBlacklistSender = new BeobBlacklist(true, false);
        private final BeobBlacklist beobBlacklistSenderThema = new BeobBlacklist(true, true);
        private final BeobBlacklist beobBlacklistThema = new BeobBlacklist(false, true);
        private final JMenuItem miPlay = createPlayItem();
        private final JMenuItem miSave = createSaveFilmItem();
        private final ShowFilmInformationAction showFilmInformationAction = new ShowFilmInformationAction(false);
        private final ActionListener unseenActionListener = new BeobHistory(false);
        private final ActionListener seenActionListener = new BeobHistory(true);
        private Point p;

        public BeobMausTabelle() {
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
                    //filmAbspielen_();
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
                if (tabelle.convertColumnIndexToModel(column) == DatenFilm.FILM_ABSPIELEN) {
                    Optional<DatenFilm> filmSelection = getCurrentlySelectedFilm();
                    filmSelection.ifPresent(datenFilm -> {
                        boolean stop = false;
                        final DatenDownload datenDownload = daten.getListeDownloadsButton().getDownloadUrlFilm(datenFilm.getUrl());
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
                } else if (tabelle.convertColumnIndexToModel(column) == DatenFilm.FILM_AUFZEICHNEN) {
                    saveFilm(null);
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

        private void showMenu(MouseEvent evt) {
            p = evt.getPoint();
            final int nr = tabelle.rowAtPoint(p);
            if (nr >= 0) {
                tabelle.setRowSelectionInterval(nr, nr);
            }

            JPopupMenu jPopupMenu = new JPopupMenu();

            //Thema laden
            jPopupMenu.add(miPlay);
            //Url
            jPopupMenu.add(miSave);

            //##Trenner##
            jPopupMenu.addSeparator();
            //##Trenner##
            JMenu submenueAbo = new JMenu("Abo");
            jPopupMenu.add(submenueAbo);
            //Abo anlegen
            JMenuItem itemAboLoeschen = new JMenuItem("Abo Löschen");
            JMenuItem itemAbo = new JMenuItem("Abo mit Sender und Thema anlegen");
            JMenuItem itemAboMitTitel = new JMenuItem("Abo mit Sender und Thema und Titel anlegen");
            JMenuItem itemChangeAboFilter = new JMenuItem("Abo ändern");

            Optional<DatenFilm> res = getFilm(nr);
            res.ifPresent(film -> {
                if ((daten.getListeAbo().getAboFuerFilm_schnell(film, false /*die Länge nicht prüfen*/)) != null) {
                    //gibts schon, dann löschen
                    itemAbo.setEnabled(false);
                    itemAboMitTitel.setEnabled(false);
                    itemAboLoeschen.addActionListener(beobAbo);

                    // dann können wir auch ändern
                    itemChangeAboFilter.addActionListener(new BeobChangeAbo());
                } else {
                    itemAboLoeschen.setEnabled(false);
                    itemChangeAboFilter.setEnabled(false);
                    //neues Abo anlegen
                    itemAbo.addActionListener(beobAbo);
                    itemAboMitTitel.addActionListener(beobAboMitTitel);
                }
            });

            submenueAbo.add(itemAboLoeschen);
            submenueAbo.add(itemChangeAboFilter);
            submenueAbo.add(itemAbo);
            submenueAbo.add(itemAboMitTitel);

            //Programme einblenden
            JMenu submenue = new JMenu("Film mit Set starten");
            jPopupMenu.add(submenue);
            ListePset liste = Daten.listePset.getListeButton();
            for (DatenPset pset : liste) {
                if (pset.getListeProg().isEmpty() && pset.arr[DatenPset.PROGRAMMSET_NAME].equals("")) {
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
                    item.addActionListener(new BeobOpenPlayer(GuiFilme.this, pset));
                    if (col != null) {
                        item.setBackground(col);
                    }
                }
                submenue.add(item);
            }

            JMenu submenueBlack = new JMenu("Blacklist");
            jPopupMenu.add(submenueBlack);
            //anlegen
            JMenuItem itemBlackSender;
            JMenuItem itemBlackSenderThema;
            JMenuItem itemBlackThema;
            itemBlackSender = new JMenuItem("Sender in die Blacklist einfügen");
            itemBlackThema = new JMenuItem("Thema in die Blacklist einfügen");
            itemBlackSenderThema = new JMenuItem("Sender und Thema in die Blacklist einfügen");
            itemBlackSender.addActionListener(beobBlacklistSender);
            itemBlackThema.addActionListener(beobBlacklistThema);
            itemBlackSenderThema.addActionListener(beobBlacklistSenderThema);
            submenueBlack.add(itemBlackSender);
            submenueBlack.add(itemBlackThema);
            submenueBlack.add(itemBlackSenderThema);

            //Url
            res.ifPresent(film -> {
                JMenuItem item;
                String uNormal = film.getUrlFuerAufloesung(FilmResolution.AUFLOESUNG_NORMAL);
                String uHd = film.getUrlFuerAufloesung(FilmResolution.AUFLOESUNG_HD);
                String uLow = film.getUrlFuerAufloesung(FilmResolution.AUFLOESUNG_KLEIN);
                if (uHd.equals(uNormal)) {
                    uHd = ""; // dann gibts keine
                }
                if (uLow.equals(uNormal)) {
                    uLow = ""; // dann gibts keine
                }
                if (!uNormal.isEmpty()) {
                    jPopupMenu.addSeparator();

                    final ActionListener copyNormalUrlListener = e -> GuiFunktionen.copyToClipboard(film.getUrlFuerAufloesung(FilmResolution.AUFLOESUNG_NORMAL));
                    if (!uHd.isEmpty() || !uLow.isEmpty()) {
                        JMenu submenueURL = new JMenu("Film-URL kopieren");
                        // HD
                        if (!uHd.isEmpty()) {
                            item = new JMenuItem("in höchster/hoher Qualität");
                            item.addActionListener(e -> GuiFunktionen.copyToClipboard(film.getUrlFuerAufloesung(FilmResolution.AUFLOESUNG_HD)));
                            submenueURL.add(item);
                        }

                        // normale Auflösung, gibts immer
                        item = new JMenuItem("in mittlerer Qualität");
                        item.addActionListener(copyNormalUrlListener);
                        submenueURL.add(item);

                        // kleine Auflösung
                        if (!uLow.isEmpty()) {
                            item = new JMenuItem("in niedriger Qualität");
                            item.addActionListener(e -> GuiFunktionen.copyToClipboard(film.getUrlFuerAufloesung(FilmResolution.AUFLOESUNG_KLEIN)));
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
            });

            jPopupMenu.addSeparator();

            // Film in der MediaDB suchen
            res.ifPresent(film -> {
                JMenuItem itemDb = new JMenuItem("Titel in der Mediensammlung suchen");
                itemDb.addActionListener(mediensammlungAction);
                jPopupMenu.add(itemDb);
            });

            //Drucken
            JMenuItem item = new JMenuItem("Tabelle drucken");
            item.addActionListener(beobPrint);
            jPopupMenu.add(item);

            jPopupMenu.add(showFilmInformationAction);

            //History
            res.ifPresent(film -> {
                JMenuItem miHistory;
                if (daten.getSeenHistoryController().urlPruefen(film.getUrl())) {
                    miHistory = new JMenuItem("Film als ungesehen markieren");
                    miHistory.addActionListener(unseenActionListener);
                } else {
                    miHistory = new JMenuItem("Film als gesehen markieren");
                    miHistory.addActionListener(seenActionListener);
                }
                jPopupMenu.add(miHistory);
            });
            //anzeigen
            jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
        }

        private class BeobHistory implements ActionListener {

            private final boolean eintragen;

            public BeobHistory(boolean eeintragen) {
                eintragen = eeintragen;
            }

            private void updateHistory(DatenFilm film) {
                final var list = Lists.newArrayList(film);
                final var history = daten.getSeenHistoryController();
                if (eintragen) {
                    history.markAsSeen(list);
                } else {
                    history.markAsUnseen(list);
                }
                list.clear();
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
                    Log.errorLog(688542239, ex);
                }
            }
        }

        private class BeobChangeAbo implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                if (Daten.listePset.getListeAbo().isEmpty()) {
                    new DialogAboNoSet(mediathekGui, daten).setVisible(true);
                } else {
                    final int nr = tabelle.rowAtPoint(p);
                    if (nr >= 0) {
                        stopBeob = true;
                        Optional<DatenFilm> res = getFilm(nr);
                        res.ifPresent(film -> {
                            DatenAbo datenAbo;
                            if ((datenAbo = daten.getListeAbo().getAboFuerFilm_schnell(film, false /*ohne Länge*/)) != null) {
                                //gibts schon, dann löschen
                                DialogEditAbo dialog = new DialogEditAbo(mediathekGui, true, daten, datenAbo, false/*onlyOne*/);
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

            public BeobAbo(boolean mmitTitel) {
                mitTitel = mmitTitel;
            }

            @Override
            public void actionPerformed(ActionEvent e) {
                if (Daten.listePset.getListeAbo().isEmpty()) {
                    new DialogAboNoSet(mediathekGui, daten).setVisible(true);
                } else {
                    final int nr = tabelle.rowAtPoint(p);
                    if (nr >= 0) {
                        stopBeob = true;
                        Optional<DatenFilm> res = getFilm(nr);
                        res.ifPresent(film -> {
                            DatenAbo datenAbo;
                            if ((datenAbo = daten.getListeAbo().getAboFuerFilm_schnell(film, false /*ohne Länge*/)) != null) {
                                //gibts schon, dann löschen
                                daten.getListeAbo().aboLoeschen(datenAbo);
                            } else //neues Abo anlegen
                            {
                                if (mitTitel) {
                                    daten.getListeAbo().addAbo(film.getThema()/*aboname*/,
                                            film.getSender(), film.getThema(), film.getTitle());
                                } else {
                                    daten.getListeAbo().addAbo(film.getThema()/*aboname*/,
                                            film.getSender(), film.getThema(), "");
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

            public BeobBlacklist(boolean ssender, boolean tthema) {
                sender = ssender;
                thema = tthema;
            }

            @Override
            public void actionPerformed(ActionEvent e) {
                final int nr = tabelle.rowAtPoint(p);
                if (nr >= 0) {
                    Optional<DatenFilm> res = getFilm(nr);
                    res.ifPresent(film -> {
                        final String th = film.getThema();
                        final String se = film.getSender();
                        // Blackliste für alle Fälle einschalten, notify kommt beim add()
                        MVConfig.add(MVConfig.Configs.SYSTEM_BLACKLIST_ON, Boolean.TRUE.toString());
                        var listeBlacklist = daten.getListeBlacklist();
                        if (!sender) {
                            listeBlacklist.add(new DatenBlacklist("", th, "", ""));
                        } else if (!thema) {
                            listeBlacklist.add(new DatenBlacklist(se, "", "", ""));
                        } else {
                            listeBlacklist.add(new DatenBlacklist(se, th, "", ""));
                        }
                    });
                }
            }
        }
    }
}
