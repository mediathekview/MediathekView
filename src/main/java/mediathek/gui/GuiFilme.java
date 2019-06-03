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
package mediathek.gui;

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
import mSearch.daten.DatenFilm;
import mSearch.daten.FilmResolution;
import mSearch.filmeSuchen.ListenerFilmeLaden;
import mSearch.filmeSuchen.ListenerFilmeLadenEvent;
import mSearch.tool.ApplicationConfiguration;
import mSearch.tool.Datum;
import mSearch.tool.Listener;
import mSearch.tool.Log;
import mediathek.MediathekGui;
import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.config.MVConfig;
import mediathek.controller.starter.Start;
import mediathek.daten.*;
import mediathek.gui.actions.ShowBlacklistDialogAction;
import mediathek.gui.actions.ShowFilmInformationAction;
import mediathek.gui.dialog.DialogAboNoSet;
import mediathek.gui.dialog.DialogAddDownload;
import mediathek.gui.dialog.DialogAddMoreDownload;
import mediathek.gui.dialog.DialogEditAbo;
import mediathek.gui.messages.*;
import mediathek.gui.messages.history.DownloadHistoryChangedEvent;
import mediathek.javafx.descriptionPanel.DescriptionPanelController;
import mediathek.javafx.filmtab.FilmTabInfoPane;
import mediathek.javafx.filterpanel.FilmActionPanel;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.MVMessageDialog;
import mediathek.tool.NoSelectionErrorDialog;
import mediathek.tool.cellrenderer.CellRendererFilme;
import mediathek.tool.listener.BeobTableHeader;
import mediathek.tool.models.TModel;
import mediathek.tool.models.TModelFilm;
import mediathek.tool.table.MVFilmTable;
import mediathek.tool.table.MVTable;
import net.engio.mbassy.listener.Handler;
import org.apache.commons.lang3.SystemUtils;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.LineBorder;
import java.awt.*;
import java.awt.event.*;
import java.awt.print.PrinterException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Optional;

@SuppressWarnings("serial")
public class GuiFilme extends JPanel {

    private final MVTable tabelle;
    public static final String NAME = "Filme";
    private final Daten daten;
    private boolean stopBeob = false;
    private final MediathekGui mediathekGui;

    public void tabelleSpeichern() {
        if (tabelle != null) {
            tabelle.tabelleNachDatenSchreiben();
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

    private FilmTabInfoPane filmInfoLabel;

    private void installTabInfoStatusBarControl() {
        final var leftItems = mediathekGui.getStatusBarController().getStatusBar().getLeftItems();

        Platform.runLater(() -> {
            filmInfoLabel = new FilmTabInfoPane(daten,this);
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

    public GuiFilme(Daten aDaten, MediathekGui mediathekGui) {
        super();
        daten = aDaten;
        
        this.mediathekGui = mediathekGui;
        initComponents();

        fxFilmActionPanel = new JFXPanel();
        add(fxFilmActionPanel, BorderLayout.NORTH);

        fxDescriptionPanel = new JFXPanel();
        jPanelBeschreibung.add(fxDescriptionPanel, BorderLayout.CENTER);

        tabelle = new MVFilmTable();
        jScrollPane1.setViewportView(tabelle);

        installTabInfoStatusBarControl();

        setupFilmSelectionPropertyListener(mediathekGui);

        setupPanelVideoplayer();
        setupDescriptionPanel();

        setupFilmActionPanel();

        start_init();
        start_addListener();

        setupActionListeners();
    }

    private final JCheckBoxMenuItem cbkShowDescription = new JCheckBoxMenuItem("Beschreibung anzeigen");

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

        if (SystemUtils.IS_OS_MAC_OSX)
            cbkShowDescription.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F10, InputEvent.META_DOWN_MASK));
        else
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

    /**
     * The JavaFx Film action popup panel.
     */
    public FilmActionPanel fap;

    /**
     * The swing helper panel FilmAction bar.
     */
    private final JFXPanel fxFilmActionPanel;

    private final JFXPanel fxDescriptionPanel;

    private void setupDescriptionPanel() {
        Platform.runLater(() -> {
            try {
                URL url = getClass().getResource("/mediathek/res/programm/fxml/filmdescription.fxml");

                FXMLLoader loader = new FXMLLoader();
                loader.setLocation(url);

                TabPane descriptionPane = loader.load();
                final DescriptionPanelController descriptionPanelController = loader.getController();
                descriptionPanelController.setOnCloseRequest(e -> {
                    SwingUtilities.invokeLater(() -> jPanelBeschreibung.setVisible(false));
                    e.consume();
                });

                fxDescriptionPanel.setScene(new Scene(descriptionPane));
                tabelle.getSelectionModel().addListSelectionListener(e -> {
                    Optional<DatenFilm> optFilm = getCurrentlySelectedFilm();
                    Platform.runLater(() -> descriptionPanelController.showFilmDescription(optFilm));
                });
            }
            catch (Exception ex) {
                ex.printStackTrace();
            }
        });
    }

    /**
     * Show description panel based on settings.
     */
    private void showDescriptionPanel() {
        jPanelBeschreibung.setVisible(ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.FILM_SHOW_DESCRIPTION, true));
    }

    public void onComponentShown() {
        mediathekGui.tabPaneIndexProperty().setValue(TabPaneIndex.FILME);

        updateFilmData();
        setInfoStatusbar();
    }

    public class FilterFilmAction extends AbstractAction {

        @Override
        public void actionPerformed(ActionEvent e) {
            loadTable();
        }
    }

    public final FilterFilmAction filterFilmAction = new FilterFilmAction();

    public int getTableRowCount() {
        if (tabelle != null) {
            return tabelle.getModel().getRowCount();
        } else {
            return 0;
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

    public final PlayFilmAction playAction = new PlayFilmAction();

    public final SaveFilmAction saveFilmAction = new SaveFilmAction();

    public class SaveFilmAction extends AbstractAction {

        @Override
        public void actionPerformed(ActionEvent e) {
            saveFilm(null);
        }
    }

    private static final String ACTION_MAP_KEY_PLAY_FILM = "film_abspielen";
    private static final String ACTION_MAP_KEY_SAVE_FILM = "download_film";
    private static final String ACTION_MAP_KEY_COPY_NORMAL_URL = "copy_url";
    private static final String ACTION_MAP_KEY_COPY_HD_URL = "copy_url_hd";
    private static final String ACTION_MAP_KEY_COPY_KLEIN_URL = "copy_url_klein";
    private static final String ACTION_MAP_KEY_MEDIA_DB = "mediadb";
    private static final String ACTION_MAP_KEY_MARK_SEEN = "seen";
    private static final String ACTION_MAP_KEY_MARK_UNSEEN = "unseen";

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

    public MediensammlungAction mediensammlungAction = new MediensammlungAction();

    private class MarkFilmAsSeenAction extends AbstractAction {

        @Override
        public void actionPerformed(ActionEvent e) {
            daten.getSeenHistoryController().markAsSeen(getSelFilme());
        }
    }

    public final MarkFilmAsSeenAction markFilmAsSeenAction = new MarkFilmAsSeenAction();

    public final MarkFilmAsUnseenAction markFilmAsUnseenAction = new MarkFilmAsUnseenAction();

    private class MarkFilmAsUnseenAction extends AbstractAction {

        @Override
        public void actionPerformed(ActionEvent e) {
            daten.getSeenHistoryController().markAsUnseen(getSelFilme());
        }
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
        BeobMausTabelle beobMausTabelle = new BeobMausTabelle();
        tabelle.addMouseListener(beobMausTabelle);
        tabelle.getSelectionModel().addListSelectionListener(event -> {
            final ListSelectionModel m = (ListSelectionModel) event.getSource();
            if (!m.isSelectionEmpty() && !m.getValueIsAdjusting() && !stopBeob) {
                updateFilmData();
            }
        });

        setupCellRenderer();

        tabelle.setLineBreak(MVConfig.getBool(MVConfig.Configs.SYSTEM_TAB_FILME_LINEBREAK));

        final var headerListener = new BeobTableHeader(tabelle,
                Daten.spaltenAnzeigenFilme,
                HIDDEN_COLUMNS,
                new int[]{DatenFilm.FILM_ABSPIELEN, DatenFilm.FILM_AUFZEICHNEN},
                true, MVConfig.Configs.SYSTEM_TAB_FILME_LINEBREAK);
        tabelle.getTableHeader().addMouseListener(headerListener);

        Icon closeIcon = IconFontSwing.buildIcon(FontAwesome.TIMES_CIRCLE_O, 16);
        jCheckBoxProgamme.setIcon(closeIcon);
        jCheckBoxProgamme.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN, Boolean.FALSE.toString());
            Listener.notify(Listener.EREIGNIS_LISTE_PSET, GuiFilme.class.getSimpleName());
            setupPanelVideoplayer();
        });

        setVisFilterPanelAndLoad();
        tabelle.initTabelle();
        if (tabelle.getRowCount() > 0) {
            tabelle.setRowSelectionInterval(0, 0);
        }
    }

    private static final int[] HIDDEN_COLUMNS = new int[]{
            DatenFilm.FILM_ABSPIELEN,
            DatenFilm.FILM_AUFZEICHNEN
    };
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
                setupPanelVideoplayer();
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

    private void playerStarten(DatenPset pSet) {
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

    private void setupPanelVideoplayer() {
        // erst sauber machen
        // zum Anlegen der Button:
        // Programmgruppe ohne Namen: Leerfeld
        // Programmgruppe ohen Programme: Label
        // sonst ein Button
        jPanelExtraInnen.removeAll();
        jPanelExtraInnen.updateUI();
        jPanelExtraInnen.addMouseListener(new BeobMausButton());
        ListePset listeButton = Daten.listePset.getListeButton();
        int maxSpalten = MVConfig.getInt(MVConfig.Configs.SYSTEM_TAB_FILME_ANZAHL_BUTTON); //Anzahl der Spalten der Schalter

        GridBagLayout gridbag = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
        c.weightx = 0;
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(4, 10, 4, 10);
        jPanelExtraInnen.setLayout(gridbag);
        int spalte = 0;
        int zeile = 0;
        for (int i = 0; i < listeButton.size(); ++i) {
            if (!listeButton.get(i).isFreeLine()) {
                addExtraFeld(i, spalte, zeile, gridbag, c, jPanelExtraInnen, listeButton);
            }
            ++spalte;
            if (spalte > maxSpalten - 1) {
                spalte = 0;
                ++zeile;
            }
        }
        // zum zusammenschieben
        c.weightx = 10;
        c.gridx = maxSpalten + 1;
        c.gridy = 0;
        JLabel label = new JLabel();
        gridbag.setConstraints(label, c);
        jPanelExtraInnen.add(label);
        // und jetzt noch anzeigen
        jPanelExtra.setVisible(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN)));
    }

    private class BeobOpen implements ActionListener {
        //ext. Programme starten

        DatenPset pset;

        public BeobOpen(DatenPset p) {
            pset = p;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            playerStarten(pset);
        }
    }

    private void reloadTable() {
        if (!stopBeob) {
            loadTable();
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

        private final JMenuItem miPlay = createPlayItem();

        private JMenuItem createPlayItem() {
            JMenuItem item = new JMenuItem("Film abspielen");
            item.setIcon(IconFontSwing.buildIcon(FontAwesome.PLAY, 16));
            item.addActionListener(playAction);
            return item;
        }

        private final JMenuItem miSave = createSaveFilmItem();

        private JMenuItem createSaveFilmItem() {
            JMenuItem item = new JMenuItem("Film aufzeichnen");
            item.setIcon(IconFontSwing.buildIcon(FontAwesome.DOWNLOAD, 16));
            item.addActionListener(saveFilmAction);
            return item;
        }

        private final ShowFilmInformationAction showFilmInformationAction = new ShowFilmInformationAction();

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
                    item.addActionListener(new BeobOpen(pset));
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
                            item = new JMenuItem("in HD-Auflösung");
                            item.addActionListener(e -> GuiFunktionen.copyToClipboard(film.getUrlFuerAufloesung(FilmResolution.AUFLOESUNG_HD)));
                            submenueURL.add(item);
                        }

                        // normale Auflösung, gibts immer
                        item = new JMenuItem("in hoher Auflösung");
                        item.addActionListener(copyNormalUrlListener);
                        submenueURL.add(item);

                        // kleine Auflösung
                        if (!uLow.isEmpty()) {
                            item = new JMenuItem("in geringer Auflösung");
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
                if (daten.getSeenHistoryController().urlPruefen(film.getUrlHistory())) {
                    miHistory = new JMenuItem("Film als ungesehen markieren");
                    miHistory.addActionListener(new BeobHistory(false));
                } else {
                    miHistory = new JMenuItem("Film als gesehen markieren");
                    miHistory.addActionListener(new BeobHistory(true));
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
                if (eintragen) {
                    daten.getSeenHistoryController().zeileSchreiben(film.getThema(), film.getTitle(), film.getUrlHistory());
                } else {
                    daten.getSeenHistoryController().urlAusLogfileLoeschen(film.getUrlHistory());
                }
            }

            @Override
            public void actionPerformed(ActionEvent e) {
                final int nr = tabelle.rowAtPoint(p);
                if (nr >= 0) {
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

        private class BeobBlacklist implements ActionListener {

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
                        if (!sender) {
                            daten.getListeBlacklist().add(new DatenBlacklist("", th, "" /*Titel*/, "" /*Thema-Titel*/));
                        } else if (!thema) {
                            daten.getListeBlacklist().add(new DatenBlacklist(se, "", "" /*Titel*/, "" /*Thema-Titel*/));
                        } else {
                            daten.getListeBlacklist().add(new DatenBlacklist(se, th, "" /*Titel*/, "" /*Thema-Titel*/));
                        }
                    });
                }
            }
        }
    }

    private class BeobMausButton extends MouseAdapter {

        JSpinner jSpinner = new JSpinner(new SpinnerNumberModel(4, 4, 10, 1));

        public BeobMausButton() {
            int start = MVConfig.getInt(MVConfig.Configs.SYSTEM_TAB_FILME_ANZAHL_BUTTON);
            jSpinner.setValue(start);
            jSpinner.setToolTipText("Damit kann die Anzahl der Button verändert werden");
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

        private void showMenu(MouseEvent evt) {
            JPopupMenu jPopupMenu = new JPopupMenu();
            jSpinner.addChangeListener(e -> {
                MVConfig.add(MVConfig.Configs.SYSTEM_TAB_FILME_ANZAHL_BUTTON, String.valueOf(((Number) jSpinner.getModel().getValue()).intValue()));
                setupPanelVideoplayer();
            });
            JPanel jPanelAnzahl = new JPanel();
            jPanelAnzahl.setLayout(new BorderLayout());
            jPanelAnzahl.setBorder(new EmptyBorder(3, 5, 3, 5));
            jPanelAnzahl.add(new JLabel("Anzahl Button je Zeile: "), BorderLayout.WEST);
            jPanelAnzahl.add(jSpinner, BorderLayout.EAST);

            jPopupMenu.add(jPanelAnzahl);

            //anzeigen
            jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
        }

    }

    private void addExtraFeld(int i, int spalte, int zeile, GridBagLayout gridbag, GridBagConstraints c, JPanel panel, ListePset liste) {
        JButton button;
        c.gridx = spalte;
        c.gridy = zeile;
        if (liste.get(i).isLable()) {
            JLabel label = new JLabel(liste.get(i).arr[DatenPset.PROGRAMMSET_NAME]);
            Color col = liste.get(i).getFarbe();
            if (col != null) {
                label.setForeground(col);
            }
            gridbag.setConstraints(label, c);
            panel.add(label);
        } else {
            button = new JButton(liste.get(i).arr[DatenPset.PROGRAMMSET_NAME]);
            button.addActionListener(new BeobOpen(liste.get(i)));
            Color col = liste.get(i).getFarbe();
            if (col != null) {
                button.setBackground(col);
            }

            gridbag.setConstraints(button, c);
            panel.add(button);
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
        cbkShowDescription.addActionListener(l -> jPanelBeschreibung.setVisible(cbkShowDescription.isSelected()));
        cbkShowDescription.addItemListener(e -> ApplicationConfiguration.getConfiguration().setProperty(ApplicationConfiguration.FILM_SHOW_DESCRIPTION, cbkShowDescription.isSelected()));
        jPanelBeschreibung.addComponentListener(new ComponentAdapter() {
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

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    // Generated using JFormDesigner non-commercial license
    private void initComponents() {
        jScrollPane1 = new JScrollPane();
        var jTable1 = new JTable();
        var jPanel2 = new JPanel();
        jPanelBeschreibung = new JPanel();
        jPanelExtra = new JPanel();
        jCheckBoxProgamme = new JCheckBox();
        jPanelExtraInnen = new JPanel();

        //======== this ========
        setLayout(new BorderLayout());

        //======== jScrollPane1 ========
        {

            //---- jTable1 ----
            jTable1.setModel(new TModel());
            jTable1.setAutoCreateRowSorter(true);
            jTable1.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
            jScrollPane1.setViewportView(jTable1);
        }
        add(jScrollPane1, BorderLayout.CENTER);

        //======== jPanel2 ========
        {
            jPanel2.setLayout(new BorderLayout());

            //======== jPanelBeschreibung ========
            {
                jPanelBeschreibung.setLayout(new BorderLayout());
            }
            jPanel2.add(jPanelBeschreibung, BorderLayout.CENTER);

            //======== jPanelExtra ========
            {
                jPanelExtra.setBorder(new LineBorder(new Color(153, 153, 153)));

                //---- jCheckBoxProgamme ----
                jCheckBoxProgamme.setFont(new Font(Font.DIALOG, Font.BOLD, 10));
                jCheckBoxProgamme.setToolTipText("Buttons ausblenden"); //NON-NLS

                //======== jPanelExtraInnen ========
                {

                    GroupLayout jPanelExtraInnenLayout = new GroupLayout(jPanelExtraInnen);
                    jPanelExtraInnen.setLayout(jPanelExtraInnenLayout);
                    jPanelExtraInnenLayout.setHorizontalGroup(
                        jPanelExtraInnenLayout.createParallelGroup()
                            .addGap(0, 597, Short.MAX_VALUE)
                    );
                    jPanelExtraInnenLayout.setVerticalGroup(
                        jPanelExtraInnenLayout.createParallelGroup()
                            .addGap(0, 0, Short.MAX_VALUE)
                    );
                }

                GroupLayout jPanelExtraLayout = new GroupLayout(jPanelExtra);
                jPanelExtra.setLayout(jPanelExtraLayout);
                jPanelExtraLayout.setHorizontalGroup(
                    jPanelExtraLayout.createParallelGroup()
                        .addGroup(jPanelExtraLayout.createSequentialGroup()
                            .addComponent(jCheckBoxProgamme)
                            .addGap(5, 5, 5)
                            .addComponent(jPanelExtraInnen, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addGap(5, 5, 5))
                );
                jPanelExtraLayout.setVerticalGroup(
                    jPanelExtraLayout.createParallelGroup()
                        .addGroup(jPanelExtraLayout.createSequentialGroup()
                            .addGroup(jPanelExtraLayout.createParallelGroup()
                                .addGroup(jPanelExtraLayout.createSequentialGroup()
                                    .addComponent(jCheckBoxProgamme)
                                    .addGap(0, 0, Short.MAX_VALUE))
                                .addGroup(jPanelExtraLayout.createSequentialGroup()
                                    .addGap(5, 5, 5)
                                    .addComponent(jPanelExtraInnen, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)))
                            .addGap(5, 5, 5))
                );
            }
            jPanel2.add(jPanelExtra, BorderLayout.SOUTH);
        }
        add(jPanel2, BorderLayout.SOUTH);
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    private JScrollPane jScrollPane1;
    private JPanel jPanelBeschreibung;
    private JPanel jPanelExtra;
    private JCheckBox jCheckBoxProgamme;
    private JPanel jPanelExtraInnen;
    // End of variables declaration//GEN-END:variables
}
