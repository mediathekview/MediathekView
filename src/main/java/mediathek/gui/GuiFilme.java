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

import com.jidesoft.utils.SystemInfo;
import javafx.animation.PauseTransition;
import javafx.application.Platform;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.embed.swing.JFXPanel;
import javafx.util.Duration;
import mSearch.daten.DatenFilm;
import mSearch.daten.ListeFilme;
import mSearch.filmeSuchen.ListenerFilmeLaden;
import mSearch.filmeSuchen.ListenerFilmeLadenEvent;
import mSearch.tool.Datum;
import mSearch.tool.Listener;
import mSearch.tool.Log;
import mediathek.MediathekGui;
import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.config.MVConfig;
import mediathek.controller.starter.Start;
import mediathek.daten.*;
import mediathek.gui.dialog.DialogAboNoSet;
import mediathek.gui.dialog.DialogAddDownload;
import mediathek.gui.dialog.DialogAddMoreDownload;
import mediathek.gui.dialog.DialogEditAbo;
import mediathek.gui.messages.StartEvent;
import mediathek.javafx.filterpanel.FilmActionPanel;
import mediathek.tool.*;
import mediathek.tool.cellrenderer.CellRendererFilme;
import mediathek.tool.listener.BeobTableHeader;
import net.engio.mbassy.listener.Handler;

import javax.swing.*;
import javax.swing.RowSorter.SortKey;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.awt.event.*;
import java.awt.print.PrinterException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

@SuppressWarnings("serial")
public class GuiFilme extends PanelVorlage {

    public GuiFilme(Daten aDaten, MediathekGui mediathekGui) {
        super(aDaten, mediathekGui);
        initComponents();

        tabelle = new MVTable(MVTable.TableType.FILME);
        jScrollPane1.setViewportView(tabelle);

        setupPanelVideoplayer();
        setupDescriptionPanel();

        setupFilmActionPanel();

        start_init(mediathekGui);
        start_addListener();

        setupActionListeners();

        daten.getListeFilmeNachBlackList().senderList.addListener((ListChangeListener<String>) c -> {
            String selectedItem = fap.senderBox.getSelectionModel().getSelectedItem();
            if (selectedItem != null) {
                ObservableList<String> list = fap.senderBox.getItems();
                list.clear();
                list.addAll(daten.getListeFilmeNachBlackList().senderList);
                fap.senderBox.getSelectionModel().select(selectedItem);
                SwingUtilities.invokeLater(this::reloadTable);
            }
        });

        setupSenderBox();
    }

    private void setupFilmActionPanel() {
        add(fxPanel, BorderLayout.NORTH);
        fap = new FilmActionPanel(daten);
        Platform.runLater(() -> fxPanel.setScene(fap.getFilmActionPanelScene()));
    }

    private void setupSenderBox() {
        fap.senderBox.valueProperty().addListener((observable, oldValue, newValue) -> {
            //senderList changes, reload Thema as well...
            String selectedItem = fap.themaBox.getSelectionModel().getSelectedItem();
            String sender;
            if (selectedItem == null)
                selectedItem = "";

            if (newValue == null)
                sender = "";
            else
                sender = newValue;

            setupThemaEntries(sender);
            if (fap.themaBox.getItems().contains(selectedItem))
                fap.themaBox.getSelectionModel().select(selectedItem);
            else
                fap.themaBox.getSelectionModel().select("");
        });
    }

    private void setupThemaEntries(String sender) {
        ObservableList<String> list = fap.themaBox.getItems();
        list.clear();
        list.addAll(Arrays.asList(getThemen(sender)));
    }

    /**
     * The JavaFx Film action popup panel.
     */
    public FilmActionPanel fap;

    /**
     * The swing helper panel for using JavaFX inside Swing.
     */
    private final JFXPanel fxPanel = new JFXPanel();

    private void setupDescriptionPanel() {
        PanelFilmBeschreibung panelBeschreibung = new PanelFilmBeschreibung(daten, tabelle, true /*film*/);
        jPanelBeschreibung.setLayout(new BorderLayout());
        jPanelBeschreibung.add(panelBeschreibung, BorderLayout.CENTER);
    }

    /**
     * Model für die Tabelle Filme zusammenbauen.
     */
    private synchronized void prepareTableModel() {
        final boolean nurNeue = fap.showNewOnly.getValue();
        final boolean nurUt = fap.showSubtitlesOnly.getValue();
        final boolean showOnlyHd = fap.showOnlyHd.getValue();
        final boolean kGesehen = fap.showUnseenOnly.getValue();
        final boolean keineAbos = fap.dontShowAbos.getValue();
        final boolean showOnlyLivestreams = fap.showLivestreamsOnly.getValue();
        final boolean dontShowTrailers = fap.dontShowTrailers.getValue();
        final boolean dontShowGebaerdensprache = fap.dontShowSignLanguage.getValue();
        final boolean dontShowAudioVersions = fap.dontShowAudioVersions.getValue();

        final int minLength = (int) fap.filmLengthSlider.getLowValue();
        final int maxLength = (int) fap.filmLengthSlider.getHighValue();

        final String filterSender = fap.senderBox.getSelectionModel().getSelectedItem();
        String filterThema = fap.themaBox.getSelectionModel().getSelectedItem();
        if (filterThema == null) {
            filterThema = "";
        }
        String filterThemaTitel = fap.roSearchStringProperty.getValueSafe();

        ListeFilme listeFilme = daten.getListeFilmeNachBlackList();

        TModel tModel = new TModelFilm(new Object[][]{}, DatenFilm.COLUMN_NAMES);
        if (listeFilme.isEmpty()) {
            // wenn die Liste leer ist, leeres Modell erzeugen
            tabelle.setModel(tModel);
        } else {
            // dann ein neues Model anlegen
            if (filterSender.isEmpty() && filterThema.isEmpty() && filterThemaTitel.isEmpty()
                    && minLength == 0 && maxLength == FilmActionPanel.UNLIMITED_VALUE
                    && !keineAbos && !kGesehen && !showOnlyHd && !nurUt && !showOnlyLivestreams && !nurNeue
                    && !dontShowTrailers && !dontShowGebaerdensprache && !dontShowAudioVersions) {
                // dann ganze Liste laden
                addObjectDataTabFilme(listeFilme, tModel);
            } else {
                // ThemaTitel
                String[] arrThemaTitel;
                if (Filter.isPattern(filterThemaTitel)) {
                    arrThemaTitel = new String[]{filterThemaTitel};
                } else {
                    arrThemaTitel = filterThemaTitel.split(",");
                    for (int i = 0; i < arrThemaTitel.length; ++i) {
                        arrThemaTitel[i] = arrThemaTitel[i].trim().toLowerCase();
                    }
                }

                for (DatenFilm film : listeFilme) {

                    if (film.dauerL < TimeUnit.SECONDS.convert(minLength, TimeUnit.MINUTES))
                        continue;

                    if (maxLength < FilmActionPanel.UNLIMITED_VALUE) {
                        if (film.dauerL > TimeUnit.SECONDS.convert(maxLength, TimeUnit.MINUTES))
                            continue;

                    }
                    if (nurNeue) {
                        if (!film.isNew()) {
                            continue;
                        }
                    }
                    if (showOnlyLivestreams) {
                        if (!film.arr[DatenFilm.FILM_THEMA].equals(ListeFilme.THEMA_LIVE)) {
                            continue;
                        }
                    }
                    if (showOnlyHd) {
                        if (!film.isHD()) {
                            continue;
                        }
                    }
                    if (nurUt) {
                        if (!film.hasSubtitle()) {
                            continue;
                        }
                    }
                    if (keineAbos) {
                        if (!film.arr[DatenFilm.FILM_ABO_NAME].isEmpty()) {
                            continue;
                        }
                    }
                    if (kGesehen) {
                        if (daten.history.urlPruefen(film.getUrlHistory())) {
                            continue;
                        }
                    }

                    if (dontShowTrailers) {
                        String titel = film.arr[DatenFilm.FILM_TITEL];
                        if (titel.contains("Trailer") || titel.contains("Teaser") || titel.contains("Vorschau") ||
                                titel.contains("trailer") || titel.contains("teaser") || titel.contains("vorschau"))
                            continue;
                    }

                    if (dontShowGebaerdensprache) {
                        String titel = film.arr[DatenFilm.FILM_TITEL];
                        if (titel.contains("Gebärden"))
                            continue;
                    }

                    if (dontShowAudioVersions) {
                        String titel = film.arr[DatenFilm.FILM_TITEL];
                        if (titel.contains("Hörfassung") || titel.contains("Audiodeskription"))
                            continue;
                    }

                    //filter mitLaenge false dann aufrufen
                    //je nachdem dann das ganze herausoperieren
                    String[] arrIrgendwo = {};
                    String[] arrTitel = {};
                    if (Filter.filterAufFilmPruefen(filterSender, filterThema, arrTitel, arrThemaTitel, arrIrgendwo, 0, true, film, false)) {
                        addObjectDataTabFilme(tModel, film);
                    }
                }
                // listeFilme.stream().filter((DatenFilm film) -> Filter.filterAufFilmPruefen(filterSender,
                //      filterThema, arrTitel, arrThemaTitel, arrIrgendwo, laenge, film, true /*länge nicht prüfen*/)).forEach(f -> addObjectDataTabFilme(tModel, f));

            }
            tabelle.setModel(tModel);
        }
    }

    private void addObjectDataTabFilme(ListeFilme listefilme, TModel tModel) {
        if (!listefilme.isEmpty()) {
            for (DatenFilm film : listefilme) {
                addObjectDataTabFilme(tModel, film);
            }
        }
    }

    private void addObjectDataTabFilme(TModel tModel, DatenFilm film) {
        Object[] object = new Object[DatenFilm.MAX_ELEM];
        for (int m = 0; m < DatenFilm.MAX_ELEM; ++m) {
            switch (m) {
                case DatenFilm.FILM_NR:
                    object[m] = film.nr;
                    break;
                case DatenFilm.FILM_DATUM:
                    object[m] = film.datumFilm;
                    break;
                case DatenFilm.FILM_GROESSE:
                    object[m] = film.dateigroesseL;
                    break;
                case DatenFilm.FILM_REF:
                    object[m] = film;
                    break;
                case DatenFilm.FILM_NEU:
                    object[m] = film.isNew() ? "1" : "0";
                    break;
                case DatenFilm.FILM_HD:
                    object[m] = film.isHD() ? "1" : "0";
                    break;
                case DatenFilm.FILM_UT:
                    object[m] = film.hasSubtitle() ? "1" : "0";
                    break;
                default:
                    object[m] = film.arr[m];
                    break;
            }
        }
        tModel.addRow(object);
    }

    /**
     * Show description panel based on settings.
     */
    private void showDescriptionPanel() {
        jPanelBeschreibung.setVisible(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_FILME_BESCHREIBUNG_ANZEIGEN)));
    }

    @Override
    public void isShown() {
        super.isShown();
        daten.getMediathekGui().getStatusBar().setIndexForLeftDisplay(MVStatusBar.StatusbarIndex.FILME);
        updateFilmData();
        setInfoStatusbar();
        Listener.notify(Listener.EREIGNIS_FILM_BESCHREIBUNG_ANZEIGEN, PanelFilmBeschreibung.class.getSimpleName());
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

    private class SortBySenderAction extends AbstractAction {
        private final ArrayList<SortKey> listSortKeys = new ArrayList<>();

        public SortBySenderAction() {
            SortKey sk = new SortKey(DatenFilm.FILM_SENDER, SortOrder.ASCENDING);
            listSortKeys.add(sk);
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            tabelle.getRowSorter().setSortKeys(listSortKeys);
            tabelle.requestFocusSelect(jScrollPane1, 0);
        }
    }

    private class ShowFilmInformationAction extends AbstractAction {

        @Override
        public void actionPerformed(ActionEvent e) {
            if (!Daten.filmInfo.isVisible()) {
                Daten.filmInfo.showInfo();
            }
        }
    }

    public class PlayFilmAction extends AbstractAction {

        @Override
        public synchronized void actionPerformed(ActionEvent e) {
            DatenPset pset = Daten.listePset.getPsetAbspielen();
            if (pset != null) {
                playerStarten(pset);
            } else {
                MVMessageDialog.showMessageDialog(parentComponent, "Im Menü unter \"Datei->Einstellungen->Set bearbeiten\" ein Programm zum Abspielen festlegen.",
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

    private void setupKeyMapping() {
        final JRootPane rootPane = daten.getMediathekGui().getRootPane();
        final InputMap focusedWindowMap = getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
        final ActionMap actionMap = getActionMap();

        rootPane.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_S, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), "sender");
        rootPane.getActionMap().put("sender", new SortBySenderAction());

        focusedWindowMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_P, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), "abspielen");
        actionMap.put("abspielen", playAction);

        focusedWindowMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_D, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), "download");
        actionMap.put("download", saveFilmAction);
        focusedWindowMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_T, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), "tabelle");
        actionMap.put("tabelle", new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                tabelle.requestFocusSelect(jScrollPane1);
            }
        });

        focusedWindowMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_I, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), "info");
        actionMap.put("info", new ShowFilmInformationAction());

        focusedWindowMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_U, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), "url-copy");
        actionMap.put("url-copy", new CopyUrlToClipboardAction(DatenFilm.AUFLOESUNG_NORMAL));

        if (SystemInfo.isMacOSX()) {
            focusedWindowMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_H, Event.SHIFT_MASK + Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), "url-hd-copy");
        } else {
            focusedWindowMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_H, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), "url-hd-copy");
        }
        actionMap.put("url-hd-copy", new CopyUrlToClipboardAction(DatenFilm.AUFLOESUNG_HD));

        focusedWindowMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_K, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), "url-k-copy");
        actionMap.put("url-k-copy", new CopyUrlToClipboardAction(DatenFilm.AUFLOESUNG_KLEIN));

        focusedWindowMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_M, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), "mediensammlung");
        actionMap.put("mediensammlung", mediensammlungAction);

        focusedWindowMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_G, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), "gesehen");
        actionMap.put("gesehen", markFilmAsSeenAction);
        focusedWindowMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_N, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), "ungesehen");
        actionMap.put("ungesehen", markFilmAsUnseenAction);
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
                daten.getDialogMediaDB().setVis();
                daten.getDialogMediaDB().setFilter(film.arr[DatenFilm.FILM_TITEL]);
            });
        }
    }

    public MediensammlungAction mediensammlungAction = new MediensammlungAction();

    private class MarkFilmAsSeenAction extends AbstractAction {

        @Override
        public void actionPerformed(ActionEvent e) {
            daten.history.setGesehen(true, getSelFilme(), daten.getListeFilmeHistory());
        }
    }

    public final MarkFilmAsSeenAction markFilmAsSeenAction = new MarkFilmAsSeenAction();

    public final MarkFilmAsUnseenAction markFilmAsUnseenAction = new MarkFilmAsUnseenAction();

    private class MarkFilmAsUnseenAction extends AbstractAction {

        @Override
        public void actionPerformed(ActionEvent e) {
            daten.history.setGesehen(false, getSelFilme(), daten.getListeFilmeHistory());
        }
    }

    private void setupTableKeyMapping() {
        tabelle.getInputMap().put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0), "film_starten");
        tabelle.getActionMap().put("film_starten", playAction);
    }

    private void setupCellRenderer(MediathekGui mediathekGui) {
        final CellRendererFilme cellRenderer = new CellRendererFilme(daten, mediathekGui.getSenderIconCache());
        tabelle.setDefaultRenderer(Object.class, cellRenderer);
        tabelle.setDefaultRenderer(Datum.class, cellRenderer);
        tabelle.setDefaultRenderer(Integer.class, cellRenderer);
    }

    private void start_init(MediathekGui mediathekGui) {
        showDescriptionPanel();
        daten.getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
            @Override
            public void start(ListenerFilmeLadenEvent event) {
                loadTable();
            }

            @Override
            public void fertig(ListenerFilmeLadenEvent event) {
                loadTable();
                setupThemaEntries("");
            }
        });

        setupKeyMapping();
        setupTableKeyMapping();

        tabelle.setModel(new TModelFilm(new Object[][]{}, DatenFilm.COLUMN_NAMES));
        BeobMausTabelle beobMausTabelle = new BeobMausTabelle();
        tabelle.addMouseListener(beobMausTabelle);
        tabelle.getSelectionModel().addListSelectionListener(event -> {
            final ListSelectionModel m = (ListSelectionModel) event.getSource();
            if (!m.isSelectionEmpty() && !m.getValueIsAdjusting() && !stopBeob) {
                updateFilmData();
            }
        });

        setupCellRenderer(mediathekGui);

        tabelle.lineBreak = MVConfig.getBool(MVConfig.Configs.SYSTEM_TAB_FILME_LINEBREAK);
        tabelle.getTableHeader().addMouseListener(new BeobTableHeader(tabelle, DatenFilm.COLUMN_NAMES, DatenFilm.spaltenAnzeigen,
                new int[]{DatenFilm.FILM_ABSPIELEN, DatenFilm.FILM_AUFZEICHNEN, DatenFilm.FILM_DATUM_LONG, DatenFilm.FILM_REF},
                new int[]{DatenFilm.FILM_ABSPIELEN, DatenFilm.FILM_AUFZEICHNEN},
                true, MVConfig.Configs.SYSTEM_TAB_FILME_LINEBREAK));

        jCheckBoxProgamme.setIcon(Icons.ICON_CHECKBOX_CLOSE);
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

    private void start_addListener() {
        //register message bus handler
        daten.getMessageBus().subscribe(this);

        Listener.addListener(new Listener(Listener.EREIGNIS_LISTE_PSET, GuiFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                setupPanelVideoplayer();
            }
        });
        Listener.addListener(new Listener(Listener.EREIGNIS_LISTE_HISTORY_GEAENDERT, GuiFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                if (fap.showUnseenOnly.getValue()) {
                    loadTable();
                } else {
                    tabelle.fireTableDataChanged(true);
                }
            }
        });
        Listener.addListener(new Listener(Listener.EREIGNIS_LISTE_ABOS, GuiFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                loadTable();
            }
        });
        Listener.addListener(new Listener(Listener.EREIGNIS_BESCHREIBUNG, GuiFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                loadTable();
            }
        });
        Listener.addListener(new Listener(Listener.EREIGNIS_BLACKLIST_GEAENDERT, GuiFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                loadTable();
            }
        });
        Listener.addListener(new Listener(Listener.EREIGNIS_START_EVENT_BUTTON, GuiFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                tabelle.fireTableDataChanged(true /*setSpalten*/);
                setInfoStatusbar();
            }
        });
        Listener.addListener(new Listener(Listener.EREIGNIS_FILM_BESCHREIBUNG_ANZEIGEN, GuiFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                showDescriptionPanel();
            }
        });
    }

    @Handler
    private void handleStartEvent(StartEvent msg) {
        SwingUtilities.invokeLater(this::setInfoStatusbar);
    }

    private synchronized void saveFilm(DatenPset pSet) {
        if (Daten.listePset.getListeSpeichern().isEmpty()) {
            new DialogAboNoSet(parentComponent, daten).setVisible(true);
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
            DialogAddMoreDownload damd = new DialogAddMoreDownload(daten.getMediathekGui(), pSet);
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
            DatenDownload datenDownload = daten.getListeDownloads().getDownloadUrlFilm(datenFilm.arr[DatenFilm.FILM_URL]);
            if (datenDownload != null) {
                int ret = JOptionPane.showConfirmDialog(parentComponent, "Download für den Film existiert bereits.\n"
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
                Listener.notify(Listener.EREIGNIS_LISTE_DOWNLOADS, this.getClass().getSimpleName());
                if (Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_DIALOG_DOWNLOAD_D_STARTEN))) {
                    // und evtl. auch gleich starten
                    datenDownload.startDownload(daten);
                }
            } else {
                //dann alle Downloads im Dialog abfragen
                String aufloesung = "";
                if (fap.showOnlyHd.getValue()) {
                    aufloesung = DatenFilm.AUFLOESUNG_HD;
                }
                DialogAddDownload dialog = new DialogAddDownload(daten.getMediathekGui(), daten, datenFilm, pSet, aufloesung);
                dialog.setVisible(true);
            }
        }
    }

    private void playerStarten(DatenPset pSet) {
        // Url mit Prognr. starten
        if (tabelle.getSelectedRow() == -1) {
            new HinweisKeineAuswahl().zeigen(parentComponent);
        } else if (pSet.istSpeichern()) {
            // wenn das pSet zum Speichern (über die Button) gewählt wurde,
            // weiter mit dem Dialog "Speichern"
            saveFilm(pSet);
        } else {
            // mit dem flvstreamer immer nur einen Filme starten
            String aufloesung = "";
            if (fap.showOnlyHd.getValue()) {
                aufloesung = DatenFilm.AUFLOESUNG_HD;
            }
            Optional<DatenFilm> filmSelection = getCurrentlySelectedFilm();
            if (filmSelection.isPresent()) {
                daten.starterClass.urlMitProgrammStarten(pSet, filmSelection.get(), aufloesung);
            }
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
        int rows[] = tabelle.getSelectedRows();
        if (rows.length > 0) {
            for (int row : rows) {
                DatenFilm datenFilm = (DatenFilm) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(row), DatenFilm.FILM_REF);
                arrayFilme.add(datenFilm);
            }
        } else {
            new HinweisKeineAuswahl().zeigen(parentComponent);
        }
        return arrayFilme;
    }

    /**
     * Update Film Information and description panel with updated film...
     */
    private void updateFilmData() {
        final Optional<DatenFilm> filmSelection = getCurrentlySelectedFilm();
        filmSelection.ifPresent(Daten.filmInfo::updateCurrentFilm);
    }

    private void setInfoStatusbar() {
        // Infopanel setzen
        daten.getMediathekGui().getStatusBar().setTextForLeftDisplay();
    }

    // ############################################
    // Panel mit den Extra-Videoprogrammen
    // ############################################
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
                    if (!Daten.filmInfo.isVisible()) {
                        Daten.filmInfo.showInfo();
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
                        final DatenDownload datenDownload = daten.getListeDownloadsButton().getDownloadUrlFilm(datenFilm.arr[DatenFilm.FILM_URL]);
                        if (datenDownload != null) {
                            if (datenDownload.start != null) {
                                if (datenDownload.start.status == Start.STATUS_RUN) {
                                    stop = true;
                                    daten.getListeDownloadsButton().delDownloadButton(datenFilm.arr[DatenFilm.FILM_URL]);
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
            item.setIcon(Icons.ICON_MENUE_FILM_START);
            item.addActionListener(playAction);
            return item;
        }

        private final JMenuItem miSave = createSaveFilmItem();

        private JMenuItem createSaveFilmItem() {
            JMenuItem item = new JMenuItem("Film aufzeichnen");
            item.setIcon(Icons.ICON_MENUE_FILM_REC);
            item.addActionListener(saveFilmAction);
            return item;
        }

        private void showMenu(MouseEvent evt) {
            p = evt.getPoint();
            final int nr = tabelle.rowAtPoint(p);
            if (nr >= 0) {
                tabelle.setRowSelectionInterval(nr, nr);
            }

            Optional<DatenFilm> res = getFilm(nr);
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

            //##Trenner##
            submenueBlack.addSeparator();
            //##Trenner##

            final JCheckBoxMenuItem jCheckBoxBlackBoxOn = new JCheckBoxMenuItem("Blacklist ist eingeschaltet");
            jCheckBoxBlackBoxOn.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_ON)));
            jCheckBoxBlackBoxOn.addActionListener(e -> {
                MVConfig.add(MVConfig.Configs.SYSTEM_BLACKLIST_ON, Boolean.toString(jCheckBoxBlackBoxOn.isSelected()));
                daten.getListeBlacklist().filterListe();
                Listener.notify(Listener.EREIGNIS_BLACKLIST_GEAENDERT, GuiFilme.class.getName());
            });
            submenueBlack.add(jCheckBoxBlackBoxOn);

            final JCheckBoxMenuItem jCheckBoxBlackBoxStart = new JCheckBoxMenuItem("Blacklist ist beim Programmstart eingeschaltet");
            jCheckBoxBlackBoxStart.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_START_ON)));
            jCheckBoxBlackBoxStart.addActionListener(e -> {
                MVConfig.add(MVConfig.Configs.SYSTEM_BLACKLIST_START_ON, Boolean.toString(jCheckBoxBlackBoxStart.isSelected()));
                Listener.notify(Listener.EREIGNIS_BLACKLIST_START_GEAENDERT, GuiFilme.class.getName());
            });
            submenueBlack.add(jCheckBoxBlackBoxStart);

            //Url
            if (res.isPresent()) {
                JMenuItem item;
                final DatenFilm film = res.get();
                String uNormal = film.getUrlFuerAufloesung(DatenFilm.AUFLOESUNG_NORMAL);
                String uHd = film.getUrlFuerAufloesung(DatenFilm.AUFLOESUNG_HD);
                String uLow = film.getUrlFuerAufloesung(DatenFilm.AUFLOESUNG_KLEIN);
                if (uHd.equals(uNormal)) {
                    uHd = ""; // dann gibts keine
                }
                if (uLow.equals(uNormal)) {
                    uLow = ""; // dann gibts keine
                }
                if (!uNormal.isEmpty()) {
                    //##Trenner##
                    jPopupMenu.addSeparator();
                    //##Trenner##
                    if (!uHd.isEmpty() || !uLow.isEmpty()) {
                        JMenu submenueURL = new JMenu("Film-URL kopieren");
                        // HD
                        if (!uHd.isEmpty()) {
                            item = new JMenuItem("in HD-Auflösung");
                            KeyStroke ctrlH;
                            if (SystemInfo.isMacOSX()) {
                                ctrlH = KeyStroke.getKeyStroke(KeyEvent.VK_H, Event.SHIFT_MASK + Toolkit.getDefaultToolkit().getMenuShortcutKeyMask());
                            } else {
                                ctrlH = KeyStroke.getKeyStroke(KeyEvent.VK_H, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask());
                            }
                            item.setAccelerator(ctrlH);

                            item.addActionListener(e -> GuiFunktionen.copyToClipboard(film.getUrlFuerAufloesung(DatenFilm.AUFLOESUNG_HD)));
                            submenueURL.add(item);
                        }

                        // normale Auflösung, gibts immer
                        item = new JMenuItem("in hoher Auflösung");

                        KeyStroke ctrlU = KeyStroke.getKeyStroke(KeyEvent.VK_U, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask());
                        item.setAccelerator(ctrlU);

                        item.addActionListener(e -> GuiFunktionen.copyToClipboard(film.getUrlFuerAufloesung(DatenFilm.AUFLOESUNG_NORMAL)));
                        submenueURL.add(item);

                        // kleine Auflösung
                        if (!uLow.isEmpty()) {
                            item = new JMenuItem("in geringer Auflösung");

                            KeyStroke ctrlK = KeyStroke.getKeyStroke(KeyEvent.VK_K, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask());
                            item.setAccelerator(ctrlK);

                            item.addActionListener(e -> GuiFunktionen.copyToClipboard(film.getUrlFuerAufloesung(DatenFilm.AUFLOESUNG_KLEIN)));
                            submenueURL.add(item);
                        }
                        jPopupMenu.add(submenueURL);
                    } else {
                        item = new JMenuItem("Film-URL kopieren");
                        item.addActionListener(e -> GuiFunktionen.copyToClipboard(film.getUrlFuerAufloesung(DatenFilm.AUFLOESUNG_NORMAL)));
                        jPopupMenu.add(item);
                    }
                }
                if (!film.getUrlSubtitle().isEmpty()) {
                    item = new JMenuItem("Untertitel-URL kopieren");
                    item.addActionListener(e -> GuiFunktionen.copyToClipboard(film.getUrlSubtitle()));
                    jPopupMenu.add(item);
                }
            }

            //##Trenner##
            jPopupMenu.addSeparator();
            //##Trenner##

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
            //Infos
            item = new JMenuItem("Filminformation anzeigen");
            item.addActionListener(e -> {
                if (!Daten.filmInfo.isVisible()) {
                    Daten.filmInfo.showInfo();
                }
            });
            jPopupMenu.add(item);
            //History
            res.ifPresent(film -> {
                JMenuItem miHistory;
                if (daten.history.urlPruefen(film.getUrlHistory())) {
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
                    daten.history.zeileSchreiben(film.arr[DatenFilm.FILM_THEMA], film.arr[DatenFilm.FILM_TITEL], film.getUrlHistory());
                    daten.getListeFilmeHistory().add(film);
                } else {
                    daten.history.urlAusLogfileLoeschen(film.getUrlHistory());
                    daten.getListeFilmeHistory().remove(film);
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
                    new DialogAboNoSet(parentComponent, daten).setVisible(true);
                } else {
                    final int nr = tabelle.rowAtPoint(p);
                    if (nr >= 0) {
                        stopBeob = true;
                        Optional<DatenFilm> res = getFilm(nr);
                        res.ifPresent(film -> {
                            DatenAbo datenAbo;
                            if ((datenAbo = daten.getListeAbo().getAboFuerFilm_schnell(film, false /*ohne Länge*/)) != null) {
                                //gibts schon, dann löschen
                                DialogEditAbo dialog = new DialogEditAbo(daten.getMediathekGui(), true, daten, datenAbo, false/*onlyOne*/);
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
                    new DialogAboNoSet(parentComponent, daten).setVisible(true);
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
                                    daten.getListeAbo().addAbo(film.arr[DatenFilm.FILM_THEMA]/*aboname*/,
                                            film.arr[DatenFilm.FILM_SENDER], film.arr[DatenFilm.FILM_THEMA], film.arr[DatenFilm.FILM_TITEL]);
                                } else {
                                    daten.getListeAbo().addAbo(film.arr[DatenFilm.FILM_THEMA]/*aboname*/,
                                            film.arr[DatenFilm.FILM_SENDER], film.arr[DatenFilm.FILM_THEMA], "");
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
                        final String th = film.arr[DatenFilm.FILM_THEMA];
                        final String se = film.arr[DatenFilm.FILM_SENDER];
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
            fap.showOnlyHd.addListener((observable, oldValue, newValue) -> SwingUtilities.invokeLater(this::reloadTable));
            fap.showSubtitlesOnly.addListener((observable, oldValue, newValue) -> SwingUtilities.invokeLater(this::reloadTable));
            fap.showNewOnly.addListener((observable, oldValue, newValue) -> SwingUtilities.invokeLater(this::reloadTable));
            fap.showUnseenOnly.addListener((observable, oldValue, newValue) -> SwingUtilities.invokeLater(this::reloadTable));
            fap.dontShowAbos.addListener((observable, oldValue, newValue) -> SwingUtilities.invokeLater(this::reloadTable));
            fap.dontShowTrailers.addListener((observable, oldValue, newValue) -> SwingUtilities.invokeLater(this::reloadTable));
            fap.dontShowSignLanguage.addListener((observable, oldValue, newValue) -> SwingUtilities.invokeLater(this::reloadTable));
            fap.dontShowAudioVersions.addListener((observable, oldValue, newValue) -> SwingUtilities.invokeLater(this::reloadTable));
            fap.showLivestreamsOnly.addListener((observable, oldValue, newValue) -> SwingUtilities.invokeLater(this::reloadTable));
            fap.filmLengthSlider.lowValueChangingProperty().addListener((observable, oldValue, newValue) -> {
                if (!newValue)
                    SwingUtilities.invokeLater(this::reloadTable);
            });
            fap.filmLengthSlider.highValueChangingProperty().addListener((observable, oldValue, newValue) -> {
                if (!newValue)
                    SwingUtilities.invokeLater(this::reloadTable);
            });
            fap.senderBox.setOnAction(evt -> SwingUtilities.invokeLater(this::reloadTable));

            PauseTransition trans = new PauseTransition(Duration.millis(250));
            fap.zeitraumProperty.addListener((observable, oldValue, newValue) -> {
                trans.setOnFinished(evt -> SwingUtilities.invokeLater(() -> {
                    daten.getListeBlacklist().filterListe();
                    loadTable();
                }));
                trans.playFromStart();
            });

            fap.themaBox.setOnAction(evt -> SwingUtilities.invokeLater(this::reloadTable));
        });
    }

    private String[] getThemen(String ssender) {
        for (int i = 1; i < daten.getListeFilmeNachBlackList().themenPerSender.length; ++i) {
            if (daten.getListeFilmeNachBlackList().sender[i].equals(ssender)) {
                return daten.getListeFilmeNachBlackList().themenPerSender[i];
            }
        }
        return daten.getListeFilmeNachBlackList().themenPerSender[0];
    }

    // ############################################
    // Filterprofile
    // ############################################
    /*private void setFilterProfile(int filter) {
        stopBeob = true;
        boolean bChanged = false;

        fap.dontShowAbos.setValue(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_FILTER_PROFILE__KEINE_ABO, filter)));
        fap.showUnseenOnly.setValue(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_FILTER_PROFILE__KEINE_GESEHENE, filter)));
        fap.showOnlyHd.setValue(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_FILTER_PROFILE__NUR_HD, filter)));
        fap.showSubtitlesOnly.setValue(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_FILTER_PROFILE__NUR_UT, filter)));
        fap.showNewOnly.setValue(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_FILTER_PROFILE__NUR_NEUE, filter)));

        mVFilterPanel.get_jSliderTage().setValue(MVConfig.getInt(MVConfig.Configs.SYSTEM_FILTER_PROFILE__TAGE, filter));

        // Blackliste
        if (Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_FILTER_PROFILE__BLACKLIST_ON, filter))
                != Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_ON))) {
            bChanged = true;
            MVConfig.add(MVConfig.Configs.SYSTEM_BLACKLIST_ON, MVConfig.get(MVConfig.Configs.SYSTEM_FILTER_PROFILE__BLACKLIST_ON, filter));
        }

        setTextSlider();

        // und jetzt wieder laden
        daten.getListeBlacklist().filterListe();

        // erst jetzt da Sender/Thema evtl. in der Blacklist
        mVFilterPanel.get_jComboBoxFilterThema().setModel(new javax.swing.DefaultComboBoxModel<>(getThemen("")));
        mVFilterPanel.get_jComboBoxFilterThema().setSelectedItem(MVConfig.get(MVConfig.Configs.SYSTEM_FILTER_PROFILE__THEMA, filter));

        SortKey sk = sortKeyLesen(MVConfig.get(MVConfig.Configs.SYSTEM_FILTER_PROFILE__SORT_KEY, filter),
                MVConfig.get(MVConfig.Configs.SYSTEM_FILTER_PROFILE__SORT_KEY_UPDOWN, filter));
        if (sk != null) {
            ArrayList<SortKey> lst = new ArrayList<>();
            lst.add(sk);
            tabelle.getRowSorter().setSortKeys(lst);
        }

        stopBeob = false;

        if (bChanged) {
            Listener.notify(Listener.EREIGNIS_BLACKLIST_GEAENDERT, GuiFilme.class.getSimpleName());
        }
        //dann laden
        loadTable();
        //beim Filter umschalten immer auf die erste Zeile setzen
        tabelle.setSelRow(0);
    }

    private void delFilterProfile(int filter) {
        // jetzt noch speichern
        MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__SENDER, String.valueOf(""), filter);
        MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__THEMA, String.valueOf(""), filter);

        MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__TITEL, String.valueOf(""), filter);
        MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__THEMA_TITEL, String.valueOf(""), filter);
        MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__TT, Boolean.toString(true), filter);

        MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__KEINE_ABO, String.valueOf(false), filter);
        MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__KEINE_GESEHENE, String.valueOf(false), filter);
        MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__NUR_HD, String.valueOf(false), filter);
        MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__NUR_NEUE, String.valueOf(false), filter);

        MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__TAGE, MVConfig.Configs.SYSTEM_FILTER_PROFILE__TAGE.initValue, filter);
        MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__DAUER, MVConfig.Configs.SYSTEM_FILTER_PROFILE__DAUER.initValue, filter);
        MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__DAUER_MIN, MVConfig.Configs.SYSTEM_FILTER_PROFILE__DAUER_MIN.initValue, filter);
        MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__BLACKLIST_ON, Boolean.FALSE.toString(), filter);

        MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__SORT_KEY, "", filter);
        MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__SORT_KEY_UPDOWN, "", filter);
    }

    private void saveFilterProfile(int filter) {
        // jetzt noch speichern
        MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__THEMA, String.valueOf(mVFilterPanel.get_jComboBoxFilterThema().getSelectedItem()), filter);

        MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__KEINE_ABO, String.valueOf(fap.dontShowAbos.getValue()), filter);
        MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__KEINE_GESEHENE, String.valueOf(fap.showUnseenOnly.getValue()), filter);
        MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__NUR_HD, String.valueOf(fap.showOnlyHd.getValue()), filter);
        MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__NUR_UT, String.valueOf(fap.showSubtitlesOnly.getValue()), filter);
        MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__NUR_NEUE, String.valueOf(fap.showNewOnly.getValue()), filter);

        MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__TAGE, String.valueOf(mVFilterPanel.get_jSliderTage().getValue()), filter);

        java.util.List<? extends RowSorter.SortKey> listeSortKeys;
        listeSortKeys = tabelle.getRowSorter().getSortKeys();
        String key = "";
        String upDown = "";

        if (listeSortKeys != null) {
            if (!listeSortKeys.isEmpty()) {
                SortKey sk = listeSortKeys.get(0);
                key = String.valueOf(sk.getColumn());
                upDown = sk.getSortOrder().equals(SortOrder.ASCENDING) ? SORT_ASCENDING : SORT_DESCENDING;
            }
        }
        MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__SORT_KEY, key, filter);
        MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__SORT_KEY_UPDOWN, upDown, filter);

    }*/

    // ####################################
    // Tabelle laden
    // ####################################
    private synchronized void loadTable() {
        try {
            stopBeob = true;
            tabelle.getSpalten();

            prepareTableModel();

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
    private void initComponents() {

        javax.swing.JPanel jPanel1 = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        javax.swing.JTable jTable1 = new javax.swing.JTable();
        jPanelBeschreibung = new javax.swing.JPanel();
        jPanelExtra = new javax.swing.JPanel();
        jCheckBoxProgamme = new javax.swing.JCheckBox();
        jPanelExtraInnen = new javax.swing.JPanel();

        setLayout(new java.awt.BorderLayout());

        jTable1.setAutoCreateRowSorter(true);
        jTable1.setModel(new TModel());
        jTable1.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_OFF);
        jScrollPane1.setViewportView(jTable1);

        javax.swing.GroupLayout jPanelBeschreibungLayout = new javax.swing.GroupLayout(jPanelBeschreibung);
        jPanelBeschreibung.setLayout(jPanelBeschreibungLayout);
        jPanelBeschreibungLayout.setHorizontalGroup(
                jPanelBeschreibungLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGap(0, 0, Short.MAX_VALUE)
        );
        jPanelBeschreibungLayout.setVerticalGroup(
                jPanelBeschreibungLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGap(0, 155, Short.MAX_VALUE)
        );

        jPanelExtra.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 153)));

        jCheckBoxProgamme.setFont(new java.awt.Font("Dialog", 1, 10)); // NOI18N
        jCheckBoxProgamme.setToolTipText("Buttons ausblenden");

        javax.swing.GroupLayout jPanelExtraInnenLayout = new javax.swing.GroupLayout(jPanelExtraInnen);
        jPanelExtraInnen.setLayout(jPanelExtraInnenLayout);
        jPanelExtraInnenLayout.setHorizontalGroup(
                jPanelExtraInnenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGap(0, 597, Short.MAX_VALUE)
        );
        jPanelExtraInnenLayout.setVerticalGroup(
                jPanelExtraInnenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGap(0, 0, Short.MAX_VALUE)
        );

        javax.swing.GroupLayout jPanelExtraLayout = new javax.swing.GroupLayout(jPanelExtra);
        jPanelExtra.setLayout(jPanelExtraLayout);
        jPanelExtraLayout.setHorizontalGroup(
                jPanelExtraLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanelExtraLayout.createSequentialGroup()
                                .addComponent(jCheckBoxProgamme)
                                .addGap(5, 5, 5)
                                .addComponent(jPanelExtraInnen, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                .addGap(5, 5, 5))
        );
        jPanelExtraLayout.setVerticalGroup(
                jPanelExtraLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanelExtraLayout.createSequentialGroup()
                                .addGroup(jPanelExtraLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                        .addGroup(jPanelExtraLayout.createSequentialGroup()
                                                .addComponent(jCheckBoxProgamme)
                                                .addGap(0, 0, Short.MAX_VALUE))
                                        .addGroup(jPanelExtraLayout.createSequentialGroup()
                                                .addGap(5, 5, 5)
                                                .addComponent(jPanelExtraInnen, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)))
                                .addGap(5, 5, 5))
        );

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
                jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addComponent(jPanelExtra, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jPanelBeschreibung, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 637, Short.MAX_VALUE)
        );
        jPanel1Layout.setVerticalGroup(
                jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanel1Layout.createSequentialGroup()
                                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 264, Short.MAX_VALUE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jPanelBeschreibung, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jPanelExtra, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        );

        add(jPanel1, java.awt.BorderLayout.CENTER);
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JCheckBox jCheckBoxProgamme;
    private javax.swing.JPanel jPanelBeschreibung;
    private javax.swing.JPanel jPanelExtra;
    private javax.swing.JPanel jPanelExtraInnen;
    private javax.swing.JScrollPane jScrollPane1;
    // End of variables declaration//GEN-END:variables
}
