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
import mediathek.filmlisten.GetModelTabFilme;
import mediathek.gui.dialog.DialogAboNoSet;
import mediathek.gui.dialog.DialogAddDownload;
import mediathek.gui.dialog.DialogAddMoreDownload;
import mediathek.gui.dialog.DialogEditAbo;
import mediathek.gui.filmInformation.IFilmInformation;
import mediathek.tool.*;
import mediathek.tool.MVTable;

import javax.swing.*;
import javax.swing.RowSorter.SortKey;
import javax.swing.border.EmptyBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.*;
import java.awt.event.*;
import java.awt.print.PrinterException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Optional;

import static mediathek.tool.MVTable.*;

@SuppressWarnings("serial")
public class GuiFilme extends PanelVorlage {
    private JButton buttonArray[];
    private MVFilter mVFilter;
    public MVFilterFrame mVFilterFrame;
    private final MVFilterPanel mVFilterPanel;
    private ToolBar toolBar;

    public GuiFilme(Daten aDaten, MediathekGui aMediathekGui) {
        super(aDaten, aMediathekGui);
        initComponents();
        tabelle = new MVTable(MVTable.TableType.FILME);
        jScrollPane1.setViewportView(tabelle);

        jScrollPaneFilter.getVerticalScrollBar().setUnitIncrement(16);
        jPanelFilter.setLayout(new BorderLayout());
        mVFilterPanel = new MVFilterPanel(parentComponent, daten,aMediathekGui) {
            @Override
            public void mvFfilter(int i) {
                setFilterProfile(i);
            }

            @Override
            public void mvFdeleteFilter(int i) {
                delFilterProfile(i);
            }

            @Override
            public void mvFsaveFilter(int i) {
                saveFilterProfile(i);
            }
        };
        mVFilterFrame = new MVFilterFrame(aDaten,aMediathekGui) {
            @Override
            public void mvFfilter(int i) {
                setFilterProfile(i);
            }

            @Override
            public void mvFdeleteFilter(int i) {
                delFilterProfile(i);
            }

            @Override
            public void mvFsaveFilter(int i) {
                saveFilterProfile(i);
            }
        };

        panelVideoplayerSetzen();
        setupDescriptionPanel();
        toolBar = new ToolBar(daten, MediathekGui.TABS.FILME);
        jPanelToolBar.setLayout(new BorderLayout());
        jPanelToolBar.add(toolBar, BorderLayout.CENTER);
        setToolbarVisible();
        start_init();
        start_addListener();
    }

    private void setupDescriptionPanel() {
        PanelFilmBeschreibung panelBeschreibung = new PanelFilmBeschreibung(daten, tabelle, true /*film*/);
        jPanelBeschreibung.setLayout(new BorderLayout());
        jPanelBeschreibung.add(panelBeschreibung, BorderLayout.CENTER);
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

    public String getFilterTextFromSearchField() {
        return toolBar.jTextFieldFilter.getText();
    }

    public void guiFilmeFilmAbspielen() {
        playFilm();
    }

    public void guiFilmeFilmSpeichern() {
        saveFilm();
    }

    public void guiFilmMediensammlung() {
        mediensammlung();
    }

    public void guiFilmeFilterLoeschen() {
        delFilter();
    }

    public void guiFilmeFiltern() {
        loadTable();
    }

    public void filmGesehen() {
        daten.history.setGesehen(true, getSelFilme(), daten.getListeFilmeHistory());
    }

    public void filmUngesehen() {
        daten.history.setGesehen(false, getSelFilme(), daten.getListeFilmeHistory());
    }

    public int getTableRowCount() {
        if (tabelle != null) {
            return tabelle.getModel().getRowCount();
        } else {
            return 0;
        }
    }

    private void start_init() {
        showDescriptionPanel();
        daten.getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
            @Override
            public void start(ListenerFilmeLadenEvent event) {
                GuiFunktionen.enableComponents(mVFilterFrame, false);
                GuiFunktionen.enableComponents(mVFilterPanel, false);
                loadTable();
            }

            @Override
            public void fertig(ListenerFilmeLadenEvent event) {
                loadTable();
                GuiFunktionen.enableComponents(mVFilterFrame, true);
                GuiFunktionen.enableComponents(mVFilterPanel, true);
                //mVFilter.enableFilter(true);
            }
        });
        daten.getMediathekGui().getRootPane().getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_S, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), "sender");
        daten.getMediathekGui().getRootPane().getActionMap().put("sender", new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {

                SortKey sk = new SortKey(DatenFilm.FILM_SENDER, SortOrder.ASCENDING);
                LinkedList<SortKey> listSortKeys = new LinkedList<>();
                listSortKeys.add(sk);
                tabelle.getRowSorter().setSortKeys(listSortKeys);
                tabelle.requestFocusSelelct(jScrollPane1, 0);
            }
        });
        this.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_P, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), "abspielen");
        this.getActionMap().put("abspielen", new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                playFilm();
            }
        });
        this.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_D, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), "download");
        this.getActionMap().put("download", new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                saveFilm();
            }
        });
        this.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_T, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), "tabelle");
        this.getActionMap().put("tabelle", new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                tabelle.requestFocusSelelct(jScrollPane1);
            }
        });
        this.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_I, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), "info");
        this.getActionMap().put("info", new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                IFilmInformation hud = daten.getMediathekGui().getFilmInformationHud();
                if (!hud.isVisible()) {
                    hud.showInfo();
                }
            }
        });

        this.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_U, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), "url-copy");
        this.getActionMap().put("url-copy", new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Optional<DatenFilm> filmSelection = getCurrentlySelectedFilm();
                if (filmSelection.isPresent()) {
                    final DatenFilm film = filmSelection.get();
                    GuiFunktionen.copyToClipboard(film.getUrlFuerAufloesung(DatenFilm.AUFLOESUNG_NORMAL));
                }
            }
        });

        if (SystemInfo.isMacOSX()) {
            this.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_H, Event.SHIFT_MASK + Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), "url-hd-copy");
        } else {
            this.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_H, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), "url-hd-copy");
        }
        this.getActionMap().put("url-hd-copy", new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Optional<DatenFilm> filmSelection = getCurrentlySelectedFilm();
                if (filmSelection.isPresent()) {
                    final DatenFilm film = filmSelection.get();
                    GuiFunktionen.copyToClipboard(film.getUrlFuerAufloesung(DatenFilm.AUFLOESUNG_HD));
                }
            }
        });

        this.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_K, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), "url-k-copy");
        this.getActionMap().put("url-k-copy", new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Optional<DatenFilm> filmSelection = getCurrentlySelectedFilm();
                if (filmSelection.isPresent()) {
                    final DatenFilm film = filmSelection.get();
                    GuiFunktionen.copyToClipboard(film.getUrlFuerAufloesung(DatenFilm.AUFLOESUNG_KLEIN));
                }
            }
        });

        this.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_M, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), "mediensammlung");
        this.getActionMap().put("mediensammlung", new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Optional<DatenFilm> filmSelection = getCurrentlySelectedFilm();
                if (filmSelection.isPresent()) {
                    final DatenFilm film = filmSelection.get();
                    MVConfig.add(MVConfig.Configs.SYSTEM_MEDIA_DB_DIALOG_ANZEIGEN, Boolean.TRUE.toString());
                    daten.getDialogMediaDB().setVis();
                    daten.getDialogMediaDB().setFilter(film.arr[DatenFilm.FILM_TITEL]);
                }
            }
        });
        this.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_G, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), "gesehen");
        this.getActionMap().put("gesehen", new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                filmGesehen();
            }
        });
        this.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_N, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), "ungesehen");
        this.getActionMap().put("ungesehen", new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                filmUngesehen();
            }
        });
        //Tabelle einrichten
        ActionMap am = tabelle.getActionMap();
        InputMap im = tabelle.getInputMap();
        im.put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0), "film_starten");
        am.put("film_starten", new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                playFilm();
            }
        });

        tabelle.setModel(new TModelFilm(new Object[][]{}, DatenFilm.COLUMN_NAMES));
        BeobMausTabelle beobMausTabelle = new BeobMausTabelle();
        tabelle.addMouseListener(beobMausTabelle);
        tabelle.getSelectionModel().addListSelectionListener(event -> {
            final ListSelectionModel m = (ListSelectionModel) event.getSource();
            if (!m.isSelectionEmpty() && !m.getValueIsAdjusting() && !stopBeob) {
                updateFilmData();
            }
        });

        final CellRendererFilme cellRenderer = new CellRendererFilme(daten);
        tabelle.setDefaultRenderer(Object.class, cellRenderer);
        tabelle.setDefaultRenderer(Datum.class, cellRenderer);
        tabelle.setDefaultRenderer(Integer.class, cellRenderer);
        tabelle.lineBreak = MVConfig.getBool(MVConfig.Configs.SYSTEM_TAB_FILME_LINEBREAK);
        tabelle.getTableHeader().addMouseListener(new BeobTableHeader(tabelle, DatenFilm.COLUMN_NAMES, DatenFilm.spaltenAnzeigen,
                new int[]{DatenFilm.FILM_ABSPIELEN, DatenFilm.FILM_AUFZEICHNEN, DatenFilm.FILM_DATUM_LONG, DatenFilm.FILM_REF},
                new int[]{DatenFilm.FILM_ABSPIELEN, DatenFilm.FILM_AUFZEICHNEN},
                true /*Icon*/, MVConfig.Configs.SYSTEM_TAB_FILME_LINEBREAK));

        jCheckBoxProgamme.setIcon(Icons.ICON_CHECKBOX_CLOSE);
        jCheckBoxProgamme.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN, Boolean.FALSE.toString());
            Listener.notify(Listener.EREIGNIS_LISTE_PSET, GuiFilme.class.getSimpleName());
            panelVideoplayerSetzen();
        });
        jSplitPane1.setDividerLocation(MVConfig.getInt(MVConfig.Configs.SYSTEM_PANEL_FILME_DIVIDER));
        jSplitPane1.addPropertyChangeListener(JSplitPane.DIVIDER_LOCATION_PROPERTY, pce -> {
            if (jScrollPaneFilter.isVisible()) {
                MVConfig.add(MVConfig.Configs.SYSTEM_PANEL_FILME_DIVIDER, String.valueOf(jSplitPane1.getDividerLocation()));
            }
        });

        setVisFilterPanelAndLoad();
        tabelle.initTabelle();
        if (tabelle.getRowCount() > 0) {
            tabelle.setRowSelectionInterval(0, 0);
        }
    }

    private void setToolbarVisible() {
        toolBar.setVisible(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_TOOLBAR_ALLES_ANZEIGEN)));
        if (!Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_TOOLBAR_ALLES_ANZEIGEN))) {
            MVConfig.add(MVConfig.Configs.SYSTEM_VIS_FILTER, Boolean.toString(true));
            Listener.notify(Listener.EREIGNIS_PANEL_FILTER_ANZEIGEN, GuiFilme.class.getSimpleName());
            setVisFilterPanelAndLoad();
        }
    }

    private void start_addListener() {
        Listener.addListener(new Listener(Listener.EREIGNIS_TOOLBAR_VIS, GuiFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                setToolbarVisible();
            }
        });
        Listener.addListener(new Listener(Listener.EREIGNIS_LISTE_PSET, GuiFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                panelVideoplayerSetzen();
            }
        });
        Listener.addListener(new Listener(Listener.EREIGNIS_LISTE_HISTORY_GEAENDERT, GuiFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                if (mVFilter.get_jCheckBoxKeineGesehenen().isSelected() || mVFilter.get_jToggleButtonHistory().isSelected()) {
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
        Listener.addListener(new Listener(new int[]{/*ListenerMediathekView.EREIGNIS_ART_DOWNLOAD_PROZENT,*/
                Listener.EREIGNIS_START_EVENT, Listener.EREIGNIS_LISTE_DOWNLOADS}, GuiFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                setInfoStatusbar();
            }
        });
        Listener.addListener(new Listener(Listener.EREIGNIS_PANEL_FILTER_ANZEIGEN, GuiFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                // Panel anzeigen und die Filmliste anpassen
                setVisFilterPanelAndLoad();
            }
        });
        Listener.addListener(new Listener(Listener.EREIGNIS_FILM_BESCHREIBUNG_ANZEIGEN, GuiFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                showDescriptionPanel();
            }
        });
        Listener.addListener(new Listener(Listener.EREIGNIS_SUCHFELD_FOCUS_SETZEN, GuiFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                if (Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_VIS_FILTER))) {
                    mVFilter.get_jTextFieldFilterThemaTitel().requestFocus();
                    mVFilter.get_jTextFieldFilterThemaTitel().setCaretPosition(0);
                }
            }
        });
    }

    private synchronized void playFilm() {
        DatenPset pset = Daten.listePset.getPsetAbspielen();
        if (pset != null) {
            playerStarten(pset);
        } else {
            MVMessageDialog.showMessageDialog(parentComponent, "Im Menü unter \"Datei->Einstellungen->Set bearbeiten\" ein Programm zum Abspielen festlegen.",
                    "kein Videoplayer!", JOptionPane.INFORMATION_MESSAGE);
        }
    }

    private synchronized void saveFilm() {
        saveFilm(null);
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
                if (mVFilter.get_jCheckBoxNurHd().isSelected()) {
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
            if (mVFilter.get_jCheckBoxNurHd().isSelected()) {
                aufloesung = DatenFilm.AUFLOESUNG_HD;
            }
            Optional<DatenFilm> filmSelection = getCurrentlySelectedFilm();
            if (filmSelection.isPresent()) {
                daten.starterClass.urlMitProgrammStarten(pSet, filmSelection.get(), aufloesung);
            }
        }
    }

    private void mediensammlung() {
        final Optional<DatenFilm> filmSelection = getCurrentlySelectedFilm();
        if (filmSelection.isPresent()) {
            MVConfig.add(MVConfig.Configs.SYSTEM_MEDIA_DB_DIALOG_ANZEIGEN, Boolean.TRUE.toString());
            daten.getDialogMediaDB().setVis();
            daten.getDialogMediaDB().setFilter(filmSelection.get().arr[DatenFilm.FILM_TITEL]);
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
        filmSelection.ifPresent(daten.getMediathekGui().getFilmInformationHud()::updateCurrentFilm);
    }

    private void setInfoStatusbar() {
        // Infopanel setzen
        daten.getMediathekGui().getStatusBar().setTextForLeftDisplay();
    }

    // ############################################
    // Panel mit den Extra-Videoprogrammen
    // ############################################
    private void panelVideoplayerSetzen() {
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
        buttonArray = new JButton[listeButton.size()];
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
                panelVideoplayerSetzen();
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

    private Component addExtraFeld(int i, int spalte, int zeile, GridBagLayout gridbag, GridBagConstraints c, JPanel panel, ListePset liste) {
        Component ret;
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
            ret = label;
        } else {
            button = new JButton(liste.get(i).arr[DatenPset.PROGRAMMSET_NAME]);
            button.addActionListener(new BeobOpen(liste.get(i)));
            Color col = liste.get(i).getFarbe();
            if (col != null) {
                button.setBackground(col);
            }
            buttonArray[i] = button;
            gridbag.setConstraints(button, c);
            panel.add(button);
            ret = button;
        }
        return ret;
    }

    // ############################################
    // Filter
    // ############################################
    private void setVisFilterPanelAndLoad() {
        boolean history = false;
        if (mVFilter != null) {
            mVFilter.removeAllListener();
            history = mVFilter.get_jToggleButtonHistory().isSelected();
        }
        if (Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_FENSTER_FILTER))) {
            jPanelFilter.removeAll();
            jScrollPaneFilter.setVisible(false);
            mVFilter = mVFilterFrame;
            mVFilterFrame.setVisible(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_VIS_FILTER)));
        } else {
            mVFilterFrame.setVisible(false);
            mVFilter = mVFilterPanel;
            jPanelFilter.add(mVFilterPanel, BorderLayout.CENTER);
            jScrollPaneFilter.setVisible(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_VIS_FILTER)));
            if (jScrollPaneFilter.isVisible()) {
                jSplitPane1.setDividerLocation(MVConfig.getInt(MVConfig.Configs.SYSTEM_PANEL_FILME_DIVIDER));
            }
        }
        // einrichten
        mVFilter.setVisible(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_VIS_FILTER)));
        mVFilter.get_jComboBoxFilterSender().setModel(new javax.swing.DefaultComboBoxModel<>(daten.getListeFilmeNachBlackList().sender));
        mVFilter.get_jComboBoxFilterThema().setModel(new javax.swing.DefaultComboBoxModel<>(getThemen("")));
        mVFilter.get_jToggleButtonHistory().setSelected(history);

        //===========
        // Slider
        mVFilter.get_jSliderTage().setValue(MVConfig.getInt(MVConfig.Configs.SYSTEM_FILTER_TAGE));
        mVFilter.get_jSliderMinuten().setValue(MVConfig.getInt(MVConfig.Configs.SYSTEM_FILTER_DAUER));
        mVFilter.get_rbMin().setSelected(MVConfig.getBool(MVConfig.Configs.SYSTEM_FILTER_DAUER_MIN));
        mVFilter.get_rbMax().setSelected(!MVConfig.getBool(MVConfig.Configs.SYSTEM_FILTER_DAUER_MIN));
        setTextSlider();

        //==========================
        // listener anhängen
        mVFilter.get_jSliderTage().addChangeListener(e -> {
            if (!stopBeob) {
                setTextSlider();
                if (!mVFilter.get_jSliderTage().getValueIsAdjusting()) {
                    MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_TAGE, String.valueOf(mVFilter.get_jSliderTage().getValue()));
                    daten.getListeBlacklist().filterListe();
                    loadTable();

                }
            }
        });
        mVFilter.get_jSliderMinuten().addChangeListener(e -> {
            if (!stopBeob) {
                setTextSlider();
                if (!mVFilter.get_jSliderMinuten().getValueIsAdjusting()) {
                    MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_DAUER, String.valueOf(mVFilter.get_jSliderMinuten().getValue()));
                    loadTable();
                }
            }
        });
        mVFilter.get_rbMin().addActionListener(l -> {
            if (!stopBeob) {
                MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_DAUER_MIN, String.valueOf(mVFilter.get_rbMin().isSelected()));
                loadTable();
            }
        });
        mVFilter.get_rbMax().addActionListener(l -> {
            if (!stopBeob) {
                MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_DAUER_MIN, String.valueOf(mVFilter.get_rbMin().isSelected()));
                loadTable();
            }
        });
        mVFilter.get_jToggleButtonLivestram().addActionListener(e -> {
            if (!stopBeob && mVFilter.get_jToggleButtonLivestram().isSelected()) {
                stopBeob = true;
                delAlles();
                mVFilter.get_jToggleButtonLivestram().setSelected(true);
                stopBeob = false;
            }
            daten.getListeBlacklist().filterListe();
            loadTable();
        });
        mVFilter.get_jToggleButtonHistory().addActionListener(e -> {
            if (!stopBeob && mVFilter.get_jToggleButtonHistory().isSelected()) {
                stopBeob = true;
                delAlles();
                mVFilter.get_jToggleButtonHistory().setSelected(true);
                stopBeob = false;
            }
            daten.getListeBlacklist().filterListe();
            loadTable();
        });
        mVFilter.get_jButtonFilterLoeschen().addActionListener(l -> delFilter());
        mVFilter.get_jButtonClearAll().addActionListener(l -> delFilterAlles());
        mVFilter.get_jComboBoxFilterSender().addActionListener(new BeobFilter());
        mVFilter.get_jComboBoxFilterThema().addActionListener(new BeobFilter());
        mVFilter.get_jTextFieldFilterTitel().addActionListener(new BeobFilter());
        mVFilter.get_jTextFieldFilterTitel().getDocument().addDocumentListener(new BeobFilterTitelDoc());
        mVFilter.get_jTextFieldFilterThemaTitel().addActionListener(new BeobFilter());
        mVFilter.get_jTextFieldFilterThemaTitel().getDocument().addDocumentListener(new BeobFilterTitelDoc());
        mVFilter.get_jCheckBoxKeineAbos().addActionListener(new BeobFilter());
        mVFilter.get_jCheckBoxKeineGesehenen().addActionListener(new BeobFilter());
        mVFilter.get_jCheckBoxNurHd().addActionListener(new BeobFilter());
        mVFilter.get_jCheckBoxNurUt().addActionListener(new BeobFilter());
        mVFilter.get_jCheckBoxNeue().addActionListener(new BeobFilter());
        mVFilter.get_jRadioButtonTT().addActionListener(new BeobFilter());
        mVFilter.get_JRadioButtonIrgendwo().addActionListener(new BeobFilter());

        //=======================================
        // und jezt die Anzeige
        this.updateUI();
        loadTable();
    }

    private void setTextSlider() {
        mVFilter.get_jTextFieldFilterTage().setText(String.valueOf(mVFilter.get_jSliderTage().getValue()));
        if (mVFilter.get_jSliderTage().getValue() == 0) {
            mVFilter.get_jTextFieldFilterTage().setText("alles");
        }
        mVFilter.get_jTextFieldFilterMinuten().setText(String.valueOf(mVFilter.get_jSliderMinuten().getValue()));
        if (mVFilter.get_jSliderMinuten().getValue() == 0) {
            mVFilter.get_jTextFieldFilterMinuten().setText("alles");
        }
    }

    private void delFilter() {
        stopBeob = true;
        delOben();
        stopBeob = false;
        // und jetzt wieder laden
        loadTable();
    }

    private void delFilterAlles() {
        stopBeob = true;
        delAlles();
        stopBeob = false;
        // und jetzt wieder laden
        daten.getListeBlacklist().filterListe();
        loadTable();
    }

    private void delOben() {
        mVFilter.get_jComboBoxFilterSender().setModel(new javax.swing.DefaultComboBoxModel<>(daten.getListeFilmeNachBlackList().sender));
        mVFilter.get_jComboBoxFilterThema().setModel(new javax.swing.DefaultComboBoxModel<>(getThemen("")));
        mVFilter.get_jTextFieldFilterTitel().setText("");
        mVFilter.get_jTextFieldFilterThemaTitel().setText("");
        mVFilter.setThemaTitel(true);
    }

    private void delAlles() {
        mVFilter.get_jComboBoxFilterSender().setModel(new javax.swing.DefaultComboBoxModel<>(daten.getListeFilmeNachBlackList().sender));
        mVFilter.get_jComboBoxFilterThema().setModel(new javax.swing.DefaultComboBoxModel<>(getThemen("")));
        mVFilter.get_jTextFieldFilterTitel().setText("");
        mVFilter.get_jTextFieldFilterThemaTitel().setText("");
        mVFilter.setThemaTitel(true);
        //untere Hälfte
        mVFilter.get_jCheckBoxKeineAbos().setSelected(false);
        mVFilter.get_jCheckBoxKeineGesehenen().setSelected(false);
        mVFilter.get_jCheckBoxNurHd().setSelected(false);
        mVFilter.get_jCheckBoxNurUt().setSelected(false);
        mVFilter.get_jCheckBoxNeue().setSelected(false);

        mVFilter.get_jToggleButtonHistory().setSelected(false);
        mVFilter.get_jToggleButtonLivestram().setSelected(false);

        mVFilter.get_jSliderMinuten().setValue(0);
        mVFilter.get_jSliderTage().setValue(0);
        mVFilter.get_rbMin().setSelected(true);

        MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_TAGE, String.valueOf(mVFilter.get_jSliderTage().getValue()));
        MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_DAUER, String.valueOf(mVFilter.get_jSliderMinuten().getValue()));
        MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_DAUER_MIN, String.valueOf(mVFilter.get_rbMin().isSelected()));
        setTextSlider();
    }

    public int getFilterTage() {
        return mVFilter.get_jSliderTage().getValue();
    }

    private String[] getThemen(String ssender) {
        for (int i = 1; i < daten.getListeFilmeNachBlackList().themenPerSender.length; ++i) {
            if (daten.getListeFilmeNachBlackList().sender[i].equals(ssender)) {
                return daten.getListeFilmeNachBlackList().themenPerSender[i];
            }
        }
        return daten.getListeFilmeNachBlackList().themenPerSender[0];
        //return alleThemen;
    }

    // ############################################
    // Filterprofile
    // ############################################
    private void setFilterProfile(int filter) {
        stopBeob = true;
        boolean geändert = false;
        mVFilter.get_jToggleButtonHistory().setSelected(false);
        mVFilter.get_jToggleButtonLivestram().setSelected(false);
        mVFilter.get_jTextFieldFilterTitel().setText(MVConfig.get(MVConfig.Configs.SYSTEM_FILTER_PROFILE__TITEL, filter));
        mVFilter.get_jTextFieldFilterThemaTitel().setText(MVConfig.get(MVConfig.Configs.SYSTEM_FILTER_PROFILE__THEMA_TITEL, filter));
        mVFilter.setThemaTitel(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_FILTER_PROFILE__TT, filter)));

        mVFilter.get_jCheckBoxKeineAbos().setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_FILTER_PROFILE__KEINE_ABO, filter)));
        mVFilter.get_jCheckBoxKeineGesehenen().setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_FILTER_PROFILE__KEINE_GESEHENE, filter)));
        mVFilter.get_jCheckBoxNurHd().setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_FILTER_PROFILE__NUR_HD, filter)));
        mVFilter.get_jCheckBoxNurUt().setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_FILTER_PROFILE__NUR_UT, filter)));
        mVFilter.get_jCheckBoxNeue().setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_FILTER_PROFILE__NUR_NEUE, filter)));

        mVFilter.get_jSliderTage().setValue(MVConfig.getInt(MVConfig.Configs.SYSTEM_FILTER_PROFILE__TAGE, filter));

        // Blackliste
        if (Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_FILTER_PROFILE__BLACKLIST_ON, filter))
                != Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_ON))) {
            geändert = true;
            MVConfig.add(MVConfig.Configs.SYSTEM_BLACKLIST_ON, MVConfig.get(MVConfig.Configs.SYSTEM_FILTER_PROFILE__BLACKLIST_ON, filter));
        }

        mVFilter.get_jSliderMinuten().setValue(MVConfig.getInt(MVConfig.Configs.SYSTEM_FILTER_PROFILE__DAUER, filter));
        mVFilter.get_rbMin().setSelected(MVConfig.getBool(MVConfig.Configs.SYSTEM_FILTER_PROFILE__DAUER_MIN, filter));
        mVFilter.get_rbMax().setSelected(!MVConfig.getBool(MVConfig.Configs.SYSTEM_FILTER_PROFILE__DAUER_MIN, filter));

        setTextSlider();

        // und jetzt wieder laden
        daten.getListeBlacklist().filterListe();

        // erst jetzt da Sender/Thema evtl. in der Blacklist
        mVFilter.get_jComboBoxFilterSender().setModel(new javax.swing.DefaultComboBoxModel<>(daten.getListeFilmeNachBlackList().sender));
        mVFilter.get_jComboBoxFilterThema().setModel(new javax.swing.DefaultComboBoxModel<>(getThemen("")));
        mVFilter.get_jComboBoxFilterSender().setSelectedItem(MVConfig.get(MVConfig.Configs.SYSTEM_FILTER_PROFILE__SENDER, filter));
        mVFilter.get_jComboBoxFilterThema().setSelectedItem(MVConfig.get(MVConfig.Configs.SYSTEM_FILTER_PROFILE__THEMA, filter));

        SortKey sk = sortKeyLesen(MVConfig.get(MVConfig.Configs.SYSTEM_FILTER_PROFILE__SORT_KEY, filter),
                MVConfig.get(MVConfig.Configs.SYSTEM_FILTER_PROFILE__SORT_KEY_UPDOWN, filter));
        if (sk != null) {
            LinkedList<SortKey> lst = new LinkedList<>();
            lst.add(sk);
            tabelle.getRowSorter().setSortKeys(lst);
        }

        stopBeob = false;

        if (geändert) {
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
        MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__SENDER, String.valueOf(mVFilter.get_jComboBoxFilterSender().getSelectedItem()), filter);
        MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__THEMA, String.valueOf(mVFilter.get_jComboBoxFilterThema().getSelectedItem()), filter);

        MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__TITEL, String.valueOf(mVFilter.get_jTextFieldFilterTitel().getText()), filter);
        MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__THEMA_TITEL, String.valueOf(mVFilter.get_jTextFieldFilterThemaTitel().getText()), filter);
        MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__TT, Boolean.toString(mVFilter.getThemaTitel()), filter);

        MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__KEINE_ABO, String.valueOf(mVFilter.get_jCheckBoxKeineAbos().isSelected()), filter);
        MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__KEINE_GESEHENE, String.valueOf(mVFilter.get_jCheckBoxKeineGesehenen().isSelected()), filter);
        MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__NUR_HD, String.valueOf(mVFilter.get_jCheckBoxNurHd().isSelected()), filter);
        MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__NUR_UT, String.valueOf(mVFilter.get_jCheckBoxNurUt().isSelected()), filter);
        MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__NUR_NEUE, String.valueOf(mVFilter.get_jCheckBoxNeue().isSelected()), filter);

        MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__TAGE, String.valueOf(mVFilter.get_jSliderTage().getValue()), filter);
        MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__DAUER, String.valueOf(mVFilter.get_jSliderMinuten().getValue()), filter);
        MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__DAUER_MIN, String.valueOf(mVFilter.get_rbMin().isSelected()), filter);

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

    }

    // ####################################
    // Tabelle laden
    // ####################################
    private synchronized void loadTable() {
        boolean themaNichtDa = false;
        try {
            stopBeob = true;
            tabelle.getSpalten();
            if (daten.getListeFilmeNachBlackList().isEmpty()) {
                // die Liste in leer
                delOben();
                listeInModellLaden(); // zum löschen der Tabelle
            } else if (!Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_VIS_FILTER))) {
                // Filtern mit dem Filter in der Toolbar
                listeInModellLaden();
            } else {
                String filterThema = mVFilter.get_jComboBoxFilterThema().getSelectedItem().toString();
                String filterSender = mVFilter.get_jComboBoxFilterSender().getSelectedItem().toString();
                boolean themaOpen = mVFilter.get_jComboBoxFilterThema().isPopupVisible();
                boolean senderOpen = mVFilter.get_jComboBoxFilterSender().isPopupVisible();
                //Filme neu laden
                listeInModellLaden();
                //Filter Sender
                mVFilter.get_jComboBoxFilterSender().setModel(new javax.swing.DefaultComboBoxModel<>(daten.getListeFilmeNachBlackList().sender));
                mVFilter.get_jComboBoxFilterSender().setSelectedIndex(0);
                if (!filterSender.isEmpty()) {
                    mVFilter.get_jComboBoxFilterSender().setSelectedItem(filterSender);
                    if (mVFilter.get_jComboBoxFilterSender().getSelectedIndex() == 0) {
                        // war wohl nix, der gewählte Sender wurde in die Blacklist eingetragen
                        filterSender = "";
                        listeInModellLaden();
                    }
                }
                mVFilter.get_jComboBoxFilterSender().setPopupVisible(senderOpen);
                // Filter Thema
                if (filterSender.isEmpty()) {
                    mVFilter.get_jComboBoxFilterThema().setModel(new DefaultComboBoxModel<>(getThemen("")));
                } else {
                    mVFilter.get_jComboBoxFilterThema().setModel(new DefaultComboBoxModel<>(getThemen(filterSender)));
                }
                // wenn Thema bei dem Sender vorhanden, dann wieder setzen
                mVFilter.get_jComboBoxFilterThema().setSelectedItem(filterThema);
                if (!filterThema.isEmpty() && mVFilter.get_jComboBoxFilterThema().getSelectedIndex() == 0) {
                    // war wohl nix
                    themaNichtDa = true;
                }
                mVFilter.get_jComboBoxFilterThema().setPopupVisible(themaOpen);
            }
            setInfoStatusbar();
            tabelle.setSpalten();
            updateFilmData();
            stopBeob = false;
            if (themaNichtDa) {
                // Thema gibts beim Sender nicht, nochmal filtern anschieben
                loadTable();
            }
        } catch (Exception ex) {
            Log.errorLog(558965421, ex);
        }

        tabelle.scrollToSelection();

    }

    private synchronized void listeInModellLaden() {
        ListeFilme lf;
        if (mVFilter.get_jToggleButtonHistory().isSelected()) {
            lf = daten.getListeFilmeHistory();
        } else {
            lf = daten.getListeFilmeNachBlackList();
        }
        if (Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_VIS_FILTER))) {
            // normal mit den Filtern aus dem Filterpanel suchen
            GetModelTabFilme.getModelTabFilme(lf, daten, tabelle,
                    mVFilter.get_jComboBoxFilterSender().getSelectedItem().toString(),
                    mVFilter.get_jComboBoxFilterThema().getSelectedItem().toString(), mVFilter.get_jTextFieldFilterTitel().getText(),
                    mVFilter.getThemaTitel() ? mVFilter.get_jTextFieldFilterThemaTitel().getText() : "",
                    mVFilter.getThemaTitel() ? "" : mVFilter.get_jTextFieldFilterThemaTitel().getText(),
                    mVFilter.get_jSliderMinuten().getValue(),
                    mVFilter.get_rbMin().isSelected(),
                    mVFilter.get_jCheckBoxKeineAbos().isSelected(), mVFilter.get_jCheckBoxKeineGesehenen().isSelected(),
                    mVFilter.get_jCheckBoxNurHd().isSelected(), mVFilter.get_jCheckBoxNurUt().isSelected(),
                    mVFilter.get_jToggleButtonLivestram().isSelected(), mVFilter.get_jCheckBoxNeue().isSelected());
        } else {
            // jetzt nur den Filter aus der Toolbar
            GetModelTabFilme.getModelTabFilme(lf, daten, tabelle,
                    "", "", "",
                    getFilterTextFromSearchField(),
                    "",
                    mVFilter.get_jSliderMinuten().getValue(),
                    mVFilter.get_rbMin().isSelected(),
                    mVFilter.get_jCheckBoxKeineAbos().isSelected(), mVFilter.get_jCheckBoxKeineGesehenen().isSelected(),
                    mVFilter.get_jCheckBoxNurHd().isSelected(), mVFilter.get_jCheckBoxNurUt().isSelected(),
                    mVFilter.get_jToggleButtonLivestram().isSelected(), mVFilter.get_jCheckBoxNeue().isSelected());
        }
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jSplitPane1 = new javax.swing.JSplitPane();
        javax.swing.JPanel jPanel1 = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        javax.swing.JTable jTable1 = new javax.swing.JTable();
        jPanelBeschreibung = new javax.swing.JPanel();
        jPanelExtra = new javax.swing.JPanel();
        jCheckBoxProgamme = new javax.swing.JCheckBox();
        jPanelExtraInnen = new javax.swing.JPanel();
        jScrollPaneFilter = new javax.swing.JScrollPane();
        jPanelFilter = new javax.swing.JPanel();
        jPanelToolBar = new javax.swing.JPanel();

        setLayout(new java.awt.BorderLayout(0, 5));

        jSplitPane1.setDividerLocation(240);

        jTable1.setAutoCreateRowSorter(true);
        jTable1.setModel(new TModel());
        jTable1.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_OFF);
        jScrollPane1.setViewportView(jTable1);

        jPanelBeschreibung.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 153)));

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
                        .addGap(0, 587, Short.MAX_VALUE)
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
                        .addComponent(jScrollPane1)
        );
        jPanel1Layout.setVerticalGroup(
                jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanel1Layout.createSequentialGroup()
                                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 328, Short.MAX_VALUE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jPanelBeschreibung, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jPanelExtra, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        );

        jSplitPane1.setRightComponent(jPanel1);

        javax.swing.GroupLayout jPanelFilterLayout = new javax.swing.GroupLayout(jPanelFilter);
        jPanelFilter.setLayout(jPanelFilterLayout);
        jPanelFilterLayout.setHorizontalGroup(
                jPanelFilterLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGap(0, 236, Short.MAX_VALUE)
        );
        jPanelFilterLayout.setVerticalGroup(
                jPanelFilterLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGap(0, 515, Short.MAX_VALUE)
        );

        jScrollPaneFilter.setViewportView(jPanelFilter);

        jSplitPane1.setLeftComponent(jScrollPaneFilter);

        add(jSplitPane1, java.awt.BorderLayout.CENTER);

        javax.swing.GroupLayout jPanelToolBarLayout = new javax.swing.GroupLayout(jPanelToolBar);
        jPanelToolBar.setLayout(jPanelToolBarLayout);
        jPanelToolBarLayout.setHorizontalGroup(
                jPanelToolBarLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGap(0, 0, Short.MAX_VALUE)
        );
        jPanelToolBarLayout.setVerticalGroup(
                jPanelToolBarLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGap(0, 17, Short.MAX_VALUE)
        );

        add(jPanelToolBar, java.awt.BorderLayout.NORTH);
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JCheckBox jCheckBoxProgamme;
    private javax.swing.JPanel jPanelBeschreibung;
    private javax.swing.JPanel jPanelExtra;
    private javax.swing.JPanel jPanelExtraInnen;
    private javax.swing.JPanel jPanelFilter;
    private javax.swing.JPanel jPanelToolBar;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPaneFilter;
    private javax.swing.JSplitPane jSplitPane1;
    // End of variables declaration//GEN-END:variables

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

    private class BeobFilter implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            if (!stopBeob) {
                loadTable();
            }
        }
    }

    private class BeobFilterTitelDoc implements DocumentListener {

        @Override
        public void insertUpdate(DocumentEvent e) {
            tus();
        }

        @Override
        public void removeUpdate(DocumentEvent e) {
            tus();
        }

        @Override
        public void changedUpdate(DocumentEvent e) {
            tus();
        }

        private void tus() {
            Filter.checkPattern1(mVFilter.get_jTextFieldFilterThemaTitel());
            Filter.checkPattern1(mVFilter.get_jTextFieldFilterTitel());
            if (Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_ECHTZEITSUCHE))) {
                loadTable();
            }
        }
    }

    public class BeobMausTabelle extends MouseAdapter {
        //rechhte Maustaste in der Tabelle

        private final BeobPrint beobPrint = new BeobPrint();
        private final BeobFilterLoeschen beobLoeschen = new BeobFilterLoeschen();
        private final BeobAbo beobAbo = new BeobAbo(false /* mit Titel */);
        private final BeobAbo beobAboMitTitel = new BeobAbo(true /* mit Titel */);
        private final BeobAboFilter beobAboFilter = new BeobAboFilter();
        private final BeobFilterThema beobThema = new BeobFilterThema();
        private final BeobFilterSender beobSender = new BeobFilterSender();
        private final BeobFilterSenderThema beobSenderThema = new BeobFilterSenderThema();
        private final BeobFilterSenderThemaTitel beobSenderThemaTitel = new BeobFilterSenderThemaTitel();
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
                    IFilmInformation hud = daten.getMediathekGui().getFilmInformationHud();
                    if (!hud.isVisible()) {
                        hud.showInfo();
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
                            playFilm();
                        }
                    });
                } else if (tabelle.convertColumnIndexToModel(column) == DatenFilm.FILM_AUFZEICHNEN) {
                    saveFilm();
                }
            }
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
            JMenuItem item = new JMenuItem("Film abspielen");
            item.setIcon(Icons.ICON_MENUE_FILM_START);
            item.addActionListener(e -> playFilm());
            jPopupMenu.add(item);
            //Url
            item = new JMenuItem("Film aufzeichnen");
            item.setIcon(Icons.ICON_MENUE_FILM_REC);
            item.addActionListener(e -> saveFilm());
            jPopupMenu.add(item);

            //##Trenner##
            jPopupMenu.addSeparator();
            //##Trenner##
            if (Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_VIS_FILTER))) {
                // nur dann ist das Filterpanel sichtbar
                JMenu submenueFilter = new JMenu("Filter");
                jPopupMenu.add(submenueFilter);
                //Filter löschen
                item = new JMenuItem("Filter löschen");
                item.addActionListener(beobLoeschen);
                submenueFilter.add(item);
                //Sender
                item = new JMenuItem("nach Sender filtern");
                item.addActionListener(beobSender);
                submenueFilter.add(item);
                //Thema
                item = new JMenuItem("nach Thema filtern");
                item.addActionListener(beobThema);
                submenueFilter.add(item);
                //Thema+Sender
                item = new JMenuItem("nach Sender und Thema filtern");
                item.addActionListener(beobSenderThema);
                submenueFilter.add(item);
                //Thema+Sender+Titel
                item = new JMenuItem("nach Sender, Thema und Titel filtern");
                item.addActionListener(beobSenderThemaTitel);
                submenueFilter.add(item);
            }
            JMenu submenueAbo = new JMenu("Abo");
            jPopupMenu.add(submenueAbo);
            //Abo anlegen
            JMenuItem itemAboLoeschen = new JMenuItem("Abo Löschen");
            JMenuItem itemAbo = new JMenuItem("Abo mit Sender und Thema anlegen");
            JMenuItem itemAboMitTitel = new JMenuItem("Abo mit Sender und Thema und Titel anlegen");
            JMenuItem itemAboFilter = new JMenuItem("Abo aus Filter anlegen");
            JMenuItem itemChangeAboFilter = new JMenuItem("Abo ändern");

            res.ifPresent(film -> {
                if ((daten.getListeAbo().getAboFuerFilm_schnell(film, false /*die Länge nicht prüfen*/)) != null) {
                    //gibts schon, dann löschen
                    itemAbo.setEnabled(false);
                    itemAboMitTitel.setEnabled(false);
                    itemAboFilter.setEnabled(false);
                    itemAboLoeschen.addActionListener(beobAbo);

                    // dann können wir auch ändern
                    itemChangeAboFilter.addActionListener(new BeobChangeAbo());
                } else {
                    itemAboLoeschen.setEnabled(false);
                    itemChangeAboFilter.setEnabled(false);
                    //neues Abo anlegen
                    itemAbo.addActionListener(beobAbo);
                    itemAboMitTitel.addActionListener(beobAboMitTitel);
                    itemAboFilter.addActionListener(beobAboFilter);
                }
            });

            submenueAbo.add(itemAboLoeschen);
            submenueAbo.add(itemChangeAboFilter);
            submenueAbo.add(itemAbo);
            submenueAbo.add(itemAboMitTitel);
            submenueAbo.add(itemAboFilter);

            //Programme einblenden
            JMenu submenue = new JMenu("Film mit Set starten");
            jPopupMenu.add(submenue);
            ListePset liste = Daten.listePset.getListeButton();
            for (DatenPset pset : liste) {
                if (pset.getListeProg().isEmpty() && pset.arr[DatenPset.PROGRAMMSET_NAME].isEmpty()) {
                    // ein "leeres" Pset, Platzhalter
                    continue;
                }
                Color col = pset.getFarbe();
                item = new JMenuItem(pset.arr[DatenPset.PROGRAMMSET_NAME]);
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
            if (res.isPresent()) {
                JMenuItem itemDb = new JMenuItem("Titel in der Mediensammlung suchen");
                itemDb.addActionListener(e -> mediensammlung());
                jPopupMenu.add(itemDb);
            }

            //Drucken
            item = new JMenuItem("Tabelle drucken");
            item.addActionListener(beobPrint);
            jPopupMenu.add(item);
            //Infos
            item = new JMenuItem("Filminformation anzeigen");
            item.addActionListener(e -> {
                IFilmInformation hud = daten.getMediathekGui().getFilmInformationHud();
                if (!hud.isVisible()) {
                    hud.showInfo();
                }
            });
            jPopupMenu.add(item);
            //History
            if (res.isPresent()) {
                final DatenFilm film = res.get();
                if (daten.history.urlPruefen(film.getUrlHistory())) {
                    item = new JMenuItem("Film als ungesehen markieren");
                    item.addActionListener(new BeobHistory(false));
                } else {
                    item = new JMenuItem("Film als gesehen markieren");
                    item.addActionListener(new BeobHistory(true));
                }
                jPopupMenu.add(item);
            }
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

        private class BeobFilterThema implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                final int nr = tabelle.rowAtPoint(p);
                if (nr >= 0) {
                    stopBeob = true;
                    Optional<DatenFilm> res = getFilm(nr);
                    res.ifPresent(film -> {
                        final String thema = film.arr[DatenFilm.FILM_THEMA];
                        mVFilter.get_jComboBoxFilterThema().setSelectedIndex(0);
                        mVFilter.get_jComboBoxFilterThema().setSelectedItem(thema);
                    });
                    stopBeob = false;
                    loadTable();
                }
            }
        }

        private class BeobFilterSender implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                final int nr = tabelle.rowAtPoint(p);
                if (nr >= 0) {
                    stopBeob = true;
                    Optional<DatenFilm> res = getFilm(nr);
                    res.ifPresent(film -> {
                        final String sen = film.arr[DatenFilm.FILM_SENDER];
                        mVFilter.get_jComboBoxFilterSender().setSelectedIndex(0);
                        mVFilter.get_jComboBoxFilterSender().setSelectedItem(sen);
                    });
                    stopBeob = false;
                    loadTable();
                }
            }
        }

        private class BeobFilterSenderThema implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                final int nr = tabelle.rowAtPoint(p);
                if (nr >= 0) {
                    stopBeob = true;
                    Optional<DatenFilm> res = getFilm(nr);
                    res.ifPresent(film -> {
                        final String sen = film.arr[DatenFilm.FILM_SENDER];
                        mVFilter.get_jComboBoxFilterSender().setSelectedIndex(0);
                        mVFilter.get_jComboBoxFilterSender().setSelectedItem(sen);
                        final String thema = film.arr[DatenFilm.FILM_THEMA];
                        mVFilter.get_jComboBoxFilterThema().setSelectedIndex(0);
                        mVFilter.get_jComboBoxFilterThema().setSelectedItem(thema);
                        if (mVFilter.get_jComboBoxFilterThema().getSelectedIndex() == 0) {
                            final String themaFilter = getThemaFilter(sen, thema);
                            mVFilter.get_jComboBoxFilterThema().setSelectedItem(themaFilter);
                        }
                    });
                    stopBeob = false;
                    loadTable();
                }
            }
        }

        private class BeobFilterSenderThemaTitel implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                final int nr = tabelle.rowAtPoint(p);
                if (nr >= 0) {
                    stopBeob = true;
                    Optional<DatenFilm> res = getFilm(nr);
                    res.ifPresent(film -> {
                        final String sen = film.arr[DatenFilm.FILM_SENDER];
                        mVFilter.get_jComboBoxFilterSender().setSelectedIndex(0);
                        mVFilter.get_jComboBoxFilterSender().setSelectedItem(sen);
                        final String thema = film.arr[DatenFilm.FILM_THEMA];
                        mVFilter.get_jComboBoxFilterThema().setSelectedIndex(0);
                        mVFilter.get_jComboBoxFilterThema().setSelectedItem(thema);
                        if (mVFilter.get_jComboBoxFilterThema().getSelectedIndex() == 0) {
                            final String themaFilter = getThemaFilter(sen, thema);
                            mVFilter.get_jComboBoxFilterThema().setSelectedItem(themaFilter);
                        }
                        final String tit = film.arr[DatenFilm.FILM_TITEL];
                        mVFilter.get_jTextFieldFilterTitel().setText(tit);
                    });
                    stopBeob = false;
                    loadTable();
                }
            }
        }

        private String getThemaFilter(String sender, String thema) {
            // Thema für den Filter suchen bei zB: "Hallo" und "hallo" steht nur eines im FilterThema
            String ret = "";
            for (int i = 1; i < daten.getListeFilmeNachBlackList().themenPerSender.length; ++i) {
                if (daten.getListeFilmeNachBlackList().sender[i].equals(sender)) {
                    for (int k = 1; k < daten.getListeFilmeNachBlackList().themenPerSender[i].length; ++k) {
                        if (daten.getListeFilmeNachBlackList().themenPerSender[i][k].equalsIgnoreCase(thema)) {
                            ret = daten.getListeFilmeNachBlackList().themenPerSender[i][k];
                        }
                    }
                }
            }
            return ret;
        }

        private class BeobFilterLoeschen implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                guiFilmeFilterLoeschen();
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

            private boolean mitTitel = false;

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

        private class BeobAboFilter implements ActionListener {

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
                            final String thema = film.arr[DatenFilm.FILM_THEMA];
                            //neues Abo anlegen
                            daten.getListeAbo().addAbo(mVFilter.get_jComboBoxFilterSender().getSelectedItem().toString(),
                                    mVFilter.get_jComboBoxFilterThema().getSelectedItem().toString(),
                                    mVFilter.get_jTextFieldFilterTitel().getText(),
                                    mVFilter.getThemaTitel() ? mVFilter.get_jTextFieldFilterThemaTitel().getText() : "",
                                    mVFilter.getThemaTitel() ? "" : mVFilter.get_jTextFieldFilterThemaTitel().getText(),
                                    mVFilter.get_jSliderMinuten().getValue(),
                                    mVFilter.get_rbMin().isSelected(),
                                    thema);
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

}
