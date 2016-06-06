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

import mediathek.controller.Log;
import mediathek.controller.starter.Start;
import mediathek.daten.*;
import mediathek.gui.dialog.DialogAddDownload;
import mediathek.gui.dialog.DialogEditAbo;
import mediathek.gui.dialog.MVFilmInfo;
import mediathek.res.GetIcon;
import mediathek.tool.*;
import msearch.daten.DatenFilm;
import msearch.daten.ListeFilme;
import msearch.filmeSuchen.MSListenerFilmeLaden;
import msearch.filmeSuchen.MSListenerFilmeLadenEvent;
import msearch.tool.Datum;

import javax.swing.*;
import javax.swing.RowSorter.SortKey;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.*;
import java.awt.event.*;
import java.awt.print.PrinterException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Optional;

public class GuiFilme extends PanelVorlage {

    private JButton buttonArray[];
    private final String[] COMBO_ZEIT = new String[]{"alles", "1 Tag", "2 Tage", "3 Tage", "7 Tage", "15 Tage", "20 Tage", "30 Tage"};
    public static final int[] COMBO_ZEIT_INT = {0, 1, 2, 3, 7, 15, 20, 30};
    private static final int FILTER_ZEIT_STARTWERT = 5;
    private static final int FILTER_DAUER_STARTWERT = 0;
    private final MVFilmInfo filmInfo;
    private MVFilter mVFilter;
    public MVFilterFrame mVFilterFrame;
    private final MVFilterPanel mVFilterPanel;

    public GuiFilme(Daten d, JFrame parentComponent) {
        super(d, parentComponent);
        initComponents();
        tabelle = new MVTable(MVTable.TABELLE_TAB_FILME);
        jScrollPane1.setViewportView(tabelle);
        jScrollPaneFilter.getVerticalScrollBar().setUnitIncrement(16);
        panelVideoplayerSetzen();
        mVFilterPanel = new MVFilterPanel(parentComponent, daten) {
            @Override
            public void mvFfilter(int i) {
                filter(i);
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
        mVFilterFrame = new MVFilterFrame(d) {
            @Override
            public void mvFfilter(int i) {
                filter(i);
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

        setupDescriptionPanel();

        jPanelFilter.setLayout(new BorderLayout());
        filmInfo = daten.filmInfo;
    }

    private void setupDescriptionPanel() {
        PanelFilmBeschreibung panelBeschreibung = new PanelFilmBeschreibung(daten.mediathekGui, daten, tabelle);
        jPanelBeschreibung.setLayout(new BorderLayout());
        jPanelBeschreibung.add(panelBeschreibung, BorderLayout.CENTER);
    }

    //===================================
    // Public
    //===================================
    @Override
    public void isShown() {
        super.isShown();
        daten.mediathekGui.setToolbar(MVToolBar.TOOLBAR_TAB_FILME);
        daten.mediathekGui.getStatusBar().setIndexForLeftDisplay(MVStatusBar.StatusbarIndex.FILME);
        updateFilmData();
        setInfo();
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

    public void searchUrl(String url) {
        searchUrl_(url);
    }

    public void filmGesehen() {
        daten.history.setGesehen(true, getSelFilme());
    }

    public void filmUngesehen() {
        daten.history.setGesehen(false, getSelFilme());
    }

    public void init() {
        showDescriptionPanel();
        Daten.filmeLaden.addAdListener(new MSListenerFilmeLaden() {
            @Override
            public void start(MSListenerFilmeLadenEvent event) {
                mVFilter.enableFilter(false);
                loadTable();
            }

            @Override
            public void fertig(MSListenerFilmeLadenEvent event) {
                loadTable();
                mVFilter.enableFilter(true);
            }
        });
        daten.mediathekGui.getRootPane().getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_S, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), "sender");
        daten.mediathekGui.getRootPane().getActionMap().put("sender", new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {

                SortKey sk = new SortKey(DatenFilm.FILM_SENDER_NR, SortOrder.ASCENDING);
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
                if (!filmInfo.isVisible()) {
                    filmInfo.showInfo();
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

        this.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_H, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), "url-hd-copy");
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
                    Daten.mVConfig.add(MVConfig.SYSTEM_MEDIA_DB_DIALOG_ANZEIGEN, Boolean.TRUE.toString());
                    daten.dialogMediaDB.setVis();
                    daten.dialogMediaDB.setFilter(film.arr[DatenFilm.FILM_TITEL_NR]);
                }
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
            if (!m.isSelectionEmpty() && !m.getValueIsAdjusting() && !stopBeob)
                updateFilmData();
        });

        final CellRendererFilme cellRenderer = new CellRendererFilme(daten);
        tabelle.setDefaultRenderer(Object.class, cellRenderer);
        tabelle.setDefaultRenderer(Datum.class, cellRenderer);
        tabelle.setDefaultRenderer(Integer.class, cellRenderer);

        tabelle.getTableHeader().addMouseListener(new BeobTableHeader(tabelle, DatenFilm.COLUMN_NAMES, DatenFilm.spaltenAnzeigen,
                new int[]{DatenFilm.FILM_ABSPIELEN_NR, DatenFilm.FILM_AUFZEICHNEN_NR, DatenFilm.FILM_DATUM_LONG_NR, DatenFilm.FILM_REF_NR},
                new int[]{DatenFilm.FILM_ABSPIELEN_NR, DatenFilm.FILM_AUFZEICHNEN_NR},
                true /*Icon*/));

        jCheckBoxProgamme.setIcon(GetIcon.getProgramIcon("close_15.png"));
        jCheckBoxProgamme.addActionListener(e -> {
            Daten.mVConfig.add(MVConfig.SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN, Boolean.FALSE.toString());
            daten.mediathekGui.videoplayerAnzeigen(true);
            panelVideoplayerSetzen();
        });
        setSplitPane();
        jSplitPane1.addPropertyChangeListener(JSplitPane.DIVIDER_LOCATION_PROPERTY,
                pce -> {
                    if (jScrollPaneFilter.isVisible()) {
                        Daten.mVConfig.add(MVConfig.SYSTEM_PANEL_FILME_DIVIDER, String.valueOf(jSplitPane1.getDividerLocation()));
                    }
                });

        setFilterPanel();
        MVListeFilme.checkBlacklist();
        loadTable(); //Filme laden
        tabelle.initTabelle();
        if (tabelle.getRowCount() > 0) {
            tabelle.setRowSelectionInterval(0, 0);
        }
        addMVListener();
    }

    public int getTableRowCount() {
        if (tabelle != null) {
            return tabelle.getModel().getRowCount();
        } else {
            return 0;
        }
    }

    //===================================
    // Private
    //===================================
    private void addMVListener() {
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_LISTE_PSET, GuiFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                panelVideoplayerSetzen();
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_LISTE_HISTORY_GEAENDERT, GuiFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                if (mVFilter.get_jCheckBoxKeineGesehenen().isSelected() || mVFilter.get_jToggleButtonHistory().isSelected()) {
                    loadTable();
                } else {
                    tabelle.fireTableDataChanged(true);
                }
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_LISTE_ABOS, GuiFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                loadTable();
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_BESCHREIBUNG, GuiFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                loadTable();
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_BLACKLIST_GEAENDERT, GuiFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                loadTable();
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_FILMLISTE_GEAENDERT, GuiFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                MVListeFilme.checkBlacklist();
                loadTable();
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_START_EVENT_BUTTON, GuiFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                tabelle.fireTableDataChanged(true /*setSpalten*/);
                setInfo();
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_GEO, GuiFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                MVListeFilme.checkBlacklist();
                loadTable();
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(new int[]{/*ListenerMediathekView.EREIGNIS_ART_DOWNLOAD_PROZENT,*/
            ListenerMediathekView.EREIGNIS_START_EVENT, ListenerMediathekView.EREIGNIS_LISTE_DOWNLOADS}, GuiFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                setInfo();
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_PANEL_FILTER_ANZEIGEN, GuiFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                // Panel anzeigen und die Filmliste anpassen
                setFilterPanel();
                //setFilterAction();
                MVListeFilme.checkBlacklist();
                loadTable();
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_PANEL_BESCHREIBUNG_ANZEIGEN, GuiFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                showDescriptionPanel();
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_SUCHFELD_FOCUS_SETZEN, GuiFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                if (Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_VIS_FILTER))) {
                    mVFilter.get_jTextFieldFilterThemaTitel().requestFocus();
                    mVFilter.get_jTextFieldFilterThemaTitel().setCaretPosition(0);
                }
            }
        });
    }

    /**
     * Show description panel based on settings.
     */
    private void showDescriptionPanel() {
        jPanelBeschreibung.setVisible(Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_PANEL_BESCHREIBUNG_ANZEIGEN)));
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
            MVMessageDialog.showMessageDialog(parentComponent, "Im Menü unter \"Datei->Einstellungen->Set bearbeiten\" ein Programm zum Aufzeichnen festlegen.",
                    "fehlende Einstellungen zum Speichern!", JOptionPane.INFORMATION_MESSAGE);
            // Satz mit x, war wohl nix
            return;
        }

        ArrayList<DatenFilm> liste = getSelFilme();
        for (DatenFilm datenFilm : liste) {
            // erst mal schauen obs den schon gibt
            DatenDownload datenDownload = Daten.listeDownloads.getDownloadUrlFilm(datenFilm.arr[DatenFilm.FILM_URL_NR]);
            if (datenDownload != null) {
                int ret = JOptionPane.showConfirmDialog(parentComponent, "Download für den Film existiert bereits.\n"
                        + "Nochmal anlegen?", "Anlegen?", JOptionPane.YES_NO_OPTION);
                if (ret != JOptionPane.OK_OPTION) {
                    continue;
                }
            }

            // weiter
            String aufloesung = "";
            if (mVFilter.get_jCheckBoxNurHd().isSelected()) {
                aufloesung = DatenFilm.AUFLOESUNG_HD;
            }
            DialogAddDownload dialog = new DialogAddDownload(daten.mediathekGui, daten, datenFilm, pSet, aufloesung);
            dialog.setVisible(true);
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
            //DatenFilm datenFilm = Daten.listeFilmeNachBlackList.getFilmByUrl(tabelle.getModel().getValueAt(selectedModelRow, DatenFilm.FILM_URL_NR).toString());
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
            Daten.mVConfig.add(MVConfig.SYSTEM_MEDIA_DB_DIALOG_ANZEIGEN, Boolean.TRUE.toString());
            daten.dialogMediaDB.setVis();
            daten.dialogMediaDB.setFilter(filmSelection.get().arr[DatenFilm.FILM_TITEL_NR]);
        }
    }

    /**
     * Update Film Information and description panel with updated film...
     */
    private void updateFilmData() {
        final Optional<DatenFilm> filmSelection = getCurrentlySelectedFilm();
        filmSelection.ifPresent(filmInfo::updateCurrentFilm);
    }

    private Optional<DatenFilm> getCurrentlySelectedFilm() {
        final int selectedTableRow = tabelle.getSelectedRow();
        if (selectedTableRow >= 0)
            return Optional.of((DatenFilm) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(selectedTableRow), DatenFilm.FILM_REF_NR));
        else
            return Optional.empty();
    }

    private DatenFilm getFilm(int zeileTabelle) {
        if (zeileTabelle >= 0 && zeileTabelle < tabelle.getRowCount()) {
            return (DatenFilm) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(zeileTabelle), DatenFilm.FILM_REF_NR);
        }
        return null;
    }

    private void setInfo() {
        // Infopanel setzen
        daten.mediathekGui.getStatusBar().setTextForLeftDisplay();
    }

    private ArrayList<DatenFilm> getSelFilme() {
        ArrayList<DatenFilm> arrayFilme = new ArrayList<>();
        int rows[] = tabelle.getSelectedRows();
        if (rows.length > 0) {
            for (int row : rows) {
                DatenFilm datenFilm = (DatenFilm) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(row), DatenFilm.FILM_REF_NR);
                arrayFilme.add(datenFilm);
            }
        } else {
            new HinweisKeineAuswahl().zeigen(parentComponent);
        }
        return arrayFilme;
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
        ListePset listeButton = Daten.listePset.getListeButton();
        int maxSpalten = 4; //Anzahl der Spalten der Schalter
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
        jPanelExtra.setVisible(Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN)));
    }

    private Component addExtraFeld(int i, int spalte, int zeile, GridBagLayout gridbag, GridBagConstraints c, JPanel panel, ListePset liste) {
        Component ret;
        JButton button;
        c.gridx = spalte;
        c.gridy = zeile;
        if (liste.get(i).isLable()) {
            JLabel label = new JLabel(liste.get(i).arr[DatenPset.PROGRAMMSET_NAME_NR]);
            Color col = liste.get(i).getFarbe();
            if (col != null) {
                label.setForeground(col);
            }
            gridbag.setConstraints(label, c);
            panel.add(label);
            ret = label;
        } else {
            button = new JButton(liste.get(i).arr[DatenPset.PROGRAMMSET_NAME_NR]);
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
    private void searchUrl_(String url) {
        // nur für Tests
        url = url.trim();
        TModelFilm m = (TModelFilm) tabelle.getModel();
        for (int i = 0; i < m.getRowCount(); ++i) {
            if (!url.equals(m.getValueAt(i, DatenFilm.FILM_URL_NR).toString())) {
                m.removeRow(i);
                --i;
            }
        }
    }

    private void setFilterPanel() {
        boolean history = false;
        if (mVFilter != null) {
            mVFilter.removeAllListener();
            history = mVFilter.get_jToggleButtonHistory().isSelected();
        }
        if (Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_FENSTER_FILTER))) {
            jPanelFilter.removeAll();
            jScrollPaneFilter.setVisible(false);
            mVFilter = mVFilterFrame;
            mVFilterFrame.setVisible(Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_VIS_FILTER)));
        } else {
            mVFilterFrame.setVisible(false);
            mVFilter = mVFilterPanel;
            jPanelFilter.add(mVFilterPanel, BorderLayout.CENTER);
            jScrollPaneFilter.setVisible(Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_VIS_FILTER)));
            if (jScrollPaneFilter.isVisible()) {
                setSplitPane();
            }
        }
        // einrichten
        mVFilter.setVisible(Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_VIS_FILTER)));
        mVFilter.get_jComboBoxZeitraum().setModel(new DefaultComboBoxModel<>(COMBO_ZEIT));
        mVFilter.get_jComboBoxFilterSender().setModel(new javax.swing.DefaultComboBoxModel<>(Daten.listeFilmeNachBlackList.sender));
        mVFilter.get_jComboBoxFilterThema().setModel(new javax.swing.DefaultComboBoxModel<>(getThemen("")));
        mVFilter.get_jToggleButtonHistory().setSelected(history);
        try {
            mVFilter.get_jComboBoxZeitraum().setSelectedIndex(Integer.parseInt(Daten.mVConfig.get(MVConfig.SYSTEM_FILTER__TAGE)));
        } catch (Exception ignored) {
            mVFilter.get_jComboBoxZeitraum().setSelectedIndex(FILTER_ZEIT_STARTWERT);
        }
        mVFilter.get_jSliderMinuten().setValue(FILTER_DAUER_STARTWERT);
        mVFilter.get_jTextFieldFilterMinuten().setText(String.valueOf(mVFilter.get_jSliderMinuten().getValue()));

        setFilterAction();
        this.updateUI();
    }

    private void setSplitPane() {
        try {
            jSplitPane1.setDividerLocation(Integer.parseInt(Daten.mVConfig.get(MVConfig.SYSTEM_PANEL_FILME_DIVIDER)));
        } catch (Exception ignore) {
            Daten.mVConfig.add(MVConfig.SYSTEM_PANEL_FILME_DIVIDER, Konstanten.GUIFILME_DIVIDER_LOCATION);
            jSplitPane1.setDividerLocation(Integer.parseInt(Konstanten.GUIFILME_DIVIDER_LOCATION));
        }
    }

    private void setFilterAction() {
        mVFilter.get_jComboBoxZeitraum().addActionListener(e -> {
            Daten.mVConfig.add(MVConfig.SYSTEM_FILTER__TAGE, String.valueOf(mVFilter.get_jComboBoxZeitraum().getSelectedIndex()));
            if (!stopBeob) {
                MVListeFilme.checkBlacklist();
                loadTable();
            }
        });
        //beobachter Filter
        mVFilter.get_jToggleButtonLivestram().addActionListener(e -> {
            if (!stopBeob) {
                stopBeob = true;
                //auch die Filter löschen
                mVFilter.get_jComboBoxFilterSender().setModel(new DefaultComboBoxModel<>(Daten.listeFilmeNachBlackList.sender));
                mVFilter.get_jComboBoxFilterThema().setModel(new DefaultComboBoxModel<>(getThemen("")));
                mVFilter.get_jTextFieldFilterTitel().setText("");
            }
            loadTable();
        });
        //Combo Sender
        mVFilter.get_jButtonFilterLoeschen().addActionListener(new BeobFilterLoeschen());
        mVFilter.get_jComboBoxFilterSender().addActionListener(new BeobFilter());
        mVFilter.get_jComboBoxFilterThema().addActionListener(new BeobFilter());
        mVFilter.get_jTextFieldFilterTitel().addActionListener(new BeobFilter());
        mVFilter.get_jTextFieldFilterTitel().getDocument().addDocumentListener(new BeobFilterTitelDoc());
        mVFilter.get_jTextFieldFilterThemaTitel().addActionListener(new BeobFilter());
        mVFilter.get_jTextFieldFilterThemaTitel().getDocument().addDocumentListener(new BeobFilterTitelDoc());
        mVFilter.get_jSliderMinuten().addChangeListener(e -> {
            if (!stopBeob) {
                mVFilter.get_jTextFieldFilterMinuten().setText(String.valueOf(mVFilter.get_jSliderMinuten().getValue()));
                if (!mVFilter.get_jSliderMinuten().getValueIsAdjusting()) {
                    loadTable();
                }
            }
        });
        mVFilter.get_jCheckBoxKeineAbos().addActionListener(new BeobFilter());
        mVFilter.get_jCheckBoxKeineGesehenen().addActionListener(new BeobFilter());
        mVFilter.get_jCheckBoxNurHd().addActionListener(new BeobFilter());
        mVFilter.get_jCheckBoxNeue().addActionListener(new BeobFilter());
        mVFilter.get_jToggleButtonHistory().addActionListener(new BeobFilter());
        mVFilter.get_jRadioButtonTT().addActionListener(new BeobFilter());
        mVFilter.get_JRadioButtonIrgendwo().addActionListener(new BeobFilter());
    }

    private void filter(int filter) {
        stopBeob = true;
        boolean geändert = false;
        try {
            mVFilter.get_jTextFieldFilterTitel().setText(Daten.mVConfig.get(MVConfig.SYSTEM_FILTER_PROFILE__TITEL, filter));
            mVFilter.get_jTextFieldFilterThemaTitel().setText(Daten.mVConfig.get(MVConfig.SYSTEM_FILTER_PROFILE__THEMA_TITEL, filter));
            if (Daten.mVConfig.get(MVConfig.SYSTEM_FILTER_PROFILE__TT, filter).isEmpty()) {
                Daten.mVConfig.add(MVConfig.SYSTEM_FILTER_PROFILE__TT, Boolean.TRUE.toString(), filter, MVFilter.MAX_FILTER);
            }
            mVFilter.setThemaTitel(Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_FILTER_PROFILE__TT, filter)));

            mVFilter.get_jCheckBoxKeineAbos().setSelected(Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_FILTER_PROFILE_KEINE_ABO, filter)));
            mVFilter.get_jCheckBoxKeineGesehenen().setSelected(Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_FILTER_PROFILE_KEINE_GESEHENE, filter)));
            mVFilter.get_jCheckBoxNurHd().setSelected(Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_FILTER_PROFILE__NUR_HD, filter)));
            mVFilter.get_jCheckBoxNeue().setSelected(Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_FILTER_PROFILE__NUR_NEUE, filter)));
            mVFilter.get_jComboBoxZeitraum().setSelectedIndex(Integer.parseInt(Daten.mVConfig.get(MVConfig.SYSTEM_FILTER_PROFILE__TAGE, filter)));
            // Blackliste
            if (Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_FILTER_PROFILE__BLACKLIST_ON, filter))
                    != Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_BLACKLIST_ON))) {
                geändert = true;
                Daten.mVConfig.add(MVConfig.SYSTEM_BLACKLIST_ON, Daten.mVConfig.get(MVConfig.SYSTEM_FILTER_PROFILE__BLACKLIST_ON, filter));
            }

        } catch (NumberFormatException ex) {
            mVFilter.get_jComboBoxZeitraum().setSelectedIndex(FILTER_ZEIT_STARTWERT);
            Daten.mVConfig.add(MVConfig.SYSTEM_FILTER_PROFILE__TAGE, FILTER_ZEIT_STARTWERT + "", filter, MVFilter.MAX_FILTER);
        }
        try {
            mVFilter.get_jSliderMinuten().setValue(Integer.parseInt(Daten.mVConfig.get(MVConfig.SYSTEM_FILTER_PROFILE__DAUER, filter)));
        } catch (Exception ex) {
            mVFilter.get_jSliderMinuten().setValue(FILTER_DAUER_STARTWERT);
            Daten.mVConfig.add(MVConfig.SYSTEM_FILTER_PROFILE__DAUER, FILTER_DAUER_STARTWERT + "", filter, MVFilter.MAX_FILTER);
        }
        mVFilter.get_jTextFieldFilterMinuten().setText(String.valueOf(mVFilter.get_jSliderMinuten().getValue()));

        // und jetzt wieder laden
        MVListeFilme.checkBlacklist();

        // erst jetzt da Sender/Thema evtl. in der Blacklist
        mVFilter.get_jComboBoxFilterSender().setModel(new javax.swing.DefaultComboBoxModel<>(Daten.listeFilmeNachBlackList.sender));
        mVFilter.get_jComboBoxFilterThema().setModel(new javax.swing.DefaultComboBoxModel<>(getThemen("")));
        mVFilter.get_jComboBoxFilterSender().setSelectedItem(Daten.mVConfig.get(MVConfig.SYSTEM_FILTER_PROFILE__SENDER, filter));
        mVFilter.get_jComboBoxFilterThema().setSelectedItem(Daten.mVConfig.get(MVConfig.SYSTEM_FILTER_PROFILE__THEMA, filter));

        stopBeob = false;

        if (geändert) {
            ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_BLACKLIST_GEAENDERT, GuiFilme.class.getSimpleName());
        }
        //dann laden
        loadTable();
        //beim Filter umschalten immer auf die erste Zeile setzen
        tabelle.setSelRow(0);
    }

    private void delFilter() {
        stopBeob = true;
        delFilter_();
        stopBeob = false;
        // und jetzt wieder laden
        MVListeFilme.checkBlacklist();
        loadTable();
    }

    private void delFilter_() {
        mVFilter.get_jComboBoxFilterSender().setModel(new javax.swing.DefaultComboBoxModel<>(Daten.listeFilmeNachBlackList.sender));
        mVFilter.get_jComboBoxFilterThema().setModel(new javax.swing.DefaultComboBoxModel<>(getThemen("")));
        mVFilter.get_jTextFieldFilterTitel().setText("");
        mVFilter.get_jTextFieldFilterThemaTitel().setText("");
        mVFilter.setThemaTitel(true);
    }

    private void delFilterProfile(int filter) {
        // jetzt noch speichern
        Daten.mVConfig.add(MVConfig.SYSTEM_FILTER_PROFILE__SENDER, String.valueOf(""), filter, MVFilter.MAX_FILTER);
        Daten.mVConfig.add(MVConfig.SYSTEM_FILTER_PROFILE__THEMA, String.valueOf(""), filter, MVFilter.MAX_FILTER);

        Daten.mVConfig.add(MVConfig.SYSTEM_FILTER_PROFILE__TITEL, String.valueOf(""), filter, MVFilter.MAX_FILTER);
        Daten.mVConfig.add(MVConfig.SYSTEM_FILTER_PROFILE__THEMA_TITEL, String.valueOf(""), filter, MVFilter.MAX_FILTER);
        Daten.mVConfig.add(MVConfig.SYSTEM_FILTER_PROFILE__TT, Boolean.toString(true), filter, MVFilter.MAX_FILTER);

        Daten.mVConfig.add(MVConfig.SYSTEM_FILTER_PROFILE_KEINE_ABO, String.valueOf(false), filter, MVFilter.MAX_FILTER);
        Daten.mVConfig.add(MVConfig.SYSTEM_FILTER_PROFILE_KEINE_GESEHENE, String.valueOf(false), filter, MVFilter.MAX_FILTER);
        Daten.mVConfig.add(MVConfig.SYSTEM_FILTER_PROFILE__NUR_HD, String.valueOf(false), filter, MVFilter.MAX_FILTER);
        Daten.mVConfig.add(MVConfig.SYSTEM_FILTER_PROFILE__NUR_NEUE, String.valueOf(false), filter, MVFilter.MAX_FILTER);

        Daten.mVConfig.add(MVConfig.SYSTEM_FILTER_PROFILE__TAGE, FILTER_ZEIT_STARTWERT + "", filter, MVFilter.MAX_FILTER);
        Daten.mVConfig.add(MVConfig.SYSTEM_FILTER_PROFILE__DAUER, FILTER_DAUER_STARTWERT + "", filter, MVFilter.MAX_FILTER);
        Daten.mVConfig.add(MVConfig.SYSTEM_FILTER_PROFILE__BLACKLIST_ON, Boolean.FALSE.toString(), filter, MVFilter.MAX_FILTER);
    }

    private void saveFilterProfile(int filter) {
        // jetzt noch speichern
        Daten.mVConfig.add(MVConfig.SYSTEM_FILTER_PROFILE__SENDER, String.valueOf(mVFilter.get_jComboBoxFilterSender().getSelectedItem()), filter, MVFilter.MAX_FILTER);
        Daten.mVConfig.add(MVConfig.SYSTEM_FILTER_PROFILE__THEMA, String.valueOf(mVFilter.get_jComboBoxFilterThema().getSelectedItem()), filter, MVFilter.MAX_FILTER);

        Daten.mVConfig.add(MVConfig.SYSTEM_FILTER_PROFILE__TITEL, String.valueOf(mVFilter.get_jTextFieldFilterTitel().getText()), filter, MVFilter.MAX_FILTER);
        Daten.mVConfig.add(MVConfig.SYSTEM_FILTER_PROFILE__THEMA_TITEL, String.valueOf(mVFilter.get_jTextFieldFilterThemaTitel().getText()), filter, MVFilter.MAX_FILTER);
        Daten.mVConfig.add(MVConfig.SYSTEM_FILTER_PROFILE__TT, Boolean.toString(mVFilter.getThemaTitel()), filter, MVFilter.MAX_FILTER);

        Daten.mVConfig.add(MVConfig.SYSTEM_FILTER_PROFILE_KEINE_ABO, String.valueOf(mVFilter.get_jCheckBoxKeineAbos().isSelected()), filter, MVFilter.MAX_FILTER);
        Daten.mVConfig.add(MVConfig.SYSTEM_FILTER_PROFILE_KEINE_GESEHENE, String.valueOf(mVFilter.get_jCheckBoxKeineGesehenen().isSelected()), filter, MVFilter.MAX_FILTER);
        Daten.mVConfig.add(MVConfig.SYSTEM_FILTER_PROFILE__NUR_HD, String.valueOf(mVFilter.get_jCheckBoxNurHd().isSelected()), filter, MVFilter.MAX_FILTER);
        Daten.mVConfig.add(MVConfig.SYSTEM_FILTER_PROFILE__NUR_NEUE, String.valueOf(mVFilter.get_jCheckBoxNeue().isSelected()), filter, MVFilter.MAX_FILTER);

        Daten.mVConfig.add(MVConfig.SYSTEM_FILTER_PROFILE__TAGE, String.valueOf(mVFilter.get_jComboBoxZeitraum().getSelectedIndex()), filter, MVFilter.MAX_FILTER);
        Daten.mVConfig.add(MVConfig.SYSTEM_FILTER_PROFILE__DAUER, String.valueOf(mVFilter.get_jSliderMinuten().getValue()), filter, MVFilter.MAX_FILTER);
    }

    public int getFilterTage() {
        return mVFilter.get_jComboBoxZeitraum().getSelectedIndex();
    }

    private String[] getThemen(String ssender) {
        for (int i = 1; i < Daten.listeFilmeNachBlackList.themenPerSender.length; ++i) {
            if (Daten.listeFilmeNachBlackList.sender[i].equals(ssender)) {
                return Daten.listeFilmeNachBlackList.themenPerSender[i];
            }
        }
        return Daten.listeFilmeNachBlackList.themenPerSender[0];
        //return alleThemen;
    }

    // ####################################
    // Tabelle laden
    // ####################################
    private synchronized void loadTable() {
        boolean themaNichtDa = false;
        try {
            stopBeob = true;
            tabelle.getSpalten();
            if (Daten.listeFilmeNachBlackList.isEmpty()) {
                // die Liste in leer
                delFilter_();
                listeInModellLaden(); // zum löschen der Tabelle
            } else if (!Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_VIS_FILTER))) {
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
                mVFilter.get_jComboBoxFilterSender().setModel(new javax.swing.DefaultComboBoxModel<>(Daten.listeFilmeNachBlackList.sender));
                mVFilter.get_jComboBoxFilterSender().setSelectedIndex(0);
                if (!filterSender.equals("")) {
                    mVFilter.get_jComboBoxFilterSender().setSelectedItem(filterSender);
                    if (mVFilter.get_jComboBoxFilterSender().getSelectedIndex() == 0) {
                        // war wohl nix, der gewählte Sender wurde in die Blacklist eingetragen
                        filterSender = "";
                        listeInModellLaden();
                    }
                }
                mVFilter.get_jComboBoxFilterSender().setPopupVisible(senderOpen);
                // Filter Thema
                if (filterSender.equals("")) {
                    mVFilter.get_jComboBoxFilterThema().setModel(new javax.swing.DefaultComboBoxModel<>(getThemen("")));
                } else {
                    mVFilter.get_jComboBoxFilterThema().setModel(new javax.swing.DefaultComboBoxModel<>(getThemen(filterSender)));
                }
                // wenn Thema bei dem Sender vorhanden, dann wieder setzen
                mVFilter.get_jComboBoxFilterThema().setSelectedItem(filterThema);
                if (!filterThema.equals("") && mVFilter.get_jComboBoxFilterThema().getSelectedIndex() == 0) {
                    // war wohl nix
                    themaNichtDa = true;
                }
                mVFilter.get_jComboBoxFilterThema().setPopupVisible(themaOpen);
            }
            setInfo();
            tabelle.setSpalten();
            updateFilmData();
            stopBeob = false;
            if (themaNichtDa) {
                // Thema gibts beim Sender nicht, nochmal filtern anschieben
                loadTable();
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(558965421, ex);
        }

        tabelle.scrollToSelection();

    }

    private synchronized void listeInModellLaden() {
        ListeFilme lf;
        if (mVFilter.get_jToggleButtonHistory().isSelected()) {
            lf = Daten.listeFilmeHistory;
        } else {
            lf = Daten.listeFilmeNachBlackList;
        }
        if (Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_VIS_FILTER))) {
            // normal mit den Filtern aus dem Filterpanel suchen
            MVListeFilme.getModelTabFilme(lf, daten, tabelle,
                    mVFilter.get_jComboBoxFilterSender().getSelectedItem().toString(),
                    mVFilter.get_jComboBoxFilterThema().getSelectedItem().toString(), mVFilter.get_jTextFieldFilterTitel().getText(),
                    mVFilter.getThemaTitel() ? mVFilter.get_jTextFieldFilterThemaTitel().getText() : "",
                    mVFilter.getThemaTitel() ? "" : mVFilter.get_jTextFieldFilterThemaTitel().getText(),
                    mVFilter.get_jSliderMinuten().getValue(),
                    mVFilter.get_jCheckBoxKeineAbos().isSelected(), mVFilter.get_jCheckBoxKeineGesehenen().isSelected(),
                    mVFilter.get_jCheckBoxNurHd().isSelected(), mVFilter.get_jToggleButtonLivestram().isSelected(), mVFilter.get_jCheckBoxNeue().isSelected());
        } else {
            // jetzt nur den Filter aus der Toolbar
            MVListeFilme.getModelTabFilme(lf, daten, tabelle,
                    "", "", "",
                    daten.mediathekGui.getFilterTextFromSearchField(),
                    "",
                    mVFilter.get_jSliderMinuten().getValue(),
                    mVFilter.get_jCheckBoxKeineAbos().isSelected(), mVFilter.get_jCheckBoxKeineGesehenen().isSelected(),
                    mVFilter.get_jCheckBoxNurHd().isSelected(), mVFilter.get_jToggleButtonLivestram().isSelected(), mVFilter.get_jCheckBoxNeue().isSelected());
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
            .addGap(0, 128, Short.MAX_VALUE)
        );

        jPanelExtra.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 153)));

        jCheckBoxProgamme.setFont(new java.awt.Font("Dialog", 1, 10)); // NOI18N
        jCheckBoxProgamme.setToolTipText("Buttons ausblenden");

        javax.swing.GroupLayout jPanelExtraInnenLayout = new javax.swing.GroupLayout(jPanelExtraInnen);
        jPanelExtraInnen.setLayout(jPanelExtraInnenLayout);
        jPanelExtraInnenLayout.setHorizontalGroup(
            jPanelExtraInnenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 0, Short.MAX_VALUE)
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
            .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 612, Short.MAX_VALUE)
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 337, Short.MAX_VALUE)
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
            .addGap(0, 504, Short.MAX_VALUE)
        );

        jScrollPaneFilter.setViewportView(jPanelFilter);

        jSplitPane1.setLeftComponent(jScrollPaneFilter);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jSplitPane1)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jSplitPane1)
        );
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JCheckBox jCheckBoxProgamme;
    private javax.swing.JPanel jPanelBeschreibung;
    private javax.swing.JPanel jPanelExtra;
    private javax.swing.JPanel jPanelExtraInnen;
    private javax.swing.JPanel jPanelFilter;
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

    private class BeobFilterLoeschen implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            guiFilmeFilterLoeschen();
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
            if (Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_ECHTZEITSUCHE))) {
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
        private DatenFilm film = null;

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
                    if (!filmInfo.isVisible()) {
                        filmInfo.showInfo();
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
                if (tabelle.convertColumnIndexToModel(column) == DatenFilm.FILM_ABSPIELEN_NR) {
                    Optional<DatenFilm> filmSelection = getCurrentlySelectedFilm();
                    if (filmSelection.isPresent()) {
                        boolean stop = false;
                        final DatenFilm datenFilm = filmSelection.get();
                        final DatenDownload datenDownload = Daten.listeDownloadsButton.getDownloadUrlFilm(datenFilm.arr[DatenFilm.FILM_URL_NR]);
                        if (datenDownload != null) {
                            if (datenDownload.start != null) {
                                if (datenDownload.start.status == Start.STATUS_RUN) {
                                    stop = true;
                                    Daten.listeDownloadsButton.delDownloadButton(datenFilm.arr[DatenFilm.FILM_URL_NR]);
                                }
                            }
                        }
                        if (!stop) {
                            playFilm();
                        }
                    }
                } else if (tabelle.convertColumnIndexToModel(column) == DatenFilm.FILM_AUFZEICHNEN_NR) {
                    saveFilm();
                }
            }
        }

        private void showMenu(MouseEvent evt) {
            p = evt.getPoint();
            int nr = tabelle.rowAtPoint(p);
            if (nr >= 0) {
                tabelle.setRowSelectionInterval(nr, nr);
            }
            //int selectedModelRow = tabelle.convertRowIndexToModel(nr);
            film = getFilm(nr);
            JPopupMenu jPopupMenu = new JPopupMenu();

            //Thema laden
            JMenuItem item = new JMenuItem("Film abspielen");
            item.setIcon(GetIcon.getProgramIcon("film_start_16.png"));
            item.addActionListener(e -> playFilm());
            jPopupMenu.add(item);
            //Url
            item = new JMenuItem("Film aufzeichnen");
            item.setIcon(GetIcon.getProgramIcon("film_rec_16.png"));
            item.addActionListener(e -> saveFilm());
            jPopupMenu.add(item);

            //##Trenner##
            jPopupMenu.addSeparator();
            //##Trenner##
            if (Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_VIS_FILTER))) {
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
            if (film != null) {
                if ((Daten.listeAbo.getAboFuerFilm_schnell(film, false /*die Länge nicht prüfen*/)) != null) {
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
            }
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
                if (pset.getListeProg().isEmpty() && pset.arr[DatenPset.PROGRAMMSET_NAME_NR].equals("")) {
                    // ein "leeres" Pset, Platzhalter
                    continue;
                }
                Color col = pset.getFarbe();
                item = new JMenuItem(pset.arr[DatenPset.PROGRAMMSET_NAME_NR]);
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
            jCheckBoxBlackBoxOn.setSelected(Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_BLACKLIST_ON)));
            jCheckBoxBlackBoxOn.addActionListener(e -> {
                Daten.mVConfig.add(MVConfig.SYSTEM_BLACKLIST_ON, Boolean.toString(jCheckBoxBlackBoxOn.isSelected()));
                MVListeFilme.checkBlacklist();
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_BLACKLIST_GEAENDERT, GuiFilme.class.getName());
            });
            submenueBlack.add(jCheckBoxBlackBoxOn);

            final JCheckBoxMenuItem jCheckBoxBlackBoxStart = new JCheckBoxMenuItem("Blacklist ist beim Programmstart eingeschaltet");
            jCheckBoxBlackBoxStart.setSelected(Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_BLACKLIST_START_ON)));
            jCheckBoxBlackBoxStart.addActionListener(e -> {
                Daten.mVConfig.add(MVConfig.SYSTEM_BLACKLIST_START_ON, Boolean.toString(jCheckBoxBlackBoxStart.isSelected()));
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_BLACKLIST_START_GEAENDERT, GuiFilme.class.getName());
            });
            submenueBlack.add(jCheckBoxBlackBoxStart);

            //Url
            if (film != null) {
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

                            KeyStroke ctrlH = KeyStroke.getKeyStroke(KeyEvent.VK_H, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask());
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
            if (film != null) {
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
                if (!filmInfo.isVisible()) {
                    filmInfo.showInfo();
                }
            });
            jPopupMenu.add(item);
            //History
            if (film != null) {
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

            boolean eintragen;

            public BeobHistory(boolean eeintragen) {
                eintragen = eeintragen;
            }

            @Override
            public void actionPerformed(ActionEvent e) {
                int nr = tabelle.rowAtPoint(p);
                if (nr >= 0) {
                    DatenFilm film = getFilm(nr);
                    if (eintragen) {
                        daten.history.zeileSchreiben(film.arr[DatenFilm.FILM_THEMA_NR], film.arr[DatenFilm.FILM_TITEL_NR], film.getUrlHistory());
                        Daten.listeFilmeHistory.add(film);
                    } else {
                        daten.history.urlAusLogfileLoeschen(film.getUrlHistory());
                        Daten.listeFilmeHistory.remove(film);
                    }
                }
            }
        }

        private class BeobPrint implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                try {
                    tabelle.print();
                } catch (PrinterException ex) {
                    Log.fehlerMeldung(688542239, ex);
                }
            }
        }

        private class BeobFilterThema implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                int nr = tabelle.rowAtPoint(p);
                if (nr >= 0) {
                    stopBeob = true;
                    DatenFilm film = getFilm(nr);
                    String thema = film.arr[DatenFilm.FILM_THEMA_NR];
                    mVFilter.get_jComboBoxFilterThema().setSelectedIndex(0);
                    mVFilter.get_jComboBoxFilterThema().setSelectedItem(thema);
                    stopBeob = false;
                    loadTable();
                }
            }
        }

        private class BeobFilterSender implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                int nr = tabelle.rowAtPoint(p);
                if (nr >= 0) {
                    stopBeob = true;
                    DatenFilm film = getFilm(nr);
                    String sen = film.arr[DatenFilm.FILM_SENDER_NR];
                    mVFilter.get_jComboBoxFilterSender().setSelectedIndex(0);
                    mVFilter.get_jComboBoxFilterSender().setSelectedItem(sen);
                    stopBeob = false;
                    loadTable();
                }
            }
        }

        private class BeobFilterSenderThema implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                int nr = tabelle.rowAtPoint(p);
                if (nr >= 0) {
                    stopBeob = true;
                    DatenFilm film = getFilm(nr);
                    String sen = film.arr[DatenFilm.FILM_SENDER_NR];
                    mVFilter.get_jComboBoxFilterSender().setSelectedIndex(0);
                    mVFilter.get_jComboBoxFilterSender().setSelectedItem(sen);
                    String thema = film.arr[DatenFilm.FILM_THEMA_NR];
                    mVFilter.get_jComboBoxFilterThema().setSelectedIndex(0);
                    mVFilter.get_jComboBoxFilterThema().setSelectedItem(thema);
                    if (mVFilter.get_jComboBoxFilterThema().getSelectedIndex() == 0) {
                        String themaFilter = getThemaFilter(sen, thema);
                        mVFilter.get_jComboBoxFilterThema().setSelectedItem(themaFilter);
                    }
                    stopBeob = false;
                    loadTable();
                }
            }
        }

        private class BeobFilterSenderThemaTitel implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                int nr = tabelle.rowAtPoint(p);
                if (nr >= 0) {
                    stopBeob = true;
                    DatenFilm film = getFilm(nr);
                    String sen = film.arr[DatenFilm.FILM_SENDER_NR];
                    mVFilter.get_jComboBoxFilterSender().setSelectedIndex(0);
                    mVFilter.get_jComboBoxFilterSender().setSelectedItem(sen);
                    String thema = film.arr[DatenFilm.FILM_THEMA_NR];
                    mVFilter.get_jComboBoxFilterThema().setSelectedIndex(0);
                    mVFilter.get_jComboBoxFilterThema().setSelectedItem(thema);
                    if (mVFilter.get_jComboBoxFilterThema().getSelectedIndex() == 0) {
                        String themaFilter = getThemaFilter(sen, thema);
                        mVFilter.get_jComboBoxFilterThema().setSelectedItem(themaFilter);
                    }
                    String tit = film.arr[DatenFilm.FILM_TITEL_NR];
                    mVFilter.get_jTextFieldFilterTitel().setText(tit);
                    stopBeob = false;
                    loadTable();
                }
            }
        }

        private String getThemaFilter(String sender, String thema) {
            // Thema für den Filter suchen bei zB: "Hallo" und "hallo" steht nur eines im FilterThema
            String ret = "";
            for (int i = 1; i < Daten.listeFilmeNachBlackList.themenPerSender.length; ++i) {
                if (Daten.listeFilmeNachBlackList.sender[i].equals(sender)) {
                    for (int k = 1; k < Daten.listeFilmeNachBlackList.themenPerSender[i].length; ++k) {
                        if (Daten.listeFilmeNachBlackList.themenPerSender[i][k].equalsIgnoreCase(thema)) {
                            ret = Daten.listeFilmeNachBlackList.themenPerSender[i][k];
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
                    MVMessageDialog.showMessageDialog(parentComponent, "Im Menü unter \"Datei->Optionen->Videoplayer\" ein Programm zum Aufzeichnen festlegen.",
                            "kein Videoplayer!", JOptionPane.INFORMATION_MESSAGE);
                } else {
                    int nr = tabelle.rowAtPoint(p);
                    if (nr >= 0) {
                        stopBeob = true;
                        DatenFilm film = getFilm(nr);
                        DatenAbo datenAbo;
                        if (film != null) {
                            if ((datenAbo = Daten.listeAbo.getAboFuerFilm_schnell(film, false /*ohne Länge*/)) != null) {
                                //gibts schon, dann löschen
                                DialogEditAbo dialog = new DialogEditAbo(daten.mediathekGui, true, daten, datenAbo);
                                dialog.setVisible(true);
                                if (dialog.ok) {
                                    Daten.listeAbo.aenderungMelden();
                                }
                            }
                        }
                        stopBeob = false;
                    }
                }
            }
        }

        private class BeobAbo implements ActionListener {

            boolean mitTitel = false;

            public BeobAbo(boolean mmitTitel) {
                mitTitel = mmitTitel;
            }

            @Override
            public void actionPerformed(ActionEvent e) {
                if (Daten.listePset.getListeAbo().isEmpty()) {
                    MVMessageDialog.showMessageDialog(parentComponent, "Im Menü unter \"Datei->Optionen->Videoplayer\" ein Programm zum Aufzeichnen festlegen.",
                            "kein Videoplayer!", JOptionPane.INFORMATION_MESSAGE);
                } else {
                    int nr = tabelle.rowAtPoint(p);
                    if (nr >= 0) {
                        stopBeob = true;
                        DatenFilm film = getFilm(nr);
                        DatenAbo datenAbo;
                        if (film != null) {
                            if ((datenAbo = Daten.listeAbo.getAboFuerFilm_schnell(film, false /*ohne Länge*/)) != null) {
                                //gibts schon, dann löschen
                                Daten.listeAbo.aboLoeschen(datenAbo);
                            } else {
                                //neues Abo anlegen
                                if (mitTitel) {
                                    Daten.listeAbo.addAbo(film.arr[DatenFilm.FILM_THEMA_NR]/*aboname*/,
                                            film.arr[DatenFilm.FILM_SENDER_NR], film.arr[DatenFilm.FILM_THEMA_NR], film.arr[DatenFilm.FILM_TITEL_NR]);
                                } else {
                                    Daten.listeAbo.addAbo(film.arr[DatenFilm.FILM_THEMA_NR]/*aboname*/,
                                            film.arr[DatenFilm.FILM_SENDER_NR], film.arr[DatenFilm.FILM_THEMA_NR], "");
                                }
                            }
                        }
                        stopBeob = false;
                    }
                }
            }
        }

        private class BeobAboFilter implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                if (Daten.listePset.getListeAbo().isEmpty()) {
                    MVMessageDialog.showMessageDialog(parentComponent, "Im Menü unter \"Datei->Optionen->Videoplayer\" ein Programm zum Aufzeichnen festlegen.",
                            "kein Videoplayer!", JOptionPane.INFORMATION_MESSAGE);
                } else {
                    int nr = tabelle.rowAtPoint(p);
                    if (nr >= 0) {
                        stopBeob = true;
                        DatenFilm film = getFilm(nr);
                        String thema = film.arr[DatenFilm.FILM_THEMA_NR];
                        //neues Abo anlegen
                        // addAbo(String filmSender, String filmThema, String filmTitel, String filmThemaTitel, String irgendwo, int mindestdauer, String namePfad)
                        Daten.listeAbo.addAbo(mVFilter.get_jComboBoxFilterSender().getSelectedItem().toString(),
                                mVFilter.get_jComboBoxFilterThema().getSelectedItem().toString(),
                                mVFilter.get_jTextFieldFilterTitel().getText(),
                                mVFilter.getThemaTitel() ? mVFilter.get_jTextFieldFilterThemaTitel().getText() : "",
                                mVFilter.getThemaTitel() ? "" : mVFilter.get_jTextFieldFilterThemaTitel().getText(),
                                mVFilter.get_jSliderMinuten().getValue(), thema);
                        stopBeob = false;
                    }
                }
            }
        }

        private class BeobBlacklist implements ActionListener {

            boolean sender;
            boolean thema;

            public BeobBlacklist(boolean ssender, boolean tthema) {
                sender = ssender;
                thema = tthema;
            }

            @Override
            public void actionPerformed(ActionEvent e) {
                int nr = tabelle.rowAtPoint(p);
                if (nr >= 0) {
                    DatenFilm film = getFilm(nr);
                    String th = film.arr[DatenFilm.FILM_THEMA_NR];
                    String se = film.arr[DatenFilm.FILM_SENDER_NR];
                    // Blackliste für alle Fälle einschalten, notify kommt beim add()
                    Daten.mVConfig.add(MVConfig.SYSTEM_BLACKLIST_ON, Boolean.TRUE.toString());
                    if (!sender) {
                        Daten.listeBlacklist.add(new DatenBlacklist("", th, "" /*Titel*/, "" /*Thema-Titel*/));
                    } else if (!thema) {
                        Daten.listeBlacklist.add(new DatenBlacklist(se, "", "" /*Titel*/, "" /*Thema-Titel*/));
                    } else {
                        Daten.listeBlacklist.add(new DatenBlacklist(se, th, "" /*Titel*/, "" /*Thema-Titel*/));
                    }
                }
            }
        }
    }

}
