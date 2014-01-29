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

import com.jidesoft.popup.JidePopup;
import com.jidesoft.swing.JideMenu;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.print.PrinterException;
import java.util.LinkedList;
import javax.swing.AbstractAction;
import javax.swing.ActionMap;
import javax.swing.DefaultComboBoxModel;
import javax.swing.InputMap;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.KeyStroke;
import javax.swing.RowSorter.SortKey;
import javax.swing.SortOrder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import mediathek.MVStatusBar_Mac;
import mediathek.MVToolBar;
import mediathek.controller.starter.Start;
import mediathek.daten.Daten;
import mediathek.daten.DatenAbo;
import mediathek.daten.DatenBlacklist;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenPset;
import mediathek.daten.ListePset;
import mediathek.gui.dialog.MVFilmInformation;
import mediathek.res.GetIcon;
import mediathek.tool.BeobTableHeader;
import mediathek.tool.CellRendererFilme;
import mediathek.tool.Datum;
import mediathek.tool.Filter;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.Konstanten;
import mediathek.controller.Log;
import mediathek.gui.dialog.DialogAddDownload;
import mediathek.tool.HinweisKeineAuswahl;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.MVListeFilme;
import mediathek.tool.MVTable;
import mediathek.tool.MVMessageDialog;
import mediathek.tool.TModel;
import mediathek.tool.TModelFilm;
import msearch.daten.DatenFilm;
import msearch.filmeSuchen.MSearchListenerFilmeLaden;
import msearch.filmeSuchen.MSearchListenerFilmeLadenEvent;

public class GuiFilme extends PanelVorlage {

    private JButton buttonArray[];
    private final String[] COMBO_ZEIT = new String[]{"alles", "1 Tag", "2 Tage", "3 Tage", "4 Tage", "5 Tage", "10 Tage", "15 Tage", "20 Tage", "30 Tage"};
    public static final int[] COMBO_ZEIT_INT = {0, 1, 2, 3, 4, 5, 10, 15, 20, 30};
    private MVFilmInformation filmInfoHud;
    private PanelBeschreibung panelBeschreibung;
    private MVFrameFilter frameFilter;

    public GuiFilme(Daten d, JFrame parentComponent) {
        super(d, parentComponent);
        initComponents();
        tabelle = new MVTable(MVTable.TABELLE_TAB_FILME);
        jScrollPane1.setViewportView(tabelle);
        panelVideoplayerSetzen();
        panelBeschreibung = new PanelBeschreibung(daten.mediathekGui, daten);
        frameFilter = new MVFrameFilter(d);
        jPanelBeschreibung.setLayout(new BorderLayout());
        jPanelBeschreibung.add(panelBeschreibung, BorderLayout.CENTER);
        filmInfoHud = daten.filmInfoHud;
        init(); //alles einrichten, Beobachter anhängen
        tabelleLaden(); //Filme laden
        tabelle.initTabelle();
        if (tabelle.getRowCount() > 0) {
            tabelle.setRowSelectionInterval(0, 0);
        }
        addListenerMediathekView();
    }

    //===================================
    // Public
    //===================================
    @Override
    public void isShown() {
        super.isShown();
        daten.mediathekGui.setToolbar(MVToolBar.TOOLBAR_TAB_FILME);
        daten.mediathekGui.getStatusBar().setIndexForCenterDisplay(MVStatusBar_Mac.StatusbarIndex.FILME);
        aktFilmSetzen();
        setInfo();
    }

    public void filmAbspielen() {
        filmAbspielen_();
    }

    public void filmSpeichern() {
        filmSpeichern_();
    }

    public void filtern() {
        tabelleLaden();
    }
    //===================================
    // Private
    //===================================

    private void init() {
        frameFilter.jToggleButtonNeue.setEnabled(false);
        panelBeschreibungSetzen();
        frameFilter.setVisible(Boolean.parseBoolean(Daten.mVConfig.get(Konstanten.SYSTEM_PANEL_FILTER_ANZEIGEN)));
        frameFilter.jComboBoxZeitraum.setModel(new DefaultComboBoxModel<>(COMBO_ZEIT));
        try {
            frameFilter.jCheckBoxKeineAbos.setSelected(Boolean.parseBoolean(Daten.mVConfig.get(Konstanten.SYSTEM_FILTER_KEINE_ABO)));
            frameFilter.jCheckBoxKeineAbos.setSelected(Boolean.parseBoolean(Daten.mVConfig.get(Konstanten.SYSTEM_FILTER_KEINE_ABO)));
            frameFilter.jCheckBoxKeineGesehenen.setSelected(Boolean.parseBoolean(Daten.mVConfig.get(Konstanten.SYSTEM_FILTER_KEINE_GESEHENE)));
            frameFilter.jCheckBoxNurHd.setSelected(Boolean.parseBoolean(Daten.mVConfig.get(Konstanten.SYSTEM_FILTER_NUR_HD)));
            frameFilter.jComboBoxZeitraum.setSelectedIndex(Integer.parseInt(Daten.mVConfig.get(Konstanten.SYSTEM_FILTER_TAGE)));
        } catch (Exception ex) {
            frameFilter.jComboBoxZeitraum.setSelectedIndex(6);
            Daten.mVConfig.add(Konstanten.SYSTEM_FILTER_TAGE, "6");
        }
        frameFilter.jComboBoxZeitraum.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (!stopBeob) {
                    Daten.mVConfig.add(Konstanten.SYSTEM_FILTER_TAGE, String.valueOf(frameFilter.jComboBoxZeitraum.getSelectedIndex()));
                    MVListeFilme.checkBlacklist();
                    tabelleLaden();
                }
            }
        });
        Daten.filmeLaden.addAdListener(new MSearchListenerFilmeLaden() {
            @Override
            public void start(MSearchListenerFilmeLadenEvent event) {
                frameFilter.jToggleButtonNeue.setEnabled(false);
                tabelleLaden();
            }

            @Override
            public void fertig(MSearchListenerFilmeLadenEvent event) {
                frameFilter.jToggleButtonNeue.setEnabled(Daten.listeFilmeNachBlackList.neueFilme);
                tabelleLaden();
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
                filmAbspielen_();
            }
        });
        this.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_D, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), "download");
        this.getActionMap().put("download", new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                filmSpeichern_();
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
                if (!filmInfoHud.isVisible()) {
                    filmInfoHud.show();
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
                filmAbspielen_();
            }
        });

        tabelle.setModel(new TModelFilm(new Object[][]{}, DatenFilm.COLUMN_NAMES));
        BeobMausTabelle beobMausTabelle = new BeobMausTabelle();
        tabelle.addMouseListener(beobMausTabelle);
        tabelle.getSelectionModel().addListSelectionListener(new BeobachterTableSelect());
        tabelle.setDefaultRenderer(Object.class, new CellRendererFilme(daten));
        tabelle.setDefaultRenderer(Datum.class, new CellRendererFilme(daten));
        tabelle.setDefaultRenderer(Integer.class, new CellRendererFilme(daten));
        tabelle.getTableHeader().addMouseListener(new BeobTableHeader(tabelle, DatenFilm.COLUMN_NAMES, DatenFilm.spaltenAnzeigen,
                new int[]{DatenFilm.FILM_ABSPIELEN_NR, DatenFilm.FILM_AUFZEICHNEN_NR, DatenFilm.FILM_DATUM_LONG_NR, /* DatenFilm.FILM_URL_HISTORY_NR, */ DatenFilm.FILM_REF_NR}) {
                    @Override
                    public void tabelleLaden_() {
                        tabelleLaden();
                    }
                });

        //beobachter Filter
        frameFilter.jToggleButtonLivestram.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (!stopBeob) {
                    stopBeob = true;
                    //auch die Filter löschen
                    frameFilter.jComboBoxFilterSender.setModel(new javax.swing.DefaultComboBoxModel<>(Daten.listeFilmeNachBlackList.sender));
                    frameFilter.jComboBoxFilterThema.setModel(new javax.swing.DefaultComboBoxModel<>(getThemen("")));
                    frameFilter.jTextFieldFilterTitel.setText("");
                }
                tabelleLaden();
            }
        });
        //Combo Sender
        frameFilter.jButtonFilterLoeschen.addActionListener(new BeobFilterLoeschen());
        frameFilter.jComboBoxFilterSender.setModel(new javax.swing.DefaultComboBoxModel<>(Daten.listeFilmeNachBlackList.sender));
        frameFilter.jComboBoxFilterSender.addActionListener(new BeobFilterSender());
        frameFilter.jComboBoxFilterThema.setModel(new javax.swing.DefaultComboBoxModel<>(getThemen("")));
        frameFilter.jComboBoxFilterThema.addActionListener(new BeobFilter());
        frameFilter.jTextFieldFilterTitel.addActionListener(new BeobFilter());
        frameFilter.jTextFieldFilterTitel.getDocument().addDocumentListener(new BeobFilterTitelDoc());
        frameFilter.jTextFieldFilterThemaTitel.addActionListener(new BeobFilter());
        frameFilter.jTextFieldFilterThemaTitel.getDocument().addDocumentListener(new BeobFilterTitelDoc());
        frameFilter.jTextFieldFilterIrgendwo.addActionListener(new BeobFilter());
        frameFilter.jTextFieldFilterIrgendwo.getDocument().addDocumentListener(new BeobFilterTitelDoc());
        try {
            frameFilter.jSliderMinuten.setValue(Integer.parseInt(Daten.mVConfig.get(Konstanten.SYSTEM_FILTER_DAUER)));
        } catch (Exception ex) {
            frameFilter.jSliderMinuten.setValue(0);
            Daten.mVConfig.add(Konstanten.SYSTEM_FILTER_DAUER, "0");
        }
        frameFilter.jTextFieldFilterMinuten.setText(String.valueOf(frameFilter.jSliderMinuten.getValue()));
        frameFilter.jSliderMinuten.addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                frameFilter.jTextFieldFilterMinuten.setText(String.valueOf(frameFilter.jSliderMinuten.getValue()));
                if (!frameFilter.jSliderMinuten.getValueIsAdjusting()) {
                    Daten.mVConfig.add(Konstanten.SYSTEM_FILTER_DAUER, String.valueOf(frameFilter.jSliderMinuten.getValue()));
                    tabelleLaden();
                }
            }
        });
        frameFilter.jCheckBoxKeineAbos.addActionListener(new BeobFilter());
        frameFilter.jCheckBoxKeineAbos.addActionListener(new BeobFilter());
        frameFilter.jCheckBoxKeineGesehenen.addActionListener(new BeobFilter());
        frameFilter.jCheckBoxNurHd.addActionListener(new BeobFilter());
        frameFilter.jToggleButtonNeue.addActionListener(new BeobFilter());
        jCheckBoxProgamme.setIcon(GetIcon.getIcon("close_15.png"));
        jCheckBoxProgamme.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.mVConfig.add(Konstanten.SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN, Boolean.FALSE.toString());
                daten.mediathekGui.videoplayerAnzeigen(true);
                panelVideoplayerSetzen();
            }
        });
    }

    private void addListenerMediathekView() {
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_LISTE_PSET, GuiFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                panelVideoplayerSetzen();
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(new int[]{ListenerMediathekView.EREIGNIS_LISTE_HISTORY_GEAENDERT,
            ListenerMediathekView.EREIGNIS_LISTE_ABOS}, GuiFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                if (Boolean.parseBoolean(Daten.mVConfig.get(Konstanten.SYSTEM_PANEL_FILTER_ANZEIGEN))) {
                    if (frameFilter.jCheckBoxKeineGesehenen.isSelected()) {
                        tabelleLaden();
                    } else {
                        tabelle.fireTableDataChanged(true);
                    }
                }
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_BLACKLIST_GEAENDERT, GuiFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                tabelleLaden();
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_FILMLISTE_GEAENDERT, GuiFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                MVListeFilme.checkBlacklist();
                tabelleLaden();
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
                tabelle.fireTableDataChanged(true /*setSpalten*/);
                setInfo();
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
                panelFilterSetzen();
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_PANEL_BESCHREIBUNG_ANZEIGEN, GuiFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                panelBeschreibungSetzen();
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_SUCHFELD_FOCUS_SETZEN, GuiFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                if (Boolean.parseBoolean(Daten.mVConfig.get(Konstanten.SYSTEM_PANEL_FILTER_ANZEIGEN))) {
                    frameFilter.jTextFieldFilterThemaTitel.requestFocus();
                    frameFilter.jTextFieldFilterThemaTitel.setCaretPosition(0);
                }
            }
        });
    }

    private void panelBeschreibungSetzen() {
        jPanelBeschreibung.setVisible(Boolean.parseBoolean(Daten.mVConfig.get(Konstanten.SYSTEM_PANEL_BESCHREIBUNG_ANZEIGEN)));
    }

    private void panelFilterSetzen() {
        // Panel anzeigen und die Filmliste anpassen
        frameFilter.setVisible(Boolean.parseBoolean(Daten.mVConfig.get(Konstanten.SYSTEM_PANEL_FILTER_ANZEIGEN)));
        MVListeFilme.checkBlacklist();
        tabelleLaden();
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

    private synchronized void filmAbspielen_() {
        DatenPset pset = daten.listePset.getPsetAbspielen();
        if (pset != null) {
            playerStarten(pset);
        } else {
            MVMessageDialog.showMessageDialog(parentComponent, "Im Menü unter \"Datei->Einstellungen->Aufzeichnen und Abspielen\" ein Programm zum Abspielen festlegen.",
                    "kein Videoplayer!", JOptionPane.INFORMATION_MESSAGE);
        }
    }

    private synchronized void filmSpeichern_() {
        filmSpeichern_(null);
    }

    private synchronized void filmSpeichern_(DatenPset pSet) {
        if (daten.listePset.getListeSpeichern().isEmpty()) {
            MVMessageDialog.showMessageDialog(parentComponent, "Im Menü unter \"Datei->Einstellungen->Aufzeichnen und Abspielen\" ein Programm zum Aufzeichnen festlegen.",
                    "fehlende Einstellungen zum Speichern!", JOptionPane.INFORMATION_MESSAGE);
            // Satz mit x, war wohl nix
        } else {
            DatenFilm film;
            int[] selRows = tabelle.getSelectedRows();
            if (selRows.length == 0) {
                new HinweisKeineAuswahl().zeigen(parentComponent);
            } else {
                for (int selRow : selRows) {
                    // film = Daten.listeFilme.getFilmByUrl(tabelle.getModel().getValueAt(selRow, DatenFilm.FILM_URL_NR).toString());
                    film = (DatenFilm) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(selRow), DatenFilm.FILM_REF_NR);
                    // erst mal schauen obs den schon gibt
                    DatenDownload datenDownload = Daten.listeDownloads.getDownloadUrlFilm(film.arr[DatenFilm.FILM_URL_NR]);
                    if (datenDownload != null) {
                        int ret = JOptionPane.showConfirmDialog(parentComponent, "Download für den Film existiert bereits.\n"
                                + "Nochmal anlegen?", "Anlegen?", JOptionPane.YES_NO_OPTION);
                        if (ret != JOptionPane.OK_OPTION) {
                            continue;
                        }
                    }
                    // weiter
                    String aufloesung = "";
                    if (frameFilter.jCheckBoxNurHd.isSelected()) {
                        aufloesung = DatenFilm.AUFLOESUNG_HD;
                    }
                    DialogAddDownload dialog = new DialogAddDownload(daten.mediathekGui, daten, film, pSet, aufloesung);
                    dialog.setVisible(true);
                }
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
            filmSpeichern_(pSet);
        } else {
            // mit dem flvstreamer immer nur einen Filme starten
            //DatenFilm datenFilm = Daten.listeFilmeNachBlackList.getFilmByUrl(tabelle.getModel().getValueAt(selectedModelRow, DatenFilm.FILM_URL_NR).toString());
            String aufloesung = "";
            if (frameFilter.jCheckBoxNurHd.isSelected()) {
                aufloesung = DatenFilm.AUFLOESUNG_HD;
            }
            daten.starterClass.urlMitProgrammStarten(pSet, getSelFilm(), aufloesung);
        }
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
        ListePset listeButton = daten.listePset.getListeButton();
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
        jPanelExtra.setVisible(Boolean.parseBoolean(Daten.mVConfig.get(Konstanten.SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN)));
    }

    private Component addExtraFeld(int i, int spalte, int zeile, GridBagLayout gridbag, GridBagConstraints c, JPanel panel, ListePset liste) {
        Component ret;
        JButton button;
        c.gridx = spalte;
        c.gridy = zeile;
        if (liste.get(i).isLable()) {
            JLabel label = new JLabel(liste.get(i).arr[DatenPset.PROGRAMMSET_NAME_NR]);
            Color col = liste.get(i).getFarbe(daten);
            if (col != null) {
                label.setForeground(col);
            }
            gridbag.setConstraints(label, c);
            panel.add(label);
            ret = label;
        } else {
            button = new JButton(liste.get(i).arr[DatenPset.PROGRAMMSET_NAME_NR]);
            button.addActionListener(new BeobOpen(liste.get(i)));
            Color col = liste.get(i).getFarbe(daten);
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

    private synchronized void tabelleLaden() {
        try {
            if (!Boolean.parseBoolean(Daten.mVConfig.get(Konstanten.SYSTEM_PANEL_FILTER_ANZEIGEN))) {
                // Filtern mit dem Filter in der Toolbar
                stopBeob = true;
                tabelle.getSpalten();
                listeInModellLaden();
                setInfo();
                tabelle.setSpalten();
                aktFilmSetzen();
                stopBeob = false;
            } else {
                boolean themaNichtDa = false;
                stopBeob = true;
                tabelle.getSpalten();
                String filterThema = frameFilter.jComboBoxFilterThema.getSelectedItem().toString();
                String filterSender = frameFilter.jComboBoxFilterSender.getSelectedItem().toString();
                boolean themaOpen = frameFilter.jComboBoxFilterThema.isPopupVisible();
                boolean senderOpen = frameFilter.jComboBoxFilterSender.isPopupVisible();
                if (Daten.listeFilmeNachBlackList.isEmpty()) {
                    //jComboBoxFilterSender.setModel(new javax.swing.DefaultComboBoxModel(Daten.listeFilmeNachBlackList.getModelOfFieldSender()));
                    //jComboBoxFilterThema.setModel(new javax.swing.DefaultComboBoxModel(Daten.listeFilmeNachBlackList.getModelOfFieldThema("")));
                    frameFilter.jComboBoxFilterSender.setModel(new javax.swing.DefaultComboBoxModel<>(Daten.listeFilmeNachBlackList.sender));
                    frameFilter.jComboBoxFilterThema.setModel(new javax.swing.DefaultComboBoxModel<>(getThemen("")));
                    frameFilter.jComboBoxFilterSender.setSelectedIndex(0);
                    frameFilter.jComboBoxFilterThema.setSelectedIndex(0);
                    listeInModellLaden(); // zum löschen der Tabelle
                } else {
                    //Filme neu laden
                    listeInModellLaden();
                    //Filter Sender
                    frameFilter.jComboBoxFilterSender.setModel(new javax.swing.DefaultComboBoxModel<>(Daten.listeFilmeNachBlackList.sender));
                    frameFilter.jComboBoxFilterSender.setSelectedIndex(0);
                    if (!filterSender.equals("")) {
                        // ist wohl ein Bug beim Combo, klappt nur richtig wenn editable?!
                        frameFilter.jComboBoxFilterSender.setEditable(true);
                        frameFilter.jComboBoxFilterSender.setSelectedItem(filterSender);
                        frameFilter.jComboBoxFilterSender.setEditable(false);
                        if (frameFilter.jComboBoxFilterSender.getSelectedIndex() == 0) {
                            // war wohl nix, der gewählte Sender wurde in die Blacklist eingetragen
                            filterSender = "";
                            listeInModellLaden();
                        }
                    }
                    frameFilter.jComboBoxFilterSender.setPopupVisible(senderOpen);
                    // Filter Thema
                    if (filterSender.equals("")) {
                        frameFilter.jComboBoxFilterThema.setModel(new javax.swing.DefaultComboBoxModel<>(getThemen("")));
                    } else {
                        frameFilter.jComboBoxFilterThema.setModel(new javax.swing.DefaultComboBoxModel<>(getThemen(filterSender)));
                    }
                    // wenn Thema bei dem Sender vorhanden, dann wieder setzen
                    // ist wohl ein Bug beim Combo, klappt nur richtig wenn editable?!
                    frameFilter.jComboBoxFilterThema.setEditable(true);
                    frameFilter.jComboBoxFilterThema.setSelectedItem(filterThema);
                    frameFilter.jComboBoxFilterThema.setEditable(false);
                    if (!filterThema.equals("") && frameFilter.jComboBoxFilterThema.getSelectedIndex() == 0) {
                        // war wohl nix
                        themaNichtDa = true;
                    }
                    frameFilter.jComboBoxFilterThema.setPopupVisible(themaOpen);
                }
                setInfo();
                tabelle.setSpalten();
                aktFilmSetzen();
                stopBeob = false;
                //filtern
                if (themaNichtDa) {
                    // nochmal filtern anschieben
                    tabelleLaden();
                }
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(558965421, Log.FEHLER_ART_PROG, "GuiFilme.tabelleBauen", ex);
        }
    }

    private synchronized void listeInModellLaden() {
        if (Boolean.parseBoolean(Daten.mVConfig.get(Konstanten.SYSTEM_PANEL_FILTER_ANZEIGEN))) {
            // normal mit den Filtern aus dem Filterpanel suchen
            MVListeFilme.getModelTabFilme(Daten.listeFilmeNachBlackList, daten, tabelle,
                    frameFilter.jComboBoxFilterSender.getSelectedItem().toString(),
                    frameFilter.jComboBoxFilterThema.getSelectedItem().toString(), frameFilter.jTextFieldFilterTitel.getText(),
                    frameFilter.jTextFieldFilterThemaTitel.getText(),
                    frameFilter.jTextFieldFilterIrgendwo.getText(),
                    frameFilter.jSliderMinuten.getValue(),
                    frameFilter.jCheckBoxKeineAbos.isSelected(), frameFilter.jCheckBoxKeineGesehenen.isSelected(),
                    frameFilter.jCheckBoxNurHd.isSelected(), frameFilter.jToggleButtonLivestram.isSelected(), frameFilter.jToggleButtonNeue.isSelected());
        } else {
            // jetzt nur den Filter aus der Toolbar
            MVListeFilme.getModelTabFilme(Daten.listeFilmeNachBlackList, daten, tabelle,
                    "", "", "",
                    daten.mediathekGui.getFilterTextFromSearchField(),
                    "",
                    frameFilter.jSliderMinuten.getValue(),
                    frameFilter.jCheckBoxKeineAbos.isSelected(), frameFilter.jCheckBoxKeineGesehenen.isSelected(),
                    frameFilter.jCheckBoxNurHd.isSelected(), frameFilter.jToggleButtonLivestram.isSelected(), frameFilter.jToggleButtonNeue.isSelected());
        }
    }
    // ####################################
    // Ende Tabelle asynchron füllen
    // ####################################

    private void filterLoeschen() {
        stopBeob = true;
        //ComboModels neu aufbauen
        frameFilter.jComboBoxFilterSender.setModel(new javax.swing.DefaultComboBoxModel<>(Daten.listeFilmeNachBlackList.sender));
        frameFilter.jComboBoxFilterThema.setModel(new javax.swing.DefaultComboBoxModel<>(getThemen("")));
        frameFilter.jTextFieldFilterTitel.setText("");
        frameFilter.jTextFieldFilterThemaTitel.setText("");
        frameFilter.jTextFieldFilterIrgendwo.setText("");
        //jSliderMinuten.setValue(0);
        //neu laden
        tabelleLaden();
    }

    private void aktFilmSetzen() {
        if (this.isShowing()) {
            DatenFilm aktFilm = new DatenFilm();
            DatenFilm film = getSelFilm();
            if (film != null) {
                aktFilm = film;
            }
            filmInfoHud.updateCurrentFilm(aktFilm);
            // Beschreibung setzen
            panelBeschreibung.setAktFilm(aktFilm);
        }
    }

    private DatenFilm getSelFilm() {
        int selectedTableRow = tabelle.getSelectedRow();
        if (selectedTableRow >= 0) {
            return (DatenFilm) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(selectedTableRow), DatenFilm.FILM_REF_NR);
        }
        return null;
    }

    private DatenFilm getFilm(int zeileTabelle) {
        if (zeileTabelle >= 0 && zeileTabelle < tabelle.getRowCount()) {
            return (DatenFilm) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(zeileTabelle), DatenFilm.FILM_REF_NR);
        }
        return null;
    }

    private void setInfo() {
        String textLinks;
        final String TRENNER = "  ||  ";
        int gesamt = Daten.listeFilme.size();
        int anzListe = tabelle.getModel().getRowCount();
        int runs = Daten.listeDownloadsButton.getListteStartsNotFinished(Start.QUELLE_BUTTON).size();
        // Anzahl der Filme
        if (gesamt == anzListe) {
            if (anzListe == 1) {
                textLinks = "1 Film";
            } else {
                textLinks = anzListe + " Filme";
            }
        } else {
            if (anzListe == 1) {
                textLinks = "1 Film";
            } else {
                textLinks = anzListe + " Filme";
            }
            textLinks += " (Insgesamt: " + gesamt + " )";
        }
        // laufende Programme
        if (runs == 1) {
            textLinks += TRENNER;
            textLinks += (runs + " laufender Film");
        } else if (runs > 1) {
            textLinks += TRENNER;
            textLinks += (runs + " laufende Filme");
        }
        // auch die Downloads anzeigen
        textLinks += TRENNER;
        textLinks += Daten.listeDownloads.getInfo(false /*mitAbo*/);
        // Infopanel setzen
        daten.mediathekGui.getStatusBar().setTextLeft(MVStatusBar_Mac.StatusbarIndex.FILME, textLinks);
    }

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jPanel4 = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        javax.swing.JTable jTable1 = new javax.swing.JTable();
        jPanelExtra = new javax.swing.JPanel();
        jCheckBoxProgamme = new javax.swing.JCheckBox();
        jPanelExtraInnen = new javax.swing.JPanel();
        jPanelBeschreibung = new javax.swing.JPanel();
        jPanel5 = new javax.swing.JPanel();

        javax.swing.GroupLayout jPanel4Layout = new javax.swing.GroupLayout(jPanel4);
        jPanel4.setLayout(jPanel4Layout);
        jPanel4Layout.setHorizontalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 100, Short.MAX_VALUE)
        );
        jPanel4Layout.setVerticalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 100, Short.MAX_VALUE)
        );

        jTable1.setAutoCreateRowSorter(true);
        jTable1.setModel(new TModel());
        jTable1.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_OFF);
        jScrollPane1.setViewportView(jTable1);

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

        jPanel5.setMaximumSize(new java.awt.Dimension(32767, 5));
        jPanel5.setMinimumSize(new java.awt.Dimension(100, 5));

        javax.swing.GroupLayout jPanel5Layout = new javax.swing.GroupLayout(jPanel5);
        jPanel5.setLayout(jPanel5Layout);
        jPanel5Layout.setHorizontalGroup(
            jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 0, Short.MAX_VALUE)
        );
        jPanel5Layout.setVerticalGroup(
            jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 5, Short.MAX_VALUE)
        );

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jPanel5, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanelExtra, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanelBeschreibung, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 839, Short.MAX_VALUE))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 316, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanelBeschreibung, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanelExtra, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel5, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        );
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JCheckBox jCheckBoxProgamme;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JPanel jPanelBeschreibung;
    private javax.swing.JPanel jPanelExtra;
    private javax.swing.JPanel jPanelExtraInnen;
    private javax.swing.JScrollPane jScrollPane1;
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

    private class BeobFilterSender implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            if (!stopBeob) {
                tabelleLaden();
            }
        }
    }

    private class BeobFilter implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            if (!stopBeob) {
                Daten.mVConfig.add(Konstanten.SYSTEM_FILTER_KEINE_ABO, String.valueOf(frameFilter.jCheckBoxKeineAbos.isSelected()));
                Daten.mVConfig.add(Konstanten.SYSTEM_FILTER_KEINE_GESEHENE,String.valueOf(frameFilter.jCheckBoxKeineGesehenen.isSelected()));
                Daten.mVConfig.add(Konstanten.SYSTEM_FILTER_NUR_HD, String.valueOf(frameFilter.jCheckBoxNurHd.isSelected()));
                tabelleLaden();
            }
        }
    }

    private class BeobFilterLoeschen implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            filterLoeschen();
        }
    }

    private class BeobachterTableSelect implements ListSelectionListener {

        @Override
        public void valueChanged(ListSelectionEvent event) {
            if (!event.getValueIsAdjusting() && !stopBeob) {
                aktFilmSetzen();
            }
        }
    }

    public class BeobMausTabelle extends MouseAdapter {
        //rechhte Maustaste in der Tabelle

        private BeobUrl beobUrl = new BeobUrl();
        private BeobPrint beobPrint = new BeobPrint();
        private BeobFilterLoeschen beobLoeschen = new BeobFilterLoeschen();
        private BeobAbo beobAbo = new BeobAbo(false /* mit Titel */);
        private BeobAbo beobAboMitTitel = new BeobAbo(true /* mit Titel */);
        private BeobAboFilter beobAboFilter = new BeobAboFilter();
        private BeobFilterThema beobThema = new BeobFilterThema();
        private BeobFilterSender beobSender = new BeobFilterSender();
        private BeobFilterSenderThema beobSenderThema = new BeobFilterSenderThema();
        private BeobFilterSenderThemaTitel beobSenderThemaTitel = new BeobFilterSenderThemaTitel();
        private BeobBlacklist boeobBlacklistSender = new BeobBlacklist(true, false);
        private BeobBlacklist boeobBlacklistSenderThema = new BeobBlacklist(true, true);
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
                    if (!filmInfoHud.isVisible()) {
                        filmInfoHud.show();
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
                    DatenFilm datenFilm = getSelFilm();
                    boolean stop = false;
                    DatenDownload datenDownload = Daten.listeDownloadsButton.getDownloadUrlFilm(datenFilm.arr[DatenFilm.FILM_URL_NR]);
                    if (datenDownload != null) {
                        if (datenDownload.start != null) {
                            if (datenDownload.start.status == Start.STATUS_RUN) {
                                stop = true;
                                Daten.listeDownloadsButton.delDownloadByUrl(datenFilm.arr[DatenFilm.FILM_URL_NR], true);
                            }
                        }
                    }
                    if (!stop) {
                        filmAbspielen_();
                    }
                } else if (tabelle.convertColumnIndexToModel(column) == DatenFilm.FILM_AUFZEICHNEN_NR) {
                    filmSpeichern_();
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
            DatenFilm film = getFilm(nr);
            JPopupMenu jPopupMenu = new JPopupMenu();

            //Thema laden
            JMenuItem item = new JMenuItem("Film abspielen");
            item.setIcon(GetIcon.getIcon("film_start_16.png"));
            item.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    filmAbspielen_();
                }
            });
            jPopupMenu.add(item);
            //Url
            item = new JMenuItem("Film aufzeichnen");
            item.setIcon(GetIcon.getIcon("film_rec_16.png"));
            item.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    filmSpeichern_();
                }
            });
            jPopupMenu.add(item);

            //##Trenner##
            jPopupMenu.addSeparator();
            //##Trenner##
            if (Boolean.parseBoolean(Daten.mVConfig.get(Konstanten.SYSTEM_PANEL_FILTER_ANZEIGEN))) {
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
            JMenuItem itemAboLoeschen;
            JMenuItem itemAbo;
            JMenuItem itemAboMitTitel;
            JMenuItem itemAboFilter;
            itemAboLoeschen = new JMenuItem("Abo Löschen");
            itemAbo = new JMenuItem("Abo mit Sender und Thema anlegen");
            itemAboMitTitel = new JMenuItem("Abo mit Sender und Thema und Titel anlegen");
            itemAboFilter = new JMenuItem("Abo aus Filter anlegen");
            if (film != null) {
                if ((Daten.listeAbo.getAboFuerFilm_schnell(film, false /*die Länge nicht prüfen*/)) != null) {
                    //gibts schon, dann löschen
                    itemAbo.setEnabled(false);
                    itemAboMitTitel.setEnabled(false);
                    itemAboFilter.setEnabled(false);
                    itemAboLoeschen.addActionListener(beobAbo);
                } else {
                    itemAboLoeschen.setEnabled(false);
                    //neues Abo anlegen
                    itemAbo.addActionListener(beobAbo);
                    itemAboMitTitel.addActionListener(beobAboMitTitel);
                    itemAboFilter.addActionListener(beobAboFilter);
                }
            }
            submenueAbo.add(itemAboLoeschen);
            submenueAbo.add(itemAbo);
            submenueAbo.add(itemAboMitTitel);
            submenueAbo.add(itemAboFilter);

            //Programme einblenden
            JMenu submenue = new JMenu("Film mit Set starten");
            jPopupMenu.add(submenue);
            ListePset liste = daten.listePset.getListeButton();
            for (DatenPset pset : liste) {
                if (pset.getListeProg().isEmpty() && pset.arr[DatenPset.PROGRAMMSET_NAME_NR].equals("")) {
                    // ein "leeres" Pset, Platzhalter
                    continue;
                }
                Color col = pset.getFarbe(daten);
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
            itemBlackSender = new JMenuItem("Sender in die Blacklist einfügen");
            itemBlackSenderThema = new JMenuItem("Sender und Thema in die Blacklist einfügen");
            itemBlackSender.addActionListener(boeobBlacklistSender);
            itemBlackSenderThema.addActionListener(boeobBlacklistSenderThema);
            submenueBlack.add(itemBlackSender);
            submenueBlack.add(itemBlackSenderThema);

            //##Trenner##
            jPopupMenu.addSeparator();
            //##Trenner##

            //Url
            item = new JMenuItem("URL kopieren");
            item.addActionListener(beobUrl);
            jPopupMenu.add(item);
            //Drucken
            item = new JMenuItem("Tabelle drucken");
            item.addActionListener(beobPrint);
            jPopupMenu.add(item);
            //Infos
            item = new JMenuItem("Filminformation anzeigen");
            item.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent actionEvent) {
                    if (!filmInfoHud.isVisible()) {
                        filmInfoHud.show();
                    }
                }
            });
            jPopupMenu.add(item);
            //History
            if (film != null) {
                if (daten.history.contains(film.getUrlHistory())) {
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

        private class BeobUrl implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                int nr = tabelle.rowAtPoint(p);
                if (nr >= 0) {
                    GuiFunktionen.copyToClipboard(
                            tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(nr),
                                    DatenFilm.FILM_URL_NR).toString());
                }
            }
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
                        daten.history.add(film.getUrlHistory());
                    } else {
                        daten.history.remove(film.getUrlHistory());
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
                    Log.fehlerMeldung(688542239, Log.FEHLER_ART_PROG, "GuiFilme.BeobPrint", ex);
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
                    frameFilter.jComboBoxFilterThema.setSelectedIndex(0);
                    frameFilter.jComboBoxFilterThema.setSelectedItem(thema);
                    stopBeob = false;
                    tabelleLaden();
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
                    frameFilter.jComboBoxFilterSender.setSelectedIndex(0);
                    frameFilter.jComboBoxFilterSender.setSelectedItem(sen);
                    stopBeob = false;
                    tabelleLaden();
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
                    frameFilter.jComboBoxFilterSender.setSelectedIndex(0);
                    frameFilter.jComboBoxFilterSender.setSelectedItem(sen);
                    String thema = film.arr[DatenFilm.FILM_THEMA_NR];
                    frameFilter.jComboBoxFilterThema.setSelectedIndex(0);
                    frameFilter.jComboBoxFilterThema.setSelectedItem(thema);
                    if (frameFilter.jComboBoxFilterThema.getSelectedIndex() == 0) {
                        String themaFilter = getThemaFilter(sen, thema);
                        frameFilter.jComboBoxFilterThema.setSelectedItem(themaFilter);
                    }
                    stopBeob = false;
                    tabelleLaden();
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
                    frameFilter.jComboBoxFilterSender.setSelectedIndex(0);
                    frameFilter.jComboBoxFilterSender.setSelectedItem(sen);
                    String thema = film.arr[DatenFilm.FILM_THEMA_NR];
                    frameFilter.jComboBoxFilterThema.setSelectedIndex(0);
                    frameFilter.jComboBoxFilterThema.setSelectedItem(thema);
                    if (frameFilter.jComboBoxFilterThema.getSelectedIndex() == 0) {
                        String themaFilter = getThemaFilter(sen, thema);
                        frameFilter.jComboBoxFilterThema.setSelectedItem(themaFilter);
                    }
                    String tit = film.arr[DatenFilm.FILM_TITEL_NR];
                    frameFilter.jTextFieldFilterTitel.setText(tit);
                    stopBeob = false;
                    tabelleLaden();
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
                filterLoeschen();
            }
        }

        private class BeobAbo implements ActionListener {

            boolean mitTitel = false;

            public BeobAbo(boolean mmitTitel) {
                mitTitel = mmitTitel;
            }

            @Override
            public void actionPerformed(ActionEvent e) {
                if (daten.listePset.getListeAbo().isEmpty()) {
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
                                    Daten.listeAbo.addAbo(film.arr[DatenFilm.FILM_SENDER_NR], film.arr[DatenFilm.FILM_THEMA_NR], film.arr[DatenFilm.FILM_TITEL_NR]);
                                } else {
                                    Daten.listeAbo.addAbo(film.arr[DatenFilm.FILM_SENDER_NR], film.arr[DatenFilm.FILM_THEMA_NR], "");
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
                if (daten.listePset.getListeAbo().isEmpty()) {
                    MVMessageDialog.showMessageDialog(parentComponent, "Im Menü unter \"Datei->Optionen->Videoplayer\" ein Programm zum Aufzeichnen festlegen.",
                            "kein Videoplayer!", JOptionPane.INFORMATION_MESSAGE);
                } else {
                    int nr = tabelle.rowAtPoint(p);
                    if (nr >= 0) {
                        stopBeob = true;
                        DatenFilm film = getFilm(nr);
                        String thema = film.arr[DatenFilm.FILM_THEMA_NR];
                        //neues Abo anlegen
                        Daten.listeAbo.addAbo(frameFilter.jComboBoxFilterSender.getSelectedItem().toString(),
                                frameFilter.jComboBoxFilterThema.getSelectedItem().toString(),
                                frameFilter.jTextFieldFilterTitel.getText(), frameFilter.jTextFieldFilterThemaTitel.getText(),
                                frameFilter.jTextFieldFilterIrgendwo.getText(), frameFilter.jSliderMinuten.getValue(), thema);
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
                    Daten.mVConfig.add(Konstanten.SYSTEM_BLACKLIST_AUSGESCHALTET, Boolean.toString(false));
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
            Filter.checkPattern1(frameFilter.jTextFieldFilterThemaTitel);
            Filter.checkPattern1(frameFilter.jTextFieldFilterTitel);
            if (Boolean.parseBoolean(Daten.mVConfig.get(Konstanten.SYSTEM_ECHTZEITSUCHE))) {
                tabelleLaden();
            }
        }
    }
}
