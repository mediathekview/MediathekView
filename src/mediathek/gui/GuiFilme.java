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

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Point;
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
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.KeyStroke;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import mediathek.MVStatusBar_Mac;
import mediathek.MediathekGui;
import mediathek.controller.starter.Start;
import mediathek.daten.Daten;
import mediathek.daten.DatenAbo;
import mediathek.daten.DatenBlacklist;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenPset;
import mediathek.daten.ListePset;
import mediathek.file.GetFile;
import mediathek.gui.dialog.DialogAddDownload;
import mediathek.gui.dialog.DialogHilfe;
import mediathek.gui.dialog.DialogLeer;
import mediathek.gui.dialog.MVFilmInformation;
import mediathek.gui.dialogEinstellungen.PanelBlacklist;
import mediathek.res.GetIcon;
import mediathek.tool.BeobTableHeader;
import mediathek.tool.CellRendererFilme;
import mediathek.tool.Datum;
import mediathek.tool.Filter;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.HinweisKeineAuswahl;
import mediathek.tool.Konstanten;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.Log;
import mediathek.tool.MVJTable;
import mediathek.tool.MVMessageDialog;
import mediathek.tool.MViewListeFilme;
import mediathek.tool.TModel;
import mediathek.tool.TModelFilm;
import msearch.daten.DatenFilm;
import msearch.filmeSuchen.MSearchListenerFilmeLaden;
import msearch.filmeSuchen.MSearchListenerFilmeLadenEvent;

public class GuiFilme extends PanelVorlage {

    private JButton buttonArray[];
    private final String[] COMBO_ZEIT = new String[]{"alles", "1 Tag", "2 Tage", "3 Tage", "4 Tage", "5 Tage", "10 Tage", "15 Tage", "20 Tage", "30 Tage"};
    public static final int[] COMBO_ZEIT_INT = {0, 1, 2, 3, 4, 5, 10, 15, 20, 30};
    private BeobMausTabelle beobMausTabelle;
    private MVFilmInformation filmInfoHud;
    private String[] sender;
    private String[][] themenPerSender;
    //private String[] alleThemen;
    private PanelBeschreibung panelBeschreibung;

    public GuiFilme(Daten d, Component parentComponent) {
        super(d, parentComponent);
        initComponents();
        tabelle = new MVJTable(MVJTable.TABELLE_TAB_FILME);
        jScrollPane1.setViewportView(tabelle);
        panelVideoplayerSetzen();
        panelBeschreibung = new PanelBeschreibung(daten);
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
        daten.mediathekGui.setToolbar(MediathekGui.UIButtonState.FILME);
        daten.mediathekGui.getStatusBar().setIndexForCenterDisplay(MVStatusBar_Mac.StatusbarIndex.FILME);
        aktFilmSetzen();
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
        jButtonBlacklist.setIcon(GetIcon.getIcon("blacklist_16.png"));
        jButtonFilterLoeschen.setIcon(GetIcon.getIcon("del_16.png"));
        jButtonHilfe.setIcon(GetIcon.getIcon("help_16.png"));
        checkBlacklist(true);
        panelBeschreibungSetzen();
        jPanelFilter.setVisible(Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_PANEL_FILTER_ANZEIGEN_NR]));
        jComboBoxZeitraum.setModel(new DefaultComboBoxModel<String>(COMBO_ZEIT));
        try {
            jCheckBoxKeineAbos.setSelected(Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_FILTER_KEINE_ABO_NR]));
            jCheckBoxKeineGesehenen.setSelected(Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_FILTER_KEINE_GESEHENE_NR]));
            jCheckBoxNurHd.setSelected(Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_FILTER_NUR_HD_NR]));
            jComboBoxZeitraum.setSelectedIndex(Integer.parseInt(Daten.system[Konstanten.SYSTEM_FILTER_TAGE_NR]));
        } catch (Exception ex) {
            jComboBoxZeitraum.setSelectedIndex(6);
            Daten.system[Konstanten.SYSTEM_FILTER_TAGE_NR] = "6";
        }
        jComboBoxZeitraum.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (!stopBeob) {
                    Daten.system[Konstanten.SYSTEM_FILTER_TAGE_NR] = String.valueOf(jComboBoxZeitraum.getSelectedIndex());
                    checkBlacklist(false);
                    tabelleLaden();
                }
            }
        });
        Daten.filmeLaden.addAdListener(new MSearchListenerFilmeLaden() {
            @Override
            public void start(MSearchListenerFilmeLadenEvent event) {
                beobMausTabelle.itemSenderLaden.setEnabled(false);
                tabelleLaden();
            }

            @Override
            public void fertig(MSearchListenerFilmeLadenEvent event) {
                checkBlacklist(true);
                tabelleLaden();
                beobMausTabelle.itemSenderLaden.setEnabled(true);
            }
        });
        jButtonHilfe.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                new DialogHilfe(null, false, new GetFile().getHilfeSuchen(GetFile.PFAD_HILFETEXT_FILTER)).setVisible(true);
            }
        });
        jButtonBlacklist.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                DialogLeer dialog = new DialogLeer(daten.mediathekGui, true);
                dialog.init("Blacklist", new PanelBlacklist(daten, daten.mediathekGui, PanelBlacklist.class.getName() + "_2"));
                dialog.setVisible(true);
            }
        });
        //Tabelle einrichten
        ActionMap am = tabelle.getActionMap();
        //ActionMap am = new ActionMap();
        //tabelle.setActionMap(am);
        am.put("film_starten", new BeobAbstractAction());

        InputMap im = tabelle.getInputMap();
        //InputMap im = new InputMap();
        //tabelle.setInputMap(JComponent.WHEN_FOCUSED, im);
        KeyStroke enter = KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0);
        im.put(enter, "film_starten");

        tabelle.setModel(new TModelFilm(new Object[][]{}, DatenFilm.COLUMN_NAMES));
        beobMausTabelle = new BeobMausTabelle();
        tabelle.addMouseListener(beobMausTabelle);
        tabelle.getSelectionModel().addListSelectionListener(new BeobachterTableSelect());
        tabelle.setDefaultRenderer(Object.class, new CellRendererFilme(daten));
        tabelle.setDefaultRenderer(Datum.class, new CellRendererFilme(daten));
        tabelle.getTableHeader().addMouseListener(new BeobTableHeader(tabelle, DatenFilm.COLUMN_NAMES, DatenFilm.spaltenAnzeigen) {
            @Override
            public void tabelleLaden_() {
                tabelleLaden();
            }
        });
        //beobachter Filter
        jToggleButtonLivestram.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (!stopBeob) {
                    stopBeob = true;
                    //auch die Filter löschen
                    jComboBoxFilterSender.setModel(new javax.swing.DefaultComboBoxModel<String>(sender));
                    jComboBoxFilterThema.setModel(new javax.swing.DefaultComboBoxModel<String>(getThemen("")));
                    jTextFieldFilterTitel.setText("");
                }
                tabelleLaden();
            }
        });
        //Combo Sender
        jButtonFilterLoeschen.addActionListener(new BeobFilterLoeschen());
        jComboBoxFilterSender.setModel(new javax.swing.DefaultComboBoxModel<String>(sender));
        jComboBoxFilterSender.addActionListener(new BeobFilterSender());
        jComboBoxFilterThema.setModel(new javax.swing.DefaultComboBoxModel<String>(getThemen("")));
        jComboBoxFilterThema.addActionListener(new BeobFilter());
        jTextFieldFilterTitel.addActionListener(new BeobFilter());
        jTextFieldFilterTitel.getDocument().addDocumentListener(new BeobFilterTitelDoc());
        jTextFieldFilterThemaTitel.addActionListener(new BeobFilter());
        jTextFieldFilterThemaTitel.getDocument().addDocumentListener(new BeobFilterTitelDoc());
        jTextFieldFilterIrgendwo.addActionListener(new BeobFilter());
        jTextFieldFilterIrgendwo.getDocument().addDocumentListener(new BeobFilterTitelDoc());
        jSliderMinuten.setValue(0);
        jTextFieldFilterMinuten.setText(String.valueOf(jSliderMinuten.getValue()));
        jSliderMinuten.addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                jTextFieldFilterMinuten.setText(String.valueOf(jSliderMinuten.getValue()));
                if (!jSliderMinuten.getValueIsAdjusting()) {
                    tabelleLaden();
                }
            }
        });
        jCheckBoxKeineAbos.addActionListener(new BeobFilter());
        jCheckBoxKeineGesehenen.addActionListener(new BeobFilter());
        jCheckBoxNurHd.addActionListener(new BeobFilter());
        //restliche Filter
        jScrollPane1.addMouseListener(new BeobMausLaufendeProgramme());
        daten.mediathekGui.getStatusBar().getComponent().addMouseListener(new BeobMausLaufendeProgramme());
        // Filter erst mal ausblenden
        //jCheckBoxFilter.addActionListener(new BeobMpanel(jCheckBoxFilter, jPanelFilter, "Filter"));
        jCheckBoxFilter.setIcon(GetIcon.getIcon("close_15.png"));
        jCheckBoxFilter.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.system[Konstanten.SYSTEM_PANEL_FILTER_ANZEIGEN_NR] = Boolean.FALSE.toString();
                daten.mediathekGui.filterAnzeigen(true);
                panelFilterSetzen();
            }
        });
        jCheckBoxProgamme.setIcon(GetIcon.getIcon("close_15.png"));
        jCheckBoxProgamme.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.system[Konstanten.SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN_NR] = Boolean.FALSE.toString();
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
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_LISTE_HISTORY_GEAENDERT, GuiFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                tabelleLaden();
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_BLACKLIST_GEAENDERT, GuiFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                checkBlacklist(false);
                tabelleLaden();
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_FILMLISTE_GEAENDERT, GuiFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                checkBlacklist(false); // nur GuiDebug zum löschen aus der Filmliste
                tabelleLaden();
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_START_EVENT, GuiFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                tabelle.fireTableDataChanged(true /*setSpalten*/);
                setInfo();
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_LISTE_ABOS, GuiFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                //checkBlacklist(true);
                tabelleLaden();
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
                if (Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_PANEL_FILTER_ANZEIGEN_NR])) {
                    jTextFieldFilterThemaTitel.requestFocus();
                    jTextFieldFilterThemaTitel.setCaretPosition(0);
                }
            }
        });
    }

    private void panelBeschreibungSetzen() {
        jPanelBeschreibung.setVisible(Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_PANEL_BESCHREIBUNG_ANZEIGEN_NR]));
    }

    private void panelFilterSetzen() {
        // Panel anzeigen und die Filmliste anpassen
        jPanelFilter.setVisible(Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_PANEL_FILTER_ANZEIGEN_NR]));
        checkBlacklist(false);
        tabelleLaden();
    }

    private void themenLaden() {
        // der erste Sender ist ""
        sender = MViewListeFilme.getModelOfFieldSender(Daten.listeFilmeNachBlackList);
        //für den Sender "" sind alle Themen im themenPerSender[0]
        themenPerSender = new String[sender.length][];
        for (int i = 0; i < sender.length; ++i) {
            themenPerSender[i] = MViewListeFilme.getModelOfFieldThema(Daten.listeFilmeNachBlackList, sender[i]);
        }
        //alleThemen = Daten.listeFilmeNachBlackList.getModelOfFieldThema("");
    }

    private String[] getThemen(String ssender) {
        for (int i = 1; i < themenPerSender.length; ++i) {
            if (sender[i].equals(ssender)) {
                return themenPerSender[i];
            }
        }
        return themenPerSender[0];
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
        if (daten.listePset.getListeSpeichern().size() == 0) {
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
                    selRow = tabelle.convertRowIndexToModel(selRow);
                    // film = Daten.listeFilme.getFilmByUrl(tabelle.getModel().getValueAt(selRow, DatenFilm.FILM_URL_NR).toString());
                    film = Daten.listeFilme.getFilmByNr(tabelle.getModel().getValueAt(selRow, DatenFilm.FILM_NR_NR).toString());
                    DialogAddDownload dialog = new DialogAddDownload(daten.mediathekGui, daten, film, pSet);
                    dialog.setVisible(true);
                }
            }
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
        jPanelExtra.setVisible(Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN_NR]));
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
            if (!Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_PANEL_FILTER_ANZEIGEN_NR])) {
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
                String filterThema = jComboBoxFilterThema.getSelectedItem().toString();
                String filterSender = jComboBoxFilterSender.getSelectedItem().toString();
                boolean themaOpen = jComboBoxFilterThema.isPopupVisible();
                boolean senderOpen = jComboBoxFilterSender.isPopupVisible();
                if (Daten.listeFilmeNachBlackList.isEmpty()) {
                    //jComboBoxFilterSender.setModel(new javax.swing.DefaultComboBoxModel(Daten.listeFilmeNachBlackList.getModelOfFieldSender()));
                    //jComboBoxFilterThema.setModel(new javax.swing.DefaultComboBoxModel(Daten.listeFilmeNachBlackList.getModelOfFieldThema("")));
                    jComboBoxFilterSender.setModel(new javax.swing.DefaultComboBoxModel<String>(sender));
                    jComboBoxFilterThema.setModel(new javax.swing.DefaultComboBoxModel<String>(getThemen("")));
                    jComboBoxFilterSender.setSelectedIndex(0);
                    jComboBoxFilterThema.setSelectedIndex(0);
                    listeInModellLaden(); // zum löschen der Tabelle
                } else {
                    //Filme neu laden
                    listeInModellLaden();
                    //Filter Sender
                    jComboBoxFilterSender.setModel(new javax.swing.DefaultComboBoxModel<String>(sender));
                    jComboBoxFilterSender.setSelectedIndex(0);
                    if (!filterSender.equals("")) {
                        // ist wohl ein Bug beim Combo, klappt nur richtig wenn editable?!
                        jComboBoxFilterSender.setEditable(true);
                        jComboBoxFilterSender.setSelectedItem(filterSender);
                        jComboBoxFilterSender.setEditable(false);
                        if (jComboBoxFilterSender.getSelectedIndex() == 0) {
                            // war wohl nix, der gewählte Sender wurde in die Blacklist eingetragen
                            filterSender = "";
                            listeInModellLaden();
                        }
                    }
                    jComboBoxFilterSender.setPopupVisible(senderOpen);
                    // Filter Thema
                    if (filterSender.equals("")) {
                        jComboBoxFilterThema.setModel(new javax.swing.DefaultComboBoxModel<String>(getThemen("")));
                    } else {
                        jComboBoxFilterThema.setModel(new javax.swing.DefaultComboBoxModel<String>(getThemen(filterSender)));
                    }
                    // wenn Thema bei dem Sender vorhanden, dann wieder setzen
                    // ist wohl ein Bug beim Combo, klappt nur richtig wenn editable?!
                    jComboBoxFilterThema.setEditable(true);
                    jComboBoxFilterThema.setSelectedItem(filterThema);
                    jComboBoxFilterThema.setEditable(false);
                    if (!filterThema.equals("") && jComboBoxFilterThema.getSelectedIndex() == 0) {
                        // war wohl nix
                        themaNichtDa = true;
                    }
                    jComboBoxFilterThema.setPopupVisible(themaOpen);
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
        if (Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_PANEL_FILTER_ANZEIGEN_NR])) {
            // normal mit den Filtern aus dem Filterpanel suchen
            MViewListeFilme.getModelTabFilme(Daten.listeFilmeNachBlackList, daten, tabelle, jComboBoxFilterSender.getSelectedItem().toString(),
                    jComboBoxFilterThema.getSelectedItem().toString(), jTextFieldFilterTitel.getText(), jTextFieldFilterThemaTitel.getText(),
                    jTextFieldFilterIrgendwo.getText(), jSliderMinuten.getValue(),
                    jCheckBoxKeineAbos.isSelected(), jCheckBoxKeineGesehenen.isSelected(), jCheckBoxNurHd.isSelected(), jToggleButtonLivestram.isSelected());
        } else {
            // jetzt nur den Filter aus der Toolbar
            MViewListeFilme.getModelTabFilme(Daten.listeFilmeNachBlackList, daten, tabelle, "",
                    "", "", daten.mediathekGui.getFilterTextFromSearchField(), "", 0,
                    false, false, false, false);
        }
    }
    // ####################################
    // Ende Tabelle asynchron füllen
    // ####################################

    private void filterLoeschen() {
        stopBeob = true;
        //ComboModels neu aufbauen
        jComboBoxFilterSender.setModel(new javax.swing.DefaultComboBoxModel<>(sender));
        jComboBoxFilterThema.setModel(new javax.swing.DefaultComboBoxModel<>(getThemen("")));
        jTextFieldFilterTitel.setText("");
        jTextFieldFilterThemaTitel.setText("");
        jTextFieldFilterIrgendwo.setText("");
        jSliderMinuten.setValue(0);
        //neu laden
        tabelleLaden();
    }

    private void aktFilmSetzen() {
        if (this.isShowing()) {
            DatenFilm aktFilm = new DatenFilm();
            int selectedTableRow = tabelle.getSelectedRow();
            if (selectedTableRow >= 0) {
                //int selectedModelRow = tabelle.convertRowIndexToModel(selectedTableRow);
                //DatenFilm film = Daten.listeFilme.getFilmByUrl(tabelle.getModel().getValueAt(selectedModelRow, DatenFilm.FILM_URL_NR).toString());
                //DatenFilm film = Daten.listeFilme.getFilmByNr(tabelle.getModel().getValueAt(selectedModelRow, DatenFilm.FILM_NR_NR).toString());
                DatenFilm film = getSelFilm();
                if (film != null) {
                    aktFilm = film;
                }
            }
            filmInfoHud.updateCurrentFilm(aktFilm);
            // Beschreibung setzen
            panelBeschreibung.setAktFilm(aktFilm);
        }
    }

    ///
    private DatenFilm getSelFilm() {
        int selectedTableRow = tabelle.getSelectedRow();
        if (selectedTableRow >= 0) {
            int selectedModelRow = tabelle.convertRowIndexToModel(selectedTableRow);
            //DatenFilm film = Daten.listeFilme.getFilmByUrl(tabelle.getModel().getValueAt(selectedModelRow, DatenFilm.FILM_URL_NR).toString());
            String nr = tabelle.getModel().getValueAt(selectedModelRow, DatenFilm.FILM_NR_NR).toString();
            if (nr == null) {
                // Tabelle noch nicht ganz geladen
                return null;
            }
            DatenFilm film = Daten.listeFilme.getFilmByNr(nr);
            return film;
        }
        return null;
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
            int selectedModelRow = tabelle.convertRowIndexToModel(tabelle.getSelectedRow());
            //DatenFilm datenFilm = Daten.listeFilmeNachBlackList.getFilmByUrl(tabelle.getModel().getValueAt(selectedModelRow, DatenFilm.FILM_URL_NR).toString());
            DatenFilm datenFilm = Daten.listeFilme.getFilmByNr(tabelle.getModel().getValueAt(selectedModelRow, DatenFilm.FILM_NR_NR).toString());
            daten.starterClass.urlStarten(pSet, datenFilm);
        }
    }

    private void senderLaden() {
        //Mauskontext "Sender aktualisieren"
        int nr = tabelle.getSelectedRow();
        if (nr >= 0) {
            DatenFilm film = Daten.listeFilme.getFilmByNr(tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(nr), DatenFilm.FILM_NR_NR).toString());
            String send = film.arr[DatenFilm.FILM_SENDER_NR];
            Daten.filmeLaden.updateSender(new String[]{send}, Daten.listeFilme, false /* senderAllesLaden */);
        }
    }

    private void setInfo() {
        String textLinks;
        // Text links: Zeilen Tabelle
        boolean open = false;
        int gesamt = Daten.listeFilme.size();
        int anzListe = tabelle.getModel().getRowCount();
        int runs = Daten.listeDownloads.getListteStartsNotFinished(Start.QUELLE_BUTTON).size();
        int laufen = Daten.listeDownloads.getStartsRun();
        int warten = Daten.listeDownloads.getStartsNotStarted();
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
            textLinks = ifOpen(open, textLinks);
            open = true;
            textLinks += "insgesamt: " + gesamt + " Filme";
        }
        // laufende Programme
        if (runs == 1) {
            textLinks = ifOpen(open, textLinks);
            open = true;
            textLinks += (runs + " laufender Film");
        } else if (runs > 1) {
            textLinks = ifOpen(open, textLinks);
            open = true;
            textLinks += (runs + " laufende Filme");
        }
        // auch die Downloads anzeigen
        if (laufen > 0 || warten > 0) {
            textLinks = ifOpen(open, textLinks);
            open = true;
            textLinks += "Downloads: ";
            if (laufen == 1) {
                textLinks += "1 läuft,";
            } else {
                textLinks += laufen + " laufen,";
            }
            if (warten == 1) {
                textLinks += " 1 wartet";
            } else {
                textLinks += " " + warten + " warten";
            }
        }
        if (open) {
            textLinks += ")";
        }
        // Infopanel setzen
        daten.mediathekGui.getStatusBar().setTextLeft(MVStatusBar_Mac.StatusbarIndex.FILME, textLinks);
    }

    private String ifOpen(boolean open, String textLinks) {
        if (!open) {
            textLinks += ", (";
        } else {
            textLinks += "  -  ";
        }
        return textLinks;
    }

    private void checkBlacklist(boolean abosEintragen) {
        if (abosEintragen) {
            // Abos eintragen in der gesamten Liste vor Blacklist da das nur beim Ändern der Filmliste oder
            // beim Ändern von Abos gemacht wird
            MViewListeFilme.abosEintragen(Daten.listeFilme, Daten.listeAbo);
        }
        daten.listeBlacklist.filterListe(Daten.listeFilme, Daten.listeFilmeNachBlackList);
        themenLaden();
    }

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jPanel4 = new javax.swing.JPanel();
        jPanelFilter = new javax.swing.JPanel();
        jCheckBoxFilter = new javax.swing.JCheckBox();
        jPanel3 = new javax.swing.JPanel();
        javax.swing.JPanel jPanel2 = new javax.swing.JPanel();
        javax.swing.JLabel jLabel2 = new javax.swing.JLabel();
        jComboBoxFilterSender = new javax.swing.JComboBox<String>();
        javax.swing.JLabel jLabel3 = new javax.swing.JLabel();
        jComboBoxFilterThema = new javax.swing.JComboBox<String>();
        javax.swing.JLabel jLabel5 = new javax.swing.JLabel();
        jTextFieldFilterTitel = new javax.swing.JTextField();
        javax.swing.JLabel jLabel6 = new javax.swing.JLabel();
        jTextFieldFilterThemaTitel = new javax.swing.JTextField();
        jLabel7 = new javax.swing.JLabel();
        jTextFieldFilterIrgendwo = new javax.swing.JTextField();
        jSliderMinuten = new javax.swing.JSlider();
        jLabel8 = new javax.swing.JLabel();
        jTextFieldFilterMinuten = new javax.swing.JTextField();
        jButtonFilterLoeschen = new javax.swing.JButton();
        javax.swing.JPanel jPanel1 = new javax.swing.JPanel();
        javax.swing.JLabel jLabel1 = new javax.swing.JLabel();
        jComboBoxZeitraum = new javax.swing.JComboBox<String>();
        jCheckBoxKeineGesehenen = new javax.swing.JCheckBox();
        jCheckBoxKeineAbos = new javax.swing.JCheckBox();
        jToggleButtonLivestram = new javax.swing.JToggleButton();
        jButtonBlacklist = new javax.swing.JButton();
        jButtonHilfe = new javax.swing.JButton();
        jCheckBoxNurHd = new javax.swing.JCheckBox();
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

        jPanelFilter.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 153)));

        jCheckBoxFilter.setToolTipText("Filter ausblenden");

        jPanel2.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(204, 204, 204)));

        jLabel2.setText("Sender:");

        jComboBoxFilterSender.setMaximumRowCount(25);

        jLabel3.setText("Thema:");

        jComboBoxFilterThema.setMaximumRowCount(25);

        jLabel5.setText("Titel:");

        jLabel6.setText("Thema oder Titel:");

        jLabel7.setText("Irgendwo:");

        jLabel8.setText("Mindestlänge [min]:");

        jTextFieldFilterMinuten.setEditable(false);
        jTextFieldFilterMinuten.setHorizontalAlignment(javax.swing.JTextField.CENTER);
        jTextFieldFilterMinuten.setText("100");

        jButtonFilterLoeschen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/del_16.png"))); // NOI18N
        jButtonFilterLoeschen.setToolTipText("Filter löschen");

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addComponent(jLabel2)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jComboBoxFilterSender, javax.swing.GroupLayout.PREFERRED_SIZE, 125, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabel3)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jComboBoxFilterThema, javax.swing.GroupLayout.PREFERRED_SIZE, 166, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabel5))
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addComponent(jLabel6)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldFilterThemaTitel, javax.swing.GroupLayout.PREFERRED_SIZE, 263, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabel7)))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addComponent(jTextFieldFilterTitel, javax.swing.GroupLayout.DEFAULT_SIZE, 76, Short.MAX_VALUE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabel8)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldFilterMinuten, javax.swing.GroupLayout.PREFERRED_SIZE, 39, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jSliderMinuten, javax.swing.GroupLayout.PREFERRED_SIZE, 107, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addComponent(jTextFieldFilterIrgendwo)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(jButtonFilterLoeschen)))
                .addContainerGap())
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jLabel2)
                    .addComponent(jComboBoxFilterSender, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel3)
                    .addComponent(jComboBoxFilterThema, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel5)
                    .addComponent(jTextFieldFilterTitel, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel8)
                    .addComponent(jTextFieldFilterMinuten, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jSliderMinuten, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jLabel6)
                    .addComponent(jTextFieldFilterThemaTitel, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel7)
                    .addComponent(jTextFieldFilterIrgendwo, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jButtonFilterLoeschen))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel2Layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonFilterLoeschen, jTextFieldFilterIrgendwo, jTextFieldFilterThemaTitel, jTextFieldFilterTitel});

        jPanel1.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(204, 204, 204)));

        jLabel1.setText("Zeitraum:");

        jComboBoxZeitraum.setMaximumRowCount(10);

        jCheckBoxKeineGesehenen.setText("keine gesehenen");

        jCheckBoxKeineAbos.setText("keine Abos");

        jToggleButtonLivestram.setText("Livestreams");

        jButtonBlacklist.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/blacklist_16.png"))); // NOI18N
        jButtonBlacklist.setToolTipText("Blacklist öffnen");

        jButtonHilfe.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/help_16.png"))); // NOI18N

        jCheckBoxNurHd.setText("nur HD");

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabel1)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jComboBoxZeitraum, javax.swing.GroupLayout.PREFERRED_SIZE, 131, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(18, 18, 18)
                .addComponent(jCheckBoxKeineGesehenen)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jCheckBoxKeineAbos)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jCheckBoxNurHd)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addComponent(jToggleButtonLivestram)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jButtonBlacklist)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jButtonHilfe)
                .addContainerGap())
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jLabel1)
                    .addComponent(jComboBoxZeitraum, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jCheckBoxKeineGesehenen)
                    .addComponent(jCheckBoxKeineAbos)
                    .addComponent(jCheckBoxNurHd)
                    .addComponent(jToggleButtonLivestram)
                    .addComponent(jButtonBlacklist)
                    .addComponent(jButtonHilfe))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        javax.swing.GroupLayout jPanel3Layout = new javax.swing.GroupLayout(jPanel3);
        jPanel3.setLayout(jPanel3Layout);
        jPanel3Layout.setHorizontalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel3Layout.createSequentialGroup()
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jPanel1, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel2, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addGap(0, 0, 0))
        );
        jPanel3Layout.setVerticalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addComponent(jPanel2, javax.swing.GroupLayout.PREFERRED_SIZE, 75, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(5, 5, 5)
                .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(0, 0, 0))
        );

        javax.swing.GroupLayout jPanelFilterLayout = new javax.swing.GroupLayout(jPanelFilter);
        jPanelFilter.setLayout(jPanelFilterLayout);
        jPanelFilterLayout.setHorizontalGroup(
            jPanelFilterLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelFilterLayout.createSequentialGroup()
                .addComponent(jCheckBoxFilter)
                .addGap(5, 5, 5)
                .addComponent(jPanel3, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addGap(5, 5, 5))
        );
        jPanelFilterLayout.setVerticalGroup(
            jPanelFilterLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelFilterLayout.createSequentialGroup()
                .addGroup(jPanelFilterLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jCheckBoxFilter)
                    .addGroup(jPanelFilterLayout.createSequentialGroup()
                        .addGap(5, 5, 5)
                        .addComponent(jPanel3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)))
                .addGap(5, 5, 5))
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
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanelFilter, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 947, Short.MAX_VALUE)
                    .addComponent(jPanelExtra, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanelBeschreibung, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
            .addComponent(jPanel5, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanelFilter, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 214, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanelBeschreibung, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanelExtra, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel5, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonBlacklist;
    private javax.swing.JButton jButtonFilterLoeschen;
    private javax.swing.JButton jButtonHilfe;
    private javax.swing.JCheckBox jCheckBoxFilter;
    private javax.swing.JCheckBox jCheckBoxKeineAbos;
    private javax.swing.JCheckBox jCheckBoxKeineGesehenen;
    private javax.swing.JCheckBox jCheckBoxNurHd;
    private javax.swing.JCheckBox jCheckBoxProgamme;
    private javax.swing.JComboBox<String> jComboBoxFilterSender;
    private javax.swing.JComboBox<String> jComboBoxFilterThema;
    private javax.swing.JComboBox<String> jComboBoxZeitraum;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JLabel jLabel8;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JPanel jPanelBeschreibung;
    private javax.swing.JPanel jPanelExtra;
    private javax.swing.JPanel jPanelExtraInnen;
    private javax.swing.JPanel jPanelFilter;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JSlider jSliderMinuten;
    private javax.swing.JTextField jTextFieldFilterIrgendwo;
    private javax.swing.JTextField jTextFieldFilterMinuten;
    private javax.swing.JTextField jTextFieldFilterThemaTitel;
    private javax.swing.JTextField jTextFieldFilterTitel;
    private javax.swing.JToggleButton jToggleButtonLivestram;
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
                Daten.system[Konstanten.SYSTEM_FILTER_KEINE_ABO_NR] = String.valueOf(jCheckBoxKeineAbos.isSelected());
                Daten.system[Konstanten.SYSTEM_FILTER_KEINE_GESEHENE_NR] = String.valueOf(jCheckBoxKeineGesehenen.isSelected());
                Daten.system[Konstanten.SYSTEM_FILTER_NUR_HD_NR] = String.valueOf(jCheckBoxNurHd.isSelected());
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

    public class BeobMausLaufendeProgramme extends MouseAdapter {
        //rechhte Maustaste im Rahmen um die Tabelle

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
            LinkedList<DatenDownload> liste = Daten.listeDownloads.getListteStartsNotFinished(Start.QUELLE_BUTTON);
            if (liste.size() > 0) {
                JPopupMenu jPopupMenu = new JPopupMenu();
                JMenuItem item;
                for (DatenDownload s : liste) {
                    // dann läuft er noch
                    item = new JMenuItem("Beenden: [" + s.arr[DatenDownload.DOWNLOAD_SENDER_NR] + "]  " + s.arr[DatenDownload.DOWNLOAD_TITEL_NR]);
                    item.addActionListener(new BeobProgramm(s));
                    jPopupMenu.add(item);
                }
                //anzeigen
                jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
            }
        }

        private class BeobProgramm implements ActionListener {

            DatenDownload datenDownload;

            public BeobProgramm(DatenDownload d) {
                datenDownload = d;
            }

            @Override
            public void actionPerformed(ActionEvent e) {
                try {
                    if (datenDownload.start != null) {
                        Daten.listeDownloads.delDownloadByUrl(datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR]);
                    }
                } catch (Exception ex) {
                    System.err.println("GuiFilme.BeobProgramm: " + ex);
                }
            }
        }
    }

    public class BeobMausTabelle extends MouseAdapter {
        //rechhte Maustaste in der Tabelle

        JMenuItem itemSenderLaden = new JMenuItem("Sender aktualisieren");
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
        private BeobSenderLaden beobSenderLaden = new BeobSenderLaden();
        private BeobBlacklist boeobBlacklistSender = new BeobBlacklist(true, false);
        private BeobBlacklist boeobBlacklistSenderThema = new BeobBlacklist(true, true);
        private Point p;
        JPanel panelNurAbo = new JPanel();
        JPanel panelKeineAbo = new JPanel();
        JPanel panelKeineGesehenen = new JPanel();
        JPanel panel24Stunden = new JPanel();
        JPanel panelLive = new JPanel();

        public BeobMausTabelle() {
        }

        @Override
        public void mouseClicked(MouseEvent arg0) {
//            if (arg0.getButton() == MouseEvent.BUTTON3) {
//                showMenu(arg0);
//            }
            if (arg0.getButton() == MouseEvent.BUTTON1) {
                if (arg0.getClickCount() > 1) {
                    filmAbspielen_();
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

        private void showMenu(MouseEvent evt) {
            p = evt.getPoint();
            int nr = tabelle.rowAtPoint(p);
            if (nr >= 0) {
                tabelle.setRowSelectionInterval(nr, nr);
            }
            int selectedModelRow = tabelle.convertRowIndexToModel(nr);
            DatenFilm film = Daten.listeFilme.getFilmByNr(tabelle.getModel().getValueAt(selectedModelRow, DatenFilm.FILM_NR_NR).toString());
            JPopupMenu jPopupMenu = new JPopupMenu();

            //Thema laden
            JMenuItem item = new JMenuItem("Film abspielen");
            item.setIcon(GetIcon.getIcon("player_play_16.png"));
            item.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    filmAbspielen_();
                }
            });
            jPopupMenu.add(item);
            //Url
            item = new JMenuItem("Film aufzeichnen");
            item.setIcon(GetIcon.getIcon("player_rec_16.png"));
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
            if (Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_PANEL_FILTER_ANZEIGEN_NR])) {
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
                if ((daten.listeAbo.getAboFuerFilm_schnell(film, false /*die Länge nicht prüfen*/)) != null) {
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
            JMenu submenue = new JMenu("Film mit Programm starten:");
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

            //Sender laden
            itemSenderLaden.addActionListener(beobSenderLaden);
            jPopupMenu.add(itemSenderLaden);
            //Url
            item = new JMenuItem("URL kopieren");
            item.addActionListener(beobUrl);
            jPopupMenu.add(item);
            //Drucken
            item = new JMenuItem("Tabelle Drucken");
            item.addActionListener(beobPrint);
            jPopupMenu.add(item);
            //Infos
            item = new JMenuItem("Infos anzeigen");
            item.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent actionEvent) {
                    if (!filmInfoHud.isVisible()) {
                        filmInfoHud.show();
                    }
                }
            });

            jPopupMenu.add(item);

            //anzeigen
            jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
        }

        private class BeobSenderLaden implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                senderLaden();
            }
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
                    DatenFilm film = Daten.listeFilme.getFilmByNr(tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(nr), DatenFilm.FILM_NR_NR).toString());
                    String thema = film.arr[DatenFilm.FILM_THEMA_NR];
                    //thema = tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(nr), DatenFilm.FILM_THEMA_NR).toString();
                    jComboBoxFilterThema.setSelectedIndex(0);
                    jComboBoxFilterThema.setSelectedItem(thema);
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
                    DatenFilm film = Daten.listeFilme.getFilmByNr(tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(nr), DatenFilm.FILM_NR_NR).toString());
                    String sen = film.arr[DatenFilm.FILM_SENDER_NR];
                    jComboBoxFilterSender.setSelectedIndex(0);
                    jComboBoxFilterSender.setSelectedItem(sen);
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
                    DatenFilm film = Daten.listeFilme.getFilmByNr(tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(nr), DatenFilm.FILM_NR_NR).toString());
                    String sen = film.arr[DatenFilm.FILM_SENDER_NR];
                    jComboBoxFilterSender.setSelectedIndex(0);
                    jComboBoxFilterSender.setSelectedItem(sen);
                    String thema = film.arr[DatenFilm.FILM_THEMA_NR];
                    jComboBoxFilterThema.setSelectedIndex(0);
                    jComboBoxFilterThema.setSelectedItem(thema);
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
                    DatenFilm film = Daten.listeFilme.getFilmByNr(tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(nr), DatenFilm.FILM_NR_NR).toString());
                    String sen = film.arr[DatenFilm.FILM_SENDER_NR];
                    jComboBoxFilterSender.setSelectedIndex(0);
                    jComboBoxFilterSender.setSelectedItem(sen);
                    String thema = film.arr[DatenFilm.FILM_THEMA_NR];
                    jComboBoxFilterThema.setSelectedIndex(0);
                    jComboBoxFilterThema.setSelectedItem(thema);
                    String tit = film.arr[DatenFilm.FILM_TITEL_NR];
                    jTextFieldFilterTitel.setText(tit);
                    stopBeob = false;
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

        private class BeobAbo implements ActionListener {

            boolean mitTitel = false;

            public BeobAbo(boolean mmitTitel) {
                mitTitel = mmitTitel;
            }

            @Override
            public void actionPerformed(ActionEvent e) {
                if (daten.listePset.getListeAbo().size() == 0) {
                    MVMessageDialog.showMessageDialog(parentComponent, "Im Menü unter \"Datei->Optionen->Videoplayer\" ein Programm zum Aufzeichnen festlegen.",
                            "kein Videoplayer!", JOptionPane.INFORMATION_MESSAGE);
                } else {
                    int nr = tabelle.rowAtPoint(p);
                    if (nr >= 0) {
                        stopBeob = true;
                        int selectedModelRow = tabelle.convertRowIndexToModel(nr);
                        //DatenFilm film = Daten.listeFilme.getFilmByUrl(tabelle.getModel().getValueAt(selectedModelRow, DatenFilm.FILM_URL_NR).toString());
                        DatenFilm film = Daten.listeFilme.getFilmByNr(tabelle.getModel().getValueAt(selectedModelRow, DatenFilm.FILM_NR_NR).toString());
                        DatenAbo datenAbo;
                        if (film != null) {
                            if ((datenAbo = daten.listeAbo.getAboFuerFilm_schnell(film, false /*ohne Länge*/)) != null) {
                                //gibts schon, dann löschen
                                daten.listeAbo.aboLoeschen(datenAbo);
                            } else {
                                //neues Abo anlegen
                                if (mitTitel) {
                                    daten.listeAbo.addAbo(film.arr[DatenFilm.FILM_SENDER_NR], film.arr[DatenFilm.FILM_THEMA_NR], film.arr[DatenFilm.FILM_TITEL_NR]);
                                } else {
                                    daten.listeAbo.addAbo(film.arr[DatenFilm.FILM_SENDER_NR], film.arr[DatenFilm.FILM_THEMA_NR], "");
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
                if (daten.listePset.getListeAbo().size() == 0) {
                    MVMessageDialog.showMessageDialog(parentComponent, "Im Menü unter \"Datei->Optionen->Videoplayer\" ein Programm zum Aufzeichnen festlegen.",
                            "kein Videoplayer!", JOptionPane.INFORMATION_MESSAGE);
                } else {
                    int nr = tabelle.rowAtPoint(p);
                    if (nr >= 0) {
                        stopBeob = true;
                        DatenFilm film = Daten.listeFilme.getFilmByNr(tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(nr), DatenFilm.FILM_NR_NR).toString());
                        String thema = film.arr[DatenFilm.FILM_THEMA_NR];
                        //neues Abo anlegen
                        //ddaten.listeAbo.addAbo(filmSender, filmThema, filmTitel);
                        daten.listeAbo.addAbo(jComboBoxFilterSender.getSelectedItem().toString(), jComboBoxFilterThema.getSelectedItem().toString(),
                                jTextFieldFilterTitel.getText(), jTextFieldFilterThemaTitel.getText(),
                                jTextFieldFilterIrgendwo.getText(), jSliderMinuten.getValue(), thema);
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
                    DatenFilm film = Daten.listeFilme.getFilmByNr(tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(nr), DatenFilm.FILM_NR_NR).toString());
                    String th = film.arr[DatenFilm.FILM_THEMA_NR];
                    String se = film.arr[DatenFilm.FILM_SENDER_NR];
                    // Blackliste für alle Fälle einschalten, notify kommt beim add()
                    Daten.system[Konstanten.SYSTEM_BLACKLIST_AUSGESCHALTET_NR] = Boolean.toString(false);
                    if (!sender) {
                        daten.listeBlacklist.add(new DatenBlacklist("", th, "" /*Titel*/, "" /*Thema-Titel*/));
                    } else if (!thema) {
                        daten.listeBlacklist.add(new DatenBlacklist(se, "", "" /*Titel*/, "" /*Thema-Titel*/));
                    } else {
                        daten.listeBlacklist.add(new DatenBlacklist(se, th, "" /*Titel*/, "" /*Thema-Titel*/));
                    }
                }
            }
        }
    }

    private class BeobAbstractAction extends AbstractAction {

        @Override
        public void actionPerformed(ActionEvent e) {
            filmAbspielen_();
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
            Filter.checkPattern1(jTextFieldFilterThemaTitel);
            Filter.checkPattern1(jTextFieldFilterTitel);
            if (Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_ECHTZEITSUCHE_NR])) {
                tabelleLaden();
            }
        }
    }
}
