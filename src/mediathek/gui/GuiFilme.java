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

import java.awt.*;
import java.awt.event.*;
import java.awt.print.PrinterException;
import java.util.Iterator;
import java.util.LinkedList;
import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import mediathek.Daten;
import mediathek.Konstanten;
import mediathek.Log;
import mediathek.MediathekGui;
import mediathek.controller.filme.BeobFilmeLaden;
import mediathek.controller.filme.FilmListenerElement;
import mediathek.controller.filme.filmeImportieren.MediathekListener;
import mediathek.controller.io.starter.StartEvent;
import mediathek.controller.io.starter.StartListener;
import mediathek.controller.io.starter.Starts;
import mediathek.daten.*;
import mediathek.gui.beobachter.BeobMpanel;
import mediathek.gui.beobachter.CellRendererFilme;
import mediathek.tool.Datum;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.HinweisKeineAuswahl;
import mediathek.tool.TModelFilm;

public class GuiFilme extends PanelVorlage {

    private JButton buttonArray[];
    private final String[] COMBO_ZEIT = new String[]{"alles", "1 Tag", "2 Tage", "3 Tage", "4 Tage", "5 Tage", "10 Tage", "15 Tage", "20 Tage", "30 Tage"};
    public static final int[] COMBO_ZEIT_INT = {0, 1, 2, 3, 4, 5, 10, 15, 20, 30};

    /**
     * Creates new form GuiFeed
     *
     * @param d
     */
    public GuiFilme(DDaten d) {
        super(d);
        initComponents();
        init(); //alles einrichten, Beobachter anhängen
        extra();
        tabelleBauen(); //Filme laden
        GuiFunktionen.spaltenFilmSetzen(jTable1, false /* ziel */);
        ListePgruppe.addAdListener(new MediathekListener() {

            @Override
            public void ping(String className) {
                if (className.equals(ListePgruppe.class.getSimpleName())) {
                    extra();
                }
            }
        });
    }
    //===================================
    // Public
    //===================================

    @Override
    public void isShown() {
        super.isShown();
        ddaten.mediathekGui.setToolbar(MediathekGui.ButtonFilme);
        ddaten.infoPanel.setIdx(InfoPanel.IDX_GUI_FILME);
    }

    /**
     * Panel Neu laden
     */
    @Override
    public void neuLaden() {
        tabelleBauen();
    }

    public void filmAbspielen() {
        DatenPgruppe gruppe = ddaten.listePgruppe.getPgruppeAbspielen();
        if (gruppe != null) {
            open(gruppe);
        } else {
            JOptionPane.showMessageDialog(null, "unter \"Datei->Programmeinstellungen\" eine Programm zum Abspielen festlegen.",
                    "keine Standardbutton!", JOptionPane.INFORMATION_MESSAGE);
        }
    }

    public void filmSpeichern() {
        DatenFilm film;
        int selRow = jTable1.getSelectedRow();
        if (selRow >= 0) {
            selRow = jTable1.convertRowIndexToModel(selRow);
            film = DDaten.listeFilmeNachBlackList.getFilmByUrl(jTable1.getModel().getValueAt(selRow, DatenFilm.FILM_URL_NR).toString());
            ddaten.listeDownloads.addFilmDownload(film);
            ddaten.setGeaendertPanel();
        }
    }

    public void videoPlayerNeuLaden() {
        extra();
    }

    public void videoPlayerAnzeigen(boolean anzeigen) {
        jPanelExtra.setVisible(anzeigen);
    }
    //===================================
    // Private
    //===================================

    private void init() {
        checkBlacklist();
        jComboBoxZeitraum.setModel(new DefaultComboBoxModel(COMBO_ZEIT));
        try {
            jCheckBoxKeineAbos.setSelected(Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_FILTER_KEINE_ABO_NR]));
            jCheckBoxKeineGesehenen.setSelected(Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_FILTER_KEINE_GESEHENE_NR]));
            jComboBoxZeitraum.setSelectedIndex(Integer.parseInt(Daten.system[Konstanten.SYSTEM_FILTER_TAGE_NR]));
        } catch (Exception ex) {
            jComboBoxZeitraum.setSelectedIndex(6);
            Daten.system[Konstanten.SYSTEM_FILTER_TAGE_NR] = "6";
        }
        jComboBoxZeitraum.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.setGeaendert();
                Daten.system[Konstanten.SYSTEM_FILTER_TAGE_NR] = String.valueOf(jComboBoxZeitraum.getSelectedIndex());
                checkBlacklist();
                tabelleBauen();
            }
        });
        DDaten.filmeLaden.addAdListener(new BeobFilmeLaden() {

            @Override
            public void fertig(FilmListenerElement filmListenerElement) {
                DDaten.setGeaendert();
                DDaten.listeFilme = DDaten.filmeLaden.getListeFilme();
                checkBlacklist();
                tabelleBauen();
            }
        });
        //Tabelle einrichten
        ActionMap am = jTable1.getActionMap();
        am.put("film_starten", new BeobAbstractAction());
        InputMap im = jTable1.getInputMap();
        KeyStroke enter = KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0);
        im.put(enter, "film_starten");
        jTable1.setModel(new TModelFilm(new Object[][]{}, DatenFilm.FILME_COLUMN_NAMES));
        jTable1.addMouseListener(new BeobMausTabelle());
        jTable1.getSelectionModel().addListSelectionListener(new BeobachterTableSelect1());
        jTable1.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_OFF);
        jTable1.setDefaultRenderer(Object.class, new CellRendererFilme(ddaten));
        jTable1.setDefaultRenderer(Datum.class, new CellRendererFilme(ddaten));
        //beobachter Filter
        jToggleButtonLivestram.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                if (!stopBeob) {
                    stopBeob = true;
                    //auch die Filter löschen
                    jComboBoxFilterSender.setModel(new javax.swing.DefaultComboBoxModel(DDaten.listeFilmeNachBlackList.getModelOfField(DatenFilm.FILM_SENDER_NR, "", 0)));
                    jComboBoxFilterThema.setModel(new javax.swing.DefaultComboBoxModel(DDaten.listeFilmeNachBlackList.getModelOfField(DatenFilm.FILM_THEMA_NR, "", 0)));
                    jTextFieldFilterTitel.setText("");
                }
                tabelleBauen();
            }
        });
        //Combo Sender
        jButtonFilterLoeschen.addActionListener(new BeobFilterLoeschen());
        jComboBoxFilterSender.setModel(new javax.swing.DefaultComboBoxModel(DDaten.listeFilmeNachBlackList.getModelOfField(DatenFilm.FILM_SENDER_NR, "", 0)));
        jComboBoxFilterSender.addActionListener(new BeobFilterSender());
        jComboBoxFilterThema.setModel(new javax.swing.DefaultComboBoxModel(DDaten.listeFilmeNachBlackList.getModelOfField(DatenFilm.FILM_THEMA_NR, "", 0)));
        jComboBoxFilterThema.addActionListener(new BeobFilter());
        jTextFieldFilterTitel.addActionListener(new BeobFilter());
        jTextFieldFilterTitel.getDocument().addDocumentListener(new BeobFilterTitelDoc());
        jCheckBoxKeineAbos.addActionListener(new BeobFilter());
        jCheckBoxKeineGesehenen.addActionListener(new BeobFilter());
        //restliche Filter
        jScrollPane1.addMouseListener(new BeobMausLaufendeProgramme());
        ddaten.starterClass.addListener(new BeobStart());
        ddaten.infoPanel.addMouseListener(new BeobMausLaufendeProgramme());
        // Filter erst mal ausblenden
        jCheckBoxFilter.addActionListener(new BeobMpanel(jCheckBoxFilter, jPanelFilter, "Filter"));
        jCheckBoxProgamme.addActionListener(new BeobMpanel(jCheckBoxProgamme, jPanelExtra, "weitere Videoplayer"));
//        jPanelExtra.addMouseListener(new BeobMausProgramme());
    }

    // ############################################
    // Panel mit den Extra-Videoprogrammen
    // ############################################
    private void extra() {
        //erst sauber machen
        //zum Anlegen der Button:
        //Programmgruppe ohne Namen: Leerfeld
        //Programmgruppe ohen Programme: Label
        //sonst ein Button
        jPanelExtraInnen.removeAll();
        jPanelExtraInnen.updateUI();
        ListePgruppe listeButton = ddaten.listePgruppe.getListeButton();
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
        //zum zusammenschieben
        c.weightx = 10;
        c.gridx = maxSpalten + 1;
        c.gridy = 0;
        JLabel label = new JLabel();
        gridbag.setConstraints(label, c);
        jPanelExtraInnen.add(label);
    }

    private Component addExtraFeld(int i, int spalte, int zeile, GridBagLayout gridbag, GridBagConstraints c, JPanel panel, ListePgruppe liste) {
        Component ret;
        JButton button;
        c.gridx = spalte;
        c.gridy = zeile;
        if (liste.get(i).isLable()) {
            JLabel label = new JLabel(liste.get(i).arr[DatenPgruppe.PROGRAMMGRUPPE_NAME_NR]);
            Color col = liste.get(i).getFarbe(ddaten);
            if (col != null) {
                label.setForeground(col);
            }
            gridbag.setConstraints(label, c);
            panel.add(label);
            ret = label;
        } else {
            button = new JButton(liste.get(i).arr[DatenPgruppe.PROGRAMMGRUPPE_NAME_NR]);
            button.addActionListener(new BeobOpen(liste.get(i)));
            Color col = liste.get(i).getFarbe(ddaten);
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

    //####################################
    // Tabelle
    //####################################
    private synchronized void tabelleBauen() {
        try {
            boolean themaNichtDa = false;
            TModelFilm tModel = new TModelFilm(new Object[][]{}, DatenFilm.FILME_COLUMN_NAMES);
            stopBeob = true;
            getSpalten(jTable1);
            //jTable1.setModel(tModel);
            String thema = jComboBoxFilterThema.getSelectedItem().toString();
            String sender = jComboBoxFilterSender.getSelectedItem().toString();
            boolean themaOpen = jComboBoxFilterThema.isPopupVisible();
            boolean senderOpen = jComboBoxFilterSender.isPopupVisible();
            if (DDaten.listeFilmeNachBlackList.isEmpty()) {
                jComboBoxFilterSender.setModel(new javax.swing.DefaultComboBoxModel(DDaten.listeFilmeNachBlackList.getModelOfField(DatenFilm.FILM_SENDER_NR, "", 0)));
                jComboBoxFilterThema.setModel(new javax.swing.DefaultComboBoxModel(DDaten.listeFilmeNachBlackList.getModelOfField(DatenFilm.FILM_THEMA_NR, "", 0)));
                jComboBoxFilterSender.setSelectedIndex(0);
                jComboBoxFilterThema.setSelectedIndex(0);
            } else {
                //Filme neu laden
                listeInModellLaden(tModel);
                //Filter Sender
                jComboBoxFilterSender.setModel(new javax.swing.DefaultComboBoxModel(DDaten.listeFilmeNachBlackList.getModelOfField(DatenFilm.FILM_SENDER_NR, "", 0)));
                jComboBoxFilterSender.setSelectedIndex(0);
                if (!sender.equals("")) {
                    jComboBoxFilterSender.setSelectedItem(sender);
                    if (jComboBoxFilterSender.getSelectedIndex() == 0) {
                        // war wohl nix, der gewählte Sender wurde in die Blacklist eingetragen
                        sender = "";
                        listeInModellLaden(tModel);
                    }
                }
                jComboBoxFilterSender.setPopupVisible(senderOpen);
                // Filter Thema
                if (sender.equals("")) {
                    jComboBoxFilterThema.setModel(new javax.swing.DefaultComboBoxModel(
                            DDaten.listeFilmeNachBlackList.getModelOfField(DatenFilm.FILM_THEMA_NR, "", 0)));
                } else {
                    jComboBoxFilterThema.setModel(new javax.swing.DefaultComboBoxModel(
                            DDaten.listeFilmeNachBlackList.getModelOfField(DatenFilm.FILM_THEMA_NR, sender, DatenFilm.FILM_SENDER_NR)));
                }
                // wenn Thema bei dem Sender vorhanden, dann wieder setzen
                // ist wohl ein Bug beim Combo, klappt nur richtig wenn editable?!
                if (jComboBoxFilterThema.isEditable()) {
                    jComboBoxFilterThema.setSelectedItem(thema);
                } else {
                    jComboBoxFilterThema.setEditable(true);
                    jComboBoxFilterThema.setSelectedItem(thema);
                    jComboBoxFilterThema.setEditable(false);
                }
                if (!thema.equals("") && jComboBoxFilterThema.getSelectedIndex() == 0) {
                    // war wohl nix
                    themaNichtDa = true;
                }
                jComboBoxFilterThema.setPopupVisible(themaOpen);
                jTable1.setModel(tModel);
            }
            GuiFunktionen.spaltenFilmLoeschen(jTable1, false /* ziel */, true /* zeit */, true /* datei */);
            setInfo();
            setSpalten(jTable1);
            this.validate();
            stopBeob = false;
            //filtern
            if (themaNichtDa) {
                // nochmal filtern anschieben
                //jComboBoxFilterThema.setSelectedIndex(0);
                this.tabelleBauen();
            }
        } catch (Exception ex) {
            Log.fehlerMeldung("GuiFilme.tabelleBauen", ex);
        }
    }

    private void listeInModellLaden(TModelFilm tModel) {
        DDaten.listeFilmeNachBlackList.getModelTabFilme(ddaten, tModel, jComboBoxFilterSender.getSelectedItem().toString(),
                jComboBoxFilterThema.getSelectedItem().toString(),
                getArr(jTextFieldFilterTitel.getText()));
        if (tModel.getRowCount() > 0) {
            if (jCheckBoxKeineGesehenen.isSelected() || jCheckBoxKeineAbos.isSelected() || jToggleButtonLivestram.isSelected()) {
                tModel.filter(ddaten, jCheckBoxKeineAbos.isSelected(), jCheckBoxKeineGesehenen.isSelected(), jToggleButtonLivestram.isSelected());
            }
        }

    }

    private String[] getArr(String str) {
        LinkedList<String> liste = new LinkedList<String>();
        String[] s;
        s = str.split(" ");
        for (int i = 0; i < s.length; ++i) {
            if (!s[i].equals("")) {
                liste.add(s[i]);
            }
        }
        return liste.toArray(new String[0]);
    }

    private void filterLoeschen() {
        stopBeob = true;
        //ComboModels neu aufbauen
        jComboBoxFilterSender.setModel(new javax.swing.DefaultComboBoxModel(DDaten.listeFilmeNachBlackList.getModelOfField(DatenFilm.FILM_SENDER_NR, "", 0)));
        jComboBoxFilterThema.setModel(new javax.swing.DefaultComboBoxModel(DDaten.listeFilmeNachBlackList.getModelOfField(DatenFilm.FILM_THEMA_NR, "", 0)));
        jTextFieldFilterTitel.setText("");
        //neu laden
        tabelleBauen();
        stopBeob = false;
    }

    private void table1Select() {
        DatenFilm aktFilm = new DatenFilm();
        int selectedTableRow = jTable1.getSelectedRow();
        if (selectedTableRow >= 0) {
            int selectedModelRow = jTable1.convertRowIndexToModel(selectedTableRow);
            for (int i = 0; i < DatenFilm.FILME_MAX_ELEM; ++i) {
                aktFilm.arr[i] = jTable1.getModel().getValueAt(selectedModelRow, i).toString();
            }
            ddaten.dialogDatenFilm.setAktFilm(aktFilm);
        }
    }

    private void open(DatenPgruppe gruppe) {
        // Url mit Prognr. starten
        if (jTable1.getSelectedRow() == -1) {
            new HinweisKeineAuswahl().zeigen();
        } else {
            String url = "";
            DatenFilm ersterFilm = new DatenFilm();
            int selectedTableRows[] = jTable1.getSelectedRows();
            for (int l = selectedTableRows.length - 1; l >= 0; --l) {
                int selectedModelRow = jTable1.convertRowIndexToModel(selectedTableRows[l]);
                ersterFilm = DDaten.listeFilmeNachBlackList.getFilmByUrl(jTable1.getModel().getValueAt(selectedModelRow, DatenFilm.FILM_URL_NR).toString());
                // jede neue URL davorsetzen
                url = ersterFilm.arr[DatenFilm.FILM_URL_NR] + " " + url;
                // und in die History eintragen
                ddaten.history.add(ersterFilm.getUrlOrg());
            }
            ersterFilm.arr[DatenFilm.FILM_URL_NR] = url.trim();
            ddaten.starterClass.urlStarten(gruppe, ersterFilm);
            DDaten.setGeaendert();
        }
    }

    private void senderLaden() {
        //Mauskontext "Sender aktualisieren"
        int selectedTableRow = jTable1.getSelectedRow();
        if (selectedTableRow >= 0) {
            int sel = jTable1.convertRowIndexToModel(selectedTableRow);
            Daten.filmeLaden.updateSender(jTable1.getModel().getValueAt(sel, DatenFilm.FILM_SENDER_NR).toString(), Daten.listeFilme);
        }
    }

    private void checkPattern(JTextField tf) {
        String text = tf.getText();
        if (ListeFilme.isPattern(text)) {
            if (ListeFilme.makePattern(text) == null) {
                //soll Pattern sein, ist aber falsch
                tf.setBackground(Color.RED);
            } else {
                tf.setBackground(Color.PINK);
            }
        } else {
            tf.setBackground(Color.WHITE);
        }
    }

    private void checkBlacklist() {
        DDaten.listeFilmeNachBlackList = ddaten.listeBlacklist.filterListe(Daten.listeFilme);
    }

    private void setInfo() {
        String textLinks;
        // Text links: Zeilen Tabelle
        String leer = "   -   ";
        int gesamt = Daten.listeFilme.size();
        int anzListe = jTable1.getModel().getRowCount();
        int runs = ddaten.starterClass.getStarts(Starts.QUELLE_BUTTON).size();
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
            textLinks += ", (insgesamt: " + gesamt + " Filme)";
        }
        if (runs == 1) {
            textLinks += (leer + runs + "    laufender Film");
        } else if (runs > 1) {
            textLinks += (leer + runs + "    laufende Filme");
        }
        // Infopanel setzen
        ddaten.infoPanel.setTextLinks(InfoPanel.IDX_GUI_FILME, textLinks);
    }

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jCheckBox1 = new javax.swing.JCheckBox();
        jPanelFilter = new javax.swing.JPanel();
        jCheckBoxFilter = new javax.swing.JCheckBox();
        jPanelFilterInnen = new javax.swing.JPanel();
        jLabel2 = new javax.swing.JLabel();
        jComboBoxFilterSender = new javax.swing.JComboBox();
        jLabel3 = new javax.swing.JLabel();
        jComboBoxFilterThema = new javax.swing.JComboBox();
        jLabel5 = new javax.swing.JLabel();
        jTextFieldFilterTitel = new javax.swing.JTextField();
        jButtonFilterLoeschen = new javax.swing.JButton();
        jLabel1 = new javax.swing.JLabel();
        jComboBoxZeitraum = new javax.swing.JComboBox();
        jCheckBoxKeineGesehenen = new javax.swing.JCheckBox();
        jCheckBoxKeineAbos = new javax.swing.JCheckBox();
        jToggleButtonLivestram = new javax.swing.JToggleButton();
        jScrollPane1 = new javax.swing.JScrollPane();
        jTable1 = new javax.swing.JTable();
        jPanelExtra = new javax.swing.JPanel();
        jCheckBoxProgamme = new javax.swing.JCheckBox();
        jPanelExtraInnen = new javax.swing.JPanel();

        jCheckBox1.setText("jCheckBox1");

        jPanelFilter.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 153)));

        jCheckBoxFilter.setBackground(new java.awt.Color(217, 217, 217));
        jCheckBoxFilter.setFont(new java.awt.Font("Dialog", 1, 10)); // NOI18N
        jCheckBoxFilter.setText("Filter");

        jLabel2.setText("Sender:");

        jComboBoxFilterSender.setMaximumRowCount(25);

        jLabel3.setText("Thema:");

        jComboBoxFilterThema.setMaximumRowCount(25);

        jLabel5.setText("Titel/Thema:");

        jButtonFilterLoeschen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/del_16.png"))); // NOI18N
        jButtonFilterLoeschen.setToolTipText("Filter löschen");

        jLabel1.setText("Zeitraum:");

        jComboBoxZeitraum.setMaximumRowCount(10);
        jComboBoxZeitraum.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));

        jCheckBoxKeineGesehenen.setText("keine gesehenen");

        jCheckBoxKeineAbos.setText("keine Abos");

        jToggleButtonLivestram.setText("nur Livestreams");

        javax.swing.GroupLayout jPanelFilterInnenLayout = new javax.swing.GroupLayout(jPanelFilterInnen);
        jPanelFilterInnen.setLayout(jPanelFilterInnenLayout);
        jPanelFilterInnenLayout.setHorizontalGroup(
            jPanelFilterInnenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelFilterInnenLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelFilterInnenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jLabel1)
                    .addComponent(jLabel2))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanelFilterInnenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                    .addComponent(jComboBoxFilterSender, 0, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jComboBoxZeitraum, 0, 131, Short.MAX_VALUE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanelFilterInnenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanelFilterInnenLayout.createSequentialGroup()
                        .addComponent(jLabel3)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jComboBoxFilterThema, 0, 193, Short.MAX_VALUE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabel5)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldFilterTitel, javax.swing.GroupLayout.DEFAULT_SIZE, 166, Short.MAX_VALUE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonFilterLoeschen))
                    .addGroup(jPanelFilterInnenLayout.createSequentialGroup()
                        .addComponent(jCheckBoxKeineGesehenen)
                        .addGap(18, 18, 18)
                        .addComponent(jCheckBoxKeineAbos)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jToggleButtonLivestram)))
                .addContainerGap())
        );
        jPanelFilterInnenLayout.setVerticalGroup(
            jPanelFilterInnenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelFilterInnenLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelFilterInnenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jTextFieldFilterTitel, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jComboBoxFilterThema, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jComboBoxFilterSender, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel2)
                    .addComponent(jLabel3)
                    .addComponent(jLabel5)
                    .addComponent(jButtonFilterLoeschen, javax.swing.GroupLayout.PREFERRED_SIZE, 16, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanelFilterInnenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanelFilterInnenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                        .addComponent(jComboBoxZeitraum, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addComponent(jLabel1))
                    .addGroup(jPanelFilterInnenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                        .addComponent(jCheckBoxKeineGesehenen)
                        .addComponent(jCheckBoxKeineAbos)
                        .addComponent(jToggleButtonLivestram)))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanelFilterInnenLayout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonFilterLoeschen, jComboBoxFilterSender, jComboBoxFilterThema, jTextFieldFilterTitel});

        javax.swing.GroupLayout jPanelFilterLayout = new javax.swing.GroupLayout(jPanelFilter);
        jPanelFilter.setLayout(jPanelFilterLayout);
        jPanelFilterLayout.setHorizontalGroup(
            jPanelFilterLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jPanelFilterInnen, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
            .addComponent(jCheckBoxFilter, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        );
        jPanelFilterLayout.setVerticalGroup(
            jPanelFilterLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelFilterLayout.createSequentialGroup()
                .addComponent(jCheckBoxFilter)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanelFilterInnen, javax.swing.GroupLayout.DEFAULT_SIZE, 78, Short.MAX_VALUE)
                .addGap(6, 6, 6))
        );

        jTable1.setAutoCreateRowSorter(true);
        jScrollPane1.setViewportView(jTable1);

        jPanelExtra.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 153)));

        jCheckBoxProgamme.setBackground(new java.awt.Color(204, 204, 204));
        jCheckBoxProgamme.setFont(new java.awt.Font("Dialog", 1, 10)); // NOI18N
        jCheckBoxProgamme.setText("weitere Videoplayer");

        javax.swing.GroupLayout jPanelExtraInnenLayout = new javax.swing.GroupLayout(jPanelExtraInnen);
        jPanelExtraInnen.setLayout(jPanelExtraInnenLayout);
        jPanelExtraInnenLayout.setHorizontalGroup(
            jPanelExtraInnenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 0, Short.MAX_VALUE)
        );
        jPanelExtraInnenLayout.setVerticalGroup(
            jPanelExtraInnenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 106, Short.MAX_VALUE)
        );

        javax.swing.GroupLayout jPanelExtraLayout = new javax.swing.GroupLayout(jPanelExtra);
        jPanelExtra.setLayout(jPanelExtraLayout);
        jPanelExtraLayout.setHorizontalGroup(
            jPanelExtraLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jCheckBoxProgamme, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
            .addComponent(jPanelExtraInnen, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        );
        jPanelExtraLayout.setVerticalGroup(
            jPanelExtraLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelExtraLayout.createSequentialGroup()
                .addComponent(jCheckBoxProgamme)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanelExtraInnen, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanelFilter, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jScrollPane1)
                    .addComponent(jPanelExtra, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanelFilter, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 366, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanelExtra, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonFilterLoeschen;
    private javax.swing.JCheckBox jCheckBox1;
    private javax.swing.JCheckBox jCheckBoxFilter;
    private javax.swing.JCheckBox jCheckBoxKeineAbos;
    private javax.swing.JCheckBox jCheckBoxKeineGesehenen;
    private javax.swing.JCheckBox jCheckBoxProgamme;
    private javax.swing.JComboBox jComboBoxFilterSender;
    private javax.swing.JComboBox jComboBoxFilterThema;
    private javax.swing.JComboBox jComboBoxZeitraum;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JPanel jPanelExtra;
    private javax.swing.JPanel jPanelExtraInnen;
    private javax.swing.JPanel jPanelFilter;
    private javax.swing.JPanel jPanelFilterInnen;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTable jTable1;
    private javax.swing.JTextField jTextFieldFilterTitel;
    private javax.swing.JToggleButton jToggleButtonLivestram;
    // End of variables declaration//GEN-END:variables

    private class BeobOpen implements ActionListener {
        //ext. Programme starten

        DatenPgruppe gruppe;

        public BeobOpen(DatenPgruppe p) {
            gruppe = p;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            open(gruppe);
        }
    }

    private class BeobFilterSender implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            if (!stopBeob) {
                tabelleBauen();
            }
        }
    }

    private class BeobFilter implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            if (!stopBeob) {
                Daten.setGeaendert();
                Daten.system[Konstanten.SYSTEM_FILTER_KEINE_ABO_NR] = String.valueOf(jCheckBoxKeineAbos.isSelected());
                Daten.system[Konstanten.SYSTEM_FILTER_KEINE_GESEHENE_NR] = String.valueOf(jCheckBoxKeineGesehenen.isSelected());
                tabelleBauen();
            }
        }
    }

    private class BeobFilterLoeschen implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            filterLoeschen();
        }
    }

    private class BeobachterTableSelect1 implements ListSelectionListener {

        public int selectedModelRow = -1;

        @Override
        public void valueChanged(ListSelectionEvent event) {
            if (!event.getValueIsAdjusting()) {
                table1Select();
            }
        }
    }

    public class BeobMausLaufendeProgramme extends MouseAdapter {
        //rechhte Maustaste im Rahmen um die Tabelle

        @Override
        public void mouseClicked(MouseEvent arg0) {
            if (arg0.getButton() == MouseEvent.BUTTON3) {
                showMenu(arg0);
            }
        }

        private void showMenu(MouseEvent evt) {
            LinkedList<Starts> liste = ddaten.starterClass.getStarts(Starts.QUELLE_BUTTON);
            if (liste.size() > 0) {
                JPopupMenu jPopupMenu = new JPopupMenu();
                JMenuItem item;
                Iterator<Starts> it = liste.iterator();
                while (it.hasNext()) {
                    Starts s = it.next();
                    // dann läuft er noch
                    item = new JMenuItem("Beenden: [" + s.download.arr[DatenDownload.DOWNLOAD_SENDER_NR] + "]  " + s.download.arr[DatenDownload.DOWNLOAD_TITEL_NR]);
                    item.addActionListener(new BeobProgramm(s));
                    jPopupMenu.add(item);
                }
                //anzeigen
                jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
            }
        }

        private class BeobProgramm implements ActionListener {

            Starts s;

            public BeobProgramm(Starts ss) {
                s = ss;
            }

            @Override
            public void actionPerformed(ActionEvent e) {
                try {
                    if (s != null) {
                        s.stoppen = true;
                    }
                } catch (Exception ex) {
                    System.err.println("GuiFilme.BeobProgramm: " + ex.getMessage());
                }
            }
        }
    }

    public class BeobMausTabelle extends MouseAdapter {
        //rechhte Maustaste in der Tabelle

        private BeobUrl beobUrl = new BeobUrl();
        private BeobPrint beobPrint = new BeobPrint();
        private BeobInfo beobInfo = new BeobInfo();
        private BeobFilterLoeschen beobLoeschen = new BeobFilterLoeschen();
        private BeobAbo beobAbo = new BeobAbo(false /* mit Titel */);
        private BeobAbo beobAboMitTitel = new BeobAbo(true /* mit Titel */);
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
            if (arg0.getButton() == MouseEvent.BUTTON3) {
                showMenu(arg0);
            }
            if (arg0.getButton() == MouseEvent.BUTTON1) {
                if (arg0.getClickCount() > 1) {
                    filmAbspielen();
                }
            }
        }

        private void showMenu(MouseEvent evt) {
            p = evt.getPoint();
            int nr = jTable1.rowAtPoint(p);
            if (nr >= 0) {
                jTable1.setRowSelectionInterval(nr, nr);
            }
            String thema = jTable1.getModel().getValueAt(jTable1.convertRowIndexToModel(nr), DatenFilm.FILM_THEMA_NR).toString();
            String sender = jTable1.getModel().getValueAt(jTable1.convertRowIndexToModel(nr), DatenFilm.FILM_SENDER_NR).toString();
            String text = jTable1.getModel().getValueAt(jTable1.convertRowIndexToModel(nr), DatenFilm.FILM_TITEL_NR).toString();
            String url = jTable1.getModel().getValueAt(jTable1.convertRowIndexToModel(nr), DatenFilm.FILM_URL_NR).toString();
            JPopupMenu jPopupMenu = new JPopupMenu();

            //Thema laden
            JMenuItem item = new JMenuItem("Film starten");
            item.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/player_play_16.png")));
            item.addActionListener(new ActionListener() {

                @Override
                public void actionPerformed(ActionEvent e) {
                    filmAbspielen();
                }
            });
            jPopupMenu.add(item);
            //Url
            item = new JMenuItem("Film speichern");
            item.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/player_rec_16.png")));
            item.addActionListener(new ActionListener() {

                @Override
                public void actionPerformed(ActionEvent e) {
                    filmSpeichern();
                }
            });
            jPopupMenu.add(item);
            //##Trenner##
            jPopupMenu.addSeparator();
            //##Trenner##

            //Thema laden
            item = new JMenuItem("Sender aktualisieren");
            item.addActionListener(beobSenderLaden);
            jPopupMenu.add(item);
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
            item.addActionListener(beobInfo);
            jPopupMenu.add(item);
            //##Trenner##
            jPopupMenu.addSeparator();
            //##Trenner##
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
            //
            JMenu submenueAbo = new JMenu("Abo");
            jPopupMenu.add(submenueAbo);
            //Abo anlegen
            JMenuItem itemAboLoeschen;
            JMenuItem itemAbo;
            JMenuItem itemAboMitTitel;
            itemAboLoeschen = new JMenuItem("Abo Löschen");
            itemAbo = new JMenuItem("Abo mit Sender und Thema anlegen");
            itemAboMitTitel = new JMenuItem("Abo mit Sender und Thema und Titel anlegen");
            if ((ddaten.listeAbo.getAbo(sender, thema, text, url)) != null) {
                //gibts schon, dann löschen
                itemAbo.setEnabled(false);
                itemAboMitTitel.setEnabled(false);
                itemAboLoeschen.addActionListener(beobAbo);
            } else {
                itemAboLoeschen.setEnabled(false);
                //neues Abo anlegen
                itemAbo.addActionListener(beobAbo);
                itemAboMitTitel.addActionListener(beobAboMitTitel);
            }
            submenueAbo.add(itemAboLoeschen);
            submenueAbo.add(itemAbo);
            submenueAbo.add(itemAboMitTitel);
            //Programme einblenden
            JMenu submenue = new JMenu("Film mit Programm starten:");
            jPopupMenu.add(submenue);
            ListePgruppe liste = ddaten.listePgruppe.getListeButton();
            for (int i = 0; i < liste.size(); ++i) {
                DatenPgruppe pgruppe = liste.get(i);
                Color col = pgruppe.getFarbe(ddaten);
                item = new JMenuItem(pgruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_NAME_NR]);
                if (pgruppe.getListeProg().isEmpty()) {
                    if (col != null) {
                        item.setForeground(col);
                    }
                } else {
                    item.addActionListener(new BeobOpen(pgruppe));
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
            //anzeigen
            jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
        }

        private class BeobInfo implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                ddaten.dialogDatenFilm.setVis();
            }
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
                int nr = jTable1.rowAtPoint(p);
                if (nr >= 0) {
                    GuiFunktionen.copyToClipboard(
                            jTable1.getModel().getValueAt(jTable1.convertRowIndexToModel(nr),
                            DatenFilm.FILM_URL_NR).toString());
                }
            }
        }

        private class BeobPrint implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                try {
                    jTable1.print();
                } catch (PrinterException ex) {
                    Log.fehlerMeldung("GuiFilme.BeobPrint", ex);
                }
            }
        }

        private class BeobFilterThema implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                int nr = jTable1.rowAtPoint(p);
                if (nr >= 0) {
                    stopBeob = true;
                    String thema = "";
                    thema = jTable1.getModel().getValueAt(jTable1.convertRowIndexToModel(nr), DatenFilm.FILM_THEMA_NR).toString();
                    jComboBoxFilterThema.setSelectedIndex(0);
                    jComboBoxFilterThema.setSelectedItem(thema);
                    stopBeob = false;
                    tabelleBauen();
                }
            }
        }

        private class BeobFilterSender implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                int nr = jTable1.rowAtPoint(p);
                if (nr >= 0) {
                    stopBeob = true;
                    String sender;
                    sender = jTable1.getModel().getValueAt(jTable1.convertRowIndexToModel(nr), DatenFilm.FILM_SENDER_NR).toString();
                    jComboBoxFilterSender.setSelectedIndex(0);
                    jComboBoxFilterSender.setSelectedItem(sender);
                    stopBeob = false;
                    tabelleBauen();
                }
            }
        }

        private class BeobFilterSenderThema implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                int nr = jTable1.rowAtPoint(p);
                if (nr >= 0) {
                    stopBeob = true;
                    String sender = jTable1.getModel().getValueAt(jTable1.convertRowIndexToModel(nr), DatenFilm.FILM_SENDER_NR).toString();
                    jComboBoxFilterSender.setSelectedIndex(0);
                    jComboBoxFilterSender.setSelectedItem(sender);
                    String thema = jTable1.getModel().getValueAt(jTable1.convertRowIndexToModel(nr), DatenFilm.FILM_THEMA_NR).toString();
                    jComboBoxFilterThema.setSelectedIndex(0);
                    jComboBoxFilterThema.setSelectedItem(thema);
                    stopBeob = false;
                    tabelleBauen();
                }
            }
        }

        private class BeobFilterSenderThemaTitel implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                int nr = jTable1.rowAtPoint(p);
                if (nr >= 0) {
                    stopBeob = true;
                    String sender = jTable1.getModel().getValueAt(jTable1.convertRowIndexToModel(nr), DatenFilm.FILM_SENDER_NR).toString();
                    jComboBoxFilterSender.setSelectedIndex(0);
                    jComboBoxFilterSender.setSelectedItem(sender);
                    String thema = jTable1.getModel().getValueAt(jTable1.convertRowIndexToModel(nr), DatenFilm.FILM_THEMA_NR).toString();
                    jComboBoxFilterThema.setSelectedIndex(0);
                    jComboBoxFilterThema.setSelectedItem(thema);
                    String titel = jTable1.getModel().getValueAt(jTable1.convertRowIndexToModel(nr), DatenFilm.FILM_TITEL_NR).toString();
                    jTextFieldFilterTitel.setText(titel);
                    stopBeob = false;
                    tabelleBauen();
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
                int nr = jTable1.rowAtPoint(p);
                if (nr >= 0) {
                    stopBeob = true;
                    String thema = jTable1.getModel().getValueAt(jTable1.convertRowIndexToModel(nr), DatenFilm.FILM_THEMA_NR).toString();
                    String sender = jTable1.getModel().getValueAt(jTable1.convertRowIndexToModel(nr), DatenFilm.FILM_SENDER_NR).toString();
                    String titel = jTable1.getModel().getValueAt(jTable1.convertRowIndexToModel(nr), DatenFilm.FILM_TITEL_NR).toString();
                    String url = jTable1.getModel().getValueAt(jTable1.convertRowIndexToModel(nr), DatenFilm.FILM_URL_NR).toString();
                    DatenAbo datenAbo;
                    if ((datenAbo = ddaten.listeAbo.getAbo(sender, thema, titel, url)) != null) {
                        //gibts schon, dann löschen
                        ddaten.listeAbo.aboLoeschen(datenAbo);
                    } else {
                        //neues Abo anlegen
                        if (mitTitel) {
                            ddaten.listeAbo.addAbo(sender, thema, true, titel);
                        } else {
                            ddaten.listeAbo.addAbo(sender, thema, true, "");
                        }
                    }
                    ddaten.setGeaendertPanel();
                    stopBeob = false;
                    tabelleBauen();
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
                int nr = jTable1.rowAtPoint(p);
                if (nr >= 0) {
                    String th = jTable1.getModel().getValueAt(jTable1.convertRowIndexToModel(nr), DatenFilm.FILM_THEMA_NR).toString();
                    String se = jTable1.getModel().getValueAt(jTable1.convertRowIndexToModel(nr), DatenFilm.FILM_SENDER_NR).toString();
                    if (!sender) {
                        ddaten.listeBlacklist.add(new DatenBlacklist("", th));
                    } else if (!thema) {
                        ddaten.listeBlacklist.add(new DatenBlacklist(se, ""));
                    } else {
                        ddaten.listeBlacklist.add(new DatenBlacklist(se, th));
                    }
                    checkBlacklist();
                    tabelleBauen();
                    DDaten.setGeaendert();
                }
            }
        }
    }

    private class BeobAbstractAction extends AbstractAction {

        @Override
        public void actionPerformed(ActionEvent e) {
            filmAbspielen();
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
            checkPattern(jTextFieldFilterTitel);
            if (Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_ECHTZEITSUCHE_NR])) {
                tabelleBauen();
            }
        }
    }

    private class BeobStart implements StartListener {

        @Override
        public void starter(StartEvent ev) {
            setInfo();
            jTable1.updateUI();
        }
    }
//    private class BeobMausProgramme extends MouseAdapter {
//
//        JMenuItem itemAusblenden = new JMenuItem("ausblenden");
//
//        public BeobMausProgramme() {
//            itemAusblenden.setSelected(true);
//        }
//
//        @Override
//        public void mouseClicked(MouseEvent arg0) {
//            if (arg0.getButton() == MouseEvent.BUTTON3) {
//                showMenu(arg0);
//            }
//        }
//
//        private void showMenu(MouseEvent evt) {
//            JPopupMenu jPopupMenu = new JPopupMenu();
//            itemAusblenden.addActionListener(new ActionListener() {
//
//                @Override
//                public void actionPerformed(ActionEvent e) {
//                    Daten.setGeaendert();
//                    Daten.system[Konstanten.SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN_NR] = Boolean.FALSE.toString();
//                    ddaten.guiFilme.videoPlayerAnzeigen(false);
//                }
//            });
//            jPopupMenu.add(itemAusblenden);
//            jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
//        }
//    }
}
