/*    
 *    MediathekView
 *    Copyright (C) 2008   W. Xaver
 *    W.Xaver[at]googlemail.com
 *    http://zdfmediathk.sourceforge.net/
 *    
 *    This program is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.gui.dialogEinstellungen;

import mSearch.filmeSuchen.ListenerFilmeLaden;
import mSearch.filmeSuchen.ListenerFilmeLadenEvent;
import mSearch.tool.Listener;
import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.config.MVConfig;
import mediathek.daten.DatenBlacklist;
import mediathek.file.GetFile;
import mediathek.gui.PanelVorlage;
import mediathek.gui.dialog.DialogHilfe;
import mediathek.tool.Filter;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.TModel;
import mediathek.tool.TextCopyPaste;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

@SuppressWarnings("serial")
public class PanelBlacklist extends PanelVorlage {
    public boolean ok = false;
    public String ziel;
    private final String name;
    private static final Color cGruen = new Color(0, 153, 51);
    private static final Color cRot = new Color(255, 0, 0);

    public PanelBlacklist(Daten d, JFrame parentComponent, String nname) {
        super(d, parentComponent);
        initComponents();
        name = nname;
        jButtonHilfe.setIcon(Icons.ICON_BUTTON_HELP);
        jButtonTabelleLoeschen.setIcon(Icons.ICON_BUTTON_DEL);
        init_();
        init();
        Listener.addListener(new Listener(Listener.EREIGNIS_BLACKLIST_GEAENDERT, name) {
            @Override
            public void ping() {
                init_();
            }
        });
        Listener.addListener(new Listener(Listener.EREIGNIS_BLACKLIST_START_GEAENDERT, name) {
            @Override
            public void ping() {
                jCheckBoxStart.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_START_ON)));
                setCheckBlacklist();
            }
        });
        Listener.addListener(new Listener(Listener.EREIGNIS_BLACKLIST_AUCH_FUER_ABOS, name) {
            @Override
            public void ping() {
                init_();
            }
        });
        daten.getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
            @Override
            public void fertig(ListenerFilmeLadenEvent event) {
                comboThemaLaden();
            }
        });
    }

    private void init_() {
        jCheckBoxAbo.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_AUCH_ABO)));
        jCheckBoxStart.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_START_ON)));
        jCheckBoxBlacklistEingeschaltet.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_ON)));

        setCheckBlacklist();
        jCheckBoxZukunftNichtAnzeigen.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_ZUKUNFT_NICHT_ANZEIGEN)));
        jCheckBoxGeo.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_GEO_NICHT_ANZEIGEN)));
        try {
            jSliderMinuten.setValue(Integer.parseInt(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_FILMLAENGE)));
        } catch (Exception ex) {
            jSliderMinuten.setValue(0);
            MVConfig.add(MVConfig.Configs.SYSTEM_BLACKLIST_FILMLAENGE, "0");
        }
        tabelleLaden();
    }

    private void init() {
        jTableBlacklist.addMouseListener(new BeobMausTabelle());
        jTableBlacklist.getSelectionModel().addListSelectionListener(new BeobachterTableSelect());
        jRadioButtonWhitelist.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_IST_WHITELIST)));
        jRadioButtonWhitelist.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_BLACKLIST_IST_WHITELIST, Boolean.toString(jRadioButtonWhitelist.isSelected()));
            notifyBlack();
        });
        jRadioButtonBlacklist.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_BLACKLIST_IST_WHITELIST, Boolean.toString(jRadioButtonWhitelist.isSelected()));
            notifyBlack();
        });
        jCheckBoxZukunftNichtAnzeigen.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_BLACKLIST_ZUKUNFT_NICHT_ANZEIGEN, Boolean.toString(jCheckBoxZukunftNichtAnzeigen.isSelected()));
            notifyBlack();
        });
        jCheckBoxGeo.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_BLACKLIST_GEO_NICHT_ANZEIGEN, Boolean.toString(jCheckBoxGeo.isSelected()));
            notifyBlack();
        });
        jCheckBoxAbo.addActionListener(e -> {
            setCheckBlacklist();
            MVConfig.add(MVConfig.Configs.SYSTEM_BLACKLIST_AUCH_ABO, Boolean.toString(jCheckBoxAbo.isSelected()));
            // bei den Downloads melden
            // damit die Änderungen im Eigenschaftendialog auch übernommen werden
            Listener.notify(Listener.EREIGNIS_BLACKLIST_AUCH_FUER_ABOS, name);
        });
        jCheckBoxStart.addActionListener(e -> {
            setCheckBlacklist();
            MVConfig.add(MVConfig.Configs.SYSTEM_BLACKLIST_START_ON, Boolean.toString(jCheckBoxStart.isSelected()));
            Listener.notify(Listener.EREIGNIS_BLACKLIST_START_GEAENDERT, name);
        });
        jCheckBoxBlacklistEingeschaltet.addActionListener(e -> {
            setCheckBlacklist();
            MVConfig.add(MVConfig.Configs.SYSTEM_BLACKLIST_ON, Boolean.toString(jCheckBoxBlacklistEingeschaltet.isSelected()));
            notifyBlack();
        });
        jButtonHinzufuegen.addActionListener(e -> {
            String se = jComboBoxSender.getSelectedItem().toString();
            String th = jComboBoxThema.getSelectedItem().toString();
            String ti = jTextFieldTitel.getText().trim();
            String thti = jTextFieldThemaTitel.getText().trim();
            if (!se.equals("") || !th.equals("") || !ti.equals("") || !thti.equals("")) {
                daten.getListeBlacklist().add(new DatenBlacklist(se, th, ti, thti));
                tabelleLaden();
            }
        });
        jButtonAendern.addActionListener(e -> {
            String se = jComboBoxSender.getSelectedItem().toString();
            String th = jComboBoxThema.getSelectedItem().toString();
            String ti = jTextFieldTitel.getText().trim();
            String thti = jTextFieldThemaTitel.getText().trim();
            if (!se.equals("") || !th.equals("") || !ti.equals("") || !thti.equals("")) {
                int selectedTableRow = jTableBlacklist.getSelectedRow();
                if (selectedTableRow >= 0) {
                    int row = jTableBlacklist.convertRowIndexToModel(selectedTableRow);
                    String delNr = jTableBlacklist.getModel().getValueAt(row, DatenBlacklist.BLACKLIST_NR).toString();
                    DatenBlacklist bl = daten.getListeBlacklist().get(delNr);
                    bl.arr[DatenBlacklist.BLACKLIST_SENDER] = se;
                    bl.arr[DatenBlacklist.BLACKLIST_THEMA] = th;
                    bl.arr[DatenBlacklist.BLACKLIST_TITEL] = ti;
                    bl.arr[DatenBlacklist.BLACKLIST_THEMA_TITEL] = thti;
                    tabelleLaden();
                    jTableBlacklist.addRowSelectionInterval(row, row);
                    notifyBlack();
                }
            }

        });
        jButtonHilfe.addActionListener(e -> new DialogHilfe(parentComponent, true, new GetFile().getHilfeSuchen(GetFile.PFAD_HILFETEXT_BLACKLIST)).setVisible(true));
        jButtonTabelleLoeschen.addActionListener(e -> {
            int ret = JOptionPane.showConfirmDialog(parentComponent, "Alle Einträge werden gelöscht.", "Löschen?", JOptionPane.YES_NO_OPTION);
            if (ret == JOptionPane.OK_OPTION) {
                daten.getListeBlacklist().clear();
                tabelleLaden();
            }
        });
        jComboBoxSender.addActionListener(e -> comboThemaLaden());
        jTextFieldTitel.getDocument().addDocumentListener(new BeobFilterTitelDoc());
        jTextFieldThemaTitel.getDocument().addDocumentListener(new BeobFilterTitelDoc());
        try {
            jSliderMinuten.setValue(Integer.parseInt(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_FILMLAENGE)));
        } catch (Exception ex) {
            jSliderMinuten.setValue(0);
            MVConfig.add(MVConfig.Configs.SYSTEM_BLACKLIST_FILMLAENGE, "0");
        }
        jTextFieldMinuten.setText(String.valueOf(jSliderMinuten.getValue()));
        if (jSliderMinuten.getValue() == 0) {
            jTextFieldMinuten.setText("alles");
        }
        jSliderMinuten.addChangeListener(e -> {
            jTextFieldMinuten.setText(String.valueOf(jSliderMinuten.getValue()));
            if (jSliderMinuten.getValue() == 0) {
                jTextFieldMinuten.setText("alles");
            }
            if (!jSliderMinuten.getValueIsAdjusting()) {
                MVConfig.add(MVConfig.Configs.SYSTEM_BLACKLIST_FILMLAENGE, String.valueOf(jSliderMinuten.getValue()));
                notifyBlack();
            }
        });
        initCombo();
        comboThemaLaden();
        jTextFieldThemaTitel.addMouseListener(new TextCopyPaste());
        jTextFieldTitel.addMouseListener(new TextCopyPaste());
    }

    private void setCheckBlacklist() {
        jCheckBoxBlacklistEingeschaltet.setForeground(jCheckBoxBlacklistEingeschaltet.isSelected() ? cGruen : cRot);
        jCheckBoxStart.setForeground(jCheckBoxStart.isSelected() ? cGruen : cRot);
        jCheckBoxAbo.setForeground(jCheckBoxAbo.isSelected() ? cGruen : cRot);
    }

    private void notifyBlack() {
        daten.getListeBlacklist().filterListe();
        Listener.notify(Listener.EREIGNIS_BLACKLIST_GEAENDERT, name);
    }

    private void comboThemaLaden() {
        String filterSender = jComboBoxSender.getSelectedItem().toString();
        if (filterSender.equals("")) {
            jComboBoxThema.setModel(new javax.swing.DefaultComboBoxModel<>(getThemen("")));
        } else {
            jComboBoxThema.setModel(new javax.swing.DefaultComboBoxModel<>(getThemen(filterSender)));
        }

    }

    private String[] getThemen(String ssender) {
        for (int i = 1; i < daten.getListeFilme().themenPerSender.length; ++i) {
            if (daten.getListeFilme().sender[i].equals(ssender)) {
                return daten.getListeFilme().themenPerSender[i];
            }
        }
        //return alleThemen;
        return daten.getListeFilme().themenPerSender[0];
    }

    private void initCombo() {
        // der erste Sender ist ""
        final String[] sender = GuiFunktionen.addLeerListe(daten.getFilmeLaden().getSenderNamen());
        jComboBoxSender.setModel(new javax.swing.DefaultComboBoxModel<>(sender));
    }

    private void tabelleLaden() {
        jTableBlacklist.setModel(new TModel(daten.getListeBlacklist().getObjectData(), DatenBlacklist.COLUMN_NAMES));
    }

    private void tableSelect() {
        DatenBlacklist bl = null;
        int selectedTableRow = jTableBlacklist.getSelectedRow();
        if (selectedTableRow >= 0) {
            int del = jTableBlacklist.convertRowIndexToModel(selectedTableRow);
            String delNr = jTableBlacklist.getModel().getValueAt(del, DatenBlacklist.BLACKLIST_NR).toString();
            bl = daten.getListeBlacklist().get(delNr);
        }
        if (bl != null) {
            jComboBoxSender.setSelectedItem(bl.arr[DatenBlacklist.BLACKLIST_SENDER]);
            jComboBoxThema.setSelectedItem(bl.arr[DatenBlacklist.BLACKLIST_THEMA]);
            jTextFieldTitel.setText(bl.arr[DatenBlacklist.BLACKLIST_TITEL]);
            jTextFieldThemaTitel.setText(bl.arr[DatenBlacklist.BLACKLIST_THEMA_TITEL]);
        }
    }

    private DatenBlacklist tabelleZeileLoeschen() {
        DatenBlacklist ret = null;
        int selectedTableRow = jTableBlacklist.getSelectedRow();
        if (selectedTableRow >= 0) {
            int del = jTableBlacklist.convertRowIndexToModel(selectedTableRow);
            String delNr = jTableBlacklist.getModel().getValueAt(del, DatenBlacklist.BLACKLIST_NR).toString();
            ret = daten.getListeBlacklist().remove(delNr);
            tabelleLaden();
        }
        return ret;
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.JPanel jPanel2 = new javax.swing.JPanel();
        javax.swing.ButtonGroup buttonGroup1 = new javax.swing.ButtonGroup();
        javax.swing.JTabbedPane jTabbedPaneBlacklist = new javax.swing.JTabbedPane();
        javax.swing.JPanel jPanel3 = new javax.swing.JPanel();
        javax.swing.JPanel jPanel5 = new javax.swing.JPanel();
        javax.swing.JLabel jLabel3 = new javax.swing.JLabel();
        jCheckBoxZukunftNichtAnzeigen = new javax.swing.JCheckBox();
        javax.swing.JPanel jPanel6 = new javax.swing.JPanel();
        jSliderMinuten = new javax.swing.JSlider();
        javax.swing.JLabel jLabel1 = new javax.swing.JLabel();
        jTextFieldMinuten = new javax.swing.JTextField();
        javax.swing.JLabel jLabel13 = new javax.swing.JLabel();
        javax.swing.JPanel jPanel7 = new javax.swing.JPanel();
        javax.swing.JLabel jLabel4 = new javax.swing.JLabel();
        jCheckBoxGeo = new javax.swing.JCheckBox();
        javax.swing.JLabel jLabel9 = new javax.swing.JLabel();
        javax.swing.JPanel jPanel1 = new javax.swing.JPanel();
        javax.swing.JScrollPane jScrollPane1 = new javax.swing.JScrollPane();
        jTableBlacklist = new javax.swing.JTable();
        javax.swing.JPanel jPanel4 = new javax.swing.JPanel();
        javax.swing.JLabel jLabel5 = new javax.swing.JLabel();
        jComboBoxSender = new javax.swing.JComboBox<>();
        javax.swing.JLabel jLabel6 = new javax.swing.JLabel();
        jComboBoxThema = new javax.swing.JComboBox<>();
        jButtonHinzufuegen = new javax.swing.JButton();
        javax.swing.JLabel jLabel7 = new javax.swing.JLabel();
        javax.swing.JLabel jLabel8 = new javax.swing.JLabel();
        jTextFieldTitel = new javax.swing.JTextField();
        javax.swing.JLabel jLabel2 = new javax.swing.JLabel();
        jTextFieldThemaTitel = new javax.swing.JTextField();
        jButtonAendern = new javax.swing.JButton();
        jRadioButtonBlacklist = new javax.swing.JRadioButton();
        jRadioButtonWhitelist = new javax.swing.JRadioButton();
        jButtonHilfe = new javax.swing.JButton();
        javax.swing.JLabel jLabel10 = new javax.swing.JLabel();
        jButtonTabelleLoeschen = new javax.swing.JButton();
        jCheckBoxBlacklistEingeschaltet = new javax.swing.JCheckBox();
        jCheckBoxAbo = new javax.swing.JCheckBox();
        jCheckBoxStart = new javax.swing.JCheckBox();

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 100, Short.MAX_VALUE)
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 100, Short.MAX_VALUE)
        );

        jPanel5.setBorder(javax.swing.BorderFactory.createEtchedBorder());

        jLabel3.setText("Filme, deren Datum in der Zukunft liegt, sind meist nur Trailer");

        jCheckBoxZukunftNichtAnzeigen.setText("Filme mit Datum in der Zukunft nicht anzeigen");

        javax.swing.GroupLayout jPanel5Layout = new javax.swing.GroupLayout(jPanel5);
        jPanel5.setLayout(jPanel5Layout);
        jPanel5Layout.setHorizontalGroup(
            jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel5Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jLabel3)
                    .addComponent(jCheckBoxZukunftNichtAnzeigen))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        jPanel5Layout.setVerticalGroup(
            jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel5Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabel3)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jCheckBoxZukunftNichtAnzeigen)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel6.setBorder(javax.swing.BorderFactory.createEtchedBorder());

        jSliderMinuten.setValue(0);

        jLabel1.setText("Nur Filme anzeigen mit einer Länge von mehr als [min]:");

        jTextFieldMinuten.setEditable(false);

        jLabel13.setText("Filme, die keine Längenangabe haben, werden immer angezeigt.");

        javax.swing.GroupLayout jPanel6Layout = new javax.swing.GroupLayout(jPanel6);
        jPanel6.setLayout(jPanel6Layout);
        jPanel6Layout.setHorizontalGroup(
            jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel6Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jSliderMinuten, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addGroup(jPanel6Layout.createSequentialGroup()
                        .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(jPanel6Layout.createSequentialGroup()
                                .addComponent(jLabel1)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jTextFieldMinuten, javax.swing.GroupLayout.PREFERRED_SIZE, 81, javax.swing.GroupLayout.PREFERRED_SIZE))
                            .addComponent(jLabel13))
                        .addGap(0, 230, Short.MAX_VALUE)))
                .addContainerGap())
        );
        jPanel6Layout.setVerticalGroup(
            jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel6Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel1)
                    .addComponent(jTextFieldMinuten, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jLabel13)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addComponent(jSliderMinuten, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap())
        );

        jPanel7.setBorder(javax.swing.BorderFactory.createEtchedBorder());

        jLabel4.setText("Geogeblockte Filme können im jeweiligen \"Ausland\" nicht abgerufen werden.");

        jCheckBoxGeo.setText("Filme, die per Geoblocking gesperrt sind, nicht anzeigen");

        jLabel9.setText("(Dazu muss die eigene Position in den Einstellungen angegeben werden)");

        javax.swing.GroupLayout jPanel7Layout = new javax.swing.GroupLayout(jPanel7);
        jPanel7.setLayout(jPanel7Layout);
        jPanel7Layout.setHorizontalGroup(
            jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel7Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel7Layout.createSequentialGroup()
                        .addGap(12, 12, 12)
                        .addComponent(jLabel9))
                    .addComponent(jLabel4)
                    .addComponent(jCheckBoxGeo))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        jPanel7Layout.setVerticalGroup(
            jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel7Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabel4)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jLabel9)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jCheckBoxGeo)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        javax.swing.GroupLayout jPanel3Layout = new javax.swing.GroupLayout(jPanel3);
        jPanel3.setLayout(jPanel3Layout);
        jPanel3Layout.setHorizontalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanel6, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel5, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel7, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        jPanel3Layout.setVerticalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel5, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel6, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel7, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(156, Short.MAX_VALUE))
        );

        jTabbedPaneBlacklist.addTab("Blacklist allgemein", jPanel3);

        jTableBlacklist.setAutoCreateRowSorter(true);
        jScrollPane1.setViewportView(jTableBlacklist);

        jPanel4.setBorder(javax.swing.BorderFactory.createEtchedBorder());

        jLabel5.setText("Sender:");

        jLabel6.setText("Thema:");

        jButtonHinzufuegen.setText("Hinzufügen");

        jLabel7.setText("Sender, Thema, Titel oder Thema/Titel:");

        jLabel8.setText("Titel:");

        jLabel2.setText("Thema oder Titel:");

        jButtonAendern.setText("Ändern");

        javax.swing.GroupLayout jPanel4Layout = new javax.swing.GroupLayout(jPanel4);
        jPanel4.setLayout(jPanel4Layout);
        jPanel4Layout.setHorizontalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel4Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel4Layout.createSequentialGroup()
                        .addComponent(jLabel7)
                        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                    .addGroup(jPanel4Layout.createSequentialGroup()
                        .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(jPanel4Layout.createSequentialGroup()
                                .addGap(68, 68, 68)
                                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                                    .addComponent(jLabel6)
                                    .addComponent(jLabel5))
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addComponent(jComboBoxSender, 0, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                    .addComponent(jComboBoxThema, 0, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)))
                            .addGroup(jPanel4Layout.createSequentialGroup()
                                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                                    .addComponent(jLabel2)
                                    .addComponent(jLabel8))
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addComponent(jTextFieldTitel)
                                    .addComponent(jTextFieldThemaTitel))))
                        .addGap(12, 12, 12))))
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel4Layout.createSequentialGroup()
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addComponent(jButtonAendern)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jButtonHinzufuegen)
                .addContainerGap())
        );

        jPanel4Layout.linkSize(javax.swing.SwingConstants.HORIZONTAL, new java.awt.Component[] {jButtonAendern, jButtonHinzufuegen});

        jPanel4Layout.setVerticalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel4Layout.createSequentialGroup()
                .addGap(23, 23, 23)
                .addComponent(jLabel7)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel5)
                    .addComponent(jComboBoxSender, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel6)
                    .addComponent(jComboBoxThema, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel8)
                    .addComponent(jTextFieldTitel, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel2)
                    .addComponent(jTextFieldThemaTitel, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonHinzufuegen)
                    .addComponent(jButtonAendern))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel4Layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jComboBoxSender, jComboBoxThema, jTextFieldThemaTitel, jTextFieldTitel});

        buttonGroup1.add(jRadioButtonBlacklist);
        jRadioButtonBlacklist.setSelected(true);
        jRadioButtonBlacklist.setText("\"Sender / Thema / Titel\" werden nicht angezeigt (Blacklist)");

        buttonGroup1.add(jRadioButtonWhitelist);
        jRadioButtonWhitelist.setText("nur diese \"Sender / Thema / Titel\" anzeigen (Whitelist)");

        jButtonHilfe.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-help.png"))); // NOI18N
        jButtonHilfe.setToolTipText("Hilfe anzeigen");

        jLabel10.setText("alle Einträge löschen:");

        jButtonTabelleLoeschen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-del.png"))); // NOI18N

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPane1, javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jPanel4, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jRadioButtonWhitelist)
                            .addComponent(jRadioButtonBlacklist))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 254, Short.MAX_VALUE)
                        .addComponent(jButtonHilfe))
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel1Layout.createSequentialGroup()
                        .addGap(0, 0, Short.MAX_VALUE)
                        .addComponent(jLabel10)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonTabelleLoeschen)))
                .addContainerGap())
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addComponent(jRadioButtonBlacklist)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jRadioButtonWhitelist))
                    .addComponent(jButtonHilfe))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 92, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jLabel10)
                    .addComponent(jButtonTabelleLoeschen))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jPanel4, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap())
        );

        jTabbedPaneBlacklist.addTab("Sender-Thema-Titel", jPanel1);

        jCheckBoxBlacklistEingeschaltet.setText("Blacklist im Tab Filme ist eingeschaltet");

        jCheckBoxAbo.setText("Die Blacklist beim Suchen der Abos berücksichtigen (sonst komplette Filmliste)");

        jCheckBoxStart.setText("Blacklist beim Programmstart einschalten");

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(layout.createSequentialGroup()
                        .addGap(21, 21, 21)
                        .addComponent(jCheckBoxStart)
                        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                    .addGroup(layout.createSequentialGroup()
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jTabbedPaneBlacklist)
                            .addGroup(layout.createSequentialGroup()
                                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addComponent(jCheckBoxAbo)
                                    .addComponent(jCheckBoxBlacklistEingeschaltet))
                                .addGap(0, 0, Short.MAX_VALUE)))
                        .addContainerGap())))
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jCheckBoxBlacklistEingeschaltet)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jCheckBoxStart)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jCheckBoxAbo)
                .addGap(18, 18, 18)
                .addComponent(jTabbedPaneBlacklist)
                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonAendern;
    private javax.swing.JButton jButtonHilfe;
    private javax.swing.JButton jButtonHinzufuegen;
    private javax.swing.JButton jButtonTabelleLoeschen;
    private javax.swing.JCheckBox jCheckBoxAbo;
    private javax.swing.JCheckBox jCheckBoxBlacklistEingeschaltet;
    private javax.swing.JCheckBox jCheckBoxGeo;
    private javax.swing.JCheckBox jCheckBoxStart;
    private javax.swing.JCheckBox jCheckBoxZukunftNichtAnzeigen;
    private javax.swing.JComboBox<String> jComboBoxSender;
    private javax.swing.JComboBox<String> jComboBoxThema;
    private javax.swing.JRadioButton jRadioButtonBlacklist;
    private javax.swing.JRadioButton jRadioButtonWhitelist;
    private javax.swing.JSlider jSliderMinuten;
    private javax.swing.JTable jTableBlacklist;
    private javax.swing.JTextField jTextFieldMinuten;
    private javax.swing.JTextField jTextFieldThemaTitel;
    private javax.swing.JTextField jTextFieldTitel;
    // End of variables declaration//GEN-END:variables

    private class BeobachterTableSelect implements ListSelectionListener {

        @Override
        public void valueChanged(ListSelectionEvent event) {
            if (!event.getValueIsAdjusting()) {
                tableSelect();
            }
        }
    }

    private class BeobMausTabelle extends MouseAdapter {

        //rechhte Maustaste in der Tabelle
        BeobLoeschen beobLoeschen = new BeobLoeschen();
        private Point p;

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
            int nr;
            p = evt.getPoint();
            nr = jTableBlacklist.rowAtPoint(p);
            if (nr >= 0) {
                jTableBlacklist.setRowSelectionInterval(nr, nr);
            }
            JPopupMenu jPopupMenu = new JPopupMenu();
            //löschen
            JMenuItem item = new JMenuItem("Zeile löschen");
            item.addActionListener(beobLoeschen);
            jPopupMenu.add(item);
            //anzeigen
            jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
        }

        private class BeobLoeschen implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                tabelleZeileLoeschen();
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
            Filter.checkPattern1(jTextFieldThemaTitel);
            Filter.checkPattern1(jTextFieldTitel);
        }
    }
}
