/*    
 *    MediathekView
 *    Copyright (C) 2012   W. Xaver
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

import com.jidesoft.utils.SystemInfo;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.FileDialog;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.net.URISyntaxException;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import mediathek.controller.Log;
import mediathek.daten.Daten;
import mediathek.daten.ListePset;
import mediathek.daten.ListePsetVorlagen;
import mediathek.gui.PanelVorlage;
import mediathek.res.GetIcon;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.GuiFunktionenProgramme;
import mediathek.tool.TModel;
import mediathek.tool.UrlHyperlinkAction;

public class PanelPsetImport extends PanelVorlage {

    ListePsetVorlagen listePsetVorlagen = new ListePsetVorlagen();

    public PanelPsetImport(Daten d, JFrame parentComponent) {
        super(d, parentComponent);
        initComponents();
        init();
    }

    private void init() {
        jButtonAktualisieren.setIcon(GetIcon.getProgramIcon("view-refresh_16.png"));
        jButtonPfad.setIcon(GetIcon.getProgramIcon("fileopen_16.png"));
        jComboBoxBs.setModel(new DefaultComboBoxModel<>(ListePsetVorlagen.BS));
        jComboBoxBs.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                tabelleLaden();
            }
        });
        jButtonImportDatei.setEnabled(false);
        jButtonImportText.setEnabled(false);
        jButtonPfad.addActionListener(new BeobPfad());
        jTextFieldDatei.getDocument().addDocumentListener(new BeobPfadDoc());
        jTextAreaImport.getDocument().addDocumentListener(new BeobTextArea());
        jButtonImportVorlage.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (!jTextFieldUrl.getText().equals("")) {
                    importDatei(jTextFieldUrl.getText());
                }
            }
        });
        jButtonImportDatei.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                importDatei(jTextFieldDatei.getText());
            }
        });
        jButtonImportText.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                importText();
            }
        });
        jButtonAktualisieren.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
                listePsetVorlagen.loadListOfSets();
                setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
                tabelleLaden();
            }
        });
        jTableVorlagen.getSelectionModel().addListSelectionListener(new BeobTableSelect());
        jTableVorlagen.setModel(new TModel(new Object[][]{}, ListePsetVorlagen.PGR_COLUMN_NAMES));
        if (!Daten.debug) {
            jTableVorlagen.getColumnModel().getColumn(jTableVorlagen.convertColumnIndexToView(ListePsetVorlagen.PGR_VERSION_NR)).setMinWidth(0);
            jTableVorlagen.getColumnModel().getColumn(jTableVorlagen.convertColumnIndexToView(ListePsetVorlagen.PGR_VERSION_NR)).setPreferredWidth(0);
            jTableVorlagen.getColumnModel().getColumn(jTableVorlagen.convertColumnIndexToView(ListePsetVorlagen.PGR_VERSION_NR)).setMaxWidth(0);
        }
        jButtonImportStandard.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                //GuiFunktionenProgramme.addVorlagen(ddaten, GuiFunktionenProgramme.getStandardprogramme(ddaten), false /* auto */);
                GuiFunktionenProgramme.addSetVorlagen(parentComponent, daten, ListePsetVorlagen.getStandarset(parentComponent, daten, true /*replaceMuster*/), false /*auto*/, true /*setVersion*/);
            }
        });

        jXHyperlinkInfos.setText("");
        try {
            jXHyperlinkInfos.setAction(new UrlHyperlinkAction(parentComponent, ""));
        } catch (URISyntaxException ignored) {
        }
        setHyperLink(""); // zum löschen
    }

    private void importDatei(String datei) {
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        ListePset listePset = ListePsetVorlagen.importPsetFile(parentComponent, datei, true);
        if (listePset != null) {
            // damit die Variablen ersetzt werden
            ListePset.progMusterErsetzen(parentComponent, listePset);
        }

        setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
        GuiFunktionenProgramme.addSetVorlagen(parentComponent, daten, listePset, false /*auto*/, false /*setVersion*/);
    }

    private void importText() {
        ListePset listePset = ListePsetVorlagen.importPsetText( daten, jTextAreaImport.getText(), true);
        if (listePset != null) {
            // damit die Variablen ersetzt werden
            ListePset.progMusterErsetzen(parentComponent, listePset);
        }
        GuiFunktionenProgramme.addSetVorlagen(parentComponent, daten, listePset, false /*auto*/, false /*setVersion*/);
    }

    private void tabelleLaden() {
        jTableVorlagen.setModel(listePsetVorlagen.getTModel(jComboBoxBs.getSelectedItem().toString()));
        if (!Daten.debug) {
            jTableVorlagen.getColumnModel().getColumn(jTableVorlagen.convertColumnIndexToView(ListePsetVorlagen.PGR_VERSION_NR)).setMinWidth(0);
            jTableVorlagen.getColumnModel().getColumn(jTableVorlagen.convertColumnIndexToView(ListePsetVorlagen.PGR_VERSION_NR)).setPreferredWidth(0);
            jTableVorlagen.getColumnModel().getColumn(jTableVorlagen.convertColumnIndexToView(ListePsetVorlagen.PGR_VERSION_NR)).setMaxWidth(0);
        }
    }

    private void table1Select() {
        String[] vorlage = new String[ListePsetVorlagen.PGR_MAX_ELEM];
        for (int i = 0; i < ListePsetVorlagen.PGR_MAX_ELEM; i++) {
            vorlage[i] = "";
        }
        int selectedTableRow = jTableVorlagen.getSelectedRow();
        if (selectedTableRow >= 0) {
            int selectedModelRow = jTableVorlagen.convertRowIndexToModel(selectedTableRow);
            for (int i = 0; i < ListePsetVorlagen.PGR_MAX_ELEM; ++i) {
                vorlage[i] = jTableVorlagen.getModel().getValueAt(selectedModelRow, i).toString();
            }
        }
        jTextFieldName.setText(vorlage[ListePsetVorlagen.PGR_NAME_NR]);
        jTextFieldBs.setText(vorlage[ListePsetVorlagen.PGR_BS_NR]);
        jTextFieldUrl.setText(vorlage[ListePsetVorlagen.PGR_URL_NR]);
        jTextAreaBeschreibung.setText(vorlage[ListePsetVorlagen.PGR_BESCHREIBUNG_NR]);
        setHyperLink(vorlage[ListePsetVorlagen.PGR_INFO_NR]);
    }

    private void setHyperLink(String uurl) {
        if (!uurl.equals("")) {
            jLabelInfo.setVisible(true);
            jXHyperlinkInfos.setVisible(true);
            jXHyperlinkInfos.setText(uurl);
        } else {
            jLabelInfo.setVisible(false);
            jXHyperlinkInfos.setVisible(false);
        }

    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.JTabbedPane jTabbedPane1 = new javax.swing.JTabbedPane();
        javax.swing.JPanel jPanel3 = new javax.swing.JPanel();
        javax.swing.JScrollPane jScrollPane2 = new javax.swing.JScrollPane();
        jTableVorlagen = new javax.swing.JTable();
        javax.swing.JPanel jPanel4 = new javax.swing.JPanel();
        javax.swing.JLabel jLabel1 = new javax.swing.JLabel();
        jTextFieldName = new javax.swing.JTextField();
        javax.swing.JLabel jLabel2 = new javax.swing.JLabel();
        jTextFieldUrl = new javax.swing.JTextField();
        javax.swing.JLabel jLabel3 = new javax.swing.JLabel();
        javax.swing.JScrollPane jScrollPane3 = new javax.swing.JScrollPane();
        jTextAreaBeschreibung = new javax.swing.JTextArea();
        jButtonImportVorlage = new javax.swing.JButton();
        javax.swing.JLabel jLabel6 = new javax.swing.JLabel();
        jTextFieldBs = new javax.swing.JTextField();
        jLabelInfo = new javax.swing.JLabel();
        jXHyperlinkInfos = new org.jdesktop.swingx.JXHyperlink();
        jButtonAktualisieren = new javax.swing.JButton();
        javax.swing.JLabel jLabel4 = new javax.swing.JLabel();
        javax.swing.JLabel jLabel5 = new javax.swing.JLabel();
        jComboBoxBs = new javax.swing.JComboBox<String>();
        javax.swing.JPanel jPanel5 = new javax.swing.JPanel();
        jButtonImportStandard = new javax.swing.JButton();
        javax.swing.JScrollPane jScrollPane4 = new javax.swing.JScrollPane();
        jTextArea1 = new javax.swing.JTextArea();
        jPanel1 = new javax.swing.JPanel();
        javax.swing.JPanel jPanel6 = new javax.swing.JPanel();
        jTextFieldDatei = new javax.swing.JTextField();
        jButtonPfad = new javax.swing.JButton();
        jButtonImportDatei = new javax.swing.JButton();
        javax.swing.JLabel jLabel7 = new javax.swing.JLabel();
        javax.swing.JPanel jPanel7 = new javax.swing.JPanel();
        jTextAreaImport = new javax.swing.JTextArea();
        jButtonImportText = new javax.swing.JButton();

        jTableVorlagen.setModel(new TModel());
        jScrollPane2.setViewportView(jTableVorlagen);

        jPanel4.setBorder(javax.swing.BorderFactory.createEtchedBorder());

        jLabel1.setText("Name:");

        jTextFieldName.setEditable(false);

        jLabel2.setText("URL:");

        jTextFieldUrl.setEditable(false);

        jLabel3.setText("Beschreibung:");

        jTextAreaBeschreibung.setEditable(false);
        jTextAreaBeschreibung.setColumns(20);
        jTextAreaBeschreibung.setRows(5);
        jScrollPane3.setViewportView(jTextAreaBeschreibung);

        jButtonImportVorlage.setText("Set importieren");

        jLabel6.setText("Betriebssystem:");

        jTextFieldBs.setEditable(false);

        jLabelInfo.setText("Infos:");

        jXHyperlinkInfos.setText("Info-Url");

        javax.swing.GroupLayout jPanel4Layout = new javax.swing.GroupLayout(jPanel4);
        jPanel4.setLayout(jPanel4Layout);
        jPanel4Layout.setHorizontalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel4Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel4Layout.createSequentialGroup()
                        .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabel1)
                            .addComponent(jLabel6))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jTextFieldBs)
                            .addComponent(jTextFieldName)))
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel4Layout.createSequentialGroup()
                        .addGap(0, 0, Short.MAX_VALUE)
                        .addComponent(jButtonImportVorlage))
                    .addGroup(jPanel4Layout.createSequentialGroup()
                        .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabel2)
                            .addComponent(jLabel3)
                            .addComponent(jLabelInfo))
                        .addGap(26, 26, 26)
                        .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jTextFieldUrl)
                            .addComponent(jScrollPane3)
                            .addComponent(jXHyperlinkInfos, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))))
                .addContainerGap())
        );
        jPanel4Layout.setVerticalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel4Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel1)
                    .addComponent(jTextFieldName, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel6)
                    .addComponent(jTextFieldBs, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel2)
                    .addComponent(jTextFieldUrl, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jLabel3)
                    .addComponent(jScrollPane3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jXHyperlinkInfos, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabelInfo))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jButtonImportVorlage)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel4Layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonImportVorlage, jTextFieldBs, jTextFieldName, jTextFieldUrl, jXHyperlinkInfos});

        jButtonAktualisieren.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/view-refresh_16.png"))); // NOI18N
        jButtonAktualisieren.setToolTipText("Neu laden");

        jLabel4.setText("Vorlagen von der Website laden:");

        jLabel5.setText("Betriebssystem:");

        javax.swing.GroupLayout jPanel3Layout = new javax.swing.GroupLayout(jPanel3);
        jPanel3.setLayout(jPanel3Layout);
        jPanel3Layout.setHorizontalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanel4, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jScrollPane2, javax.swing.GroupLayout.Alignment.TRAILING)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel3Layout.createSequentialGroup()
                        .addComponent(jLabel5)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jComboBoxBs, javax.swing.GroupLayout.PREFERRED_SIZE, 149, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jLabel4)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonAktualisieren)))
                .addContainerGap())
        );
        jPanel3Layout.setVerticalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addGap(7, 7, 7)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jLabel4)
                    .addComponent(jButtonAktualisieren)
                    .addComponent(jLabel5)
                    .addComponent(jComboBoxBs, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane2, javax.swing.GroupLayout.DEFAULT_SIZE, 183, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel4, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap())
        );

        jTabbedPane1.addTab("Setvorlagen", jPanel3);

        jButtonImportStandard.setText("Set anlegen");

        jTextArea1.setEditable(false);
        jTextArea1.setBackground(javax.swing.UIManager.getDefaults().getColor("Panel.background"));
        jTextArea1.setColumns(20);
        jTextArea1.setRows(5);
        jTextArea1.setText("Die Standardsets, die beim ersten Programmstart\nangelegt wurden, nochmals anlegen.\n\nDie bestehenden Sets bleiben unverändert erhalten,\nsollten jedoch vorher unter \"Set bearbeiten\" umbenannt\nwerden, z.B. von \"Speichern\" zu \"Speichern alt\".\n\nSoll aber der Ursprungszustand wieder hergestellt werden,\nmüssen vorher alle bestehenden Sets gelöscht werden.");
        jTextArea1.setMargin(new java.awt.Insets(5, 5, 5, 5));
        jScrollPane4.setViewportView(jTextArea1);

        javax.swing.GroupLayout jPanel5Layout = new javax.swing.GroupLayout(jPanel5);
        jPanel5.setLayout(jPanel5Layout);
        jPanel5Layout.setHorizontalGroup(
            jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel5Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPane4, javax.swing.GroupLayout.DEFAULT_SIZE, 584, Short.MAX_VALUE)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel5Layout.createSequentialGroup()
                        .addGap(0, 0, Short.MAX_VALUE)
                        .addComponent(jButtonImportStandard)))
                .addContainerGap())
        );
        jPanel5Layout.setVerticalGroup(
            jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel5Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jScrollPane4, javax.swing.GroupLayout.PREFERRED_SIZE, 172, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jButtonImportStandard)
                .addContainerGap(288, Short.MAX_VALUE))
        );

        jTabbedPane1.addTab("Standardset", jPanel5);

        jPanel6.setBorder(javax.swing.BorderFactory.createTitledBorder("Set aus Datei importieren"));

        jButtonPfad.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/fileopen_16.png"))); // NOI18N

        jButtonImportDatei.setText("Set importieren");

        jLabel7.setText("Datei/URL:");

        javax.swing.GroupLayout jPanel6Layout = new javax.swing.GroupLayout(jPanel6);
        jPanel6.setLayout(jPanel6Layout);
        jPanel6Layout.setHorizontalGroup(
            jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel6Layout.createSequentialGroup()
                .addContainerGap(12, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel6Layout.createSequentialGroup()
                        .addComponent(jLabel7)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldDatei)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonPfad))
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel6Layout.createSequentialGroup()
                        .addGap(0, 0, Short.MAX_VALUE)
                        .addComponent(jButtonImportDatei)))
                .addContainerGap())
        );
        jPanel6Layout.setVerticalGroup(
            jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel6Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jButtonPfad)
                    .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                        .addComponent(jTextFieldDatei, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addComponent(jLabel7)))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jButtonImportDatei)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel6Layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonPfad, jTextFieldDatei});

        jPanel7.setBorder(javax.swing.BorderFactory.createTitledBorder("Set als Text importieren"));

        jTextAreaImport.setColumns(20);
        jTextAreaImport.setRows(5);
        jTextAreaImport.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 153)));

        jButtonImportText.setText("Set importieren");

        javax.swing.GroupLayout jPanel7Layout = new javax.swing.GroupLayout(jPanel7);
        jPanel7.setLayout(jPanel7Layout);
        jPanel7Layout.setHorizontalGroup(
            jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel7Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jTextAreaImport, javax.swing.GroupLayout.DEFAULT_SIZE, 550, Short.MAX_VALUE)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel7Layout.createSequentialGroup()
                        .addGap(0, 0, Short.MAX_VALUE)
                        .addComponent(jButtonImportText)))
                .addContainerGap())
        );
        jPanel7Layout.setVerticalGroup(
            jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel7Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jTextAreaImport, javax.swing.GroupLayout.DEFAULT_SIZE, 275, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jButtonImportText)
                .addContainerGap())
        );

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jPanel7, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel6, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel6, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jPanel7, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );

        jTabbedPane1.addTab("Set importieren", jPanel1);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jTabbedPane1)
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jTabbedPane1)
                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonAktualisieren;
    private javax.swing.JButton jButtonImportDatei;
    private javax.swing.JButton jButtonImportStandard;
    private javax.swing.JButton jButtonImportText;
    private javax.swing.JButton jButtonImportVorlage;
    private javax.swing.JButton jButtonPfad;
    private javax.swing.JComboBox<String> jComboBoxBs;
    private javax.swing.JLabel jLabelInfo;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JTable jTableVorlagen;
    private javax.swing.JTextArea jTextArea1;
    private javax.swing.JTextArea jTextAreaBeschreibung;
    private javax.swing.JTextArea jTextAreaImport;
    private javax.swing.JTextField jTextFieldBs;
    private javax.swing.JTextField jTextFieldDatei;
    private javax.swing.JTextField jTextFieldName;
    private javax.swing.JTextField jTextFieldUrl;
    private org.jdesktop.swingx.JXHyperlink jXHyperlinkInfos;
    // End of variables declaration//GEN-END:variables

    private class BeobPfadDoc implements DocumentListener {

        @Override
        public void insertUpdate(DocumentEvent arg0) {
            eingabe();
        }

        @Override
        public void removeUpdate(DocumentEvent arg0) {
            eingabe();
        }

        @Override
        public void changedUpdate(DocumentEvent arg0) {
            eingabe();
        }

        private void eingabe() {
            jButtonImportDatei.setEnabled(!jTextFieldDatei.getText().equals(""));
            if (jTextFieldDatei.getText().equals("")) {
                jTextFieldDatei.setBackground(javax.swing.UIManager.getDefaults().getColor("TextField.background"));
            } else {
                if (ListePsetVorlagen.importPsetFile(parentComponent, jTextFieldDatei.getText(), false) != null) {
                    jTextFieldDatei.setBackground(javax.swing.UIManager.getDefaults().getColor("TextField.background"));
                } else {
                    jTextFieldDatei.setBackground(new Color(255, 200, 200));
                }
            }
        }
    }

    private class BeobTextArea implements DocumentListener {

        @Override
        public void insertUpdate(DocumentEvent arg0) {
            eingabe();
        }

        @Override
        public void removeUpdate(DocumentEvent arg0) {
            eingabe();
        }

        @Override
        public void changedUpdate(DocumentEvent arg0) {
            eingabe();
        }

        private void eingabe() {
            jButtonImportText.setEnabled(!jTextAreaImport.getText().equals(""));
            if (jTextAreaImport.getText().equals("")) {
                jTextAreaImport.setBackground(javax.swing.UIManager.getDefaults().getColor("TextArea.background"));
            } else {
                if (ListePsetVorlagen.importPsetText( daten, jTextAreaImport.getText(), false) != null) {
                    jTextAreaImport.setBackground(javax.swing.UIManager.getDefaults().getColor("TextArea.background"));
                    jButtonImportText.setEnabled(true);
                } else {
                    jTextAreaImport.setBackground(new Color(255, 200, 200));
                    jButtonImportText.setEnabled(false);
                }
            }
        }
    }

    private class BeobPfad implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            //we can use native chooser on Mac...
            if (SystemInfo.isMacOSX()) {
                FileDialog chooser = new FileDialog(daten.mediathekGui, "Programmset auswählen");
                chooser.setMode(FileDialog.LOAD);
                chooser.setVisible(true);
                if (chooser.getFile() != null) {
                    try {
                        jTextFieldDatei.setText(new File(chooser.getDirectory() + chooser.getFile()).getAbsolutePath());
                    } catch (Exception ex) {
                        Log.fehlerMeldung(989563047, ex);
                    }
                }
            } else {
                int returnVal;
                JFileChooser chooser = new JFileChooser();
                chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
                chooser.setFileHidingEnabled(false);
                if (jTextFieldDatei.getText().equals("")) {
                    chooser.setCurrentDirectory(new File(GuiFunktionen.getHomePath()));
                } else {
                    chooser.setCurrentDirectory(new File(jTextFieldDatei.getText()));
                }
                returnVal = chooser.showOpenDialog(null);
                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    try {
                        jTextFieldDatei.setText(chooser.getSelectedFile().getAbsolutePath());
                    } catch (Exception ex) {
                        Log.fehlerMeldung(925004992, ex);
                    }
                }
            }
        }
    }

    private class BeobTableSelect implements ListSelectionListener {

        public int selectedModelRow = -1;

        @Override
        public void valueChanged(ListSelectionEvent event) {
            if (!event.getValueIsAdjusting()) {
                table1Select();
            }
        }
    }
}
