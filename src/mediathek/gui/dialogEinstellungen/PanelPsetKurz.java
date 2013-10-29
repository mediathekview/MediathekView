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

import java.awt.Color;
import java.awt.Component;
import java.awt.FileDialog;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import com.jidesoft.utils.SystemInfo;
import java.awt.Dimension;
import mediathek.daten.Daten;
import mediathek.daten.DatenProg;
import mediathek.daten.DatenPset;
import mediathek.daten.ListePset;
import mediathek.gui.PanelVorlage;
import mediathek.res.GetIcon;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.Log;

public class PanelPsetKurz extends PanelVorlage {

    public boolean ok = false;
    public String zielPfad = "";
    private DatenPset pSet = null;
    private ListePset listePset;

    public PanelPsetKurz(Daten d, Component parentComponent, ListePset llistePset) {
        super(d, parentComponent);
        initComponents();
        jButtonZiel.setIcon(GetIcon.getIcon("fileopen_16.png"));
        listePset = llistePset;
        jListPset.setModel(new DefaultComboBoxModel<String>(listePset.getObjectDataCombo()));
        if (listePset.size() > 0) {
            jListPset.setSelectedIndex(0);
            init();
            jListPset.addListSelectionListener(new ListSelectionListener() {
                @Override
                public void valueChanged(ListSelectionEvent e) {
                    if (!stopBeob) {
                        stopBeob = true;
                        init();
                        stopBeob = false;
                    }
                }
            });
        }
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_LISTE_PSET, PanelPsetKurz.class.getSimpleName()) {
            @Override
            public void ping() {
                if (!stopBeob) {
                    stopBeob = true;
                    jListPset.setModel(new DefaultComboBoxModel<String>(listePset.getObjectDataCombo()));
                    if (listePset.size() > 0) {
                        jListPset.setSelectedIndex(0);
                    }
                    init();
                    stopBeob = false;
                }
            }
        });
        initBeob();
    }

    private void initBeob() {
        jTextFieldName.getDocument().addDocumentListener(new BeobDocName());
        jTextFieldZiel.getDocument().addDocumentListener(new BeobDoc(jTextFieldZiel, DatenPset.PROGRAMMSET_ZIEL_PFAD_NR));
        jButtonZiel.addActionListener(new ZielBeobachter(jTextFieldZiel, DatenPset.PROGRAMMSET_ZIEL_PFAD_NR));
    }

    private void init() {
        if (jListPset.getSelectedIndex() != -1) {
            pSet = listePset.get(jListPset.getSelectedIndex());
        } else {
            pSet = null;
        }
        if (pSet != null) {
            jTextFieldName.setText(pSet.arr[DatenPset.PROGRAMMSET_NAME_NR]);
            jTextArea1.setText(pSet.arr[DatenPset.PROGRAMMSET_BESCHREIBUNG_NR]);
            if (!pSet.istSpeichern() && pSet.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD_NR].equals("")) {
                jTextFieldZiel.setEditable(false);
                jButtonZiel.setEnabled(false);
            } else {
                jTextFieldZiel.setEditable(true);
                jButtonZiel.setEnabled(true);
                // Zielpfad muss gesetzt werden
                if (pSet.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD_NR].equals("")) {
                    pSet.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD_NR] = GuiFunktionen.getHomePath();
                }
            }
            jTextFieldZiel.setText(pSet.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD_NR]);
            extra();
        } else {
            jTextFieldName.setText("");
            jTextArea1.setText("");
            jTextFieldZiel.setText("");
            delExtra();
        }
    }

    private void delExtra() {
        jPanelExtra.removeAll();
        jPanelExtra.updateUI();
    }

    private void extra() {
        delExtra();
        GridBagLayout gridbag = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(4, 10, 4, 10);
        c.weightx = 1;
        c.weighty = 0;
        c.gridx = 0;
        c.gridy = 0;
        String name;
        jPanelExtra.setLayout(gridbag);
        for (int i = 0; i < pSet.getListeProg().size(); ++i) {
            DatenProg prog = pSet.getProg(i);
            name = "Programmpfad";
            JPanel panel = new JPanel();
            panel.setBorder(javax.swing.BorderFactory.createTitledBorder(javax.swing.BorderFactory.createLineBorder(new Color(80, 80, 80), 1), prog.arr[DatenProg.PROGRAMM_NAME_NR]));
            setFeld(panel, name, prog.arr, DatenProg.PROGRAMM_PROGRAMMPFAD_NR);
            gridbag.setConstraints(panel, c);
            jPanelExtra.add(panel);
            ++c.gridy;
        }
        c.weighty = 10;
        JLabel label = new JLabel();
        gridbag.setConstraints(label, c);
        jPanelExtra.add(label);
    }

    private void setFeld(JPanel panel, String name, String[] arr, int idx) {
        GridBagLayout gridbag = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
        panel.setLayout(gridbag);
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(4, 10, 4, 10);
        c.weightx = 1;
        c.weighty = 0;
        c.gridx = 0;
        c.gridy = 0;
        // Label
        c.gridx = 0;
        c.weightx = 0;
        c.gridwidth = 1;
        JLabel label = new JLabel(name + ": ");
        gridbag.setConstraints(label, c);
        panel.add(label);
        // Textfeld
        c.gridx = 1;
        c.weightx = 10;
        JTextField textField = new JTextField(arr[idx]);
        textField.getDocument().addDocumentListener(new BeobDoc(textField, arr, idx));
        gridbag.setConstraints(textField, c);
        panel.add(textField);
        // Button
        c.gridx = 2;
        c.weightx = 0;
        JButton button = new JButton();
        button.setIcon(GetIcon.getIcon("fileopen_16.png"));
        button.addActionListener(new ZielBeobachter(textField, arr, idx));
        gridbag.setConstraints(button, c);
        panel.add(button);
        // Dimension(int width, int height)
        int h = button.getPreferredSize().height;
        int w = textField.getPreferredSize().width;
        textField.setPreferredSize(new Dimension(w, h));
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.JPanel jPanel2 = new javax.swing.JPanel();
        javax.swing.JTextField jTextField1 = new javax.swing.JTextField();
        jPanelExtra = new javax.swing.JPanel();
        javax.swing.JPanel jPanel1 = new javax.swing.JPanel();
        javax.swing.JTextField jTextField2 = new javax.swing.JTextField();
        javax.swing.JLabel jLabel1 = new javax.swing.JLabel();
        javax.swing.JLabel jLabel2 = new javax.swing.JLabel();
        jTextFieldZiel = new javax.swing.JTextField();
        jTextFieldName = new javax.swing.JTextField();
        jButtonZiel = new javax.swing.JButton();
        javax.swing.JScrollPane jScrollPane2 = new javax.swing.JScrollPane();
        jTextArea1 = new javax.swing.JTextArea();
        javax.swing.JScrollPane jScrollPane1 = new javax.swing.JScrollPane();
        jListPset = new javax.swing.JList<String>();

        jPanel2.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 255)));

        jTextField1.setEditable(false);
        jTextField1.setBackground(new java.awt.Color(204, 204, 255));
        jTextField1.setHorizontalAlignment(javax.swing.JTextField.CENTER);
        jTextField1.setText("Programme");

        javax.swing.GroupLayout jPanelExtraLayout = new javax.swing.GroupLayout(jPanelExtra);
        jPanelExtra.setLayout(jPanelExtraLayout);
        jPanelExtraLayout.setHorizontalGroup(
            jPanelExtraLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 0, Short.MAX_VALUE)
        );
        jPanelExtraLayout.setVerticalGroup(
            jPanelExtraLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 260, Short.MAX_VALUE)
        );

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jTextField1, javax.swing.GroupLayout.DEFAULT_SIZE, 487, Short.MAX_VALUE)
                .addContainerGap())
            .addComponent(jPanelExtra, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jTextField1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jPanelExtra, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel1.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 255)));

        jTextField2.setEditable(false);
        jTextField2.setBackground(new java.awt.Color(204, 204, 255));
        jTextField2.setFont(new java.awt.Font("Dialog", 1, 14)); // NOI18N
        jTextField2.setHorizontalAlignment(javax.swing.JTextField.CENTER);
        jTextField2.setText("Set");

        jLabel1.setText("Set Name:");

        jLabel2.setText("Zielpfad:");

        jButtonZiel.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/fileopen_16.png"))); // NOI18N

        jScrollPane2.setBorder(javax.swing.BorderFactory.createLineBorder(javax.swing.UIManager.getDefaults().getColor("TextField.selectionBackground")));

        jTextArea1.setEditable(false);
        jTextArea1.setBackground(javax.swing.UIManager.getDefaults().getColor("TextField.inactiveBackground"));
        jTextArea1.setColumns(20);
        jTextArea1.setRows(4);
        jScrollPane2.setViewportView(jTextArea1);

        jScrollPane1.setViewportView(jListPset);

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPane2, javax.swing.GroupLayout.DEFAULT_SIZE, 487, Short.MAX_VALUE)
                    .addComponent(jTextField2)
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabel1)
                            .addComponent(jLabel2))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(jPanel1Layout.createSequentialGroup()
                                .addComponent(jTextFieldZiel)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jButtonZiel))
                            .addComponent(jTextFieldName)))
                    .addComponent(jScrollPane1))
                .addContainerGap())
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jTextField2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane1, javax.swing.GroupLayout.PREFERRED_SIZE, 87, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel1)
                    .addComponent(jTextFieldName, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jButtonZiel)
                    .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                        .addComponent(jLabel2)
                        .addComponent(jTextFieldZiel, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jScrollPane2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel1Layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonZiel, jTextFieldName, jTextFieldZiel});

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel2, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jPanel2, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonZiel;
    private javax.swing.JList<String> jListPset;
    private javax.swing.JPanel jPanelExtra;
    private javax.swing.JTextArea jTextArea1;
    private javax.swing.JTextField jTextFieldName;
    private javax.swing.JTextField jTextFieldZiel;
    // End of variables declaration//GEN-END:variables

    private class ZielBeobachter implements ActionListener {

        JTextField textField;
        String[] arr = null;
        int idx;

        public ZielBeobachter(JTextField tt, String[] aarr, int iidx) {
            textField = tt;
            arr = aarr; // Programmarray
            idx = iidx;
        }

        public ZielBeobachter(JTextField tt, int iidx) {
            // für den Zielpfad
            textField = tt;
            idx = iidx;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            if (pSet == null) {
                return;
            }
            //we can use native directory chooser on Mac...
            if (SystemInfo.isMacOSX()) {
                //we want to select a directory only, so temporarily change properties
                System.setProperty("apple.awt.fileDialogForDirectories", "true");
                FileDialog chooser = new FileDialog(daten.mediathekGui, "Film speichern");
                chooser.setVisible(true);
                if (chooser.getFile() != null) {
                    //A directory was selected, that means Cancel was not pressed
                    try {
                        textField.setText(chooser.getDirectory() + chooser.getFile());
                        if (arr == null) {
                            pSet.arr[idx] = textField.getText();
                        } else {
                            arr[idx] = textField.getText();
                        }
                    } catch (Exception ex) {
                        Log.fehlerMeldung(392847589, Log.FEHLER_ART_PROG, "DialogZielPset.ZielBeobachter", ex);
                    }
                }
                System.setProperty("apple.awt.fileDialogForDirectories", "false");
            } else {
                //use the cross-platform swing chooser
                int returnVal;
                JFileChooser chooser = new JFileChooser();
                chooser.setCurrentDirectory(new File(textField.getText()));
                chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
                returnVal = chooser.showOpenDialog(null);
                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    try {
                        textField.setText(chooser.getSelectedFile().getAbsolutePath());
                        if (arr == null) {
                            pSet.arr[idx] = textField.getText();
                        } else {
                            arr[idx] = textField.getText();
                        }
                    } catch (Exception ex) {
                        Log.fehlerMeldung(613986500, Log.FEHLER_ART_PROG, "DialogZielPset.ZielBeobachter", ex);
                    }
                }
            }
        }
    }

    private class BeobDoc implements DocumentListener {

        JTextField textField;
        int idx;
        String[] arr = null; // das Programmarray

        public BeobDoc(JTextField tt, String[] aarr, int iidx) {
            textField = tt;
            arr = aarr;
            idx = iidx;
        }

        public BeobDoc(JTextField tt, int iidx) {
            // für den Zielpfad            
            textField = tt;
            idx = iidx;
        }

        @Override
        public void insertUpdate(DocumentEvent e) {
            set();
        }

        @Override
        public void removeUpdate(DocumentEvent e) {
            set();
        }

        @Override
        public void changedUpdate(DocumentEvent e) {
            set();
        }

        private void set() {
            if (pSet == null) {
                return;
            }
            if (!stopBeob) {
                stopBeob = true;
                if (arr == null) {
                    pSet.arr[idx] = textField.getText();
                } else {
                    arr[idx] = textField.getText();
                }
                stopBeob = false;
            }
        }
    }

    private class BeobDocName implements DocumentListener {

        @Override
        public void insertUpdate(DocumentEvent e) {
            set();
        }

        @Override
        public void removeUpdate(DocumentEvent e) {
            set();
        }

        @Override
        public void changedUpdate(DocumentEvent e) {
            set();
        }

        private void set() {
            if (pSet == null) {
                return;
            }
            if (!stopBeob) {
                stopBeob = true;
                pSet.arr[ DatenPset.PROGRAMMSET_NAME_NR] = jTextFieldName.getText();
                int i = jListPset.getSelectedIndex();
                jListPset.setModel(new DefaultComboBoxModel<String>(listePset.getObjectDataCombo()));
                jListPset.setSelectedIndex(i);
                stopBeob = false;
            }
        }
    }
}
