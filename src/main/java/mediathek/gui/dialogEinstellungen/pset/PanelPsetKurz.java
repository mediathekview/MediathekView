/*
 * Copyright (c) 2025 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.gui.dialogEinstellungen.pset;

import mediathek.config.Daten;
import mediathek.daten.DatenProg;
import mediathek.daten.DatenPset;
import mediathek.daten.ListePset;
import mediathek.gui.PanelVorlage;
import mediathek.gui.messages.ProgramSetChangedEvent;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.MessageBus;
import mediathek.tool.SVGIconUtilities;
import mediathek.tool.TextCopyPasteHandler;
import net.engio.mbassy.listener.Handler;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

public class PanelPsetKurz extends PanelVorlage {
    private DatenPset pSet;
    private final ListePset listePset;
    private static final Logger logger = LogManager.getLogger();

    public PanelPsetKurz(Daten d, JFrame parentComponent, ListePset llistePset) {
        super(d, parentComponent);
        initComponents();
        listePset = llistePset;
        jListPset.setModel(new DefaultComboBoxModel<>(listePset.getObjectDataCombo()));
        if (!listePset.isEmpty()) {
            jListPset.setSelectedIndex(0);
            init();
            jListPset.addListSelectionListener(le -> {
                if (!stopBeob) {
                    stopBeob = true;
                    init();
                    stopBeob = false;
                }
            });
        }

        MessageBus.getMessageBus().subscribe(this);

        initBeob();
    }

    @Handler
    private void handleProgramSetChanged(ProgramSetChangedEvent e) {
        SwingUtilities.invokeLater(() -> {
            if (!stopBeob) {
                stopBeob = true;
                jListPset.setModel(new DefaultComboBoxModel<>(listePset.getObjectDataCombo()));
                if (!listePset.isEmpty()) {
                    jListPset.setSelectedIndex(0);
                }
                init();
                stopBeob = false;
            }
        });
    }

    private void initBeob() {
        jTextFieldName.getDocument().addDocumentListener(new DuplicatePsetNameCheckListener(jTextFieldName));
        jTextFieldName.getDocument().addDocumentListener(new BeobDocName());
        var handler = new TextCopyPasteHandler<>(jTextFieldName);
        jTextFieldName.setComponentPopupMenu(handler.getPopupMenu());

        jTextFieldZiel.getDocument().addDocumentListener(new BeobDoc(jTextFieldZiel, DatenPset.PROGRAMMSET_ZIEL_PFAD));
        handler = new TextCopyPasteHandler<>(jTextFieldZiel);
        jTextFieldZiel.setComponentPopupMenu(handler.getPopupMenu());

        jButtonZiel.addActionListener(new ZielBeobachter(jTextFieldZiel, DatenPset.PROGRAMMSET_ZIEL_PFAD));
    }

    private void init() {
        if (jListPset.getSelectedIndex() != -1) {
            pSet = listePset.get(jListPset.getSelectedIndex());
        } else {
            pSet = null;
        }
        if (pSet != null) {
            jTextFieldName.setText(pSet.getName());
            jTextArea1.setText(pSet.arr[DatenPset.PROGRAMMSET_BESCHREIBUNG]);
            if (!pSet.istSpeichern() && pSet.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD].isEmpty()) {
                jTextFieldZiel.setEditable(false);
                jButtonZiel.setEnabled(false);
            } else {
                jTextFieldZiel.setEditable(true);
                jButtonZiel.setEnabled(true);
                // Zielpfad muss gesetzt werden
                if (pSet.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD].isEmpty()) {
                    pSet.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD] = SystemUtils.USER_HOME;
                }
            }
            jTextFieldZiel.setText(pSet.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD]);
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
            panel.setBorder(javax.swing.BorderFactory.createTitledBorder(javax.swing.BorderFactory.createLineBorder(new Color(80, 80, 80), 1), prog.arr[DatenProg.PROGRAMM_NAME]));
            setFeld(panel, name, prog.arr);
            gridbag.setConstraints(panel, c);
            jPanelExtra.add(panel);
            ++c.gridy;
        }
        c.weighty = 10;
        JLabel label = new JLabel();
        gridbag.setConstraints(label, c);
        jPanelExtra.add(label);
    }

    private void setFeld(JPanel panel, String name, String[] arr) {
        GridBagLayout gridbag = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
        panel.setLayout(gridbag);
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(4, 10, 4, 10);
        c.weightx = 1;
        c.weighty = 0;
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
        JTextField textField = new JTextField(arr[DatenProg.PROGRAMM_PROGRAMMPFAD]);
        textField.getDocument().addDocumentListener(new BeobDoc(textField, arr, DatenProg.PROGRAMM_PROGRAMMPFAD));
        var handler = new TextCopyPasteHandler<>(textField);
        textField.setComponentPopupMenu(handler.getPopupMenu());
        gridbag.setConstraints(textField, c);
        panel.add(textField);
        // Button
        c.gridx = 2;
        c.weightx = 0;
        JButton button = new JButton();
        button.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/folder-open.svg"));
        button.addActionListener(new ZielBeobachter(textField, arr, DatenProg.PROGRAMM_PROGRAMMPFAD));
        button.setToolTipText("Programm auswählen");
        gridbag.setConstraints(button, c);
        panel.add(button);
        // Dimension(int width, int height)
        int h = button.getPreferredSize().height;
        int w = textField.getPreferredSize().width;
        textField.setPreferredSize(new Dimension(w, h));
    }

    private class ZielBeobachter implements ActionListener {

        final JTextField textField;
        String[] arr;
        final int idx;
        final boolean file;

        public ZielBeobachter(JTextField tt, String[] aarr, int iidx) {
            textField = tt;
            arr = aarr; // Programmarray
            idx = iidx;
            file = true;
        }

        public ZielBeobachter(JTextField tt, int iidx) {
            // für den Zielpfad
            textField = tt;
            idx = iidx;
            file = false;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            if (pSet == null) {
                return;
            }
            //we can use native directory chooser on Mac...
            if (SystemUtils.IS_OS_MAC_OSX) {
                //we want to select a directory only, so temporarily change properties
                if (!file) {
                    System.setProperty("apple.awt.fileDialogForDirectories", "true");
                }
                FileDialog chooser = new FileDialog(MediathekGui.ui(), "Film speichern");
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
                        logger.error(ex);
                    }
                }
                if (!file) {
                    System.setProperty("apple.awt.fileDialogForDirectories", "false");
                }
            } else {
                //use the cross-platform swing chooser
                int returnVal;
                JFileChooser chooser = new JFileChooser();
                chooser.setCurrentDirectory(new File(textField.getText()));
                chooser.setFileSelectionMode(file ? JFileChooser.FILES_ONLY : JFileChooser.DIRECTORIES_ONLY);
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
                        logger.error(ex);
                    }
                }
            }
        }
    }

    private class BeobDoc implements DocumentListener {

        private final JTextField textField;
        private final int idx;
        private String[] arr; // das Programmarray

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
                pSet.arr[DatenPset.PROGRAMMSET_NAME] = jTextFieldName.getText();
                int i = jListPset.getSelectedIndex();
                jListPset.setModel(new DefaultComboBoxModel<>(listePset.getObjectDataCombo()));
                jListPset.setSelectedIndex(i);
                stopBeob = false;
            }
        }
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.JPanel jPanel2 = new javax.swing.JPanel();
        jPanelExtra = new javax.swing.JPanel();
        javax.swing.JPanel jPanel1 = new javax.swing.JPanel();
        javax.swing.JLabel jLabel1 = new javax.swing.JLabel();
        javax.swing.JLabel jLabel2 = new javax.swing.JLabel();
        jTextFieldZiel = new javax.swing.JTextField();
        jTextFieldName = new javax.swing.JTextField();
        jButtonZiel = new javax.swing.JButton();
        javax.swing.JScrollPane jScrollPane2 = new javax.swing.JScrollPane();
        jTextArea1 = new javax.swing.JTextArea();
        javax.swing.JScrollPane jScrollPane1 = new javax.swing.JScrollPane();
        jListPset = new javax.swing.JList<>();

        jPanel2.setBorder(javax.swing.BorderFactory.createTitledBorder("<html><b>Programme</b></html>"));

        javax.swing.GroupLayout jPanelExtraLayout = new javax.swing.GroupLayout(jPanelExtra);
        jPanelExtra.setLayout(jPanelExtraLayout);
        jPanelExtraLayout.setHorizontalGroup(
            jPanelExtraLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 491, Short.MAX_VALUE)
        );
        jPanelExtraLayout.setVerticalGroup(
            jPanelExtraLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 277, Short.MAX_VALUE)
        );

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jPanelExtra, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanelExtra, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel1.setBorder(javax.swing.BorderFactory.createTitledBorder("<html><b>Set</b></html>"));

        jLabel1.setText("Set Name:");

        jLabel2.setText("Zielpfad:");

        jButtonZiel.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/folder-open.svg"));
        jButtonZiel.setToolTipText("Pfad auswählen");

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
                    .addComponent(jScrollPane2, javax.swing.GroupLayout.DEFAULT_SIZE, 479, Short.MAX_VALUE)
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
                .addComponent(jScrollPane1, javax.swing.GroupLayout.PREFERRED_SIZE, 116, javax.swing.GroupLayout.PREFERRED_SIZE)
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

        jPanel1Layout.linkSize(javax.swing.SwingConstants.VERTICAL, jButtonZiel, jTextFieldName, jTextFieldZiel);

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
}
