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
package mediathek.gui.dialog;

import com.jidesoft.utils.SystemInfo;
import mSearch.tool.FilenameUtils;
import mSearch.tool.Log;
import mediathek.config.Icons;
import mediathek.config.MVColor;
import mediathek.config.MVConfig;
import mediathek.daten.DatenPset;
import mediathek.tool.EscBeenden;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.JTextComponent;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

@SuppressWarnings("serial")
public class DialogAddMoreDownload extends JDialog {
    public boolean addAll = false;
    public boolean cancel = false;
    public boolean info;
    public boolean subtitle;

    private JFrame parent = null;
    private final DatenPset pSet;
    private String orgPfad = "";

    public DialogAddMoreDownload(JFrame parent, DatenPset pSet) {
        super(parent, true);
        this.parent = parent;
        this.pSet = pSet;

        initComponents();
        setTitle("Alle Downloads starten");

        chkSubtitle.setSelected(Boolean.parseBoolean(pSet.arr[DatenPset.PROGRAMMSET_SUBTITLE]));
        subtitle = chkSubtitle.isSelected();
        chkSubtitle.addActionListener(l -> subtitle = chkSubtitle.isSelected());

        chkInfo.setSelected(Boolean.parseBoolean(pSet.arr[DatenPset.PROGRAMMSET_INFODATEI]));
        info = chkInfo.isSelected();
        chkInfo.addActionListener(l -> info = chkInfo.isSelected());

        chkStart.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_DIALOG_DOWNLOAD_D_STARTEN)));
        chkStart.addActionListener(e -> MVConfig.add(MVConfig.Configs.SYSTEM_DIALOG_DOWNLOAD_D_STARTEN, String.valueOf(chkStart.isSelected())));

        jCheckBoxPfadSpeichern.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_DIALOG_DOWNLOAD__LETZTEN_PFAD_ANZEIGEN)));
        jCheckBoxPfadSpeichern.addActionListener(e -> MVConfig.add(MVConfig.Configs.SYSTEM_DIALOG_DOWNLOAD__LETZTEN_PFAD_ANZEIGEN, Boolean.toString(jCheckBoxPfadSpeichern.isSelected())));

        btnChange.addActionListener(l -> beenden());
        btnOk.addActionListener(e -> {
            addAll = true;
            beenden();
        });
        btnCancel.addActionListener(l -> {
            cancel = true;
            beenden();
        });

        jButtonPath.setIcon(Icons.ICON_BUTTON_FILE_OPEN);
        jButtonDelPath.setIcon(Icons.ICON_BUTTON_DEL);
        jButtonPath.addActionListener(new ZielBeobachter());
        jButtonDelPath.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_DIALOG_DOWNLOAD__PFADE_ZUM_SPEICHERN, "");
            jComboBoxPath.setModel(new DefaultComboBoxModel<>(new String[]{pSet.getZielPfad()}));
        });
        DialogAddDownload.setModelPfad(pSet.getZielPfad(), jComboBoxPath);
        orgPfad = pSet.getZielPfad();
        ((JTextComponent) jComboBoxPath.getEditor().getEditorComponent()).setOpaque(true);
        ((JTextComponent) jComboBoxPath.getEditor().getEditorComponent()).getDocument().addDocumentListener(new DocumentListener() {

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
                String s = ((JTextComponent) jComboBoxPath.getEditor().getEditorComponent()).getText();
                if (!s.equals(FilenameUtils.checkDateiname(s, true /*pfad*/))) {
                    jComboBoxPath.getEditor().getEditorComponent().setBackground(MVColor.DOWNLOAD_FEHLER.color);
                } else {
                    jComboBoxPath.getEditor().getEditorComponent().setBackground(Color.WHITE);
                }
            }
        });
        new EscBeenden(this) {
            @Override
            public void beenden_() {
                beenden();
            }
        };
        this.pack();
    }

    public String getPath() {
        String path = jComboBoxPath.getModel().getSelectedItem().toString();
        if (path.isEmpty()) {
            path = pSet.getZielPfad();
        }
        return path;
    }

    private void beenden() {
        DialogAddDownload.saveComboPfad(jComboBoxPath, orgPfad);
        this.dispose();
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.JPanel jPanelExtra = new javax.swing.JPanel();
        javax.swing.JLabel jLabel1 = new javax.swing.JLabel();
        chkInfo = new javax.swing.JCheckBox();
        chkSubtitle = new javax.swing.JCheckBox();
        jComboBoxPath = new javax.swing.JComboBox<>();
        jButtonDelPath = new javax.swing.JButton();
        jButtonPath = new javax.swing.JButton();
        jCheckBoxPfadSpeichern = new javax.swing.JCheckBox();
        btnOk = new javax.swing.JButton();
        btnChange = new javax.swing.JButton();
        chkStart = new javax.swing.JCheckBox();
        btnCancel = new javax.swing.JButton();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);

        jPanelExtra.setBorder(javax.swing.BorderFactory.createEtchedBorder());

        jLabel1.setText("Speicherpfad:");

        chkInfo.setText("Infodatei anlegen: \"Filmname.txt\"");

        chkSubtitle.setText("Untertitel speichern: \"Filmname.xxx\"");

        jComboBoxPath.setEditable(true);
        jComboBoxPath.setModel(new javax.swing.DefaultComboBoxModel<>(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));

        jButtonDelPath.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-del.png"))); // NOI18N

        jButtonPath.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-file-open.png"))); // NOI18N

        jCheckBoxPfadSpeichern.setText("Zielpfad speichern");

        javax.swing.GroupLayout jPanelExtraLayout = new javax.swing.GroupLayout(jPanelExtra);
        jPanelExtra.setLayout(jPanelExtraLayout);
        jPanelExtraLayout.setHorizontalGroup(
            jPanelExtraLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelExtraLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabel1)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanelExtraLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanelExtraLayout.createSequentialGroup()
                        .addComponent(chkSubtitle)
                        .addGap(0, 0, Short.MAX_VALUE))
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanelExtraLayout.createSequentialGroup()
                        .addGroup(jPanelExtraLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                            .addGroup(jPanelExtraLayout.createSequentialGroup()
                                .addComponent(chkInfo)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                .addComponent(jCheckBoxPfadSpeichern))
                            .addGroup(jPanelExtraLayout.createSequentialGroup()
                                .addComponent(jComboBoxPath, 0, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jButtonPath)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jButtonDelPath)))
                        .addContainerGap())))
        );
        jPanelExtraLayout.setVerticalGroup(
            jPanelExtraLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelExtraLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelExtraLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel1)
                    .addComponent(jComboBoxPath, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jButtonDelPath)
                    .addComponent(jButtonPath))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanelExtraLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(chkInfo)
                    .addComponent(jCheckBoxPfadSpeichern))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(chkSubtitle)
                .addContainerGap(49, Short.MAX_VALUE))
        );

        btnOk.setText("OK");

        btnChange.setText("Ändern");

        chkStart.setText("Download sofort starten");

        btnCancel.setText("Abbrechen");

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanelExtra, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(chkStart)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 104, Short.MAX_VALUE)
                        .addComponent(btnChange)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(btnOk)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(btnCancel)))
                .addContainerGap())
        );

        layout.linkSize(javax.swing.SwingConstants.HORIZONTAL, new java.awt.Component[] {btnCancel, btnChange, btnOk});

        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanelExtra, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(btnOk)
                    .addComponent(btnChange)
                    .addComponent(chkStart)
                    .addComponent(btnCancel))
                .addContainerGap())
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton btnCancel;
    private javax.swing.JButton btnChange;
    private javax.swing.JButton btnOk;
    private javax.swing.JCheckBox chkInfo;
    private javax.swing.JCheckBox chkStart;
    private javax.swing.JCheckBox chkSubtitle;
    private javax.swing.JButton jButtonDelPath;
    private javax.swing.JButton jButtonPath;
    private javax.swing.JCheckBox jCheckBoxPfadSpeichern;
    private javax.swing.JComboBox<String> jComboBoxPath;
    // End of variables declaration//GEN-END:variables

    private class ZielBeobachter implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            //we can use native directory chooser on Mac...
            if (SystemInfo.isMacOSX()) {
                //we want to select a directory only, so temporarily change properties
                System.setProperty("apple.awt.fileDialogForDirectories", "true");
                FileDialog chooser = new FileDialog(parent, "Film speichern");
                chooser.setVisible(true);
                if (chooser.getFile() != null) {
                    //A directory was selected, that means Cancel was not pressed
                    try {
                        final String path = chooser.getDirectory() + chooser.getFile();
                        jComboBoxPath.addItem(path);
                        jComboBoxPath.setSelectedItem(path);
                    } catch (Exception ex) {
                        Log.errorLog(356871087, ex);
                    }
                }
                System.setProperty("apple.awt.fileDialogForDirectories", "false");
            } else {
                //use the cross-platform swing chooser
                int returnVal;
                JFileChooser chooser = new JFileChooser();
                if (!jComboBoxPath.getSelectedItem().toString().equals("")) {
                    chooser.setCurrentDirectory(new File(jComboBoxPath.getSelectedItem().toString()));
                }
                chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
                returnVal = chooser.showOpenDialog(null);
                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    try {
                        final String absolutePath = chooser.getSelectedFile().getAbsolutePath();
                        jComboBoxPath.addItem(absolutePath);
                        jComboBoxPath.setSelectedItem(absolutePath);
                    } catch (Exception ex) {
                        Log.errorLog(356871087, ex);
                    }
                }
            }
        }
    }

}
