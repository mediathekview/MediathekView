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

package mediathek.gui.dialog.add_download;

import mediathek.config.MVColor;
import mediathek.config.MVConfig;
import mediathek.daten.DatenPset;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.*;
import org.apache.commons.configuration2.Configuration;
import org.jdesktop.swingx.VerticalLayout;

import javax.swing.*;
import javax.swing.border.EtchedBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.JTextComponent;
import java.util.Objects;

public class DialogAddMoreDownload extends JDialog {
    private final DatenPset pSet;
    private final String orgPfad;
    private final Configuration config = ApplicationConfiguration.getConfiguration();
    private boolean addAll;
    private boolean cancel;
    private boolean info;
    private boolean subtitle;
    private boolean startImmediately;

    public DialogAddMoreDownload(JFrame parent, DatenPset pSet) {
        super(parent);
        this.pSet = pSet;

        initComponents();

        chkSubtitle.setSelected(Boolean.parseBoolean(pSet.arr[DatenPset.PROGRAMMSET_SUBTITLE]));
        subtitle = chkSubtitle.isSelected();
        chkSubtitle.addActionListener(_ -> subtitle = chkSubtitle.isSelected());

        chkInfo.setSelected(Boolean.parseBoolean(pSet.arr[DatenPset.PROGRAMMSET_INFODATEI]));
        info = chkInfo.isSelected();
        chkInfo.addActionListener(_ -> info = chkInfo.isSelected());

        jCheckBoxPfadSpeichern.setSelected(config.getBoolean(ApplicationConfiguration.DOWNLOAD_SHOW_LAST_USED_PATH, true));
        jCheckBoxPfadSpeichern.addActionListener(_ -> config.setProperty(ApplicationConfiguration.DOWNLOAD_SHOW_LAST_USED_PATH, jCheckBoxPfadSpeichern.isSelected()));

        btnChange.addActionListener(_ -> dispose());
        btnStartImmediately.addActionListener(_ -> {
            addAll = true;
            startImmediately = true;
            dispose();
        });
        btnQueueDownloads.addActionListener(_ -> {
            addAll = true;
            startImmediately = false;
            dispose();
        });
        btnCancel.addActionListener(_ -> {
            cancel = true;
            dispose();
        });

        jButtonPath.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/folder-open.svg"));
        jButtonPath.addActionListener(_ -> {
            var initialDirectory = "";
            var cbItem = Objects.requireNonNull(jComboBoxPath.getSelectedItem()).toString();
            if (!cbItem.isEmpty()) {
                initialDirectory = cbItem;
            }
            var selectedDirectory = FileDialogs.chooseDirectoryLocation(MediathekGui.ui(), "Film speichern", initialDirectory);
            if (selectedDirectory != null) {
                final String absolutePath = selectedDirectory.getAbsolutePath();
                jComboBoxPath.addItem(absolutePath);
                jComboBoxPath.setSelectedItem(absolutePath);
            }
        });


        jButtonDelPath.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/trash-can.svg"));
        jButtonDelPath.addActionListener(_ -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_DIALOG_DOWNLOAD__PFADE_ZUM_SPEICHERN, "");
            jComboBoxPath.setModel(new DefaultComboBoxModel<>(new String[]{pSet.getZielPfad()}));
        });

        DialogAddDownloadWithCoroutines.setModelPfad(pSet.getZielPfad(), jComboBoxPath);
        orgPfad = pSet.getZielPfad();
        ((JTextComponent) jComboBoxPath.getEditor().getEditorComponent()).setOpaque(true);
        ((JTextComponent) jComboBoxPath.getEditor().getEditorComponent()).getDocument().addDocumentListener(new IllegalFilenameListener());

        EscapeKeyHandler.installHandler(this, () -> {
            cancel = true;
            dispose();
        });

        getRootPane().setDefaultButton(btnStartImmediately);

        pack();
    }

    public boolean wasCancelled() {
        return cancel;
    }

    public DialogResult showDialog() {
        setVisible(true);
        return new DialogResult(addAll, info, subtitle, getPath(), startImmediately);
    }

    private String getPath() {
        String path = jComboBoxPath.getModel().getSelectedItem().toString();
        if (path.isEmpty()) {
            path = pSet.getZielPfad();
        }
        return path;
    }

    @Override
    public void dispose() {
        DialogAddDownloadWithCoroutines.saveComboPfad(jComboBoxPath, orgPfad);
        super.dispose();
    }

    public record DialogResult(boolean addAllWithDefaults, boolean info, boolean subtitle, String path, boolean startImmediately) {
    }

    private class IllegalFilenameListener implements DocumentListener {

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
            var editor = jComboBoxPath.getEditor().getEditorComponent();
            if (!s.equals(FilenameUtils.checkFilenameForIllegalCharacters(s, true))) {
                editor.setBackground(MVColor.DOWNLOAD_FEHLER.color);
            }
            else {
                editor.setBackground(UIManager.getDefaults().getColor("TextField.background"));
            }
        }
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    // Generated using JFormDesigner non-commercial license
    private void initComponents() {
        var jPanelExtra = new JPanel();
        var jLabel1 = new JLabel();
        jComboBoxPath = new JComboBox<>();
        jButtonDelPath = new JButton();
        jButtonPath = new JButton();
        var jPanel1 = new JPanel();
        chkInfo = new JCheckBox();
        jCheckBoxPfadSpeichern = new JCheckBox();
        chkSubtitle = new JCheckBox();
        btnChange = new JButton();
        btnCancel = new JButton();
        btnStartImmediately = new JButton();
        btnQueueDownloads = new JButton();

        //======== this ========
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Alle Downloads starten");
        setModal(true);
        setResizable(false);
        var contentPane = getContentPane();

        //======== jPanelExtra ========
        {
            jPanelExtra.setBorder(new EtchedBorder());

            //---- jLabel1 ----
            jLabel1.setText("Speicherpfad:");

            //---- jComboBoxPath ----
            jComboBoxPath.setEditable(true);
            jComboBoxPath.setModel(new DefaultComboBoxModel<>(new String[] {

            }));

            //---- jButtonDelPath ----
            jButtonDelPath.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-del.png")));
            jButtonDelPath.setToolTipText("History l\u00f6schen");

            //---- jButtonPath ----
            jButtonPath.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-file-open.png")));
            jButtonPath.setToolTipText("Zielpfad ausw\u00e4hlen");

            //======== jPanel1 ========
            {
                jPanel1.setLayout(new VerticalLayout(5));

                //---- chkInfo ----
                chkInfo.setText("Infodatei anlegen: \"Filmname.txt\"");
                jPanel1.add(chkInfo);

                //---- jCheckBoxPfadSpeichern ----
                jCheckBoxPfadSpeichern.setText("Zielpfad speichern");
                jPanel1.add(jCheckBoxPfadSpeichern);

                //---- chkSubtitle ----
                chkSubtitle.setText("Untertitel speichern: \"Filmname.xxx\"");
                jPanel1.add(chkSubtitle);
            }

            GroupLayout jPanelExtraLayout = new GroupLayout(jPanelExtra);
            jPanelExtra.setLayout(jPanelExtraLayout);
            jPanelExtraLayout.setHorizontalGroup(
                jPanelExtraLayout.createParallelGroup()
                    .addGroup(jPanelExtraLayout.createSequentialGroup()
                        .addContainerGap()
                        .addComponent(jLabel1)
                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanelExtraLayout.createParallelGroup()
                            .addGroup(jPanelExtraLayout.createSequentialGroup()
                                .addComponent(jComboBoxPath, GroupLayout.PREFERRED_SIZE, 450, GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jButtonPath)
                                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED, 107, Short.MAX_VALUE)
                                .addComponent(jButtonDelPath))
                            .addComponent(jPanel1, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                        .addContainerGap())
            );
            jPanelExtraLayout.setVerticalGroup(
                jPanelExtraLayout.createParallelGroup()
                    .addGroup(jPanelExtraLayout.createSequentialGroup()
                        .addContainerGap()
                        .addGroup(jPanelExtraLayout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                            .addComponent(jLabel1)
                            .addComponent(jComboBoxPath, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                            .addComponent(jButtonDelPath)
                            .addComponent(jButtonPath))
                        .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(jPanel1, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                        .addContainerGap(GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
            );
        }

        //---- btnChange ----
        btnChange.setText("Einzeln anlegen");
        btnChange.setToolTipText("Die Sammelaktion wird abgebrochen und Sie k\u00f6nnen im Anschlu\u00df die Einstellungen f\u00fcr jeden Download einzeln festlegen.");

        //---- btnCancel ----
        btnCancel.setText("Abbrechen");

        //---- btnStartImmediately ----
        btnStartImmediately.setText("Sofort starten");

        //---- btnQueueDownloads ----
        btnQueueDownloads.setText("In die Warteschlange");

        GroupLayout contentPaneLayout = new GroupLayout(contentPane);
        contentPane.setLayout(contentPaneLayout);
        contentPaneLayout.setHorizontalGroup(
            contentPaneLayout.createParallelGroup()
                .addGroup(contentPaneLayout.createSequentialGroup()
                    .addGroup(contentPaneLayout.createParallelGroup()
                        .addComponent(jPanelExtra, GroupLayout.Alignment.TRAILING, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addGroup(GroupLayout.Alignment.TRAILING, contentPaneLayout.createSequentialGroup()
                            .addContainerGap()
                            .addComponent(btnChange)
                            .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED, 177, Short.MAX_VALUE)
                            .addComponent(btnQueueDownloads)
                            .addGap(18, 18, 18)
                            .addComponent(btnStartImmediately)
                            .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                            .addComponent(btnCancel)))
                    .addContainerGap())
        );
        contentPaneLayout.setVerticalGroup(
            contentPaneLayout.createParallelGroup()
                .addGroup(contentPaneLayout.createSequentialGroup()
                    .addContainerGap()
                    .addComponent(jPanelExtra, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                    .addGroup(contentPaneLayout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                        .addComponent(btnCancel)
                        .addComponent(btnStartImmediately)
                        .addComponent(btnQueueDownloads)
                        .addComponent(btnChange))
                    .addContainerGap(7, Short.MAX_VALUE))
        );
        pack();
        setLocationRelativeTo(getOwner());
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    private JComboBox<String> jComboBoxPath;
    private JButton jButtonDelPath;
    private JButton jButtonPath;
    private JCheckBox chkInfo;
    private JCheckBox jCheckBoxPfadSpeichern;
    private JCheckBox chkSubtitle;
    private JButton btnChange;
    private JButton btnCancel;
    private JButton btnStartImmediately;
    private JButton btnQueueDownloads;
    // End of variables declaration//GEN-END:variables
}
