package mediathek.gui.dialogEinstellungen;

import mediathek.MediathekGui;
import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.controller.IoXmlLesen;
import mediathek.tool.Log;
import mediathek.tool.MVMessageDialog;
import mediathek.tool.TextCopyPasteHandler;
import org.apache.commons.lang3.SystemUtils;
import org.apache.commons.lang3.tuple.ImmutableTriple;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.nio.file.Path;

@SuppressWarnings("serial")
public class PanelImport extends JPanel {
    private final JFrame parentComponent;

    public PanelImport(JFrame parentComponent) {
        super();
        this.parentComponent = parentComponent;
        initComponents();
        init();
    }

    private void init() {
        jButtonPfad.setIcon(Icons.ICON_BUTTON_FILE_OPEN);
        jButtonImportDatei.setEnabled(false);
        jButtonPfad.addActionListener(new BeobPfad());
        jTextFieldDatei.getDocument().addDocumentListener(new BeobPfadDoc());
        jButtonImportDatei.addActionListener(e -> importDatei(jTextFieldDatei.getText()));
        jCheckBoxAbo.addActionListener(e -> setButtonImport());
        jCheckBoxBlack.addActionListener(e -> setButtonImport());
        jCheckBoxErsetzungstabelle.addActionListener(e -> setButtonImport());
        final Path xmlFilePath = Daten.getMediathekXmlFilePath();
        jTextFieldPfadKonfig.setText(xmlFilePath.toAbsolutePath().toString());

        var handler = new TextCopyPasteHandler<>(jTextFieldDatei);
        jTextFieldDatei.setComponentPopupMenu(handler.getPopupMenu());
    }

    private void importDatei(String datei) {
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        final IoXmlLesen configReader = new IoXmlLesen();
        final ImmutableTriple<Integer, Integer, Integer> result = configReader.importAboBlacklist(datei, jCheckBoxAbo.isSelected(), jCheckBoxBlack.isSelected(), jCheckBoxErsetzungstabelle.isSelected());
        String text = "Es wurden\n"
                + result.left + " Abos und\n"
                + result.middle + " Blacklisteinträge\n"
                + result.right + " Ersetzungen\n"
                + "hinzugefügt";
        setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
        MVMessageDialog.showMessageDialog(parentComponent, text, "Import", JOptionPane.INFORMATION_MESSAGE);
    }

    private void setButtonImport() {
        jButtonImportDatei.setEnabled(!jTextFieldDatei.getText().isEmpty() && (jCheckBoxAbo.isSelected() || jCheckBoxBlack.isSelected() || jCheckBoxErsetzungstabelle.isSelected()));
    }

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
            setButtonImport();
        }
    }

    private class BeobPfad implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            //we can use native chooser on Mac...
            if (SystemUtils.IS_OS_MAC_OSX) {
                FileDialog chooser = new FileDialog(MediathekGui.ui(), "Konfigdatei auswählen");
                chooser.setMode(FileDialog.LOAD);
                chooser.setVisible(true);
                if (chooser.getFile() != null) {
                    try {
                        jTextFieldDatei.setText(new File(chooser.getDirectory() + chooser.getFile()).getAbsolutePath());
                    } catch (Exception ex) {
                        Log.errorLog(304656587, ex);
                    }
                }
            } else {
                int returnVal;
                JFileChooser chooser = new JFileChooser();
                chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
                chooser.setFileHidingEnabled(false);
                if (jTextFieldDatei.getText().isEmpty()) {
                    chooser.setCurrentDirectory(Daten.getMediathekXmlFilePath().toFile());
                } else {
                    chooser.setCurrentDirectory(new File(jTextFieldDatei.getText()));
                }
                returnVal = chooser.showOpenDialog(null);
                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    try {
                        jTextFieldDatei.setText(chooser.getSelectedFile().getAbsolutePath());
                    } catch (Exception ex) {
                        Log.errorLog(802039730, ex);
                    }
                }
            }
        }
    }


    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    // Generated using JFormDesigner non-commercial license
    private void initComponents() {
        var jPanel6 = new JPanel();
        jTextFieldDatei = new JTextField();
        jButtonPfad = new JButton();
        jButtonImportDatei = new JButton();
        var jLabel7 = new JLabel();
        var jScrollPane1 = new JScrollPane();
        var jTextArea1 = new JTextArea();
        jCheckBoxAbo = new JCheckBox();
        jCheckBoxBlack = new JCheckBox();
        var jLabel1 = new JLabel();
        jTextFieldPfadKonfig = new JTextField();
        jCheckBoxErsetzungstabelle = new JCheckBox();

        //======== this ========

        //======== jPanel6 ========
        {
            jPanel6.setBorder(new TitledBorder("Abos und Blacklist aus Datei importieren")); //NON-NLS

            //---- jButtonPfad ----
            jButtonPfad.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-file-open.png"))); //NON-NLS
            jButtonPfad.setToolTipText("Datei ausw\u00e4hlen"); //NON-NLS

            //---- jButtonImportDatei ----
            jButtonImportDatei.setText("Import"); //NON-NLS

            //---- jLabel7 ----
            jLabel7.setText("Datei:"); //NON-NLS

            //======== jScrollPane1 ========
            {

                //---- jTextArea1 ----
                jTextArea1.setEditable(false);
                jTextArea1.setColumns(20);
                jTextArea1.setLineWrap(true);
                jTextArea1.setRows(5);
                jTextArea1.setText("Damit k\u00f6nnen Abos/Blacklist/Ersetzungstabelle aus einer alten gesicherten\nKonfigurationsdatei importiert werden.\n(mediathek.xml oder mediathek.xml_copy_1, mediathek.xml_copy_2, ..)\n\n\nSollen die aktuellen Einstellungen durch die importierten ersetzt werden,\nsollten die aktuellen zuerst gel\u00f6scht werden.\n\nDie importierten Abos/Blacklist/Ersetzungstabelle werden\nan die vorhandenen angeh\u00e4ngt.\n"); //NON-NLS
                jScrollPane1.setViewportView(jTextArea1);
            }

            //---- jCheckBoxAbo ----
            jCheckBoxAbo.setText("Abos importieren"); //NON-NLS

            //---- jCheckBoxBlack ----
            jCheckBoxBlack.setText("Blacklist importieren"); //NON-NLS

            //---- jLabel1 ----
            jLabel1.setText("aktuelle Konfigurationsdatei:"); //NON-NLS

            //---- jTextFieldPfadKonfig ----
            jTextFieldPfadKonfig.setEditable(false);
            jTextFieldPfadKonfig.setText("jTextField1"); //NON-NLS

            //---- jCheckBoxErsetzungstabelle ----
            jCheckBoxErsetzungstabelle.setText("Ersetzungstabelle"); //NON-NLS

            GroupLayout jPanel6Layout = new GroupLayout(jPanel6);
            jPanel6.setLayout(jPanel6Layout);
            jPanel6Layout.setHorizontalGroup(
                jPanel6Layout.createParallelGroup()
                    .addGroup(jPanel6Layout.createSequentialGroup()
                        .addGroup(jPanel6Layout.createParallelGroup()
                            .addGroup(GroupLayout.Alignment.TRAILING, jPanel6Layout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jScrollPane1))
                            .addGroup(jPanel6Layout.createSequentialGroup()
                                .addContainerGap(12, GroupLayout.PREFERRED_SIZE)
                                .addComponent(jLabel7)
                                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                .addGroup(jPanel6Layout.createParallelGroup()
                                    .addGroup(jPanel6Layout.createSequentialGroup()
                                        .addComponent(jTextFieldDatei)
                                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                        .addComponent(jButtonPfad))
                                    .addGroup(jPanel6Layout.createSequentialGroup()
                                        .addGroup(jPanel6Layout.createParallelGroup()
                                            .addComponent(jCheckBoxAbo)
                                            .addComponent(jCheckBoxBlack)
                                            .addComponent(jCheckBoxErsetzungstabelle))
                                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED, 401, Short.MAX_VALUE)
                                        .addComponent(jButtonImportDatei))))
                            .addGroup(jPanel6Layout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(jPanel6Layout.createParallelGroup()
                                    .addGroup(jPanel6Layout.createSequentialGroup()
                                        .addComponent(jLabel1)
                                        .addGap(0, 497, Short.MAX_VALUE))
                                    .addComponent(jTextFieldPfadKonfig, GroupLayout.Alignment.TRAILING))))
                        .addContainerGap())
            );
            jPanel6Layout.setVerticalGroup(
                jPanel6Layout.createParallelGroup()
                    .addGroup(jPanel6Layout.createSequentialGroup()
                        .addContainerGap()
                        .addGroup(jPanel6Layout.createParallelGroup()
                            .addComponent(jButtonPfad)
                            .addGroup(jPanel6Layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                                .addComponent(jTextFieldDatei, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                                .addComponent(jLabel7)))
                        .addGap(18, 18, 18)
                        .addGroup(jPanel6Layout.createParallelGroup()
                            .addComponent(jButtonImportDatei)
                            .addGroup(jPanel6Layout.createSequentialGroup()
                                .addComponent(jCheckBoxAbo)
                                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jCheckBoxBlack)))
                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jCheckBoxErsetzungstabelle)
                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jScrollPane1, GroupLayout.PREFERRED_SIZE, 205, GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabel1)
                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldPfadKonfig, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                        .addContainerGap(GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
            );
            jPanel6Layout.linkSize(SwingConstants.VERTICAL, new Component[] {jButtonPfad, jTextFieldDatei, jTextFieldPfadKonfig});
        }

        GroupLayout layout = new GroupLayout(this);
        setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup()
                .addGroup(layout.createSequentialGroup()
                    .addContainerGap()
                    .addComponent(jPanel6, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup()
                .addGroup(layout.createSequentialGroup()
                    .addContainerGap()
                    .addComponent(jPanel6, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                    .addContainerGap(5, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    private JTextField jTextFieldDatei;
    private JButton jButtonPfad;
    private JButton jButtonImportDatei;
    private JCheckBox jCheckBoxAbo;
    private JCheckBox jCheckBoxBlack;
    private JTextField jTextFieldPfadKonfig;
    private JCheckBox jCheckBoxErsetzungstabelle;
    // End of variables declaration//GEN-END:variables
}
