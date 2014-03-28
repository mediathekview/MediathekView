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
import java.awt.Color;
import java.awt.FileDialog;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.JTextComponent;
import mediathek.controller.Log;
import mediathek.daten.Daten;
import mediathek.daten.DatenDownload;
import mediathek.res.GetIcon;
import mediathek.tool.DatumZeit;
import mediathek.tool.EscBeenden;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.Konstanten;
import mediathek.tool.MVColor;
import mediathek.tool.MVConfig;

public class DialogContinueDownload extends javax.swing.JDialog {

    public boolean weiter = false;
    public boolean neuStarten = false;
    public boolean neuerName = false;
    public boolean abbrechen = false;
    public JFrame parent;
    DatenDownload datenDownload;
    File file;
    boolean stopWait = false;

    /**
     *
     * @param p
     * @param dDownload
     */
    public DialogContinueDownload(JFrame p, DatenDownload dDownload) {
        super(p, true);
        initComponents();
        parent = p;
        datenDownload = dDownload;
        setTitle("Download weiterführen?");
        if (p != null) {
            setLocationRelativeTo(p);
        }
        jLabelWait.setText("");
        jLabelWait.addMouseListener(new MouseAdapter() {

            @Override
            public void mouseClicked(MouseEvent e) {
                stopWait = true;
            }
        });

        jLabelNameExistiert.setText("");
        jTextFieldTitel.setText(datenDownload.arr[DatenDownload.DOWNLOAD_TITEL_NR]);

        jButtonZiel.setIcon(GetIcon.getIcon("fileopen_16.png"));
        jButtonZiel.addActionListener(new BeobPfad());
        jButtonNeuerName.setEnabled(false);
        jButtonNeuerName.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                neuerName = true;
                setPfadName();
                beenden();
            }
        });
        jButtonAbbrechen.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                abbrechen();
            }
        });
        new EscBeenden(this) {
            @Override
            public void beenden_() {
                abbrechen();
            }
        };
        this.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE); // soll abgefangen werden
        addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent evt) {
                abbrechen();
            }
        });
        jButtonWeiter.setSelected(true);
        jButtonWeiter.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                weiter = true;
                beenden();
            }
        });
        jButtonNeuStarten.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                neuStarten = true;
                beenden();
            }
        });
        jTextFieldName.setText(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_DATEINAME_NR]);
        jTextFieldName.getDocument().addDocumentListener(new DocumentListener() {

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
                checkPfadName();
                if (!jTextFieldName.getText().equals(GuiFunktionen.checkDateiname(jTextFieldName.getText(), true /*pfad*/))) {
                    jTextFieldName.setBackground(MVColor.DOWNLOAD_FEHLER.color);
                } else {
                    jTextFieldName.setBackground(javax.swing.UIManager.getDefaults().getColor("TextField.background"));
                }
            }
        });
        ((JTextComponent) jComboBoxPfad.getEditor().getEditorComponent()).getDocument().addDocumentListener(new DocumentListener() {

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
                checkPfadName();
                String s = ((JTextComponent) jComboBoxPfad.getEditor().getEditorComponent()).getText();
                if (!s.equals(GuiFunktionen.checkDateiname(s, true /*pfad*/))) {
                    ((JTextComponent) jComboBoxPfad.getEditor().getEditorComponent()).setBackground(MVColor.DOWNLOAD_FEHLER.color);
                } else {
                    ((JTextComponent) jComboBoxPfad.getEditor().getEditorComponent()).setBackground(Color.WHITE);
                }
            }
        });
        setModelPfad(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_NR]);
        if (datenDownload.interrupted()) {
            // annsonsten muss der User sebst entscheiden was er will
            new Thread(new Wait_(jLabelWait)).start();
        }
    }

    private class Wait_ implements Runnable {

        JLabel jLabel;
        int w = 0;

        public Wait_(JLabel jjLabel) {
            jLabel = jjLabel;
        }

        @Override
        public synchronized void run() {
            try {
                for (w = Konstanten.DOWNLOAD_WEITERFUEHREN_IN_SEKUNDEN; w > 0; --w) {
                    if (SwingUtilities.isEventDispatchThread()) {
                        jLabel.setText("weiterführen in: " + w + " s");
                    } else {
                        SwingUtilities.invokeLater(new Runnable() {
                            @Override
                            public void run() {
                                jLabel.setText("weiterführen in: " + w + " s");
                            }
                        });
                    }
                    this.wait(1000);
                    if (stopWait) {
                        jLabel.setText("");
                        return;
                    }
                }
                weiter = true;
                if (SwingUtilities.isEventDispatchThread()) {
                    beenden();
                } else {
                    SwingUtilities.invokeLater(new Runnable() {
                        @Override
                        public void run() {
                            beenden();
                        }
                    });
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(698989743, Log.FEHLER_ART_PROG, "ListenerMediathekView.pingen", ex);
            }
        }
    }

    private void abbrechen() {
        abbrechen = true;
        beenden();
    }

    private void setModelPfad(String pfad) {
        ArrayList<String> pfade = new ArrayList<>();
        if (!pfad.isEmpty()) {
            pfade.add(pfad);
        }
        if (!Daten.mVConfig.get(MVConfig.SYSTEM__DIALOG_DOWNLOAD__PFADE_ZUM_SPEICHERN).isEmpty()) {
            String[] p = Daten.mVConfig.get(MVConfig.SYSTEM__DIALOG_DOWNLOAD__PFADE_ZUM_SPEICHERN).split("<>");
            if (p.length != 0) {
                pfade.addAll(Arrays.asList(p));
            }
        }
        jComboBoxPfad.setModel(new DefaultComboBoxModel<>(pfade.toArray(new String[]{})));
    }

    private void setPfadName() {
        String pfad = jComboBoxPfad.getSelectedItem().toString();
        String name = jTextFieldName.getText();
        if (pfad.endsWith(File.separator)) {
            pfad = pfad.substring(0, pfad.length() - 1);
        }
        //##############################################
        // zur Sicherheit bei Unsinn im Set
        if (pfad.equals("")) {
            pfad = GuiFunktionen.getStandardDownloadPath();
        }
        if (name.equals("")) {
            name = DatumZeit.Heute_yyyyMMdd + "_" + datenDownload.arr[DatenDownload.DOWNLOAD_THEMA_NR] + "-" + datenDownload.arr[DatenDownload.DOWNLOAD_TITEL_NR] + ".mp4";
        }
        //##############################################
        datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_DATEINAME_NR] = name;
        datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_NR] = pfad;
        datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR] = GuiFunktionen.addsPfad(pfad, name);
    }

    private void checkPfadName() {
        String pfad = jComboBoxPfad.getSelectedItem().toString();
        String name = jTextFieldName.getText();
        if (pfad.endsWith(File.separator)) {
            pfad = pfad.substring(0, pfad.length() - 1);
        }
        String pfadName = GuiFunktionen.addsPfad(pfad, name);
        jButtonNeuerName.setEnabled(!jTextFieldName.getText().equals(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_DATEINAME_NR])
                || !(((JTextComponent) jComboBoxPfad.getEditor().getEditorComponent()).getText()).equals(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_NR]));
        try {
            file = new File(pfadName);
            if (file.exists()) {
                jLabelNameExistiert.setText("Datei existiert schon!");
            } else {
                jLabelNameExistiert.setText("");
            }
        } catch (Exception ex) {
        }
    }

    private void beenden() {
        this.dispose();
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jScrollPane1 = new javax.swing.JScrollPane();
        jTextArea1 = new javax.swing.JTextArea();
        jLabel2 = new javax.swing.JLabel();
        jTextFieldTitel = new javax.swing.JTextField();
        jButtonAbbrechen = new javax.swing.JButton();
        jPanel1 = new javax.swing.JPanel();
        jLabelWait = new javax.swing.JLabel();
        jButtonWeiter = new javax.swing.JButton();
        jButtonNeuStarten = new javax.swing.JButton();
        jPanel3 = new javax.swing.JPanel();
        jTextFieldName = new javax.swing.JTextField();
        jButtonZiel = new javax.swing.JButton();
        jButtonNeuerName = new javax.swing.JButton();
        jLabelNameExistiert = new javax.swing.JLabel();
        jLabel1 = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        jComboBoxPfad = new javax.swing.JComboBox();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);

        jTextArea1.setEditable(false);
        jTextArea1.setColumns(20);
        jTextArea1.setLineWrap(true);
        jTextArea1.setRows(6);
        jTextArea1.setText("Die Filmdatei existiert bereits.\n\n   * Download weiterführen\n   * Download neu starten und Datei überschreiben\n   * Mit neuem Namen laden");
        jScrollPane1.setViewportView(jTextArea1);

        jLabel2.setText("Filmtitel:");

        jTextFieldTitel.setEditable(false);

        jButtonAbbrechen.setText("Download abbrechen");

        jPanel1.setBorder(javax.swing.BorderFactory.createTitledBorder(""));

        jLabelWait.setText("0");

        jButtonWeiter.setText("Download weiterführen");

        jButtonNeuStarten.setText("Download neu starten");

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel1Layout.createSequentialGroup()
                        .addComponent(jLabelWait)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonWeiter))
                    .addComponent(jButtonNeuStarten, javax.swing.GroupLayout.Alignment.TRAILING))
                .addContainerGap())
        );

        jPanel1Layout.linkSize(javax.swing.SwingConstants.HORIZONTAL, new java.awt.Component[] {jButtonNeuStarten, jButtonWeiter});

        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonWeiter)
                    .addComponent(jLabelWait))
                .addGap(18, 18, 18)
                .addComponent(jButtonNeuStarten)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel3.setBorder(javax.swing.BorderFactory.createTitledBorder(""));

        jButtonZiel.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/fileopen_16.png"))); // NOI18N

        jButtonNeuerName.setText("mit neuem Namen laden");

        jLabelNameExistiert.setText("Datei existiert schon!");

        jLabel1.setText("Zielpfad:");

        jLabel3.setText("Dateiname:");

        jComboBoxPfad.setEditable(true);
        jComboBoxPfad.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));

        javax.swing.GroupLayout jPanel3Layout = new javax.swing.GroupLayout(jPanel3);
        jPanel3.setLayout(jPanel3Layout);
        jPanel3Layout.setHorizontalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel3Layout.createSequentialGroup()
                        .addComponent(jLabelNameExistiert)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 136, Short.MAX_VALUE)
                        .addComponent(jButtonNeuerName))
                    .addGroup(jPanel3Layout.createSequentialGroup()
                        .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabel3)
                            .addComponent(jLabel1))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel3Layout.createSequentialGroup()
                                .addComponent(jComboBoxPfad, 0, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addComponent(jButtonZiel))
                            .addComponent(jTextFieldName))))
                .addContainerGap())
        );
        jPanel3Layout.setVerticalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jButtonZiel)
                    .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                        .addComponent(jLabel1)
                        .addComponent(jComboBoxPfad, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel3)
                    .addComponent(jTextFieldName, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addGap(18, 18, 18)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jLabelNameExistiert)
                    .addComponent(jButtonNeuerName))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel3Layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonNeuerName, jButtonZiel, jTextFieldName});

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanel1, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jScrollPane1, javax.swing.GroupLayout.Alignment.TRAILING)
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(jLabel2)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldTitel))
                    .addComponent(jPanel3, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                        .addGap(0, 0, Short.MAX_VALUE)
                        .addComponent(jButtonAbbrechen)))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jTextFieldTitel, javax.swing.GroupLayout.PREFERRED_SIZE, 19, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel2))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jScrollPane1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(18, 18, 18)
                .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(18, 18, 18)
                .addComponent(jPanel3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(18, 18, 18)
                .addComponent(jButtonAbbrechen)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonAbbrechen;
    private javax.swing.JButton jButtonNeuStarten;
    private javax.swing.JButton jButtonNeuerName;
    private javax.swing.JButton jButtonWeiter;
    private javax.swing.JButton jButtonZiel;
    private javax.swing.JComboBox jComboBoxPfad;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabelNameExistiert;
    private javax.swing.JLabel jLabelWait;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTextArea jTextArea1;
    private javax.swing.JTextField jTextFieldName;
    private javax.swing.JTextField jTextFieldTitel;
    // End of variables declaration//GEN-END:variables

    private class BeobPfad implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            //we can use native chooser on Mac...
            if (SystemInfo.isMacOSX()) {
                FileDialog chooser = new FileDialog(parent, "Download speichern");
                chooser.setMode(FileDialog.SAVE);
                chooser.setVisible(true);
                if (chooser.getFile() != null) {
                    try {
                        File destination = new File(chooser.getDirectory() + chooser.getFile());
                        jTextFieldName.setText(destination.getAbsolutePath());
                    } catch (Exception ex) {
                        Log.fehlerMeldung(302019898, Log.FEHLER_ART_PROG, "DialogContinueDownload.BeobPfad", ex);
                    }
                }
            } else {
                int returnVal;
                JFileChooser chooser = new JFileChooser();
                if (!jTextFieldName.getText().equals("")) {
                    chooser.setCurrentDirectory(new File(jTextFieldName.getText()));
                }
                chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
                chooser.setFileHidingEnabled(false);
                returnVal = chooser.showOpenDialog(null);
                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    try {
                        jTextFieldName.setText(chooser.getSelectedFile().getAbsolutePath());
                    } catch (Exception ex) {
                        Log.fehlerMeldung(763214789, Log.FEHLER_ART_PROG, "DialogContinueDownload.BeobPfad", ex);
                    }
                }
            }
        }
    }

}
