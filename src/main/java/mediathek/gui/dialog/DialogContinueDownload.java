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

import mediathek.config.MVConfig;
import mediathek.daten.DatenDownload;
import mediathek.tool.EscBeenden;
import mediathek.tool.MVMessageDialog;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

@SuppressWarnings("serial")
public class DialogContinueDownload extends JDialog {
    public enum DownloadResult {

        CANCELLED, CONTINUE, RESTART_WITH_NEW_NAME
    }

    private DownloadResult result;

    private boolean isNewName = false;
    private MVPanelDownloadZiel mVPanelDownloadZiel;
    private Timer countdownTimer = null;
    private final boolean direkterDownload;
    final private JFrame parent;

    public DialogContinueDownload(JFrame pparent, DatenDownload datenDownload, boolean ddirekterDownload) {
        // "weiterführen"
        // true: dann kann der bereits gestartete Download weitergeführt werden, nur direkte Downloads
        // false: dann kann der Download nur neu gestartet werden, die existierende Datei wird gelöscht
        super(pparent, true);
        initComponents();
        this.parent = pparent;
        this.direkterDownload = ddirekterDownload;
        if (!direkterDownload) {
            jButtonWeiter.setText("Überschreiben");
            if (!datenDownload.checkAufrufBauen()) {
                // nur für Downloads mit Programm
                jPanelNewName.setVisible(false);
            }
        }
        mVPanelDownloadZiel = new MVPanelDownloadZiel(null, datenDownload, false);
        jPanelPath.setLayout(new BorderLayout(0, 0));
        jPanelPath.add(mVPanelDownloadZiel, BorderLayout.CENTER);

        if (parent != null) {
            setLocationRelativeTo(parent);
        }

        String dialogText = "<html>Der Film \""
                + datenDownload.arr[DatenDownload.DOWNLOAD_TITEL]
                + "\" existiert bereits.<br>Wie möchten Sie fortfahren?</html>";
        jLabel1.setText(dialogText);

        jButtonNeuerName.addActionListener(e -> {
            isNewName = mVPanelDownloadZiel.setPfadName_geaendert();
            if (!direkterDownload && !isNewName) {
                // dann gibts es nur Überschreiben oder anderer Name, sonst zickt ffmpeg
                MVMessageDialog.showMessageDialog(parent, "Der Dateiname wurde nicht geändert!",
                        "Datei existiert bereits!", JOptionPane.ERROR_MESSAGE);
            } else {
                result = DownloadResult.RESTART_WITH_NEW_NAME;
                beenden();
            }
        });

        jButtonAbbrechen.addActionListener(e -> abbrechen());
        new EscBeenden(this) {
            @Override
            public void beenden_() {
                abbrechen();
            }
        };

        addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent evt) {
                abbrechen();
            }
        });
        jButtonWeiter.addActionListener(e -> {
            result = DownloadResult.CONTINUE;
            beenden();
        });

        //start the countdown...
        countdownTimer = new Timer(0, new CountdownAction());
        countdownTimer.setRepeats(true);
        countdownTimer.start();

        pack();

        getRootPane().setDefaultButton(jButtonWeiter);
    }

    /**
     * Return the result of the user selection made in the dialog.
     *
     * @return A {@link mediathek.gui.dialog.DialogContinueDownload.DownloadResult} result.
     */
    public DownloadResult getResult() {
        return result;
    }

    /**
     * Check if a new name was specified.
     *
     * @return A new name needs to be used.
     */
    public boolean isNewName() {
        return isNewName;
    }

    private void abbrechen() {
        result = DownloadResult.CANCELLED;
        beenden();
    }

    private void beenden() {
        if (countdownTimer != null) {
            countdownTimer.stop();
        }
//        mVPanelDownloadZiel.saveComboPfad();
        dispose();
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jButtonWeiter = new javax.swing.JButton();
        jButtonAbbrechen = new javax.swing.JButton();
        jPanelNewName = new javax.swing.JPanel();
        jButtonNeuerName = new javax.swing.JButton();
        jPanelPath = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();

        setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE);
        setTitle("Download weiterführen");

        jButtonWeiter.setText("Weiterführen in XXX");

        jButtonAbbrechen.setText("Abbrechen");

        jPanelNewName.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 255), 2));

        jButtonNeuerName.setText("Mit diesem Namen neu Starten");

        javax.swing.GroupLayout jPanelPathLayout = new javax.swing.GroupLayout(jPanelPath);
        jPanelPath.setLayout(jPanelPathLayout);
        jPanelPathLayout.setHorizontalGroup(
                jPanelPathLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGap(0, 527, Short.MAX_VALUE)
        );
        jPanelPathLayout.setVerticalGroup(
                jPanelPathLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGap(0, 124, Short.MAX_VALUE)
        );

        javax.swing.GroupLayout jPanelNewNameLayout = new javax.swing.GroupLayout(jPanelNewName);
        jPanelNewName.setLayout(jPanelNewNameLayout);
        jPanelNewNameLayout.setHorizontalGroup(
                jPanelNewNameLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanelNewNameLayout.createSequentialGroup()
                                .addContainerGap(285, Short.MAX_VALUE)
                                .addComponent(jButtonNeuerName)
                                .addContainerGap())
                        .addComponent(jPanelPath, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        );
        jPanelNewNameLayout.setVerticalGroup(
                jPanelNewNameLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanelNewNameLayout.createSequentialGroup()
                                .addComponent(jPanelPath, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addComponent(jButtonNeuerName)
                                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jLabel1.setText("<html>Die Filmdatei existiert bereits.<br>Wie möchten Sie forfahren?</html>");

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
                layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(layout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                        .addComponent(jLabel1)
                                        .addComponent(jPanelNewName, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                                                .addGap(0, 0, Short.MAX_VALUE)
                                                .addComponent(jButtonAbbrechen)
                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                                .addComponent(jButtonWeiter)))
                                .addContainerGap())
        );
        layout.setVerticalGroup(
                layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(layout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jLabel1, javax.swing.GroupLayout.PREFERRED_SIZE, 40, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                                        .addComponent(jButtonWeiter)
                                        .addComponent(jButtonAbbrechen))
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                .addComponent(jPanelNewName, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addGap(16, 16, 16))
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonAbbrechen;
    private javax.swing.JButton jButtonNeuerName;
    private javax.swing.JButton jButtonWeiter;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JPanel jPanelNewName;
    private javax.swing.JPanel jPanelPath;
    // End of variables declaration//GEN-END:variables

    /**
     * Implements the countdown based on Swing Timer for automatic placement on EDT.
     */
    private class CountdownAction implements ActionListener {

        private int w = MVConfig.getInt(MVConfig.Configs.SYSTEM_PARAMETER_DOWNLOAD_WEITERFUEHREN_IN_SEKUNDEN);

        @Override
        public void actionPerformed(ActionEvent e) {
            if (w > 0) {
                if (!direkterDownload) {
                    jButtonWeiter.setText("Überschreiben in " + w + "s");
                } else {
                    jButtonWeiter.setText("Weiterführen in " + w + "s");
                }
                if (countdownTimer != null) {
                    countdownTimer.setDelay(1000);
                }
            } else {
                result = DownloadResult.CONTINUE;
                beenden();
            }
            w--;
        }
    }
}
