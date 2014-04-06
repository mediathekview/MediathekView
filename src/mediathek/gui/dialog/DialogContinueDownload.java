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

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;
import mediathek.controller.Log;
import mediathek.daten.DatenDownload;
import mediathek.tool.EscBeenden;
import mediathek.tool.Konstanten;

public class DialogContinueDownload extends javax.swing.JDialog {

    public boolean weiter = false;
    public boolean neueStarten = false;
    public boolean neuerName = false;
    public boolean abbrechen = false;
    public JFrame parent;
    DatenDownload datenDownload;
    File file;
    boolean stopWait = false;
    MVPanelDownloadZiel mVPanelDownloadZiel;

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
        mVPanelDownloadZiel = new MVPanelDownloadZiel(null, dDownload, false);
        jPanelPath.setLayout(new BorderLayout(0, 0));
        jPanelPath.add(mVPanelDownloadZiel, BorderLayout.CENTER);
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

        jTextFieldTitel.setText(datenDownload.arr[DatenDownload.DOWNLOAD_TITEL_NR]);

        jButtonNeuerName.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                neueStarten = true;
                neuerName = mVPanelDownloadZiel.setPfadName_geaendert();
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
        if (datenDownload.interrupted()) {
            // annsonsten muss der User sebst entscheiden was er will
            new Thread(new Wait_(jLabelWait)).start();
        }
        pack();
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
                        if (stopWait) {
                            jLabel.setText("");
                        } else {
                            jLabel.setText("weiterführen in: " + w + " s");
                        }
                    } else {
                        SwingUtilities.invokeLater(new Runnable() {
                            @Override
                            public void run() {
                                if (stopWait) {
                                    jLabel.setText("");
                                } else {
                                    jLabel.setText("weiterführen in: " + w + " s");
                                }
                            }
                        });
                    }
                    if (stopWait) {
                        return;
                    }
                    this.wait(1000);
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

    private void beenden() {
        mVPanelDownloadZiel.saveComboPfad();
        this.dispose();
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jScrollPane1 = new javax.swing.JScrollPane();
        jTextArea1 = new javax.swing.JTextArea();
        jLabel2 = new javax.swing.JLabel();
        jTextFieldTitel = new javax.swing.JTextField();
        jPanel1 = new javax.swing.JPanel();
        jLabelWait = new javax.swing.JLabel();
        jButtonWeiter = new javax.swing.JButton();
        jButtonAbbrechen = new javax.swing.JButton();
        jPanel3 = new javax.swing.JPanel();
        jButtonNeuerName = new javax.swing.JButton();
        jPanelPath = new javax.swing.JPanel();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);

        jTextArea1.setEditable(false);
        jTextArea1.setColumns(20);
        jTextArea1.setLineWrap(true);
        jTextArea1.setRows(6);
        jTextArea1.setText("Die Filmdatei existiert bereits.\n\n   * Download weiterführen\n   * Download neu starten und Datei überschreiben\n   * Mit neuem Namen laden");
        jScrollPane1.setViewportView(jTextArea1);

        jLabel2.setText("Filmtitel:");

        jTextFieldTitel.setEditable(false);

        jPanel1.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 255), 2));

        jLabelWait.setText("0");

        jButtonWeiter.setText("Download weiterführen");

        jButtonAbbrechen.setText("Download abbrechen");

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
                    .addComponent(jButtonAbbrechen, javax.swing.GroupLayout.Alignment.TRAILING))
                .addContainerGap())
        );

        jPanel1Layout.linkSize(javax.swing.SwingConstants.HORIZONTAL, new java.awt.Component[] {jButtonAbbrechen, jButtonWeiter});

        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonWeiter)
                    .addComponent(jLabelWait))
                .addGap(18, 18, 18)
                .addComponent(jButtonAbbrechen)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel3.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 255), 2));

        jButtonNeuerName.setText("mit diesem Namen neu Starten");

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

        javax.swing.GroupLayout jPanel3Layout = new javax.swing.GroupLayout(jPanel3);
        jPanel3.setLayout(jPanel3Layout);
        jPanel3Layout.setHorizontalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap(262, Short.MAX_VALUE)
                .addComponent(jButtonNeuerName)
                .addContainerGap())
            .addComponent(jPanelPath, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        );
        jPanel3Layout.setVerticalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addComponent(jPanelPath, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jButtonNeuerName)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

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
                    .addComponent(jPanel3, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
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
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonAbbrechen;
    private javax.swing.JButton jButtonNeuerName;
    private javax.swing.JButton jButtonWeiter;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabelWait;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanelPath;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTextArea jTextArea1;
    private javax.swing.JTextField jTextFieldTitel;
    // End of variables declaration//GEN-END:variables

}
