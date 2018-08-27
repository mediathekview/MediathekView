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

import mSearch.tool.Listener;
import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.config.MVConfig;
import mediathek.gui.PanelVorlage;
import mediathek.gui.dialog.DialogHilfe;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import java.awt.*;

@SuppressWarnings("serial")
public class PanelDownload extends PanelVorlage {
    public PanelDownload(Daten d, JFrame parent) {
        super(d, parent);
        initComponents();
        daten = d;
        jSpinnerAnzahlDownload.setModel(new javax.swing.SpinnerNumberModel(1, 1, 9, 1));
        jSpinnerAnzahlDownload.setValue(Integer.parseInt(MVConfig.get(MVConfig.Configs.SYSTEM_MAX_DOWNLOAD)));
        jSpinnerAnzahlDownload.addChangeListener(new BeobSpinnerDownload());
        jButtonHilfeAnzahl.setIcon(Icons.ICON_BUTTON_HELP);
        jButtonHilfeAnzahl.addActionListener(e -> new DialogHilfe(parentComponent, true, "\n"
                + "Hier kann angegeben werden, wie viele\n"
                + "Downloads gleichzeitig gestartet werden können.\n\n"
                + "Es gibt jedoch noch eine Begrenzung\n"
                + "die trotzdem nicht überschritten wird:\n\n"
                + "2 Downloads pro Server, das kann auch noch\n"
                + "auf 1 Download pro Server\n"
                + "(z.B. nur ein Download von \"www.zdf.de\")\n"
                + "weiter begrenzt werden.").setVisible(true));
        Listener.addListener(new Listener(Listener.EREIGNIS_ANZAHL_DOWNLOADS, PanelDownload.class.getSimpleName()) {
            @Override
            public void ping() {
                jSpinnerAnzahlDownload.setValue(Integer.parseInt(MVConfig.get(MVConfig.Configs.SYSTEM_MAX_DOWNLOAD)));
            }
        });

        jCheckBoxNotification.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_NOTIFICATION)));
        jCheckBoxNotification.addActionListener(e -> MVConfig.add(MVConfig.Configs.SYSTEM_NOTIFICATION, Boolean.toString(jCheckBoxNotification.isSelected())));
        cbkDownloadError.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_DOWNLOAD_ERRORMSG)));
        cbkDownloadError.addActionListener(e -> MVConfig.add(MVConfig.Configs.SYSTEM_DOWNLOAD_ERRORMSG, Boolean.toString(cbkDownloadError.isSelected())));
        jCheckBoxBeep.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_DOWNLOAD_BEEP)));
        jCheckBoxBeep.addActionListener(ae -> MVConfig.add(MVConfig.Configs.SYSTEM_DOWNLOAD_BEEP, String.valueOf(jCheckBoxBeep.isSelected())));
        jCheckBoxServer.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_MAX_1_DOWNLOAD_PRO_SERVER)));
        jCheckBoxServer.addActionListener(ae -> MVConfig.add(MVConfig.Configs.SYSTEM_MAX_1_DOWNLOAD_PRO_SERVER, String.valueOf(jCheckBoxServer.isSelected())));
        jButtonBeep.addActionListener(ae -> Toolkit.getDefaultToolkit().beep());
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.JPanel jPanel2 = new javax.swing.JPanel();
        jCheckBoxNotification = new javax.swing.JCheckBox();
        javax.swing.JLabel jLabel3 = new javax.swing.JLabel();
        jSpinnerAnzahlDownload = new javax.swing.JSpinner();
        jButtonHilfeAnzahl = new javax.swing.JButton();
        jCheckBoxBeep = new javax.swing.JCheckBox();
        jButtonBeep = new javax.swing.JButton();
        jCheckBoxServer = new javax.swing.JCheckBox();
        cbkDownloadError = new javax.swing.JCheckBox();

        setMinimumSize(getPreferredSize());

        jPanel2.setBorder(javax.swing.BorderFactory.createTitledBorder(""));

        jCheckBoxNotification.setText("Benachrichtigung wenn abgeschlossen");

        jLabel3.setText("gleichzeitige Downloads laden:");

        jSpinnerAnzahlDownload.setModel(new javax.swing.SpinnerNumberModel(1, 1, 9, 1));

        jButtonHilfeAnzahl.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-help.png"))); // NOI18N
        jButtonHilfeAnzahl.setToolTipText("Hilfe anzeigen");

        jCheckBoxBeep.setText("nach jedem Download einen \"Beep\" ausgeben");

        jButtonBeep.setText("Testen");

        jCheckBoxServer.setText("nur ein Download pro Downloadserver");

        cbkDownloadError.setText("Bei Downloadfehler, Fehlermeldung anzeigen");

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addComponent(jCheckBoxNotification, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addGap(64, 64, 64))
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(cbkDownloadError)
                            .addGroup(jPanel2Layout.createSequentialGroup()
                                .addGap(12, 12, 12)
                                .addComponent(jCheckBoxServer))
                            .addGroup(jPanel2Layout.createSequentialGroup()
                                .addComponent(jCheckBoxBeep)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addComponent(jButtonBeep))
                            .addGroup(jPanel2Layout.createSequentialGroup()
                                .addComponent(jLabel3)
                                .addGap(51, 51, 51)
                                .addComponent(jSpinnerAnzahlDownload, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jButtonHilfeAnzahl)))
                            .addGap(0, 131, Short.MAX_VALUE))))
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jCheckBoxNotification)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(cbkDownloadError)
                .addGap(18, 18, 18)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jCheckBoxBeep)
                    .addComponent(jButtonBeep))
                .addGap(18, 18, 18)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jLabel3)
                    .addComponent(jSpinnerAnzahlDownload, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jButtonHilfeAnzahl))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jCheckBoxServer)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                    .addComponent(jPanel2, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addContainerGap(403, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JCheckBox cbkDownloadError;
    private javax.swing.JButton jButtonBeep;
    private javax.swing.JButton jButtonHilfeAnzahl;
    private javax.swing.JCheckBox jCheckBoxBeep;
    private javax.swing.JCheckBox jCheckBoxNotification;
    private javax.swing.JCheckBox jCheckBoxServer;
    private javax.swing.JSpinner jSpinnerAnzahlDownload;
    // End of variables declaration//GEN-END:variables

    private class BeobSpinnerDownload implements ChangeListener {

        @Override
        public void stateChanged(ChangeEvent arg0) {
            MVConfig.add(MVConfig.Configs.SYSTEM_MAX_DOWNLOAD,
                    String.valueOf(((Number) jSpinnerAnzahlDownload.getModel().getValue()).intValue()));
            Listener.notify(Listener.EREIGNIS_ANZAHL_DOWNLOADS, PanelDownload.class.getSimpleName());
        }
    }

}
