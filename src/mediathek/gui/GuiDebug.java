/*
 * MediathekView
 * Copyright (C) 2008 W. Xaver
 * W.Xaver[at]googlemail.com
 * http://zdfmediathk.sourceforge.net/
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.gui;

import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.net.URL;
import java.net.URLConnection;
import javax.swing.JButton;
import javax.swing.JPanel;

import mediathek.MediathekGui;
import mediathek.daten.Daten;
import mediathek.gui.dialogEinstellungen.PanelListeFilmlistenServer;
import mediathek.gui.dialogEinstellungen.PanelSenderLaden;
import mediathek.tool.ListenerMediathekView;
import mediathek.controller.Log;
import mediathek.tool.MVNotification;
import msearch.daten.MSearchConfig;

public class GuiDebug extends JPanel {

    private JButton[] buttonSender;
    private String[] sender;
    private Daten daten;

    public GuiDebug(Daten d) {
        super();
        initComponents();
        daten = d;
        sender = Daten.filmeLaden.getSenderNamen();
        buttonSender = new JButton[sender.length];

        //Tab1 Sender löschen Panel füllen
        for (int i = 0; i < Daten.filmeLaden.getSenderNamen().length; ++i) {
            buttonSender[i] = new JButton(sender[i]);
            buttonSender[i].addActionListener(new BeobSenderLoeschen(sender[i]));
        }
        addSender();

        jToggleButtonAllesLaden.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                MSearchConfig.senderAllesLaden = jToggleButtonAllesLaden.isSelected();
            }
        });
        jButtonAllesSpeichern.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.allesSpeichern();
                Daten.filmlisteSpeichern();
            }
        });
        jButtonFilmlisteLoeschen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.listeFilme.clear();
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_FILMLISTE_GEAENDERT, MediathekGui.class.getSimpleName());
            }
        });
        jButtonFehler.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Log.printEndeMeldung();
            }
        });
        jButtonCheck.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.listeFilme.check();
            }
        });
        jButtonAlleLaden.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                new Thread(new Runnable() {
                    @Override
                    public void run() {
                        Daten.filmeLaden.filmeBeimSenderSuchen(Daten.listeFilme, jToggleButtonAllesLaden.isSelected(), true);
                    }
                }).start();
            }
        });

        jPanelSenderLaden.add(new PanelSenderLaden());

        jPanelListen.add(new PanelListeFilmlistenServer(d, daten.mediathekGui));

        jButtonCheckUrl.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                long l = 0;
                try {
                    URLConnection co = new URL(jTextFieldUrl.getText()).openConnection();
                    l = co.getContentLengthLong();
                } catch (Exception ex) {
                    ex.printStackTrace();
                }
                System.out.println("Byte: " + l);
            }
        });
        jButtonGc.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                System.gc();
            }
        });
        jButtonNotify.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                MVNotification.addNotification(daten, "Titel", "<html>"
                        + "<head>"
                        + "</head>"
                        + "<body>"
                        + "<p>"
                        + "<b>Autoren:</b><br />"
                        + "Xaver W. (W.Xaver [at] googlemail [dot] com)<br />"
                        + "Christian F. (crystalpalace1977 [at] googlemail [dot] com)"
                        + "</p>"
                        + "</body>"
                        + "</html>");
            }
        });
    }

    private void addSender() {
        jPanelLoeschen.removeAll();
        jPanelLoeschen.setLayout(new GridLayout(0, 5));
        int nr = 0;
        for (String aSender : sender) {
            JButton btn = buttonSender[nr];
            btn.setText(aSender);
            jPanelLoeschen.add(btn);
            ++nr;
        }
        jPanelLoeschen.repaint();
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.JTabbedPane jTabbedPane1 = new javax.swing.JTabbedPane();
        javax.swing.JPanel jPanel2 = new javax.swing.JPanel();
        jPanelSenderLaden = new javax.swing.JPanel();
        javax.swing.JPanel jPanel1 = new javax.swing.JPanel();
        jPanelSender = new javax.swing.JPanel();
        jPanelLoeschen = new javax.swing.JPanel();
        javax.swing.JPanel jPanel3 = new javax.swing.JPanel();
        jPanelListen = new javax.swing.JPanel();
        javax.swing.JPanel jPanel5 = new javax.swing.JPanel();
        jButtonCheckUrl = new javax.swing.JButton();
        jTextFieldUrl = new javax.swing.JTextField();
        javax.swing.JPanel jPanel4 = new javax.swing.JPanel();
        jToggleButtonAllesLaden = new javax.swing.JToggleButton();
        jButtonFilmlisteLoeschen = new javax.swing.JButton();
        jButtonAllesSpeichern = new javax.swing.JButton();
        jButtonFehler = new javax.swing.JButton();
        jButtonCheck = new javax.swing.JButton();
        jButtonAlleLaden = new javax.swing.JButton();
        jButtonGc = new javax.swing.JButton();
        jButtonNotify = new javax.swing.JButton();

        jPanelSenderLaden.setBorder(javax.swing.BorderFactory.createTitledBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.LOWERED), "Sender starten"));
        jPanelSenderLaden.setLayout(new java.awt.BorderLayout());

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanelSenderLaden, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanelSenderLaden, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );

        jTabbedPane1.addTab("tab2", jPanel2);

        jPanelSender.setBorder(javax.swing.BorderFactory.createTitledBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.LOWERED), "Sender löschen"));

        javax.swing.GroupLayout jPanelLoeschenLayout = new javax.swing.GroupLayout(jPanelLoeschen);
        jPanelLoeschen.setLayout(jPanelLoeschenLayout);
        jPanelLoeschenLayout.setHorizontalGroup(
            jPanelLoeschenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 35, Short.MAX_VALUE)
        );
        jPanelLoeschenLayout.setVerticalGroup(
            jPanelLoeschenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 111, Short.MAX_VALUE)
        );

        javax.swing.GroupLayout jPanelSenderLayout = new javax.swing.GroupLayout(jPanelSender);
        jPanelSender.setLayout(jPanelSenderLayout);
        jPanelSenderLayout.setHorizontalGroup(
            jPanelSenderLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelSenderLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanelLoeschen, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(776, Short.MAX_VALUE))
        );
        jPanelSenderLayout.setVerticalGroup(
            jPanelSenderLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelSenderLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanelLoeschen, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(322, Short.MAX_VALUE))
        );

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanelSender, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanelSender, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );

        jTabbedPane1.addTab("tab1", jPanel1);

        jPanelListen.setLayout(new java.awt.BorderLayout());

        javax.swing.GroupLayout jPanel3Layout = new javax.swing.GroupLayout(jPanel3);
        jPanel3.setLayout(jPanel3Layout);
        jPanel3Layout.setHorizontalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanelListen, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );
        jPanel3Layout.setVerticalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanelListen, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );

        jTabbedPane1.addTab("tab3", jPanel3);

        jButtonCheckUrl.setText("check URL");

        javax.swing.GroupLayout jPanel5Layout = new javax.swing.GroupLayout(jPanel5);
        jPanel5.setLayout(jPanel5Layout);
        jPanel5Layout.setHorizontalGroup(
            jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel5Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jButtonCheckUrl)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jTextFieldUrl, javax.swing.GroupLayout.DEFAULT_SIZE, 726, Short.MAX_VALUE)
                .addContainerGap())
        );
        jPanel5Layout.setVerticalGroup(
            jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel5Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonCheckUrl)
                    .addComponent(jTextFieldUrl, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(455, Short.MAX_VALUE))
        );

        jPanel5Layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonCheckUrl, jTextFieldUrl});

        jTabbedPane1.addTab("tab4", jPanel5);

        jPanel4.setBorder(javax.swing.BorderFactory.createEtchedBorder());

        jToggleButtonAllesLaden.setText("[-alles] setzen");

        jButtonFilmlisteLoeschen.setText("Filmliste löschen");

        jButtonAllesSpeichern.setText("alles speichern");

        jButtonFehler.setText("Fehler ausgeben");

        jButtonCheck.setText("Check");

        jButtonAlleLaden.setText("alle Sender laden");

        jButtonGc.setText("Gc");

        jButtonNotify.setText("Notify");

        javax.swing.GroupLayout jPanel4Layout = new javax.swing.GroupLayout(jPanel4);
        jPanel4.setLayout(jPanel4Layout);
        jPanel4Layout.setHorizontalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel4Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel4Layout.createSequentialGroup()
                        .addComponent(jButtonAlleLaden)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonGc)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonNotify)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonCheck))
                    .addGroup(jPanel4Layout.createSequentialGroup()
                        .addComponent(jToggleButtonAllesLaden)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonFilmlisteLoeschen)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonAllesSpeichern)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonFehler)))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel4Layout.linkSize(javax.swing.SwingConstants.HORIZONTAL, new java.awt.Component[] {jButtonAllesSpeichern, jButtonFilmlisteLoeschen, jToggleButtonAllesLaden});

        jPanel4Layout.setVerticalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel4Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jToggleButtonAllesLaden)
                    .addComponent(jButtonFilmlisteLoeschen)
                    .addComponent(jButtonAllesSpeichern)
                    .addComponent(jButtonFehler))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonGc)
                    .addComponent(jButtonNotify)
                    .addComponent(jButtonAlleLaden)
                    .addComponent(jButtonCheck))
                .addContainerGap(43, Short.MAX_VALUE))
        );

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanel4, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jTabbedPane1, javax.swing.GroupLayout.PREFERRED_SIZE, 0, Short.MAX_VALUE))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel4, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jTabbedPane1)
                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonAlleLaden;
    private javax.swing.JButton jButtonAllesSpeichern;
    private javax.swing.JButton jButtonCheck;
    private javax.swing.JButton jButtonCheckUrl;
    private javax.swing.JButton jButtonFehler;
    private javax.swing.JButton jButtonFilmlisteLoeschen;
    private javax.swing.JButton jButtonGc;
    private javax.swing.JButton jButtonNotify;
    private javax.swing.JPanel jPanelListen;
    private javax.swing.JPanel jPanelLoeschen;
    private javax.swing.JPanel jPanelSender;
    private javax.swing.JPanel jPanelSenderLaden;
    private javax.swing.JTextField jTextFieldUrl;
    private javax.swing.JToggleButton jToggleButtonAllesLaden;
    // End of variables declaration//GEN-END:variables

    private class BeobSenderLoeschen implements ActionListener {

        private String sender;

        public BeobSenderLoeschen(String ssender) {
            sender = ssender;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            Daten.listeFilme.delSender(sender);
            ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_FILMLISTE_GEAENDERT, MediathekGui.class.getSimpleName());
        }
    }
}
