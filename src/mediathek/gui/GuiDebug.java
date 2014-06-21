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

import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.net.URL;
import java.net.URLConnection;
import javax.swing.JButton;
import javax.swing.JPanel;
import mediathek.MediathekGui;
import mediathek.controller.Log;
import mediathek.daten.Daten;
import mediathek.gui.dialogEinstellungen.PanelInfoStarts;
import mediathek.gui.dialogEinstellungen.PanelSenderLaden;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.MVNotification;
import msearch.daten.MSConfig;

public class GuiDebug extends JPanel {
    private final JButton[] buttonSender;
    private final String[] sender;
    private Daten daten;

    public GuiDebug(Daten d) {
        super();
        initComponents();
        daten = d;
        sender = Daten.filmeLaden.getSenderNamen();
        buttonSender = new JButton[sender.length];
        
        jPanel3.setLayout(new GridLayout(1, 1));
        jPanel3.add(new PanelInfoStarts());
        jPanel3.repaint();
        
        

        //Tab1 Sender löschen Panel füllen
        for (int i = 0; i < Daten.filmeLaden.getSenderNamen().length; ++i) {
            buttonSender[i] = new JButton(sender[i]);
            buttonSender[i].addActionListener(new BeobSenderLoeschen(sender[i]));
        }
        addSender();

        jToggleButtonAllesLaden.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                MSConfig.senderAllesLaden = jToggleButtonAllesLaden.isSelected();
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
                        Daten.filmeLaden.filmeBeimSenderSuchen(jToggleButtonAllesLaden.isSelected(), true);
                    }
                }).start();
            }
        });

        jPanelSenderLaden.add(new PanelSenderLaden());

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

        jTabbed1 = new javax.swing.JTabbedPane();
        jPanel2 = new javax.swing.JPanel();
        jPanelSenderLaden = new javax.swing.JPanel();
        jToggleButtonAllesLaden = new javax.swing.JToggleButton();
        jButtonFilmlisteLoeschen = new javax.swing.JButton();
        jButtonAllesSpeichern = new javax.swing.JButton();
        jButtonAlleLaden = new javax.swing.JButton();
        jPanel1 = new javax.swing.JPanel();
        jPanelSender = new javax.swing.JPanel();
        jPanelLoeschen = new javax.swing.JPanel();
        jPanel3 = new javax.swing.JPanel();
        jPanel5 = new javax.swing.JPanel();
        jButtonCheckUrl = new javax.swing.JButton();
        jTextFieldUrl = new javax.swing.JTextField();
        jButtonCheck = new javax.swing.JButton();
        jButtonGc = new javax.swing.JButton();
        jButtonNotify = new javax.swing.JButton();
        jButtonFehler = new javax.swing.JButton();

        jPanelSenderLaden.setBorder(javax.swing.BorderFactory.createTitledBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.LOWERED), "Sender starten"));
        jPanelSenderLaden.setLayout(new java.awt.BorderLayout());

        jToggleButtonAllesLaden.setText("[-alles] setzen");

        jButtonFilmlisteLoeschen.setText("Filmliste löschen");

        jButtonAllesSpeichern.setText("alles speichern");

        jButtonAlleLaden.setText("alle Sender laden");

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanelSenderLaden, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addComponent(jButtonFilmlisteLoeschen)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(jButtonAllesSpeichern)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(jButtonAlleLaden)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(jToggleButtonAllesLaden)
                        .addGap(0, 76, Short.MAX_VALUE)))
                .addContainerGap())
        );

        jPanel2Layout.linkSize(javax.swing.SwingConstants.HORIZONTAL, new java.awt.Component[] {jButtonAllesSpeichern, jButtonFilmlisteLoeschen, jToggleButtonAllesLaden});

        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonFilmlisteLoeschen)
                    .addComponent(jButtonAllesSpeichern)
                    .addComponent(jButtonAlleLaden)
                    .addComponent(jToggleButtonAllesLaden))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jPanelSenderLaden, javax.swing.GroupLayout.DEFAULT_SIZE, 429, Short.MAX_VALUE)
                .addContainerGap())
        );

        jTabbed1.addTab("Start Sender", jPanel2);

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
                .addContainerGap(666, Short.MAX_VALUE))
        );
        jPanelSenderLayout.setVerticalGroup(
            jPanelSenderLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelSenderLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanelLoeschen, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(320, Short.MAX_VALUE))
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

        jTabbed1.addTab("Del Sender", jPanel1);

        javax.swing.GroupLayout jPanel3Layout = new javax.swing.GroupLayout(jPanel3);
        jPanel3.setLayout(jPanel3Layout);
        jPanel3Layout.setHorizontalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 751, Short.MAX_VALUE)
        );
        jPanel3Layout.setVerticalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 490, Short.MAX_VALUE)
        );

        jTabbed1.addTab("Starts", jPanel3);

        jButtonCheckUrl.setText("get URL Filesize:");

        jButtonCheck.setText("Check Filmliste");

        jButtonGc.setText("Gc");

        jButtonNotify.setText("Notify");

        jButtonFehler.setText("Fehler ausgeben");

        javax.swing.GroupLayout jPanel5Layout = new javax.swing.GroupLayout(jPanel5);
        jPanel5.setLayout(jPanel5Layout);
        jPanel5Layout.setHorizontalGroup(
            jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel5Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel5Layout.createSequentialGroup()
                        .addGroup(jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING, false)
                            .addComponent(jButtonNotify, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(jButtonGc, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(jButtonCheck, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(jButtonCheckUrl, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldUrl, javax.swing.GroupLayout.DEFAULT_SIZE, 569, Short.MAX_VALUE))
                    .addGroup(jPanel5Layout.createSequentialGroup()
                        .addComponent(jButtonFehler)
                        .addGap(0, 0, Short.MAX_VALUE)))
                .addContainerGap())
        );
        jPanel5Layout.setVerticalGroup(
            jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel5Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonCheckUrl)
                    .addComponent(jTextFieldUrl, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addGap(18, 18, 18)
                .addComponent(jButtonCheck)
                .addGap(18, 18, 18)
                .addComponent(jButtonGc)
                .addGap(18, 18, 18)
                .addComponent(jButtonNotify)
                .addGap(18, 18, 18)
                .addComponent(jButtonFehler)
                .addContainerGap(281, Short.MAX_VALUE))
        );

        jPanel5Layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonCheckUrl, jTextFieldUrl});

        jTabbed1.addTab("Tool", jPanel5);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jTabbed1)
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jTabbed1)
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
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JPanel jPanelLoeschen;
    private javax.swing.JPanel jPanelSender;
    private javax.swing.JPanel jPanelSenderLaden;
    private javax.swing.JTabbedPane jTabbed1;
    private javax.swing.JTextField jTextFieldUrl;
    private javax.swing.JToggleButton jToggleButtonAllesLaden;
    // End of variables declaration//GEN-END:variables

    private class BeobSenderLoeschen implements ActionListener {

        private final String sender;

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
