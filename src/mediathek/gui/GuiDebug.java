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
import mediathek.controller.Log;
import mediathek.daten.Daten;
import mediathek.gui.dialogEinstellungen.PanelInfoStarts;
import mediathek.tool.Duration;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.MVNotification;
import msearch.io.MSFilmlisteLesen;

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
        jButtonNeuLaden.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent ae) {
                Daten.listeFilme.clear();
                Duration duration = new Duration(MediathekGui.class.getSimpleName());
                duration.ping("Start");
                new MSFilmlisteLesen().readFilmListe(Daten.getDateiFilmliste(), Daten.listeFilme);
//                    new FilmListReader().readFilmListe(new URI("http://www.wp11128329.server-he.de/filme/Filmliste-akt.xz"), Daten.listeFilme);
                duration.ping("Fertig");
                Daten.listeFilme.themenLaden();
                Daten.listeAbo.setAboFuerFilm(Daten.listeFilme, false /*aboLoeschen*/);
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_FILMLISTE_GEAENDERT, MediathekGui.class.getSimpleName());
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

        javax.swing.JTabbedPane jTabbedSender = new javax.swing.JTabbedPane();
        javax.swing.JPanel jPanel1 = new javax.swing.JPanel();
        jPanelSender = new javax.swing.JPanel();
        jPanelLoeschen = new javax.swing.JPanel();
        jButtonFilmlisteLoeschen = new javax.swing.JButton();
        jButtonNeuLaden = new javax.swing.JButton();
        jButtonCheck = new javax.swing.JButton();
        javax.swing.JPanel jPanel5 = new javax.swing.JPanel();
        jButtonCheckUrl = new javax.swing.JButton();
        jTextFieldUrl = new javax.swing.JTextField();
        jButtonGc = new javax.swing.JButton();
        jButtonNotify = new javax.swing.JButton();
        jButtonFehler = new javax.swing.JButton();
        jButtonAllesSpeichern = new javax.swing.JButton();
        jPanel3 = new javax.swing.JPanel();

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
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        jPanelSenderLayout.setVerticalGroup(
            jPanelSenderLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelSenderLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanelLoeschen, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(240, Short.MAX_VALUE))
        );

        jButtonFilmlisteLoeschen.setText("gesamte Filmliste löschen");

        jButtonNeuLaden.setText("gespeicherte Filmliste neu laden");

        jButtonCheck.setText("Check Filmliste");

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanelSender, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(jPanel1Layout.createSequentialGroup()
                                .addComponent(jButtonFilmlisteLoeschen)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addComponent(jButtonNeuLaden))
                            .addComponent(jButtonCheck))
                        .addGap(0, 103, Short.MAX_VALUE)))
                .addContainerGap())
        );

        jPanel1Layout.linkSize(javax.swing.SwingConstants.HORIZONTAL, new java.awt.Component[] {jButtonCheck, jButtonFilmlisteLoeschen, jButtonNeuLaden});

        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonFilmlisteLoeschen)
                    .addComponent(jButtonNeuLaden))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jButtonCheck)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jPanelSender, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap())
        );

        jTabbedSender.addTab("Filmliste", jPanel1);

        jButtonCheckUrl.setText("get URL Filesize:");

        jButtonGc.setText("Gc");

        jButtonNotify.setText("Notify");

        jButtonFehler.setText("Fehler ausgeben");

        jButtonAllesSpeichern.setText("alles speichern");

        javax.swing.GroupLayout jPanel5Layout = new javax.swing.GroupLayout(jPanel5);
        jPanel5.setLayout(jPanel5Layout);
        jPanel5Layout.setHorizontalGroup(
            jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel5Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel5Layout.createSequentialGroup()
                        .addComponent(jButtonCheckUrl)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldUrl, javax.swing.GroupLayout.DEFAULT_SIZE, 489, Short.MAX_VALUE))
                    .addGroup(jPanel5Layout.createSequentialGroup()
                        .addGroup(jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                            .addComponent(jButtonFehler, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(jButtonNotify, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(jButtonGc, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(jButtonAllesSpeichern, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                        .addGap(0, 0, Short.MAX_VALUE)))
                .addContainerGap())
        );
        jPanel5Layout.setVerticalGroup(
            jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel5Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jButtonAllesSpeichern)
                .addGap(18, 18, 18)
                .addComponent(jButtonGc)
                .addGap(18, 18, 18)
                .addComponent(jButtonNotify)
                .addGap(18, 18, 18)
                .addComponent(jButtonFehler)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 282, Short.MAX_VALUE)
                .addGroup(jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonCheckUrl)
                    .addComponent(jTextFieldUrl, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap())
        );

        jPanel5Layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonCheckUrl, jTextFieldUrl});

        jTabbedSender.addTab("Tool", jPanel5);

        javax.swing.GroupLayout jPanel3Layout = new javax.swing.GroupLayout(jPanel3);
        jPanel3.setLayout(jPanel3Layout);
        jPanel3Layout.setHorizontalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 671, Short.MAX_VALUE)
        );
        jPanel3Layout.setVerticalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 485, Short.MAX_VALUE)
        );

        jTabbedSender.addTab("Starts", jPanel3);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jTabbedSender)
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jTabbedSender)
                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonAllesSpeichern;
    private javax.swing.JButton jButtonCheck;
    private javax.swing.JButton jButtonCheckUrl;
    private javax.swing.JButton jButtonFehler;
    private javax.swing.JButton jButtonFilmlisteLoeschen;
    private javax.swing.JButton jButtonGc;
    private javax.swing.JButton jButtonNeuLaden;
    private javax.swing.JButton jButtonNotify;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanelLoeschen;
    private javax.swing.JPanel jPanelSender;
    private javax.swing.JTextField jTextFieldUrl;
    // End of variables declaration//GEN-END:variables

    private class BeobSenderLoeschen implements ActionListener {

        private final String sender;

        public BeobSenderLoeschen(String ssender) {
            sender = ssender;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            Daten.listeFilme.deleteAllFilms(sender);
            ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_FILMLISTE_GEAENDERT, MediathekGui.class.getSimpleName());
        }
    }
}
