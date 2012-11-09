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

import java.awt.BorderLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import mediathek.MediathekGui;
import mediathek.daten.DDaten;
import mediathek.daten.Daten;
import mediathek.gui.dialogEinstellungen.PanelListeFilmlistenServer;
import mediathek.gui.dialogEinstellungen.PanelSenderLaden;
import mediathek.tool.Konstanten;
import mediathek.tool.ListenerMediathekView;

public class GuiDebug extends PanelVorlage {

    private JButton[] buttonSender;
    private String[] sender;

    public GuiDebug(DDaten d) {
        super(d);
        initComponents();
        ddaten = d;
        sender = Daten.filmeLaden.getSenderNamen();
        buttonSender = new JButton[sender.length];
        for (int i = 0; i < Daten.filmeLaden.getSenderNamen().length; ++i) {
            buttonSender[i] = new JButton(sender[i]);
            buttonSender[i].addActionListener(new BeobSenderLoeschen(sender[i]));
        }
        addSender();
        jToggleButtonAllesLaden.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.filmeLaden.setAllesLaden(jToggleButtonAllesLaden.isSelected());
            }
        });
        jButtonAllesSpeichern.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                ddaten.allesSpeichern();
            }
        });
        jButtonFilmlisteLoeschen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                DDaten.listeFilme.clear();
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_FILMLISTE_GEAENDERT, MediathekGui.class.getSimpleName());
            }
        });
        jPanelSenderLaden.setLayout(new BorderLayout());
        jPanelSenderLaden.add(new PanelSenderLaden(ddaten));
        jPanelListen.setLayout(new BorderLayout());
        jPanelListen.add(new PanelListeFilmlistenServer(d));
    }

    private void addSender() {
        jPanelSender.removeAll();
        GridBagLayout gridbag = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
        c.weightx = 0;
        jPanelSender.setLayout(gridbag);
        c.gridx = 0;
        c.gridy = 0;
        c.anchor = GridBagConstraints.WEST;
        c.fill = GridBagConstraints.BOTH;
        int nr = 0;
        int y = 0;
        int halbe = sender.length / 2;
        halbe += sender.length % 2;
        for (int i = 0; i < sender.length; ++i) {
            c.gridy = y;
            addPanel(gridbag, c, sender[i], nr);
            ++nr;
            ++y;
            if (y >= halbe) {
                y = 0;
                c.gridx = 1;
            }
        }
        JLabel label = new JLabel();
        c.gridx = 4;
        c.weightx = 2;
        gridbag.setConstraints(label, c);
        jPanelSender.add(label);
        jPanelSender.updateUI();
    }

    private void addPanel(GridBagLayout gridbag, GridBagConstraints c, String sender, int nr) {
        c.insets = new Insets(2, 10, 2, 2);
        gridbag.setConstraints(buttonSender[nr], c);
        jPanelSender.add(buttonSender[nr]);
    }
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jToggleButtonAllesLaden = new javax.swing.JToggleButton();
        jPanelSender = new javax.swing.JPanel();
        jPanelSenderLaden = new javax.swing.JPanel();
        jButtonFilmlisteLoeschen = new javax.swing.JButton();
        jButtonAllesSpeichern = new javax.swing.JButton();
        jPanelListen = new javax.swing.JPanel();

        jToggleButtonAllesLaden.setText("[-alles] setzen");

        jPanelSender.setBorder(javax.swing.BorderFactory.createTitledBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.LOWERED), "Sender löschen"));

        javax.swing.GroupLayout jPanelSenderLayout = new javax.swing.GroupLayout(jPanelSender);
        jPanelSender.setLayout(jPanelSenderLayout);
        jPanelSenderLayout.setHorizontalGroup(
            jPanelSenderLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 0, Short.MAX_VALUE)
        );
        jPanelSenderLayout.setVerticalGroup(
            jPanelSenderLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 36, Short.MAX_VALUE)
        );

        jPanelSenderLaden.setBorder(javax.swing.BorderFactory.createTitledBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.LOWERED), "Sender starten"));

        javax.swing.GroupLayout jPanelSenderLadenLayout = new javax.swing.GroupLayout(jPanelSenderLaden);
        jPanelSenderLaden.setLayout(jPanelSenderLadenLayout);
        jPanelSenderLadenLayout.setHorizontalGroup(
            jPanelSenderLadenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 0, Short.MAX_VALUE)
        );
        jPanelSenderLadenLayout.setVerticalGroup(
            jPanelSenderLadenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 54, Short.MAX_VALUE)
        );

        jButtonFilmlisteLoeschen.setText("Filmliste löschen");

        jButtonAllesSpeichern.setText("alles speichern");

        javax.swing.GroupLayout jPanelListenLayout = new javax.swing.GroupLayout(jPanelListen);
        jPanelListen.setLayout(jPanelListenLayout);
        jPanelListenLayout.setHorizontalGroup(
            jPanelListenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 0, Short.MAX_VALUE)
        );
        jPanelListenLayout.setVerticalGroup(
            jPanelListenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 231, Short.MAX_VALUE)
        );

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jPanelListen, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addGroup(layout.createSequentialGroup()
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING, false)
                            .addComponent(jPanelSender, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(jToggleButtonAllesLaden, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, 143, Short.MAX_VALUE))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(layout.createSequentialGroup()
                                .addComponent(jButtonFilmlisteLoeschen)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jButtonAllesSpeichern)
                                .addGap(0, 208, Short.MAX_VALUE))
                            .addComponent(jPanelSenderLaden, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jToggleButtonAllesLaden)
                    .addComponent(jButtonFilmlisteLoeschen)
                    .addComponent(jButtonAllesSpeichern))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanelSender, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jPanelSenderLaden, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jPanelListen, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonAllesSpeichern;
    private javax.swing.JButton jButtonFilmlisteLoeschen;
    private javax.swing.JPanel jPanelListen;
    private javax.swing.JPanel jPanelSender;
    private javax.swing.JPanel jPanelSenderLaden;
    private javax.swing.JToggleButton jToggleButtonAllesLaden;
    // End of variables declaration//GEN-END:variables

    private class BeobSenderLoeschen implements ActionListener {

        private String sender;

        public BeobSenderLoeschen(String ssender) {
            sender = ssender;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            DDaten.listeFilme.delSender(sender);
            ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_FILMLISTE_GEAENDERT, MediathekGui.class.getSimpleName());
        }
    }
}
