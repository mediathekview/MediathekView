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
package mediathek.gui.dialogEinstellungen;

import java.awt.Component;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import mediathek.controller.filmeLaden.ListenerFilmeLaden;
import mediathek.controller.filmeLaden.ListenerFilmeLadenEvent;
import mediathek.daten.DDaten;
import mediathek.daten.Daten;
import mediathek.gui.PanelVorlage;
import mediathek.res.GetIcon;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.Konstanten;

public class PanelSenderLaden extends PanelVorlage {

    private JButton[] buttonSender;
    private String[] sender;

    public PanelSenderLaden(DDaten d, Component parentComponent) {
        super(d, parentComponent);
        initComponents();
        jButtonStop.setIcon(GetIcon.getIcon("stop_16.png"));
        ddaten = d;
        init();
        jSpinnerWarten.addChangeListener(new BeobSpinnerWarten());
    }

    private void init() {
        sender = Daten.filmeLaden.getSenderNamen();
        buttonSender = new JButton[sender.length];
        for (int i = 0; i < Daten.filmeLaden.getSenderNamen().length; ++i) {
            buttonSender[i] = new JButton(sender[i]);
            buttonSender[i].addActionListener(new BeobSenderLaden(sender[i]));
        }
        if (Daten.system[Konstanten.SYSTEM_WARTEN_NR].equals("")) {
            jSpinnerWarten.setValue(1);
            Daten.system[Konstanten.SYSTEM_WARTEN_NR] = "1";
        } else {
            jSpinnerWarten.setValue(Integer.parseInt(Daten.system[Konstanten.SYSTEM_WARTEN_NR]));
        }
        jButtonStop.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.filmeLaden.setStop();
            }
        });
        addSender();
        DDaten.filmeLaden.addAdListener(new ListenerFilmeLaden() {
            @Override
            public void progress_(ListenerFilmeLadenEvent event) {
                if (event.max == 0) {
                    jProgressBar1.setIndeterminate(true);
                    jProgressBar1.setMaximum(0);
                    jProgressBar1.setMinimum(0);
                    jProgressBar1.setValue(0);
                    jProgressBar1.setStringPainted(false);
                } else {
                    jProgressBar1.setIndeterminate(false);
                    jProgressBar1.setMaximum(event.max);
                    jProgressBar1.setMinimum(0);
                    jProgressBar1.setValue(event.progress);
                    jProgressBar1.setStringPainted(true);
                }
                jLabelProgress.setText(GuiFunktionen.textLaenge(80, event.text, true /* mitte */, false /*addVorne*/));
            }

            @Override
            public void fertig_(ListenerFilmeLadenEvent event) {
                jProgressBar1.setIndeterminate(false);
                jProgressBar1.setMaximum(0);
                jProgressBar1.setMinimum(0);
                jProgressBar1.setValue(0);
                jProgressBar1.setStringPainted(false);
                jLabelProgress.setText("");
                for (int i = 0; i < buttonSender.length; ++i) {
                    buttonSender[i].setEnabled(true);
                }
            }
        });
    }

    private void addSender() {
        panelSender.removeAll();
        GridBagLayout gridbag = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
        c.weightx = 0;
        panelSender.setLayout(gridbag);
        c.gridx = 0;
        c.gridy = 0;
        c.anchor = GridBagConstraints.WEST;
        c.fill = GridBagConstraints.BOTH;
        int nr = 0;
        int y = 0;
        int halbe = sender.length / 2;
        halbe += sender.length % 2;
        for (String aSender : sender) {
            c.gridy = y;
            addPanel(gridbag, c, aSender, nr);
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
        panelSender.add(label);
        panelSender.updateUI();
    }

    private void addPanel(GridBagLayout gridbag, GridBagConstraints c, String sender, int nr) {
        c.insets = new Insets(2, 10, 2, 2);
        gridbag.setConstraints(buttonSender[nr], c);
        panelSender.add(buttonSender[nr]);
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        buttonGroup1 = new javax.swing.ButtonGroup();
        jLayeredPane1 = new javax.swing.JLayeredPane();
        panelSender = new javax.swing.JPanel();
        jPanel4 = new javax.swing.JPanel();
        jSpinnerWarten = new javax.swing.JSpinner();
        jLabel7 = new javax.swing.JLabel();
        jLabel1 = new javax.swing.JLabel();
        jProgressBar1 = new javax.swing.JProgressBar();
        jLabelProgress = new javax.swing.JLabel();
        jButtonStop = new javax.swing.JButton();

        panelSender.setBorder(javax.swing.BorderFactory.createTitledBorder("Sender"));

        javax.swing.GroupLayout panelSenderLayout = new javax.swing.GroupLayout(panelSender);
        panelSender.setLayout(panelSenderLayout);
        panelSenderLayout.setHorizontalGroup(
            panelSenderLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 468, Short.MAX_VALUE)
        );
        panelSenderLayout.setVerticalGroup(
            panelSenderLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 28, Short.MAX_VALUE)
        );

        jPanel4.setBorder(javax.swing.BorderFactory.createEtchedBorder());

        jSpinnerWarten.setModel(new javax.swing.SpinnerNumberModel(Integer.valueOf(1), Integer.valueOf(1), null, Integer.valueOf(1)));

        jLabel7.setText("Laden um den Faktor bremsen");

        javax.swing.GroupLayout jPanel4Layout = new javax.swing.GroupLayout(jPanel4);
        jPanel4.setLayout(jPanel4Layout);
        jPanel4Layout.setHorizontalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel4Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jSpinnerWarten, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jLabel7)
                .addContainerGap(202, Short.MAX_VALUE))
        );
        jPanel4Layout.setVerticalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel4Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel7)
                    .addComponent(jSpinnerWarten, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jLabel1.setText("Filme von den Websiten der Sender laden, kann etwas dauern!");

        jLabelProgress.setText(" ");

        jButtonStop.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/stop_16.png"))); // NOI18N
        jButtonStop.setToolTipText("Abbrechen");

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanel4, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(panelSender, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jLabelProgress, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(jLabel1)
                        .addGap(0, 0, Short.MAX_VALUE))
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(jProgressBar1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonStop)))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabel1)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(panelSender, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jPanel4, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(18, 18, 18)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jProgressBar1, javax.swing.GroupLayout.PREFERRED_SIZE, 31, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jButtonStop))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jLabelProgress)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonStop, jProgressBar1});

    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.ButtonGroup buttonGroup1;
    private javax.swing.JButton jButtonStop;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JLabel jLabelProgress;
    private javax.swing.JLayeredPane jLayeredPane1;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JProgressBar jProgressBar1;
    private javax.swing.JSpinner jSpinnerWarten;
    private javax.swing.JPanel panelSender;
    // End of variables declaration//GEN-END:variables

    private class BeobSenderLaden implements ActionListener {

        private String sender;

        public BeobSenderLaden(String ssender) {
            sender = ssender;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            jLabelProgress.setText("");
            for (JButton aButtonSender : buttonSender) {
                aButtonSender.setEnabled(false);
            }
            Daten.filmeLaden.updateSender(new String[]{sender}, Daten.listeFilme, Daten.debug ? DDaten.filmeLaden.getAllesLaden() : false /* senderAllesLaden */);
        }
    }

    private class BeobSpinnerWarten implements ChangeListener {

        @Override
        public void stateChanged(ChangeEvent arg0) {
            Daten.system[Konstanten.SYSTEM_WARTEN_NR] =
                    String.valueOf(((Number) jSpinnerWarten.getModel().getValue()).intValue());
        }
    }
}
