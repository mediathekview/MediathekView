/*    
 *    MediathekView
 *    Copyright (C) 2012   W. Xaver
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

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JTextField;
import mediathek.Daten;
import mediathek.Konstanten;
import mediathek.Log;
import mediathek.daten.DDaten;
import mediathek.gui.PanelVorlage;
import mediathek.gui.beobachter.BeobWeb;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.GuiFunktionenProgramme;

public class PanelImportStandardPgruppe extends PanelVorlage {

    public JDialog dialog = null;

    public PanelImportStandardPgruppe(DDaten d) {
        super(d);
        initComponents();
        init();
        initBeob();
    }

    private void init() {
        jTextFieldVlcGefunden.setText(GuiFunktionenProgramme.getPfadVlc());
        jTextFieldFlvGefunden.setText(GuiFunktionenProgramme.getPfadFlv());
        if (jTextFieldVlcGefunden.getText().equals("")) {
            jRadioButtonVlcManuell.setSelected(true);
        } else {
            jRadioButtonVlcAuto.setSelected(true);
        }
        if (jTextFieldFlvGefunden.getText().equals("")) {
            jRadioButtonFlvManuell.setSelected(true);
        } else {
            jRadioButtonFlvAuto.setSelected(true);
        }
        jTextFieldZielpfad.setText(GuiFunktionen.getHomePath());
        jRadioButtonPfadFest.setSelected(true);
        setTextEditable();
    }

    private void initBeob() {
        jButtonVlcUrl.addActionListener(new BeobWeb(Konstanten.ADRESSE_VLC));
        jTextFieldUrlVlc.setText(Konstanten.ADRESSE_VLC);
        jButtonFlvUrl.addActionListener(new BeobWeb(Konstanten.ADRESSE_FLVSTREAMER));
        jTextFieldUrlFlv.setText(Konstanten.ADRESSE_FLVSTREAMER);
        jButtonVlcPfad.addActionListener(new BeobPfad(jTextFieldVlcAuswaehlen, true));
        jButtonFlvPfad.addActionListener(new BeobPfad(jTextFieldFlvAuswaehlen, true));
        jButtonZielpfad.addActionListener(new BeobPfad(jTextFieldZielpfad, false));
        jButtonVlcErneut.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                jTextFieldVlcGefunden.setText(GuiFunktionenProgramme.getPfadVlc());
            }
        });
        jButtonFlvErneut.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                jTextFieldFlvGefunden.setText(GuiFunktionenProgramme.getPfadFlv());
            }
        });
        jButtonUebernehmen.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                uebernehmen();
            }
        });
        jButtonReset.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                init();
            }
        });
        jRadioButtonVlcAuto.addActionListener(new BeobRadio());
        jRadioButtonVlcManuell.addActionListener(new BeobRadio());
        jRadioButtonFlvAuto.addActionListener(new BeobRadio());
        jRadioButtonFlvManuell.addActionListener(new BeobRadio());
        jRadioButtonPfadFest.addActionListener(new BeobRadio());
        jRadioButtonPfadFragen.addActionListener(new BeobRadio());
    }

    public void uebernehmen() {
        if (GuiFunktionenProgramme.addStandardprogramme(ddaten,
                jRadioButtonVlcAuto.isSelected() ? jTextFieldVlcGefunden.getText() : jTextFieldVlcAuswaehlen.getText(),
                jRadioButtonFlvAuto.isSelected() ? jTextFieldFlvGefunden.getText() : jTextFieldFlvAuswaehlen.getText(),
                jRadioButtonPfadFest.isSelected() ? jTextFieldZielpfad.getText() : "%p")) {
            if (dialog != null) {
                dialog.dispose();
            }
        }
    }

    private void setTextEditable() {
        jTextFieldVlcAuswaehlen.setEditable(jRadioButtonVlcManuell.isSelected());
        jButtonVlcPfad.setEnabled(jRadioButtonVlcManuell.isSelected());
        jButtonVlcErneut.setEnabled(!jRadioButtonVlcManuell.isSelected());
        if (jRadioButtonVlcAuto.isSelected()) {
            jPanelVlcGefunden.setBorder(javax.swing.BorderFactory.createTitledBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 51, 102)), "Der VLC-Player wurde gefunden"));
            jPanelVlcManuell.setBorder(javax.swing.BorderFactory.createTitledBorder("Pfad zum VLC-Player auswählen"));
        } else {
            jPanelVlcGefunden.setBorder(javax.swing.BorderFactory.createTitledBorder("Der VLC-Player wurde gefunden"));
            jPanelVlcManuell.setBorder(javax.swing.BorderFactory.createTitledBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 51, 102)), "Pfad zum VLC-Player auswählen"));
        }

        jTextFieldFlvAuswaehlen.setEditable(jRadioButtonFlvManuell.isSelected());
        jButtonFlvPfad.setEnabled(jRadioButtonFlvManuell.isSelected());
        jButtonFlvErneut.setEnabled(!jRadioButtonFlvManuell.isSelected());
        if (jRadioButtonFlvAuto.isSelected()) {
            jPanelFlvGefunden.setBorder(javax.swing.BorderFactory.createTitledBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 51, 102)), "Der flvstreamer wurde gefunden"));
            jPanelFlvManuell.setBorder(javax.swing.BorderFactory.createTitledBorder("Pfad zum flvstreamer auswählen"));
        } else {
            jPanelFlvGefunden.setBorder(javax.swing.BorderFactory.createTitledBorder("Der flvstreamer wurde gefunden"));
            jPanelFlvManuell.setBorder(javax.swing.BorderFactory.createTitledBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 51, 102)), "Pfad zum flvstreamer auswählen"));
        }

        jTextFieldZielpfad.setEditable(jRadioButtonPfadFest.isSelected());
        jButtonZielpfad.setEnabled(jRadioButtonPfadFest.isSelected());
        if (jRadioButtonPfadFest.isSelected()) {
            jPanelZielpfadFest.setBorder(javax.swing.BorderFactory.createTitledBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 51, 102)), "Zielpfad in dem die Filme gespeichert werden"));
            jPanelZielpfadFragen.setBorder(javax.swing.BorderFactory.createTitledBorder("Bei jedem Film nach dem Pfad fragen"));
        } else {
            jPanelZielpfadFest.setBorder(javax.swing.BorderFactory.createTitledBorder("Zielpfad in dem die Filme gespeichert werden"));
            jPanelZielpfadFragen.setBorder(javax.swing.BorderFactory.createTitledBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 51, 102)), "Bei jedem Film nach dem Pfad fragen"));
        }
    }

    /** This method is called from within the constructor to
     *    initialize the form.
     *    WARNING: Do NOT modify this code. The content of this method is
     *    always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        buttonGroup1 = new javax.swing.ButtonGroup();
        buttonGroup2 = new javax.swing.ButtonGroup();
        buttonGroup3 = new javax.swing.ButtonGroup();
        jTabbedPane1 = new javax.swing.JTabbedPane();
        jPanel3 = new javax.swing.JPanel();
        jPanelVlcGefunden = new javax.swing.JPanel();
        jTextFieldVlcGefunden = new javax.swing.JTextField();
        jButtonVlcErneut = new javax.swing.JButton();
        jPanelVlcManuell = new javax.swing.JPanel();
        jTextFieldVlcAuswaehlen = new javax.swing.JTextField();
        jButtonVlcPfad = new javax.swing.JButton();
        jPanel2 = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        jTextFieldUrlVlc = new javax.swing.JTextField();
        jButtonVlcUrl = new javax.swing.JButton();
        jRadioButtonVlcAuto = new javax.swing.JRadioButton();
        jRadioButtonVlcManuell = new javax.swing.JRadioButton();
        jPanel6 = new javax.swing.JPanel();
        jPanelFlvGefunden = new javax.swing.JPanel();
        jTextFieldFlvGefunden = new javax.swing.JTextField();
        jButtonFlvErneut = new javax.swing.JButton();
        jPanelFlvManuell = new javax.swing.JPanel();
        jTextFieldFlvAuswaehlen = new javax.swing.JTextField();
        jButtonFlvPfad = new javax.swing.JButton();
        jPanel8 = new javax.swing.JPanel();
        jLabel2 = new javax.swing.JLabel();
        jTextFieldUrlFlv = new javax.swing.JTextField();
        jButtonFlvUrl = new javax.swing.JButton();
        jRadioButtonFlvAuto = new javax.swing.JRadioButton();
        jRadioButtonFlvManuell = new javax.swing.JRadioButton();
        jPanel4 = new javax.swing.JPanel();
        jPanelZielpfadFest = new javax.swing.JPanel();
        jTextFieldZielpfad = new javax.swing.JTextField();
        jButtonZielpfad = new javax.swing.JButton();
        jRadioButtonPfadFest = new javax.swing.JRadioButton();
        jRadioButtonPfadFragen = new javax.swing.JRadioButton();
        jPanelZielpfadFragen = new javax.swing.JPanel();
        jTextFieldZielpfad1 = new javax.swing.JTextField();
        jButtonUebernehmen = new javax.swing.JButton();
        jButtonReset = new javax.swing.JButton();
        jButton1 = new javax.swing.JButton();

        jPanelVlcGefunden.setBorder(javax.swing.BorderFactory.createTitledBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 51, 102)), "Der VLC-Player wurde gefunden"));

        jTextFieldVlcGefunden.setEditable(false);

        jButtonVlcErneut.setText("erneut suchen");

        javax.swing.GroupLayout jPanelVlcGefundenLayout = new javax.swing.GroupLayout(jPanelVlcGefunden);
        jPanelVlcGefunden.setLayout(jPanelVlcGefundenLayout);
        jPanelVlcGefundenLayout.setHorizontalGroup(
            jPanelVlcGefundenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelVlcGefundenLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelVlcGefundenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jTextFieldVlcGefunden)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanelVlcGefundenLayout.createSequentialGroup()
                        .addGap(0, 0, Short.MAX_VALUE)
                        .addComponent(jButtonVlcErneut)))
                .addContainerGap())
        );
        jPanelVlcGefundenLayout.setVerticalGroup(
            jPanelVlcGefundenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelVlcGefundenLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jTextFieldVlcGefunden, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jButtonVlcErneut)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanelVlcGefundenLayout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonVlcErneut, jTextFieldVlcGefunden});

        jPanelVlcManuell.setBorder(javax.swing.BorderFactory.createTitledBorder("Pfad zum VLC-Player auswählen"));

        jButtonVlcPfad.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/fileopen_16.png"))); // NOI18N

        javax.swing.GroupLayout jPanelVlcManuellLayout = new javax.swing.GroupLayout(jPanelVlcManuell);
        jPanelVlcManuell.setLayout(jPanelVlcManuellLayout);
        jPanelVlcManuellLayout.setHorizontalGroup(
            jPanelVlcManuellLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelVlcManuellLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jTextFieldVlcAuswaehlen)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jButtonVlcPfad)
                .addContainerGap())
        );
        jPanelVlcManuellLayout.setVerticalGroup(
            jPanelVlcManuellLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelVlcManuellLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelVlcManuellLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jButtonVlcPfad)
                    .addComponent(jTextFieldVlcAuswaehlen, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanelVlcManuellLayout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonVlcPfad, jTextFieldVlcAuswaehlen});

        jPanel2.setBorder(javax.swing.BorderFactory.createTitledBorder(""));

        jLabel1.setText("VLC von der Website laden und installieren:");

        jTextFieldUrlVlc.setEditable(false);
        jTextFieldUrlVlc.setFont(new java.awt.Font("Dialog", 1, 12)); // NOI18N
        jTextFieldUrlVlc.setForeground(new java.awt.Color(51, 51, 255));
        jTextFieldUrlVlc.setText("http://www.videolan.org/");
        jTextFieldUrlVlc.setBorder(null);

        jButtonVlcUrl.setText("im Browser öffnen");

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jTextFieldUrlVlc, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel1))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 143, Short.MAX_VALUE)
                .addComponent(jButtonVlcUrl)
                .addContainerGap())
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel2Layout.createSequentialGroup()
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jButtonVlcUrl)
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addComponent(jLabel1)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldUrlVlc, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)))
                .addContainerGap())
        );

        jPanel2Layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonVlcUrl, jTextFieldUrlVlc});

        buttonGroup1.add(jRadioButtonVlcAuto);

        buttonGroup1.add(jRadioButtonVlcManuell);

        javax.swing.GroupLayout jPanel3Layout = new javax.swing.GroupLayout(jPanel3);
        jPanel3.setLayout(jPanel3Layout);
        jPanel3Layout.setHorizontalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel3Layout.createSequentialGroup()
                        .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jRadioButtonVlcAuto)
                            .addComponent(jRadioButtonVlcManuell))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jPanelVlcGefunden, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(jPanelVlcManuell, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)))
                    .addComponent(jPanel2, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        jPanel3Layout.setVerticalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanelVlcGefunden, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jRadioButtonVlcAuto))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanelVlcManuell, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jRadioButtonVlcManuell))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 56, Short.MAX_VALUE)
                .addComponent(jPanel2, javax.swing.GroupLayout.PREFERRED_SIZE, 78, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap())
        );

        jTabbedPane1.addTab("VLC-Player", jPanel3);

        jPanelFlvGefunden.setBorder(javax.swing.BorderFactory.createTitledBorder("Der flvstreamer wurde gefunden"));

        jTextFieldFlvGefunden.setEditable(false);

        jButtonFlvErneut.setText("erneut suchen");

        javax.swing.GroupLayout jPanelFlvGefundenLayout = new javax.swing.GroupLayout(jPanelFlvGefunden);
        jPanelFlvGefunden.setLayout(jPanelFlvGefundenLayout);
        jPanelFlvGefundenLayout.setHorizontalGroup(
            jPanelFlvGefundenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelFlvGefundenLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelFlvGefundenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jTextFieldFlvGefunden)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanelFlvGefundenLayout.createSequentialGroup()
                        .addGap(0, 457, Short.MAX_VALUE)
                        .addComponent(jButtonFlvErneut)))
                .addContainerGap())
        );
        jPanelFlvGefundenLayout.setVerticalGroup(
            jPanelFlvGefundenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelFlvGefundenLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jTextFieldFlvGefunden, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jButtonFlvErneut)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanelFlvGefundenLayout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonFlvErneut, jTextFieldFlvGefunden});

        jPanelFlvManuell.setBorder(javax.swing.BorderFactory.createTitledBorder("Pfad zum flvstreamer auswählen"));

        jButtonFlvPfad.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/fileopen_16.png"))); // NOI18N

        javax.swing.GroupLayout jPanelFlvManuellLayout = new javax.swing.GroupLayout(jPanelFlvManuell);
        jPanelFlvManuell.setLayout(jPanelFlvManuellLayout);
        jPanelFlvManuellLayout.setHorizontalGroup(
            jPanelFlvManuellLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelFlvManuellLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jTextFieldFlvAuswaehlen)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jButtonFlvPfad)
                .addContainerGap())
        );
        jPanelFlvManuellLayout.setVerticalGroup(
            jPanelFlvManuellLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelFlvManuellLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelFlvManuellLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jButtonFlvPfad)
                    .addComponent(jTextFieldFlvAuswaehlen, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanelFlvManuellLayout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonFlvPfad, jTextFieldFlvAuswaehlen});

        jPanel8.setBorder(javax.swing.BorderFactory.createTitledBorder(""));

        jLabel2.setText("flvstreamer von der Website laden und installieren:");

        jTextFieldUrlFlv.setEditable(false);
        jTextFieldUrlFlv.setFont(new java.awt.Font("Dialog", 1, 12)); // NOI18N
        jTextFieldUrlFlv.setForeground(new java.awt.Color(51, 51, 255));
        jTextFieldUrlFlv.setText("https://savannah.nongnu.org/projects/flvstreamer");
        jTextFieldUrlFlv.setBorder(null);

        jButtonFlvUrl.setText("im Browser öffnen");

        javax.swing.GroupLayout jPanel8Layout = new javax.swing.GroupLayout(jPanel8);
        jPanel8.setLayout(jPanel8Layout);
        jPanel8Layout.setHorizontalGroup(
            jPanel8Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel8Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel8Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel8Layout.createSequentialGroup()
                        .addComponent(jLabel2)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jButtonFlvUrl))
                    .addGroup(jPanel8Layout.createSequentialGroup()
                        .addComponent(jTextFieldUrlFlv, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addGap(0, 0, Short.MAX_VALUE)))
                .addContainerGap())
        );
        jPanel8Layout.setVerticalGroup(
            jPanel8Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel8Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel8Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel2)
                    .addComponent(jButtonFlvUrl))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jTextFieldUrlFlv, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        buttonGroup2.add(jRadioButtonFlvAuto);

        buttonGroup2.add(jRadioButtonFlvManuell);

        javax.swing.GroupLayout jPanel6Layout = new javax.swing.GroupLayout(jPanel6);
        jPanel6.setLayout(jPanel6Layout);
        jPanel6Layout.setHorizontalGroup(
            jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel6Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel6Layout.createSequentialGroup()
                        .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jRadioButtonFlvAuto)
                            .addComponent(jRadioButtonFlvManuell))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jPanelFlvGefunden, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(jPanelFlvManuell, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)))
                    .addComponent(jPanel8, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        jPanel6Layout.setVerticalGroup(
            jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel6Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanelFlvGefunden, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jRadioButtonFlvAuto))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanelFlvManuell, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jRadioButtonFlvManuell))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 54, Short.MAX_VALUE)
                .addComponent(jPanel8, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap())
        );

        jTabbedPane1.addTab("flvstreamer", jPanel6);

        jPanelZielpfadFest.setBorder(javax.swing.BorderFactory.createTitledBorder("Zielpfad in dem die Filme gespeichert werden"));

        jButtonZielpfad.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/fileopen_16.png"))); // NOI18N

        javax.swing.GroupLayout jPanelZielpfadFestLayout = new javax.swing.GroupLayout(jPanelZielpfadFest);
        jPanelZielpfadFest.setLayout(jPanelZielpfadFestLayout);
        jPanelZielpfadFestLayout.setHorizontalGroup(
            jPanelZielpfadFestLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelZielpfadFestLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jTextFieldZielpfad, javax.swing.GroupLayout.DEFAULT_SIZE, 536, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jButtonZielpfad)
                .addContainerGap())
        );
        jPanelZielpfadFestLayout.setVerticalGroup(
            jPanelZielpfadFestLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelZielpfadFestLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelZielpfadFestLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jButtonZielpfad)
                    .addComponent(jTextFieldZielpfad, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanelZielpfadFestLayout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonZielpfad, jTextFieldZielpfad});

        buttonGroup3.add(jRadioButtonPfadFest);

        buttonGroup3.add(jRadioButtonPfadFragen);

        jPanelZielpfadFragen.setBorder(javax.swing.BorderFactory.createTitledBorder("Bei jedem Film nach dem Pfad fragen"));

        jTextFieldZielpfad1.setEditable(false);
        jTextFieldZielpfad1.setText("Fragen?");

        javax.swing.GroupLayout jPanelZielpfadFragenLayout = new javax.swing.GroupLayout(jPanelZielpfadFragen);
        jPanelZielpfadFragen.setLayout(jPanelZielpfadFragenLayout);
        jPanelZielpfadFragenLayout.setHorizontalGroup(
            jPanelZielpfadFragenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelZielpfadFragenLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jTextFieldZielpfad1)
                .addContainerGap())
        );
        jPanelZielpfadFragenLayout.setVerticalGroup(
            jPanelZielpfadFragenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelZielpfadFragenLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jTextFieldZielpfad1, javax.swing.GroupLayout.PREFERRED_SIZE, 27, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        javax.swing.GroupLayout jPanel4Layout = new javax.swing.GroupLayout(jPanel4);
        jPanel4.setLayout(jPanel4Layout);
        jPanel4Layout.setHorizontalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel4Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel4Layout.createSequentialGroup()
                        .addComponent(jRadioButtonPfadFest)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(jPanelZielpfadFest, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                    .addGroup(jPanel4Layout.createSequentialGroup()
                        .addComponent(jRadioButtonPfadFragen)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(jPanelZielpfadFragen, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)))
                .addContainerGap())
        );
        jPanel4Layout.setVerticalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel4Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jRadioButtonPfadFest)
                    .addComponent(jPanelZielpfadFest, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addGap(18, 18, 18)
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jRadioButtonPfadFragen)
                    .addComponent(jPanelZielpfadFragen, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(175, Short.MAX_VALUE))
        );

        jTabbedPane1.addTab("Zielpfad", jPanel4);

        jButtonUebernehmen.setText("Programme hinzufügen");

        jButtonReset.setText("Reset");

        jButton1.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/help_16.png"))); // NOI18N

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jTabbedPane1)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                        .addGap(0, 0, Short.MAX_VALUE)
                        .addComponent(jButtonUebernehmen)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonReset)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButton1)))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jTabbedPane1)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonUebernehmen)
                    .addComponent(jButtonReset)
                    .addComponent(jButton1))
                .addContainerGap())
        );

        layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButton1, jButtonReset, jButtonUebernehmen});

    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.ButtonGroup buttonGroup1;
    private javax.swing.ButtonGroup buttonGroup2;
    private javax.swing.ButtonGroup buttonGroup3;
    private javax.swing.JButton jButton1;
    private javax.swing.JButton jButtonFlvErneut;
    private javax.swing.JButton jButtonFlvPfad;
    private javax.swing.JButton jButtonFlvUrl;
    private javax.swing.JButton jButtonReset;
    private javax.swing.JButton jButtonUebernehmen;
    private javax.swing.JButton jButtonVlcErneut;
    private javax.swing.JButton jButtonVlcPfad;
    private javax.swing.JButton jButtonVlcUrl;
    private javax.swing.JButton jButtonZielpfad;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel6;
    private javax.swing.JPanel jPanel8;
    private javax.swing.JPanel jPanelFlvGefunden;
    private javax.swing.JPanel jPanelFlvManuell;
    private javax.swing.JPanel jPanelVlcGefunden;
    private javax.swing.JPanel jPanelVlcManuell;
    private javax.swing.JPanel jPanelZielpfadFest;
    private javax.swing.JPanel jPanelZielpfadFragen;
    private javax.swing.JRadioButton jRadioButtonFlvAuto;
    private javax.swing.JRadioButton jRadioButtonFlvManuell;
    private javax.swing.JRadioButton jRadioButtonPfadFest;
    private javax.swing.JRadioButton jRadioButtonPfadFragen;
    private javax.swing.JRadioButton jRadioButtonVlcAuto;
    private javax.swing.JRadioButton jRadioButtonVlcManuell;
    private javax.swing.JTabbedPane jTabbedPane1;
    private javax.swing.JTextField jTextFieldFlvAuswaehlen;
    private javax.swing.JTextField jTextFieldFlvGefunden;
    private javax.swing.JTextField jTextFieldUrlFlv;
    private javax.swing.JTextField jTextFieldUrlVlc;
    private javax.swing.JTextField jTextFieldVlcAuswaehlen;
    private javax.swing.JTextField jTextFieldVlcGefunden;
    private javax.swing.JTextField jTextFieldZielpfad;
    private javax.swing.JTextField jTextFieldZielpfad1;
    // End of variables declaration//GEN-END:variables

    private class BeobRadio implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            setTextEditable();
        }
    }

    private class BeobPfad implements ActionListener {

        private JTextField textField;
        private boolean datei;

        public BeobPfad(JTextField ttextField, boolean ddatei) {
            textField = ttextField;
            datei = ddatei;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            int returnVal;
            JFileChooser chooser = new JFileChooser();
            if (datei) {
                chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            } else {
                chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            }
            chooser.setFileHidingEnabled(false);
            if (textField.getText().equals("")) {
                chooser.setCurrentDirectory(new File(GuiFunktionen.getHomePath()));
            } else {
                chooser.setCurrentDirectory(new File(textField.getText()));
            }
            returnVal = chooser.showOpenDialog(null);
            if (returnVal == JFileChooser.APPROVE_OPTION) {
                try {
                    textField.setText(chooser.getSelectedFile().getAbsolutePath());
                } catch (Exception ex) {
                    Log.fehlerMeldung("PanelImportStandardProgramme.BeobPfad", ex);
                }
            }
        }
    }
}
