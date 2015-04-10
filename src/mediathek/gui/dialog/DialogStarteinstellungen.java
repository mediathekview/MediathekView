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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.GroupLayout;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import mediathek.daten.Daten;
import mediathek.daten.ListePset;
import mediathek.daten.ListePsetVorlagen;
import mediathek.gui.dialogEinstellungen.PanelEinstellungenGeo;
import mediathek.gui.dialogEinstellungen.PanelProgrammPfade;
import mediathek.gui.dialogEinstellungen.PanelPsetKurz;
import mediathek.gui.dialogEinstellungen.PanelPsetLang;
import static mediathek.tool.Funktionen.getOs;
import mediathek.tool.GuiFunktionenProgramme;
import mediathek.tool.MVConfig;

public class DialogStarteinstellungen extends javax.swing.JDialog {

    Daten daten;
    private final static int STAT_START = 1;
    private final static int STAT_PFAD = 2;
    private final static int STAT_PSET = 3;
    private final static int STAT_FERTIG = 4;
    private int status = STAT_START;
    private final JFrame parentComponent;
    JCheckBox jCheckBox = new JCheckBox("Einmal am Tag nach einer neuen Programmversion suchen");

    public DialogStarteinstellungen(JFrame parent, Daten dd) {
        super(parent, true);
        parentComponent = parent;
        initComponents();
        daten = dd;
        this.setTitle("Erster Start");
        jButtonStandard.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                jCheckBoxAnpassen.setVisible(false);
                weiter();
            }
        });
        jCheckBoxAlleEinstellungen.setVisible(false);
        jCheckBoxAlleEinstellungen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                status = STAT_PSET;
                weiter();
            }
        });
        Daten.mVConfig.add(MVConfig.SYSTEM_UPDATE_SUCHEN, Boolean.TRUE.toString());
        // setzt die Standardpfade für die wichtigsten Programme
        Daten.mVConfig.add(MVConfig.SYSTEM_PFAD_MPLAYER, GuiFunktionenProgramme.getMusterPfadMplayer());
        Daten.mVConfig.add(MVConfig.SYSTEM_PFAD_VLC, GuiFunktionenProgramme.getMusterPfadVlc());
        Daten.mVConfig.add(MVConfig.SYSTEM_PFAD_FLVSTREAMER, GuiFunktionenProgramme.getMusterPfadFlv());
        Daten.mVConfig.add(MVConfig.SYSTEM_PFAD_FFMPEG, GuiFunktionenProgramme.getMusterPfadFFmpeg());

        PanelEinstellungenGeo panelEinstellungenGeo = new PanelEinstellungenGeo(dd, parentComponent);
        jCheckBox = new JCheckBox("Einmal am Tag nach einer neuen Programmversion suchen");
        jCheckBox.setSelected(true);
        jCheckBox.addActionListener(new BeobCheckBoxSuchen());
        GroupLayout extraLayout = new javax.swing.GroupLayout(jPanelExtra);
        jPanelExtra.setLayout(extraLayout);
        extraLayout.setHorizontalGroup(
                extraLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addGroup(extraLayout.createSequentialGroup()
                        .addContainerGap()
                        .addGroup(extraLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                .addComponent(panelEinstellungenGeo)
                                .addComponent(jCheckBox)
                        )
                        .addContainerGap()
                ));

        extraLayout.setVerticalGroup(
                extraLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addGroup(extraLayout.createSequentialGroup()
                        .addContainerGap()
                        .addComponent(panelEinstellungenGeo)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(jCheckBox)
                        .addContainerGap())
        );

    }

    private void weiter() {
        switch (status) {
            case STAT_START:
                statusStart();
                break;
            case STAT_PFAD:
                statusPfade();
                break;
            case STAT_PSET:
                statusPset();
                break;
            default:
                beenden();
                break;
        }
    }

    private void statusStart() {
        jButtonStandard.setText("Weiter");
        if (Daten.mVConfig.get(MVConfig.SYSTEM_PFAD_VLC).equals("")
                || Daten.mVConfig.get(MVConfig.SYSTEM_PFAD_FLVSTREAMER).equals("")
                || Daten.mVConfig.get(MVConfig.SYSTEM_PFAD_FFMPEG).equals("")) {
            // ein Programm (VLC, flvstreamer) wurde nicht gefunden, muss der Benutzer eintragen
            status = STAT_PFAD;
        } else if (jCheckBoxAnpassen.isSelected()) {
            // der Benutzer wills verstellen
            status = STAT_PFAD;
        } else {
            // nur dann automatisch Standardprogramme einrichten, sonst fragen
            //ListePset pSet = GuiFunktionenProgramme.getStandardprogramme(ddaten);
            if (addStandarSet(parentComponent, daten)) {
                status = STAT_FERTIG;
            } else {
                status = STAT_PSET;
            }
        }
        weiter();
    }

    private void statusPfade() {
        // erst Programmpfad prüfen
        jCheckBoxAnpassen.setVisible(false);
        jCheckBoxAlleEinstellungen.setVisible(false);
        switch (getOs()) {
            case MAC:
            case WIN32:
            case WIN64:
                // da wird nur der VLC gebraucht, der Rest wird mitgeliefert
                jScrollPane1.setViewportView(new PanelProgrammPfade(parentComponent, true /* vlc */, false /* flvstreamer */, false /* mplayer */, false /*ffmpeg*/));
                break;
            default:
                // da brauchs alles
                jScrollPane1.setViewportView(new PanelProgrammPfade(parentComponent, true /* vlc */, true /* flvstreamer */, false /* mplayer */, true /*ffmpeg*/));
        }
        status = STAT_PSET;
        jButtonStandard.setText("Weiter");
    }

    private void statusPset() {
        // Einstellungen zum Ansehen und Speichern der Filme anpassen
        jCheckBoxAnpassen.setVisible(false);
        jCheckBoxAlleEinstellungen.setVisible(true);
        if (Daten.listePset.size() == 0) {
            // Standardset hinzufügen
            addStandarSet(parentComponent, daten);
        }
        if (jCheckBoxAlleEinstellungen.isSelected()) {
            jScrollPane1.setViewportView(new PanelPsetLang(daten, parentComponent, Daten.listePset));
        } else {
            jScrollPane1.setViewportView(new PanelPsetKurz(daten, parentComponent, Daten.listePset));
        }
        status = STAT_FERTIG;
        jButtonStandard.setText("Weiter");
    }

    private boolean addStandarSet(JFrame parent, Daten daten) {
        boolean ret = false;
        ListePset pSet = ListePsetVorlagen.getStandarset(parent, daten, true /*replaceMuster*/);
        if (pSet != null) {
            Daten.listePset.addPset(pSet);
            Daten.mVConfig.add(MVConfig.SYSTEM_VERSION_PROGRAMMSET, pSet.version);
            ret = true;
        }
        return ret;
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

        javax.swing.JPanel jPanel2 = new javax.swing.JPanel();
        jButtonStandard = new javax.swing.JButton();
        jCheckBoxAlleEinstellungen = new javax.swing.JCheckBox();
        jCheckBoxAnpassen = new javax.swing.JCheckBox();
        jScrollPane1 = new javax.swing.JScrollPane();
        jPanelExtra = new javax.swing.JPanel();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);

        jPanel2.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 255), 3));

        jButtonStandard.setText("Mit Standardeinstellungen starten");

        jCheckBoxAlleEinstellungen.setText("alle Einstellungen anzeigen");

        jCheckBoxAnpassen.setText("vorher anpassen");

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jCheckBoxAlleEinstellungen)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addComponent(jCheckBoxAnpassen)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jButtonStandard)
                .addContainerGap())
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonStandard)
                    .addComponent(jCheckBoxAlleEinstellungen)
                    .addComponent(jCheckBoxAnpassen))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        javax.swing.GroupLayout jPanelExtraLayout = new javax.swing.GroupLayout(jPanelExtra);
        jPanelExtra.setLayout(jPanelExtraLayout);
        jPanelExtraLayout.setHorizontalGroup(
            jPanelExtraLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 679, Short.MAX_VALUE)
        );
        jPanelExtraLayout.setVerticalGroup(
            jPanelExtraLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 510, Short.MAX_VALUE)
        );

        jScrollPane1.setViewportView(jPanelExtra);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPane1)
                    .addComponent(jPanel2, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jScrollPane1)
                .addGap(18, 18, 18)
                .addComponent(jPanel2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap())
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonStandard;
    private javax.swing.JCheckBox jCheckBoxAlleEinstellungen;
    private javax.swing.JCheckBox jCheckBoxAnpassen;
    private javax.swing.JPanel jPanelExtra;
    private javax.swing.JScrollPane jScrollPane1;
    // End of variables declaration//GEN-END:variables

    private class BeobCheckBoxSuchen implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            Daten.mVConfig.add(MVConfig.SYSTEM_UPDATE_SUCHEN, Boolean.toString(jCheckBox.isSelected()));
        }
    }
}
