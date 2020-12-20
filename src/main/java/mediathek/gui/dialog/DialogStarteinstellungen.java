package mediathek.gui.dialog;

import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.daten.ListePset;
import mediathek.daten.ListePsetVorlagen;
import mediathek.gui.dialogEinstellungen.PanelEinstellungenGeo;
import mediathek.gui.dialogEinstellungen.PanelProgrammPfade;
import mediathek.gui.dialogEinstellungen.PanelPsetKurz;
import mediathek.gui.dialogEinstellungen.PanelPsetLang;
import mediathek.tool.GuiFunktionenProgramme;

import javax.swing.*;
import java.awt.*;

import static mediathek.tool.Functions.getOs;

public class DialogStarteinstellungen extends JDialog {
    private enum State { START, PFAD, PSET, FERTIG}
    private State status = State.START;
    private final JFrame parentComponent;
    private boolean anpassen;

    public DialogStarteinstellungen(JFrame parent) {
        super(parent, true);
        parentComponent = parent;
        initComponents();
        this.setTitle("Erster Start");

        jButtonStandard.addActionListener(e -> weiter());
        jButtonAnpassen.addActionListener(e -> {
            anpassen = true;
            weiter();
        });
        jCheckBoxAlleEinstellungen.setVisible(false);
        jCheckBoxAlleEinstellungen.addActionListener(e -> {
            status = State.PSET;
            weiter();
        });

        // setzt die Standardpfade für die wichtigsten Programme
        MVConfig.add(MVConfig.Configs.SYSTEM_PFAD_VLC, GuiFunktionenProgramme.getMusterPfadVlc());
        MVConfig.add(MVConfig.Configs.SYSTEM_PFAD_FFMPEG, GuiFunktionenProgramme.getMusterPfadFFmpeg());

        createLayout();

        if (MVConfig.get(MVConfig.Configs.SYSTEM_PFAD_VLC).isEmpty()
                || MVConfig.get(MVConfig.Configs.SYSTEM_PFAD_FFMPEG).isEmpty()) {
            //dann fehlt ein Programm
            jButtonStandard.setEnabled(false);
            anpassen = true;
        }
    }

    private void createLayout() {
        PanelEinstellungenGeo panelEinstellungenGeo = new PanelEinstellungenGeo(parentComponent);
        jPanelExtra.setLayout(new BorderLayout());
        jPanelExtra.add(panelEinstellungenGeo, BorderLayout.CENTER);
    }

    private void weiter() {
        jButtonStandard.setEnabled(true);
        switch (status) {
            case START -> statusStart();
            case PFAD -> statusPfade();
            case PSET -> statusPset();
            default -> beenden();
        }
    }

    private void statusStart() {
        jButtonStandard.setText("Weiter");
        if (MVConfig.get(MVConfig.Configs.SYSTEM_PFAD_VLC).isEmpty()
                || MVConfig.get(MVConfig.Configs.SYSTEM_PFAD_FFMPEG).isEmpty()) {
            // ein Programm (VLC, flvstreamer) wurde nicht gefunden, muss der Benutzer eintragen
            status = State.PFAD;
        } else if (anpassen) {
            // der Benutzer wills verstellen
            status = State.PFAD;
        } else // nur dann automatisch Standardprogramme einrichten, sonst fragen
         if (addStandarSet(parentComponent)) {
                status = State.FERTIG;
            } else {
                status = State.PSET;
            }
        weiter();
    }

    private void statusPfade() {
        // erst Programmpfad prüfen
        jButtonAnpassen.setVisible(false);
        jCheckBoxAlleEinstellungen.setVisible(false);
        switch (getOs()) {
            case MAC, WIN32, WIN64 -> jScrollPane1.setViewportView(new PanelProgrammPfade(parentComponent, true /* vlc */, false /*ffmpeg*/));
            default -> jScrollPane1.setViewportView(new PanelProgrammPfade(parentComponent, true /* vlc */, true /*ffmpeg*/));
        }
        status = State.PSET;
        jButtonStandard.setText("Weiter");
    }

    private void statusPset() {
        // Einstellungen zum Ansehen und Speichern der Filme anpassen
        jButtonAnpassen.setVisible(false);
        jCheckBoxAlleEinstellungen.setVisible(true);
        if (Daten.listePset.isEmpty()) {
            // Standardset hinzufügen
            addStandarSet(parentComponent);
        }

        var daten = Daten.getInstance();
        if (jCheckBoxAlleEinstellungen.isSelected()) {
            jScrollPane1.setViewportView(new PanelPsetLang(daten, parentComponent, Daten.listePset));
        } else {
            jScrollPane1.setViewportView(new PanelPsetKurz(daten, parentComponent, Daten.listePset));
        }
        status = State.FERTIG;
        jButtonStandard.setText("Weiter");
    }

    private boolean addStandarSet(JFrame parent) {
        boolean ret = false;
        ListePset pSet = ListePsetVorlagen.getStandarset(parent, true);
        if (pSet != null) {
            Daten.listePset.addPset(pSet);
            MVConfig.add(MVConfig.Configs.SYSTEM_VERSION_PROGRAMMSET, pSet.version);
            ret = true;
        }
        return ret;
    }

    private void beenden() {
        this.dispose();
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.JPanel jPanel2 = new javax.swing.JPanel();
        jButtonStandard = new javax.swing.JButton();
        jCheckBoxAlleEinstellungen = new javax.swing.JCheckBox();
        jButtonAnpassen = new javax.swing.JButton();
        jScrollPane1 = new javax.swing.JScrollPane();
        jPanelExtra = new javax.swing.JPanel();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);

        jPanel2.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 255), 3));

        jButtonStandard.setText("Mit Standardeinstellungen starten");

        jCheckBoxAlleEinstellungen.setText("alle Einstellungen anzeigen");

        jButtonAnpassen.setText("Einstellungen anpassen");

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jCheckBoxAlleEinstellungen)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addComponent(jButtonAnpassen)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
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
                    .addComponent(jButtonAnpassen))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        javax.swing.GroupLayout jPanelExtraLayout = new javax.swing.GroupLayout(jPanelExtra);
        jPanelExtra.setLayout(jPanelExtraLayout);
        jPanelExtraLayout.setHorizontalGroup(
            jPanelExtraLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 788, Short.MAX_VALUE)
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
                    .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 791, Short.MAX_VALUE)
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
    private javax.swing.JButton jButtonAnpassen;
    private javax.swing.JButton jButtonStandard;
    private javax.swing.JCheckBox jCheckBoxAlleEinstellungen;
    private javax.swing.JPanel jPanelExtra;
    private javax.swing.JScrollPane jScrollPane1;
    // End of variables declaration//GEN-END:variables
}
