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
import org.apache.commons.lang3.SystemUtils;

import javax.swing.*;
import javax.swing.border.LineBorder;
import java.awt.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

public class DialogStarteinstellungen extends JDialog {
    private enum State { START, PFAD, PSET, FERTIG}
    private State status = State.START;
    private final JFrame parentComponent;
    private boolean anpassen;

    public DialogStarteinstellungen(JFrame parent) {
        super(parent, true);
        parentComponent = parent;
        initComponents();

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

        addWindowListener(new WindowAdapter() {
            @Override
            public void windowOpened(WindowEvent e) {
                toFront();
            }
        });
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

    public enum ResultCode {
        SUCCESS,
        CANCELLED
    }

    public ResultCode getResultCode() {
        if ( status != State.FERTIG)
            return ResultCode.CANCELLED;
        else
        return ResultCode.SUCCESS;
    }

    private void statusPfade() {
        // erst Programmpfad prüfen
        jButtonAnpassen.setVisible(false);
        jCheckBoxAlleEinstellungen.setVisible(false);
        boolean search_ffmpeg = !SystemUtils.IS_OS_MAC_OSX && !SystemUtils.IS_OS_WINDOWS;

        var programPathsPanel = new PanelProgrammPfade(parentComponent, true, search_ffmpeg);
        jScrollPane1.setViewportView(programPathsPanel);

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
    // Generated using JFormDesigner non-commercial license
    private void initComponents() {
        var jPanel2 = new JPanel();
        jButtonStandard = new JButton();
        jCheckBoxAlleEinstellungen = new JCheckBox();
        jButtonAnpassen = new JButton();
        jScrollPane1 = new JScrollPane();
        jPanelExtra = new JPanel();

        //======== this ========
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Erster Start"); //NON-NLS
        var contentPane = getContentPane();

        //======== jPanel2 ========
        {
            jPanel2.setBorder(new LineBorder(new Color(153, 153, 255), 3));

            //---- jButtonStandard ----
            jButtonStandard.setText("Mit Standardeinstellungen starten"); //NON-NLS

            //---- jCheckBoxAlleEinstellungen ----
            jCheckBoxAlleEinstellungen.setText("alle Einstellungen anzeigen"); //NON-NLS

            //---- jButtonAnpassen ----
            jButtonAnpassen.setText("Einstellungen anpassen"); //NON-NLS

            GroupLayout jPanel2Layout = new GroupLayout(jPanel2);
            jPanel2.setLayout(jPanel2Layout);
            jPanel2Layout.setHorizontalGroup(
                jPanel2Layout.createParallelGroup()
                    .addGroup(GroupLayout.Alignment.TRAILING, jPanel2Layout.createSequentialGroup()
                        .addContainerGap()
                        .addComponent(jCheckBoxAlleEinstellungen)
                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jButtonAnpassen)
                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonStandard)
                        .addContainerGap())
            );
            jPanel2Layout.setVerticalGroup(
                jPanel2Layout.createParallelGroup()
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addContainerGap()
                        .addGroup(jPanel2Layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                            .addComponent(jButtonStandard)
                            .addComponent(jCheckBoxAlleEinstellungen)
                            .addComponent(jButtonAnpassen))
                        .addContainerGap(GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
            );
        }

        //======== jScrollPane1 ========
        {

            //======== jPanelExtra ========
            {

                GroupLayout jPanelExtraLayout = new GroupLayout(jPanelExtra);
                jPanelExtra.setLayout(jPanelExtraLayout);
                jPanelExtraLayout.setHorizontalGroup(
                    jPanelExtraLayout.createParallelGroup()
                        .addGap(0, 788, Short.MAX_VALUE)
                );
                jPanelExtraLayout.setVerticalGroup(
                    jPanelExtraLayout.createParallelGroup()
                        .addGap(0, 510, Short.MAX_VALUE)
                );
            }
            jScrollPane1.setViewportView(jPanelExtra);
        }

        GroupLayout contentPaneLayout = new GroupLayout(contentPane);
        contentPane.setLayout(contentPaneLayout);
        contentPaneLayout.setHorizontalGroup(
            contentPaneLayout.createParallelGroup()
                .addGroup(contentPaneLayout.createSequentialGroup()
                    .addContainerGap()
                    .addGroup(contentPaneLayout.createParallelGroup()
                        .addComponent(jScrollPane1, GroupLayout.DEFAULT_SIZE, 791, Short.MAX_VALUE)
                        .addComponent(jPanel2, GroupLayout.Alignment.TRAILING, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                    .addContainerGap())
        );
        contentPaneLayout.setVerticalGroup(
            contentPaneLayout.createParallelGroup()
                .addGroup(contentPaneLayout.createSequentialGroup()
                    .addContainerGap()
                    .addComponent(jScrollPane1)
                    .addGap(18, 18, 18)
                    .addComponent(jPanel2, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                    .addContainerGap())
        );
        pack();
        setLocationRelativeTo(getOwner());
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    private JButton jButtonStandard;
    private JCheckBox jCheckBoxAlleEinstellungen;
    private JButton jButtonAnpassen;
    private JScrollPane jScrollPane1;
    private JPanel jPanelExtra;
    // End of variables declaration//GEN-END:variables
}
