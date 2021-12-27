package mediathek.gui.dialogEinstellungen;

import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.gui.PanelVorlage;

import javax.swing.*;
import javax.swing.border.EtchedBorder;
import java.awt.*;

public class PanelPset extends PanelVorlage {
    public PanelPset(Daten d, JFrame parentComponent) {
        super(d, parentComponent);
        initComponents();
        jCheckBoxAlleEinstellungen.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_ANSICHT_SET_LANG, Boolean.toString(jCheckBoxAlleEinstellungen.isSelected()));
            setupPSetVisiblePanels();
        });
        jCheckBoxAlleEinstellungen.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_ANSICHT_SET_LANG)));
        setupPSetVisiblePanels();
    }

    /**
     * Einstellungen zum Ansehen und Speichern der Filme anpassen.
     */
    private void setupPSetVisiblePanels() {
        jPanelPset.removeAll();
        if (jCheckBoxAlleEinstellungen.isSelected()) {
            jPanelPset.add(new PanelPsetLang(daten, parentComponent, Daten.listePset), BorderLayout.CENTER);
        } else {
            jPanelPset.add(new PanelPsetKurz(daten, parentComponent, Daten.listePset), BorderLayout.CENTER);
        }
        jPanelPset.updateUI();
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    // Generated using JFormDesigner non-commercial license
    private void initComponents() {
        jPanelPset = new JPanel();
        jCheckBoxAlleEinstellungen = new JCheckBox();

        //======== this ========

        //======== jPanelPset ========
        {
            jPanelPset.setBorder(new EtchedBorder());
            jPanelPset.setLayout(new BorderLayout());
        }

        //---- jCheckBoxAlleEinstellungen ----
        jCheckBoxAlleEinstellungen.setText("alle Einstellungen anzeigen"); //NON-NLS

        GroupLayout layout = new GroupLayout(this);
        setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup()
                .addGroup(layout.createSequentialGroup()
                    .addContainerGap()
                    .addGroup(layout.createParallelGroup()
                        .addComponent(jPanelPset, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addGroup(layout.createSequentialGroup()
                            .addComponent(jCheckBoxAlleEinstellungen)
                            .addGap(0, 189, Short.MAX_VALUE)))
                    .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup()
                .addGroup(layout.createSequentialGroup()
                    .addContainerGap()
                    .addComponent(jCheckBoxAlleEinstellungen)
                    .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
                    .addComponent(jPanelPset, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    private JPanel jPanelPset;
    private JCheckBox jCheckBoxAlleEinstellungen;
    // End of variables declaration//GEN-END:variables
}
