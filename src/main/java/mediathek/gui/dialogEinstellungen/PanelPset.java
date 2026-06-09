package mediathek.gui.dialogEinstellungen;

import mediathek.config.Daten;
import mediathek.config.application.ApplicationConfiguration;
import mediathek.gui.dialogEinstellungen.pset.PanelPsetKurz;
import mediathek.gui.dialogEinstellungen.pset.PanelPsetLang;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;

import javax.swing.*;
import java.awt.*;

public class PanelPset extends JPanel {
    private final JFrame parentComponent;

    public PanelPset(JFrame parentComponent) {
        this.parentComponent = parentComponent;

        initComponents();
        var applicationConfiguration = ApplicationConfiguration.getInstance();
        jCheckBoxAlleEinstellungen.addActionListener(_ -> {
            applicationConfiguration.setProgramSetShowAllSettings(jCheckBoxAlleEinstellungen.isSelected());
            setupPSetVisiblePanels();
        });
        jCheckBoxAlleEinstellungen.setSelected(applicationConfiguration.getProgramSetShowAllSettings());
        setupPSetVisiblePanels();
    }

    /**
     * Einstellungen zum Ansehen und Speichern der Filme anpassen.
     */
    private void setupPSetVisiblePanels() {
        jPanelPset.removeAll();
        var daten = Daten.getInstance();
        if (jCheckBoxAlleEinstellungen.isSelected()) {
            jPanelPset.add(new PanelPsetLang(parentComponent, daten.getListePset()), BorderLayout.CENTER);
        } else {
            jPanelPset.add(new PanelPsetKurz(daten.getListePset()), BorderLayout.CENTER);
        }
        jPanelPset.updateUI();
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    // Generated using JFormDesigner non-commercial license
    private void initComponents() {
        jPanelPset = new JPanel();
        jCheckBoxAlleEinstellungen = new JCheckBox();

        //======== this ========
        setLayout(new MigLayout(
            new LC().insets("5").hideMode(3).gridGap("5", "5"), //NON-NLS
            // columns
            new AC()
                .grow().fill(),
            // rows
            new AC()
                .fill().gap()
                .grow().fill()));

        //======== jPanelPset ========
        {
            jPanelPset.setLayout(new BorderLayout());
        }
        add(jPanelPset, new CC().cell(0, 1));

        //---- jCheckBoxAlleEinstellungen ----
        jCheckBoxAlleEinstellungen.setText("alle Einstellungen anzeigen"); //NON-NLS
        add(jCheckBoxAlleEinstellungen, new CC().cell(0, 0));
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    private JPanel jPanelPset;
    private JCheckBox jCheckBoxAlleEinstellungen;
    // End of variables declaration//GEN-END:variables
}
