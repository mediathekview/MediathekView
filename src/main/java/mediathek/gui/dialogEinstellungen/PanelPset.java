package mediathek.gui.dialogEinstellungen;

import mediathek.config.application.ApplicationConfiguration;
import mediathek.daten.DatenPset;
import mediathek.daten.ProgramSetRepository;
import mediathek.gui.dialogEinstellungen.pset.PanelPsetKurz;
import mediathek.gui.dialogEinstellungen.pset.PanelPsetLang;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;

import javax.swing.*;
import java.awt.*;
import java.util.function.BiConsumer;

public class PanelPset extends JPanel {
    private final JFrame parentComponent;
    private final ProgramSetRepository programSets;
    private final BiConsumer<DatenPset[], String> programSetExporter;

    public PanelPset(
            JFrame parentComponent,
            ProgramSetRepository programSets,
            BiConsumer<DatenPset[], String> programSetExporter
    ) {
        this.parentComponent = parentComponent;
        this.programSets = programSets;
        this.programSetExporter = programSetExporter;

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
        if (jCheckBoxAlleEinstellungen.isSelected()) {
            jPanelPset.add(new PanelPsetLang(
                    parentComponent,
                    programSets,
                    programSets.getList(),
                    programSetExporter
            ), BorderLayout.CENTER);
        } else {
            jPanelPset.add(new PanelPsetKurz(parentComponent, programSets.getList()), BorderLayout.CENTER);
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
