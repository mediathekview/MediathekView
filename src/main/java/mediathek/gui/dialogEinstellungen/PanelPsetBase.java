package mediathek.gui.dialogEinstellungen;

import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;

import javax.swing.*;
import java.awt.*;

public class PanelPsetBase extends JPanel {
    public PanelPsetBase() {
        initComponents();
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
    protected JPanel jPanelPset;
    protected JCheckBox jCheckBoxAlleEinstellungen;
    // End of variables declaration//GEN-END:variables
}
