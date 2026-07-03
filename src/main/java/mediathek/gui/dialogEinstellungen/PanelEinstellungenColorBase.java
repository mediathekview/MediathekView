package mediathek.gui.dialogEinstellungen;

import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;

import javax.swing.*;

public class PanelEinstellungenColorBase extends JPanel {
    public PanelEinstellungenColorBase() {
        initComponents();
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    // Generated using JFormDesigner non-commercial license
    private void initComponents() {
        jTabbedPane1 = new JTabbedPane();
        var panelLight = new JPanel();
        var scrollPaneLight = new JScrollPane();
        jTableLight = new JTable();
        var panelDark = new JPanel();
        var scrollPaneDark = new JScrollPane();
        jTableDark = new JTable();
        var hSpacer1 = new JPanel(null);
        jButtonReset = new JButton();

        //======== this ========
        setLayout(new MigLayout(
            new LC().insets("5").hideMode(3).gridGap("5", "5"), //NON-NLS
            // columns
            new AC()
                .fill().gap()
                .grow().fill().gap()
                .fill(),
            // rows
            new AC()
                .grow().fill().gap()
                .fill()));

        //======== jTabbedPane1 ========
        {
            //======== panelLight ========
            {
                panelLight.setLayout(new MigLayout(
                    new LC().insets("0").hideMode(3),
                    // columns
                    new AC()
                        .grow().fill(),
                    // rows
                    new AC()
                        .grow().fill()));

                //======== scrollPaneLight ========
                {
                    scrollPaneLight.setViewportView(jTableLight);
                }
                panelLight.add(scrollPaneLight, new CC().cell(0, 0));
            }
            jTabbedPane1.addTab("Hell", panelLight);

            //======== panelDark ========
            {
                panelDark.setLayout(new MigLayout(
                    new LC().insets("0").hideMode(3),
                    // columns
                    new AC()
                        .grow().fill(),
                    // rows
                    new AC()
                        .grow().fill()));

                //======== scrollPaneDark ========
                {
                    scrollPaneDark.setViewportView(jTableDark);
                }
                panelDark.add(scrollPaneDark, new CC().cell(0, 0));
            }
            jTabbedPane1.addTab("Dunkel", panelDark);
        }
        add(jTabbedPane1, new CC().cell(1, 0, 2, 1));
        add(hSpacer1, new CC().cell(1, 1));

        //---- jButtonReset ----
        jButtonReset.setText("Helle Farben zur\u00fccksetzen"); //NON-NLS
        add(jButtonReset, new CC().cell(2, 1));
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    protected JTabbedPane jTabbedPane1;
    protected JTable jTableLight;
    protected JTable jTableDark;
    protected JButton jButtonReset;
    // End of variables declaration//GEN-END:variables
}
