package mediathek.gui.dialog;

import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;

public class DialogContinueDownloadBase extends JDialog {
    protected DialogContinueDownloadBase(JFrame parent) {
        super(parent, true);
        initComponents();
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    // Generated using JFormDesigner non-commercial license
    private void initComponents() {
        jLabel1 = new JLabel();
        jPanelNewName = new JPanel();
        jPanelPath = new JPanel();
        jButtonNeuerName = new JButton();
        var buttonPanel = new JPanel();
        jButtonAbbrechen = new JButton();
        jButtonWeiter = new JButton();

        //======== this ========
        setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
        setTitle("Download weiterf\u00fchren");
        var contentPane = getContentPane();
        contentPane.setLayout(new MigLayout(
            new LC().fillX().insets("5").hideMode(3).gridGap("rel", "rel"),
            // columns
            new AC()
                .grow().fill(),
            // rows
            new AC()
                .gap()
                .gap()
                ));

        //---- jLabel1 ----
        jLabel1.setText("<html>Die Filmdatei existiert bereits.<br>Wie m\u00f6chten Sie forfahren?</html>");
        contentPane.add(jLabel1, new CC().cell(0, 0));

        //======== jPanelNewName ========
        {
            jPanelNewName.setBorder(new EmptyBorder(0, 0, 20, 0));
            jPanelNewName.setLayout(new MigLayout(
                new LC().fillX().insets("0").hideMode(3).gridGap("rel", "rel"),
                // columns
                new AC()
                    .grow(),
                // rows
                new AC()
                    .gap()
                    ));

            //======== jPanelPath ========
            {
                jPanelPath.setPreferredSize(new Dimension(527, 96));
                jPanelPath.setLayout(new BorderLayout());
            }
            jPanelNewName.add(jPanelPath, new CC().cell(0, 0).growX().width("527").height("96"));

            //---- jButtonNeuerName ----
            jButtonNeuerName.setText("Mit diesem Namen neu Starten");
            jPanelNewName.add(jButtonNeuerName, new CC().cell(0, 1).alignX("right"));
        }
        contentPane.add(jPanelNewName, new CC().cell(0, 1).growX());

        //======== buttonPanel ========
        {
            buttonPanel.setLayout(new MigLayout(
                new LC().insets("0").hideMode(3).alignX("right").gridGap("rel", "rel"),
                // columns
                new AC()
                    .gap()
                    ,
                // rows
                new AC()
                    ));

            //---- jButtonAbbrechen ----
            jButtonAbbrechen.setText("Abbrechen");
            buttonPanel.add(jButtonAbbrechen, new CC().cell(0, 0));

            //---- jButtonWeiter ----
            jButtonWeiter.setText("Weiterf\u00fchren in XXX");
            buttonPanel.add(jButtonWeiter, new CC().cell(1, 0));
        }
        contentPane.add(buttonPanel, new CC().cell(0, 2).alignX("right"));
        pack();
        setLocationRelativeTo(getOwner());
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    protected JLabel jLabel1;
    protected JPanel jPanelNewName;
    protected JPanel jPanelPath;
    protected JButton jButtonNeuerName;
    protected JButton jButtonAbbrechen;
    protected JButton jButtonWeiter;
    // End of variables declaration//GEN-END:variables
}
