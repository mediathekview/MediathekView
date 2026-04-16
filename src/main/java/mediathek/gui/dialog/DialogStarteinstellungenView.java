package mediathek.gui.dialog;

import javax.swing.*;
import javax.swing.border.LineBorder;
import java.awt.*;

public class DialogStarteinstellungenView extends JDialog {
    public DialogStarteinstellungenView(JFrame parent) {
        super(parent, true);
        initComponents();
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
    protected JButton jButtonStandard;
    protected JCheckBox jCheckBoxAlleEinstellungen;
    protected JButton jButtonAnpassen;
    protected JScrollPane jScrollPane1;
    protected JPanel jPanelExtra;
    // End of variables declaration//GEN-END:variables
}
