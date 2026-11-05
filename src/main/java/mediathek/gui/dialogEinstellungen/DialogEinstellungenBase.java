package mediathek.gui.dialogEinstellungen;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.EtchedBorder;
import java.awt.*;

/** JFormDesigner layout whose labels use the interface language selected at startup. */
public class DialogEinstellungenBase extends JFrame {
    /** Creates the localized controls using the current startup language. */
    public DialogEinstellungenBase() {
        initComponents();
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    // Generated using JFormDesigner non-commercial license
    /** Initializes the controls and their resource-bundle labels. */
    private void initComponents() {
        var bundle = mediathek.i18n.UiMessages.getBundle("mediathek.i18n.Messages");
        jButtonBeenden = new JButton();
        var jSplitPane1 = new JSplitPane();
        var jScrollPane2 = new JScrollPane();
        jPanelExtra = new JPanel();
        var jScrollPane1 = new JScrollPane();
        jTree1 = new JTree();

        //======== this ========
        setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
        setTitle(bundle.getString("settings.dialog.this.title"));
        var contentPane = getContentPane();

        //---- jButtonBeenden ----
        jButtonBeenden.setText(bundle.getString("settings.dialog.jButtonBeenden.text"));

        //======== jSplitPane1 ========
        {
            jSplitPane1.setBorder(new EtchedBorder());
            jSplitPane1.setDividerLocation(250);
            jSplitPane1.setContinuousLayout(true);
            jSplitPane1.setOneTouchExpandable(true);

            //======== jScrollPane2 ========
            {

                //======== jPanelExtra ========
                {
                    jPanelExtra.setLayout(new BorderLayout());
                }
                jScrollPane2.setViewportView(jPanelExtra);
            }
            jSplitPane1.setRightComponent(jScrollPane2);

            //======== jScrollPane1 ========
            {

                //---- jTree1 ----
                jTree1.setBorder(new EmptyBorder(5, 5, 5, 5));
                jTree1.setRootVisible(false);
                jScrollPane1.setViewportView(jTree1);
            }
            jSplitPane1.setLeftComponent(jScrollPane1);
        }

        GroupLayout contentPaneLayout = new GroupLayout(contentPane);
        contentPane.setLayout(contentPaneLayout);
        contentPaneLayout.setHorizontalGroup(
            contentPaneLayout.createParallelGroup()
                .addGroup(contentPaneLayout.createSequentialGroup()
                    .addContainerGap()
                    .addGroup(contentPaneLayout.createParallelGroup(GroupLayout.Alignment.TRAILING)
                        .addComponent(jSplitPane1, GroupLayout.DEFAULT_SIZE, 1068, Short.MAX_VALUE)
                        .addGroup(contentPaneLayout.createSequentialGroup()
                            .addGap(12, 965, Short.MAX_VALUE)
                            .addComponent(jButtonBeenden)))
                    .addContainerGap())
        );
        contentPaneLayout.setVerticalGroup(
            contentPaneLayout.createParallelGroup()
                .addGroup(contentPaneLayout.createSequentialGroup()
                    .addContainerGap()
                    .addComponent(jSplitPane1, GroupLayout.DEFAULT_SIZE, 620, Short.MAX_VALUE)
                    .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
                    .addComponent(jButtonBeenden)
                    .addGap(6, 6, 6))
        );
        pack();
        setLocationRelativeTo(getOwner());
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    protected JButton jButtonBeenden;
    protected JPanel jPanelExtra;
    protected JTree jTree1;
    // End of variables declaration//GEN-END:variables
}
