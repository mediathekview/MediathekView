package mediathek.gui.dialog;

import javax.swing.*;
import java.awt.*;

public class MVPanelDownloadZielBase extends JPanel {
    public MVPanelDownloadZielBase() {
        initComponents();
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    // Generated using JFormDesigner non-commercial license
    private void initComponents() {
        var jLabel1 = new JLabel();
        jComboBoxPath = new JComboBox<>();
        jButtonPath = new JButton();
        jButtonDelPath = new JButton();
        var jLabel2 = new JLabel();
        jTextFieldName = new JTextField();
        jLabelExists = new JLabel();

        //======== this ========

        //---- jLabel1 ----
        jLabel1.setText("Zielpfad:");

        //---- jComboBoxPath ----
        jComboBoxPath.setEditable(true);

        //---- jButtonPath ----
        jButtonPath.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-file-open.png")));
        jButtonPath.setToolTipText("Zielpfad ausw\u00e4hlen");

        //---- jButtonDelPath ----
        jButtonDelPath.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-del.png")));
        jButtonDelPath.setToolTipText("gespeicherte Pfade l\u00f6schen");

        //---- jLabel2 ----
        jLabel2.setText("Dateiname:");

        //---- jLabelExists ----
        jLabelExists.setText("Datei existiert schon!");

        GroupLayout layout = new GroupLayout(this);
        setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup()
                .addGroup(GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                    .addContainerGap()
                    .addGroup(layout.createParallelGroup(GroupLayout.Alignment.TRAILING)
                        .addGroup(layout.createSequentialGroup()
                            .addComponent(jLabelExists)
                            .addGap(0, 0, Short.MAX_VALUE))
                        .addGroup(GroupLayout.Alignment.LEADING, layout.createSequentialGroup()
                            .addGroup(layout.createParallelGroup()
                                .addComponent(jLabel2)
                                .addComponent(jLabel1))
                            .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                            .addGroup(layout.createParallelGroup()
                                .addGroup(layout.createSequentialGroup()
                                    .addComponent(jComboBoxPath, GroupLayout.DEFAULT_SIZE, 445, Short.MAX_VALUE)
                                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                    .addComponent(jButtonPath)
                                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                    .addComponent(jButtonDelPath))
                                .addComponent(jTextFieldName))))
                    .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup()
                .addGroup(layout.createSequentialGroup()
                    .addContainerGap()
                    .addGroup(layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                        .addComponent(jLabel1)
                        .addComponent(jComboBoxPath, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                        .addComponent(jButtonPath)
                        .addComponent(jButtonDelPath))
                    .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
                    .addGroup(layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                        .addComponent(jLabel2)
                        .addComponent(jTextFieldName, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE))
                    .addGap(10, 10, 10)
                    .addComponent(jLabelExists)
                    .addContainerGap(GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        layout.linkSize(SwingConstants.VERTICAL, new Component[] {jButtonDelPath, jButtonPath, jComboBoxPath, jTextFieldName});
    }// </editor-fold>//GEN-END:initComponents


    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    protected JComboBox<String> jComboBoxPath;
    protected JButton jButtonPath;
    protected JButton jButtonDelPath;
    protected JTextField jTextFieldName;
    protected JLabel jLabelExists;
    // End of variables declaration//GEN-END:variables
}
