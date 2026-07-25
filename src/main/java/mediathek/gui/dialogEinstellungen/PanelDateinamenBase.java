package mediathek.gui.dialogEinstellungen;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.EtchedBorder;
import javax.swing.table.DefaultTableModel;
import java.awt.*;

public class PanelDateinamenBase extends JPanel {
    public PanelDateinamenBase() {
        initComponents();
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    // Generated using JFormDesigner non-commercial license
    private void initComponents() {
        var jTabbedPane1 = new JTabbedPane();
        var jPanel1 = new JPanel();
        var jScrollPane5 = new JScrollPane();
        var jTextArea3 = new JTextArea();
        var jPanel2 = new JPanel();
        jCheckBoxTable = new JCheckBox();
        var jPanel3 = new JPanel();
        jButtonReset = new JButton();
        var jScrollPane3 = new JScrollPane();
        var jTextArea2 = new JTextArea();
        var jScrollPane4 = new JScrollPane();
        tabelle = new JTable();
        jLabelAlert = new JLabel();
        jLabelVon = new JLabel();
        jTextFieldVon = new JTextField();
        jLabelNach = new JLabel();
        jTextFieldNach = new JTextField();
        jButtonMinus = new JButton();
        jButtonPlus = new JButton();
        jButtonDown = new JButton();
        jButtonUp = new JButton();
        jCheckBoxAscii = new JCheckBox();

        //======== this ========

        //======== jTabbedPane1 ========
        {

            //======== jPanel1 ========
            {

                //======== jScrollPane5 ========
                {

                    //---- jTextArea3 ----
                    jTextArea3.setEditable(false);
                    jTextArea3.setColumns(20);
                    jTextArea3.setRows(5);
                    jTextArea3.setText("\nDie Dateinamen werden f\u00fcr jedes Betriebssystem passend aufbereitet.\n\nWer will, kann dar\u00fcber hinaus weitere Einstellungen mit einer Ersetzungstabelle\nvornehmen: z.B. \"\u00df\" durch \"ss\" ersetzen.\n"); //NON-NLS
                    jTextArea3.setMargin(new Insets(3, 3, 3, 3));
                    jScrollPane5.setViewportView(jTextArea3);
                }

                GroupLayout jPanel1Layout = new GroupLayout(jPanel1);
                jPanel1.setLayout(jPanel1Layout);
                jPanel1Layout.setHorizontalGroup(
                    jPanel1Layout.createParallelGroup()
                        .addGroup(jPanel1Layout.createSequentialGroup()
                            .addContainerGap()
                            .addComponent(jScrollPane5, GroupLayout.DEFAULT_SIZE, 773, Short.MAX_VALUE)
                            .addContainerGap())
                );
                jPanel1Layout.setVerticalGroup(
                    jPanel1Layout.createParallelGroup()
                        .addGroup(jPanel1Layout.createSequentialGroup()
                            .addContainerGap()
                            .addComponent(jScrollPane5, GroupLayout.PREFERRED_SIZE, 119, GroupLayout.PREFERRED_SIZE)
                            .addContainerGap(459, Short.MAX_VALUE))
                );
            }
            jTabbedPane1.addTab("Dateinamen", jPanel1); //NON-NLS

            //======== jPanel2 ========
            {

                //---- jCheckBoxTable ----
                jCheckBoxTable.setText("Ersetzungstabelle anwenden"); //NON-NLS

                //======== jPanel3 ========
                {
                    jPanel3.setBorder(new EtchedBorder());

                    //---- jButtonReset ----
                    jButtonReset.setText("Tabelle zur\u00fccksetzen"); //NON-NLS

                    //======== jScrollPane3 ========
                    {
                        jScrollPane3.setBorder(new EmptyBorder(1, 1, 1, 1));

                        //---- jTextArea2 ----
                        jTextArea2.setEditable(false);
                        jTextArea2.setBackground(UIManager.getColor("Label.background")); //NON-NLS
                        jTextArea2.setColumns(20);
                        jTextArea2.setRows(4);
                        jTextArea2.setText("Die Tabelle wird von oben nach unten abgearbeitet.\nEs ist also m\u00f6glich, dass eine Ersetzung durch eine weitere\nwieder ersetzt wird!"); //NON-NLS
                        jTextArea2.setBorder(new EmptyBorder(1, 1, 1, 1));
                        jScrollPane3.setViewportView(jTextArea2);
                    }

                    //======== jScrollPane4 ========
                    {

                        //---- tabelle ----
                        tabelle.setModel(new DefaultTableModel(
                            new Object[][] {
                                {null, null, null, null},
                                {null, null, null, null},
                                {null, null, null, null},
                                {null, null, null, null},
                            },
                            new String[] {
                                "Title 1", "Title 2", "Title 3", "Title 4" //NON-NLS
                            }
                        ));
                        jScrollPane4.setViewportView(tabelle);
                    }

                    //---- jLabelAlert ----
                    jLabelAlert.setText("Achtung"); //NON-NLS

                    //---- jLabelVon ----
                    jLabelVon.setText("von:"); //NON-NLS

                    //---- jLabelNach ----
                    jLabelNach.setText("nach:"); //NON-NLS

                    //---- jButtonMinus ----
                    jButtonMinus.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-remove.png"))); //NON-NLS

                    //---- jButtonPlus ----
                    jButtonPlus.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-add.png"))); //NON-NLS

                    //---- jButtonDown ----
                    jButtonDown.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-move-down.png"))); //NON-NLS

                    //---- jButtonUp ----
                    jButtonUp.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-move-up.png"))); //NON-NLS

                    GroupLayout jPanel3Layout = new GroupLayout(jPanel3);
                    jPanel3.setLayout(jPanel3Layout);
                    jPanel3Layout.setHorizontalGroup(
                        jPanel3Layout.createParallelGroup()
                            .addGroup(jPanel3Layout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(jPanel3Layout.createParallelGroup()
                                    .addComponent(jScrollPane4)
                                    .addGroup(jPanel3Layout.createSequentialGroup()
                                        .addComponent(jScrollPane3)
                                        .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
                                        .addComponent(jLabelAlert))
                                    .addGroup(jPanel3Layout.createSequentialGroup()
                                        .addComponent(jButtonReset)
                                        .addGap(0, 0, Short.MAX_VALUE))
                                    .addGroup(jPanel3Layout.createSequentialGroup()
                                        .addComponent(jLabelVon)
                                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                        .addComponent(jTextFieldVon, GroupLayout.PREFERRED_SIZE, 100, GroupLayout.PREFERRED_SIZE)
                                        .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
                                        .addComponent(jLabelNach)
                                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                        .addComponent(jTextFieldNach, GroupLayout.PREFERRED_SIZE, 100, GroupLayout.PREFERRED_SIZE)
                                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED, 111, Short.MAX_VALUE)
                                        .addComponent(jButtonUp)
                                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                        .addComponent(jButtonDown)
                                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                        .addComponent(jButtonPlus)
                                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                        .addComponent(jButtonMinus)))
                                .addGap(15, 15, 15))
                    );
                    jPanel3Layout.setVerticalGroup(
                        jPanel3Layout.createParallelGroup()
                            .addGroup(GroupLayout.Alignment.TRAILING, jPanel3Layout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jScrollPane4, GroupLayout.DEFAULT_SIZE, 328, Short.MAX_VALUE)
                                .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
                                .addGroup(jPanel3Layout.createParallelGroup(GroupLayout.Alignment.CENTER)
                                    .addComponent(jLabelVon)
                                    .addComponent(jTextFieldVon, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                                    .addComponent(jLabelNach)
                                    .addComponent(jTextFieldNach, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                                    .addComponent(jButtonUp)
                                    .addComponent(jButtonDown)
                                    .addComponent(jButtonPlus)
                                    .addComponent(jButtonMinus))
                                .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
                                .addGroup(jPanel3Layout.createParallelGroup(GroupLayout.Alignment.CENTER)
                                    .addComponent(jScrollPane3, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                                    .addComponent(jLabelAlert))
                                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jButtonReset)
                                .addContainerGap())
                    );
                    jPanel3Layout.linkSize(SwingConstants.VERTICAL, new Component[] {jButtonDown, jButtonMinus, jButtonPlus, jButtonUp, jLabelNach, jLabelVon, jTextFieldNach, jTextFieldVon});
                }

                //---- jCheckBoxAscii ----
                jCheckBoxAscii.setText("Nur ASCII-Zeichen erlauben"); //NON-NLS
                jCheckBoxAscii.setToolTipText("<html>Es werden alle Zeichen \"\u00fcber 127\" ersetzt.  Auch Umlaute wie \"\u00f6 -> oe\" werden ersetzt.<br>Wenn die Ersetzungstabelle aktiv ist, wird sie vorher abgearbeitet.</html>"); //NON-NLS

                GroupLayout jPanel2Layout = new GroupLayout(jPanel2);
                jPanel2.setLayout(jPanel2Layout);
                jPanel2Layout.setHorizontalGroup(
                    jPanel2Layout.createParallelGroup()
                        .addGroup(jPanel2Layout.createSequentialGroup()
                            .addContainerGap()
                            .addGroup(jPanel2Layout.createParallelGroup()
                                .addGroup(jPanel2Layout.createSequentialGroup()
                                    .addGap(21, 21, 21)
                                    .addComponent(jPanel3, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                                .addGroup(jPanel2Layout.createSequentialGroup()
                                    .addGroup(jPanel2Layout.createParallelGroup()
                                        .addComponent(jCheckBoxAscii)
                                        .addComponent(jCheckBoxTable))
                                    .addGap(0, 0, Short.MAX_VALUE)))
                            .addContainerGap())
                );
                jPanel2Layout.setVerticalGroup(
                    jPanel2Layout.createParallelGroup()
                        .addGroup(jPanel2Layout.createSequentialGroup()
                            .addContainerGap()
                            .addComponent(jCheckBoxTable)
                            .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                            .addComponent(jPanel3, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addGap(18, 18, 18)
                            .addComponent(jCheckBoxAscii)
                            .addContainerGap())
                );
            }
            jTabbedPane1.addTab("Eigene Einstellungen", jPanel2); //NON-NLS
        }

        GroupLayout layout = new GroupLayout(this);
        setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup()
                .addGroup(GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                    .addContainerGap()
                    .addComponent(jTabbedPane1)
                    .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup()
                .addGroup(layout.createSequentialGroup()
                    .addContainerGap()
                    .addComponent(jTabbedPane1)
                    .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    protected JCheckBox jCheckBoxTable;
    protected JButton jButtonReset;
    protected JTable tabelle;
    protected JLabel jLabelAlert;
    protected JLabel jLabelVon;
    protected JTextField jTextFieldVon;
    protected JLabel jLabelNach;
    protected JTextField jTextFieldNach;
    protected JButton jButtonMinus;
    protected JButton jButtonPlus;
    protected JButton jButtonDown;
    protected JButton jButtonUp;
    protected JCheckBox jCheckBoxAscii;
    // End of variables declaration//GEN-END:variables
}
