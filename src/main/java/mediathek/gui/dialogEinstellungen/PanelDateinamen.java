package mediathek.gui.dialogEinstellungen;

import jiconfont.icons.FontAwesome;
import jiconfont.swing.IconFontSwing;
import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.config.MVConfig;
import mediathek.gui.PanelVorlage;
import mediathek.gui.messages.ReplaceListChangedEvent;
import mediathek.tool.NoSelectionErrorDialog;
import mediathek.tool.ReplaceList;
import mediathek.tool.TextCopyPasteHandler;
import mediathek.tool.models.NonEditableTableModel;
import net.engio.mbassy.listener.Handler;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.EtchedBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableModel;
import java.awt.*;

@SuppressWarnings("serial")
public class PanelDateinamen extends PanelVorlage {
    public boolean ok;
    public String ziel = "";

    @Handler
    private void handleReplaceListChange(ReplaceListChangedEvent e) {
        SwingUtilities.invokeLater(() -> {
            tabelleLaden();
            setTextfelder();
        });
    }

    public PanelDateinamen(Daten d, JFrame pparentComponent) {
        super(d, pparentComponent);
        initComponents();
        daten.getMessageBus().subscribe(this);

        jLabelAlert.setVisible(false);
        jLabelAlert.setText("");
        jLabelAlert.setIcon(IconFontSwing.buildIcon(FontAwesome.EXCLAMATION_TRIANGLE, 32));
        jButtonPlus.setIcon(Icons.ICON_BUTTON_ADD);
        jButtonMinus.setIcon(Icons.ICON_BUTTON_REMOVE);
        jButtonUp.setIcon(Icons.ICON_BUTTON_MOVE_UP);
        jButtonDown.setIcon(Icons.ICON_BUTTON_MOVE_DOWN);
        jButtonReset.addActionListener(e -> {
            ReplaceList.init();
            tabelleLaden();
            setTextfelder();
        });
        jButtonPlus.addActionListener(e -> {
            ReplaceList.list.add(new String[]{"von", "nach"});
            tabelleLaden();
            tabelle.setRowSelectionInterval(tabelle.getRowCount() - 1, tabelle.getRowCount() - 1);
            setTextfelder();
        });
        jButtonMinus.addActionListener(e -> {
            final int selectedTableRow = tabelle.getSelectedRow();
            if (selectedTableRow != -1) {
                ReplaceList.list.remove(selectedTableRow);
                tabelleLaden();
                setTextfelder();
            }
        });
        jButtonUp.addActionListener(e -> upDown(true));
        jButtonDown.addActionListener(e -> upDown(false));
        tabelleLaden();
        setTextfelder();
        tabelle.getSelectionModel().addListSelectionListener(new BeobachterTableSelect());
        jTextFieldVon.getDocument().addDocumentListener(new DocumentListener() {

            @Override
            public void insertUpdate(DocumentEvent e) {
                setVon();
            }

            @Override
            public void removeUpdate(DocumentEvent e) {
                setVon();
            }

            @Override
            public void changedUpdate(DocumentEvent e) {
                setVon();
            }
        });
        jTextFieldNach.getDocument().addDocumentListener(new DocumentListener() {

            @Override
            public void insertUpdate(DocumentEvent e) {
                setNach();
            }

            @Override
            public void removeUpdate(DocumentEvent e) {
                setNach();
            }

            @Override
            public void changedUpdate(DocumentEvent e) {
                setNach();
            }
        });

        var handler = new TextCopyPasteHandler<>(jTextFieldNach);
        jTextFieldNach.setComponentPopupMenu(handler.getPopupMenu());

        handler = new TextCopyPasteHandler<>(jTextFieldVon);
        jTextFieldVon.setComponentPopupMenu(handler.getPopupMenu());

        jCheckBoxTable.addActionListener(e -> MVConfig.add(MVConfig.Configs.SYSTEM_USE_REPLACETABLE, Boolean.toString(jCheckBoxTable.isSelected())));
        jCheckBoxTable.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_USE_REPLACETABLE)));

        jCheckBoxAscii.addActionListener(e -> MVConfig.add(MVConfig.Configs.SYSTEM_ONLY_ASCII, Boolean.toString(jCheckBoxAscii.isSelected())));
        jCheckBoxAscii.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_ONLY_ASCII)));
    }

    private void setVon() {
        if (!stopBeob) {
            final int selectedTableRow = tabelle.getSelectedRow();
            if (selectedTableRow != -1) {
                ReplaceList.list.get(tabelle.convertRowIndexToModel(selectedTableRow))[ReplaceList.VON_NR] = jTextFieldVon.getText(); // leer wird beim suchen aussortiert
                tabelleLaden();
            }
        }
    }

    private void setNach() {
        if (!stopBeob) {
            final int selectedTableRow = tabelle.getSelectedRow();
            if (selectedTableRow != -1) {
                ReplaceList.list.get(tabelle.convertRowIndexToModel(selectedTableRow))[ReplaceList.NACH_NR] = jTextFieldNach.getText();
                tabelleLaden();
            }
        }
    }

    private void upDown(boolean auf) {
        final int rows = tabelle.getSelectedRow();
        if (rows != -1) {
            final int row = tabelle.convertRowIndexToModel(rows);
            final int neu = ReplaceList.up(row, auf);
            tabelleLaden();
            tabelle.setRowSelectionInterval(neu, neu);
            tabelle.scrollRectToVisible(tabelle.getCellRect(neu, 0, true));
        } else {
            NoSelectionErrorDialog.show();
        }

    }

    private void tabelleLaden() {
        stopBeob = true;
        int selectedTableRow = tabelle.getSelectedRow();
        if (selectedTableRow != -1)
            selectedTableRow = tabelle.convertRowIndexToModel(selectedTableRow);

        var model = new NonEditableTableModel(new Object[][]{}, ReplaceList.COLUMN_NAMES);
        model.setRowCount(0);
        Object[] object = new Object[ReplaceList.MAX_ELEM];
        for (String[] s : ReplaceList.list) {
            //object[i] = datenAbo.arr;
            object[0] = s[0];
            object[1] = s[1];
            model.addRow(object);
        }

        tabelle.setModel(model);
        if (selectedTableRow != -1) {
            if (tabelle.getRowCount() > 0 && selectedTableRow < tabelle.getRowCount()) {
                tabelle.setRowSelectionInterval(selectedTableRow, selectedTableRow);
            } else if (tabelle.getRowCount() > 0 && selectedTableRow > 0) {
                tabelle.setRowSelectionInterval(tabelle.getRowCount() - 1, tabelle.getRowCount() - 1);
            } else if (tabelle.getRowCount() > 0) {
                tabelle.setRowSelectionInterval(0, 0);
            }
        } else if (tabelle.getRowCount() > 0) {
            tabelle.setRowSelectionInterval(0, 0);
        }
        jLabelAlert.setVisible(ReplaceList.check());
        stopBeob = false;
    }

    private void setTextfelder() {
        final int selectedTableRow = tabelle.getSelectedRow();
        if (selectedTableRow != -1) {
            jTextFieldVon.setText(tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(selectedTableRow), ReplaceList.VON_NR).toString());
            jTextFieldNach.setText(tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(selectedTableRow), ReplaceList.NACH_NR).toString());
        } else {
            jTextFieldVon.setText("");
            jTextFieldNach.setText("");
        }

        jTextFieldNach.setEnabled(selectedTableRow >= 0);
        jTextFieldVon.setEnabled(selectedTableRow >= 0);
        jButtonUp.setEnabled(selectedTableRow >= 0);
        jButtonDown.setEnabled(selectedTableRow >= 0);
        jLabelNach.setEnabled(selectedTableRow >= 0);
        jLabelVon.setEnabled(selectedTableRow >= 0);
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
    private JCheckBox jCheckBoxTable;
    private JButton jButtonReset;
    private JTable tabelle;
    private JLabel jLabelAlert;
    private JLabel jLabelVon;
    private JTextField jTextFieldVon;
    private JLabel jLabelNach;
    private JTextField jTextFieldNach;
    private JButton jButtonMinus;
    private JButton jButtonPlus;
    private JButton jButtonDown;
    private JButton jButtonUp;
    private JCheckBox jCheckBoxAscii;
    // End of variables declaration//GEN-END:variables

    private class BeobachterTableSelect implements ListSelectionListener {

        @Override
        public void valueChanged(ListSelectionEvent event) {
            if (!stopBeob) {
                if (!event.getValueIsAdjusting()) {
                    stopBeob = true;
                    setTextfelder();
                    stopBeob = false;
                }
            }
        }
    }

}
