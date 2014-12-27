/*    
 *    MediathekView
 *    Copyright (C) 2008   W. Xaver
 *    W.Xaver[at]googlemail.com
 *    http://zdfmediathk.sourceforge.net/
 *    
 *    This program is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.gui.dialogEinstellungen;

import java.awt.Color;
import java.awt.Component;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Iterator;
import javax.swing.JCheckBox;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import mediathek.daten.Daten;
import mediathek.res.GetIcon;
import mediathek.tool.EscBeenden;
import mediathek.tool.HinweisKeineAuswahl;
import mediathek.tool.MVConfig;
import mediathek.tool.MVReplaceList;
import mediathek.tool.TModel;

public class DialogErsetzungstabelle extends javax.swing.JDialog {

    public boolean ok = false;
    public String ziel = "";
    private Frame parentComponent = null;
    private boolean stopBeob = false;
    private final Color cGruen = new Color(0, 153, 51);
    private final Color cRot = new Color(255, 0, 0);

    /**
     *
     * @param parent
     */
    public DialogErsetzungstabelle(java.awt.Frame parent) {
        super(parent, true);
        parentComponent = parent;
        initComponents();
        new EscBeenden(this) {
            @Override
            public void beenden_() {
                ok = false;
                beenden();
            }
        };
        jLabelAlert.setVisible(false);
        jButtonPlus.setIcon(GetIcon.getProgramIcon("add_16.png"));
        jButtonMinus.setIcon(GetIcon.getProgramIcon("remove_16.png"));
        jButtonUp.setIcon(GetIcon.getProgramIcon("move_up_16.png"));
        jButtonDown.setIcon(GetIcon.getProgramIcon("move_down_16.png"));
        jButtonOk.addActionListener(new OkBeobachter());
        jButtonReset.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.mVReplaceList.init();
                tabelleLaden();
                setTextfelder();
            }
        });
        jButtonPlus.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.mVReplaceList.list.add(new String[]{"von", "nach"});
                tabelleLaden();
                tabelle.setRowSelectionInterval(tabelle.getRowCount() - 1, tabelle.getRowCount() - 1);
                setTextfelder();
            }
        });
        jButtonMinus.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                int selectedTableRow = tabelle.getSelectedRow();
                if (selectedTableRow >= 0) {
                    Daten.mVReplaceList.list.remove(selectedTableRow);
                    tabelleLaden();
                    setTextfelder();
                }
            }
        });
        jButtonUp.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                upDown(true);
            }
        });
        jButtonDown.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                upDown(false);
            }
        });
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

        jCheckBoxTable.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.mVConfig.add(MVConfig.SYSTEM_USE_REPLACETABLE, Boolean.toString(jCheckBoxTable.isSelected()));
                setColor(jCheckBoxTable, jCheckBoxTable.isSelected());
            }
        });
        jCheckBoxTable.setSelected(Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_USE_REPLACETABLE)));
        setColor(jCheckBoxTable, jCheckBoxTable.isSelected());

        jCheckBoxAscii.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.mVConfig.add(MVConfig.SYSTEM_ONLY_ASCII, Boolean.toString(jCheckBoxAscii.isSelected()));
                setColor(jCheckBoxAscii, jCheckBoxAscii.isSelected());
            }
        });
        jCheckBoxAscii.setSelected(Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_ONLY_ASCII)));
        setColor(jCheckBoxAscii, jCheckBoxAscii.isSelected());
    }

    private void setColor(JCheckBox cb, boolean en) {
        cb.setForeground(cb.isSelected() ? cGruen : cRot);
    }

    private void setVon() {
        if (!stopBeob) {
            int selectedTableRow = tabelle.getSelectedRow();
            if (selectedTableRow >= 0) {
                Daten.mVReplaceList.list.get(tabelle.convertRowIndexToModel(selectedTableRow))[MVReplaceList.VON_NR]
                        = jTextFieldVon.getText().isEmpty() ? " " : jTextFieldVon.getText(); // nicht nach nix suchen
                tabelleLaden();
            }
        }
    }

    private void setNach() {
        if (!stopBeob) {
            int selectedTableRow = tabelle.getSelectedRow();
            if (selectedTableRow >= 0) {
                Daten.mVReplaceList.list.get(tabelle.convertRowIndexToModel(selectedTableRow))[MVReplaceList.NACH_NR] = jTextFieldNach.getText();
                tabelleLaden();
            }
        }
    }

    private void upDown(boolean auf) {
        int rows = tabelle.getSelectedRow();
        if (rows != -1) {
            int row = tabelle.convertRowIndexToModel(rows);
            int neu = Daten.mVReplaceList.up(row, auf);
            tabelleLaden();
            tabelle.setRowSelectionInterval(neu, neu);
            tabelle.scrollRectToVisible(tabelle.getCellRect(neu, 0, true));
        } else {
            new HinweisKeineAuswahl().zeigen(parentComponent);
        }

    }

    private void tabelleLaden() {
        stopBeob = true;
        int selectedTableRow = tabelle.getSelectedRow();
        if (selectedTableRow >= 0) {
            selectedTableRow = tabelle.convertRowIndexToModel(selectedTableRow);
        }
        TModel model = new TModel(new Object[][]{}, MVReplaceList.COLUMN_NAMES);
        Object[] object;
        model.setRowCount(0);
        Iterator<String[]> iterator = Daten.mVReplaceList.list.iterator();
        object = new Object[MVReplaceList.MAX_ELEM];
        while (iterator.hasNext()) {
            String[] s = iterator.next();
            //object[i] = datenAbo.arr;
            object[0] = s[0];
            object[1] = s[1];
            model.addRow(object);
        }
        tabelle.setModel(model);
        if (selectedTableRow >= 0) {
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
        jLabelAlert.setVisible(Daten.mVReplaceList.check());
        stopBeob = false;
    }

    private void setTextfelder() {
        int selectedTableRow = tabelle.getSelectedRow();

        if (selectedTableRow >= 0) {
            jTextFieldVon.setText(tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(selectedTableRow), MVReplaceList.VON_NR).toString());
            jTextFieldNach.setText(tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(selectedTableRow), MVReplaceList.NACH_NR).toString());
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

    private void beenden() {
        Iterator<String[]> iterator = Daten.mVReplaceList.list.iterator();
        while (iterator.hasNext()) {
            String[] s = iterator.next();
            if (s[0].isEmpty() && s[1].isEmpty()) {
                iterator.remove();
            }
        }
        this.dispose();
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jButtonOk = new javax.swing.JButton();
        jPanelAscii = new javax.swing.JPanel();
        jScrollPane3 = new javax.swing.JScrollPane();
        jTextArea2 = new javax.swing.JTextArea();
        jPanelTable = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        tabelle = new javax.swing.JTable();
        jLabelVon = new javax.swing.JLabel();
        jTextFieldVon = new javax.swing.JTextField();
        jLabelNach = new javax.swing.JLabel();
        jTextFieldNach = new javax.swing.JTextField();
        jButtonUp = new javax.swing.JButton();
        jButtonDown = new javax.swing.JButton();
        jButtonPlus = new javax.swing.JButton();
        jButtonMinus = new javax.swing.JButton();
        jScrollPane2 = new javax.swing.JScrollPane();
        jTextArea1 = new javax.swing.JTextArea();
        jLabelAlert = new javax.swing.JLabel();
        jButtonReset = new javax.swing.JButton();
        jCheckBoxTable = new javax.swing.JCheckBox();
        jCheckBoxAscii = new javax.swing.JCheckBox();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);

        jButtonOk.setText("Ok");

        jPanelAscii.setBorder(javax.swing.BorderFactory.createEtchedBorder());

        jScrollPane3.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));

        jTextArea2.setEditable(false);
        jTextArea2.setBackground(javax.swing.UIManager.getDefaults().getColor("Label.background"));
        jTextArea2.setText("Es werden alle Zeichen \"über 127\" ersetzt. Auch Umlaute \"ö -> oe\" werden ersetzt.\nWenn die Ersetzungstabelle aktiv ist, wird sie vorher abgearbeitet.");
        jTextArea2.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));
        jScrollPane3.setViewportView(jTextArea2);

        javax.swing.GroupLayout jPanelAsciiLayout = new javax.swing.GroupLayout(jPanelAscii);
        jPanelAscii.setLayout(jPanelAsciiLayout);
        jPanelAsciiLayout.setHorizontalGroup(
            jPanelAsciiLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelAsciiLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jScrollPane3)
                .addGap(37, 37, 37))
        );
        jPanelAsciiLayout.setVerticalGroup(
            jPanelAsciiLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelAsciiLayout.createSequentialGroup()
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addComponent(jScrollPane3, javax.swing.GroupLayout.PREFERRED_SIZE, 64, javax.swing.GroupLayout.PREFERRED_SIZE))
        );

        jPanelTable.setBorder(javax.swing.BorderFactory.createEtchedBorder());

        tabelle.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {
                {null, null},
                {null, null},
                {null, null}
            },
            new String [] {
                "Title 1", "Title 2"
            }
        ));
        jScrollPane1.setViewportView(tabelle);

        jLabelVon.setText("von:");

        jLabelNach.setText("nach:");

        jButtonUp.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/move_up_16.png"))); // NOI18N

        jButtonDown.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/move_down_16.png"))); // NOI18N

        jButtonPlus.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/add_16.png"))); // NOI18N

        jButtonMinus.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/remove_16.png"))); // NOI18N

        jScrollPane2.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));

        jTextArea1.setEditable(false);
        jTextArea1.setBackground(javax.swing.UIManager.getDefaults().getColor("Label.background"));
        jTextArea1.setColumns(20);
        jTextArea1.setRows(5);
        jTextArea1.setText("Die Tabelle wird von oben nach unten\nabgearbeitet. Es ist also möglich, \ndass eine Ersetzung durch eine weitere\nwieder ersetzt wird!");
        jTextArea1.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));
        jScrollPane2.setViewportView(jTextArea1);

        jLabelAlert.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/alert_32.png"))); // NOI18N

        jButtonReset.setText("Tabelle zurücksetzen");

        javax.swing.GroupLayout jPanelTableLayout = new javax.swing.GroupLayout(jPanelTable);
        jPanelTable.setLayout(jPanelTableLayout);
        jPanelTableLayout.setHorizontalGroup(
            jPanelTableLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelTableLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelTableLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPane1)
                    .addGroup(jPanelTableLayout.createSequentialGroup()
                        .addComponent(jLabelVon)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldVon, javax.swing.GroupLayout.PREFERRED_SIZE, 97, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabelNach)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldNach, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 32, Short.MAX_VALUE)
                        .addComponent(jButtonUp)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonDown)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonPlus)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonMinus))
                    .addGroup(jPanelTableLayout.createSequentialGroup()
                        .addGroup(jPanelTableLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(jPanelTableLayout.createSequentialGroup()
                                .addComponent(jScrollPane2, javax.swing.GroupLayout.PREFERRED_SIZE, 409, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jLabelAlert))
                            .addComponent(jButtonReset))
                        .addGap(0, 0, Short.MAX_VALUE)))
                .addContainerGap())
        );

        jPanelTableLayout.linkSize(javax.swing.SwingConstants.HORIZONTAL, new java.awt.Component[] {jTextFieldNach, jTextFieldVon});

        jPanelTableLayout.setVerticalGroup(
            jPanelTableLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelTableLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jScrollPane1, javax.swing.GroupLayout.PREFERRED_SIZE, 181, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanelTableLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jLabelVon)
                    .addComponent(jTextFieldVon, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabelNach)
                    .addComponent(jTextFieldNach, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jButtonUp)
                    .addComponent(jButtonDown)
                    .addComponent(jButtonPlus)
                    .addComponent(jButtonMinus))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanelTableLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jLabelAlert)
                    .addComponent(jScrollPane2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jButtonReset)
                .addContainerGap(48, Short.MAX_VALUE))
        );

        jCheckBoxTable.setText("Ersetzungstabelle");

        jCheckBoxAscii.setText("\"nur ASCII-Zeichen erlauben\"");

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addGroup(javax.swing.GroupLayout.Alignment.LEADING, layout.createSequentialGroup()
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jCheckBoxAscii)
                            .addComponent(jCheckBoxTable))
                        .addGap(0, 0, Short.MAX_VALUE))
                    .addGroup(layout.createSequentialGroup()
                        .addGap(0, 0, Short.MAX_VALUE)
                        .addComponent(jButtonOk, javax.swing.GroupLayout.PREFERRED_SIZE, 118, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addGroup(layout.createSequentialGroup()
                        .addGap(21, 21, 21)
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                            .addComponent(jPanelTable, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(jPanelAscii, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jCheckBoxTable)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanelTable, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(18, 18, 18)
                .addComponent(jCheckBoxAscii)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanelAscii, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(18, 18, 18)
                .addComponent(jButtonOk)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonDown;
    private javax.swing.JButton jButtonMinus;
    private javax.swing.JButton jButtonOk;
    private javax.swing.JButton jButtonPlus;
    private javax.swing.JButton jButtonReset;
    private javax.swing.JButton jButtonUp;
    private javax.swing.JCheckBox jCheckBoxAscii;
    private javax.swing.JCheckBox jCheckBoxTable;
    private javax.swing.JLabel jLabelAlert;
    private javax.swing.JLabel jLabelNach;
    private javax.swing.JLabel jLabelVon;
    private javax.swing.JPanel jPanelAscii;
    private javax.swing.JPanel jPanelTable;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JScrollPane jScrollPane3;
    private javax.swing.JTextArea jTextArea1;
    private javax.swing.JTextArea jTextArea2;
    private javax.swing.JTextField jTextFieldNach;
    private javax.swing.JTextField jTextFieldVon;
    private javax.swing.JTable tabelle;
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

    private class OkBeobachter implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            beenden();
        }
    }

}
