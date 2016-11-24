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

import mSearch.tool.Listener;
import mSearch.tool.ReplaceList;
import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.config.MVConfig;
import mediathek.gui.PanelVorlage;
import mediathek.tool.HinweisKeineAuswahl;
import mediathek.tool.TModel;
import mediathek.tool.TextCopyPaste;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import java.awt.*;
import java.util.Iterator;

@SuppressWarnings("serial")
public class PanelDateinamen extends PanelVorlage {
    public boolean ok = false;
    public String ziel = "";
    private static final Color cGruen = new Color(0, 153, 51);
    private static final Color cRot = new Color(255, 0, 0);

    public PanelDateinamen(Daten d, JFrame pparentComponent) {
        super(d, pparentComponent);
        initComponents();
        daten = d;
        Listener.addListener(new Listener(Listener.EREIGNIS_REPLACELIST_CHANGED, PanelDateinamen.class.getSimpleName()) {
            @Override
            public void ping() {
                tabelleLaden();
                setTextfelder();
            }
        });
        jLabelAlert.setVisible(false);
        jLabelAlert.setText("");
        jLabelAlert.setIcon(Icons.ICON_ACHTUNG_32);
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
            int selectedTableRow = tabelle.getSelectedRow();
            if (selectedTableRow >= 0) {
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
        jTextFieldNach.addMouseListener(new TextCopyPaste());
        jTextFieldVon.addMouseListener(new TextCopyPaste());

        jCheckBoxTable.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_USE_REPLACETABLE, Boolean.toString(jCheckBoxTable.isSelected()));
            setColor(jCheckBoxTable);
        });
        jCheckBoxTable.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_USE_REPLACETABLE)));
        setColor(jCheckBoxTable);

        jCheckBoxAscii.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_ONLY_ASCII, Boolean.toString(jCheckBoxAscii.isSelected()));
            setColor(jCheckBoxAscii);
        });
        jCheckBoxAscii.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_ONLY_ASCII)));
        setColor(jCheckBoxAscii);
    }

    private void setColor(final JCheckBox cb) {
        cb.setForeground(cb.isSelected() ? cGruen : cRot);
    }

    private void setVon() {
        if (!stopBeob) {
            int selectedTableRow = tabelle.getSelectedRow();
            if (selectedTableRow >= 0) {
//                Daten.mVReplaceList.list.get(tabelle.convertRowIndexToModel(selectedTableRow))[MVReplaceList.VON_NR]
//                        = jTextFieldVon.getText().isEmpty() ? " " : jTextFieldVon.getText(); // nicht nach nix suchen
                ReplaceList.list.get(tabelle.convertRowIndexToModel(selectedTableRow))[ReplaceList.VON_NR] = jTextFieldVon.getText(); // leer wird beim suchen aussortiert
                tabelleLaden();
            }
        }
    }

    private void setNach() {
        if (!stopBeob) {
            int selectedTableRow = tabelle.getSelectedRow();
            if (selectedTableRow >= 0) {
                ReplaceList.list.get(tabelle.convertRowIndexToModel(selectedTableRow))[ReplaceList.NACH_NR] = jTextFieldNach.getText();
                tabelleLaden();
            }
        }
    }

    private void upDown(boolean auf) {
        int rows = tabelle.getSelectedRow();
        if (rows != -1) {
            int row = tabelle.convertRowIndexToModel(rows);
            int neu = ReplaceList.up(row, auf);
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
        TModel model = new TModel(new Object[][]{}, ReplaceList.COLUMN_NAMES);
        Object[] object;
        model.setRowCount(0);
        Iterator<String[]> iterator = ReplaceList.list.iterator();
        object = new Object[ReplaceList.MAX_ELEM];
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
        jLabelAlert.setVisible(ReplaceList.check());
        stopBeob = false;
    }

    private void setTextfelder() {
        int selectedTableRow = tabelle.getSelectedRow();

        if (selectedTableRow >= 0) {
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
    private void initComponents() {

        javax.swing.JTabbedPane jTabbedPane1 = new javax.swing.JTabbedPane();
        javax.swing.JPanel jPanel1 = new javax.swing.JPanel();
        javax.swing.JScrollPane jScrollPane5 = new javax.swing.JScrollPane();
        javax.swing.JTextArea jTextArea3 = new javax.swing.JTextArea();
        javax.swing.JPanel jPanel2 = new javax.swing.JPanel();
        jCheckBoxTable = new javax.swing.JCheckBox();
        javax.swing.JPanel jPanel3 = new javax.swing.JPanel();
        jButtonReset = new javax.swing.JButton();
        javax.swing.JScrollPane jScrollPane3 = new javax.swing.JScrollPane();
        javax.swing.JTextArea jTextArea2 = new javax.swing.JTextArea();
        javax.swing.JScrollPane jScrollPane4 = new javax.swing.JScrollPane();
        tabelle = new javax.swing.JTable();
        jLabelAlert = new javax.swing.JLabel();
        jLabelVon = new javax.swing.JLabel();
        jTextFieldVon = new javax.swing.JTextField();
        jLabelNach = new javax.swing.JLabel();
        jTextFieldNach = new javax.swing.JTextField();
        jButtonMinus = new javax.swing.JButton();
        jButtonPlus = new javax.swing.JButton();
        jButtonDown = new javax.swing.JButton();
        jButtonUp = new javax.swing.JButton();
        jCheckBoxAscii = new javax.swing.JCheckBox();
        javax.swing.JPanel jPanel4 = new javax.swing.JPanel();
        javax.swing.JScrollPane jScrollPane1 = new javax.swing.JScrollPane();
        javax.swing.JTextArea jTextArea1 = new javax.swing.JTextArea();

        jTextArea3.setEditable(false);
        jTextArea3.setColumns(20);
        jTextArea3.setRows(5);
        jTextArea3.setText("\nDie Dateinamen werden für jedes Betriebssystem passend aufbereitet.\n\nWer will, kann darüber hinaus weitere Einstellungen mit einer Ersetzungstabelle\nvornehmen: z.B. \"ß\" durch \"ss\" ersetzen.\n");
        jTextArea3.setMargin(new java.awt.Insets(3, 3, 3, 3));
        jScrollPane5.setViewportView(jTextArea3);

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
                jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanel1Layout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jScrollPane5, javax.swing.GroupLayout.DEFAULT_SIZE, 608, Short.MAX_VALUE)
                                .addContainerGap())
        );
        jPanel1Layout.setVerticalGroup(
                jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanel1Layout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jScrollPane5, javax.swing.GroupLayout.PREFERRED_SIZE, 119, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addContainerGap(472, Short.MAX_VALUE))
        );

        jTabbedPane1.addTab("Dateinamen", jPanel1);

        jCheckBoxTable.setText("Ersetzungstabelle");

        jPanel3.setBorder(javax.swing.BorderFactory.createEtchedBorder());

        jButtonReset.setText("Tabelle zurücksetzen");

        jScrollPane3.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));

        jTextArea2.setEditable(false);
        jTextArea2.setBackground(javax.swing.UIManager.getDefaults().getColor("Label.background"));
        jTextArea2.setColumns(20);
        jTextArea2.setRows(4);
        jTextArea2.setText("Die Tabelle wird von oben nach unten abgearbeitet.\nEs ist also möglich, dass eine Ersetzung durch eine weitere\nwieder ersetzt wird!");
        jTextArea2.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));
        jScrollPane3.setViewportView(jTextArea2);

        tabelle.setModel(new javax.swing.table.DefaultTableModel(
                new Object [][] {
                        {null, null, null, null},
                        {null, null, null, null},
                        {null, null, null, null},
                        {null, null, null, null}
                },
                new String [] {
                        "Title 1", "Title 2", "Title 3", "Title 4"
                }
        ));
        jScrollPane4.setViewportView(tabelle);

        jLabelAlert.setText("Achtung");

        jLabelVon.setText("von:");

        jLabelNach.setText("nach:");

        jButtonMinus.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-remove.png"))); // NOI18N

        jButtonPlus.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-add.png"))); // NOI18N

        jButtonDown.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-move-down.png"))); // NOI18N

        jButtonUp.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-move-up.png"))); // NOI18N

        javax.swing.GroupLayout jPanel3Layout = new javax.swing.GroupLayout(jPanel3);
        jPanel3.setLayout(jPanel3Layout);
        jPanel3Layout.setHorizontalGroup(
                jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanel3Layout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                        .addComponent(jScrollPane4)
                                        .addGroup(jPanel3Layout.createSequentialGroup()
                                                .addComponent(jScrollPane3)
                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                                .addComponent(jLabelAlert))
                                        .addGroup(jPanel3Layout.createSequentialGroup()
                                                .addComponent(jButtonReset)
                                                .addGap(0, 0, Short.MAX_VALUE))
                                        .addGroup(jPanel3Layout.createSequentialGroup()
                                                .addComponent(jLabelVon)
                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                                .addComponent(jTextFieldVon, javax.swing.GroupLayout.PREFERRED_SIZE, 100, javax.swing.GroupLayout.PREFERRED_SIZE)
                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                                .addComponent(jLabelNach)
                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                                .addComponent(jTextFieldNach, javax.swing.GroupLayout.PREFERRED_SIZE, 100, javax.swing.GroupLayout.PREFERRED_SIZE)
                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                                .addComponent(jButtonUp)
                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                                .addComponent(jButtonDown)
                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                                .addComponent(jButtonPlus)
                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                                .addComponent(jButtonMinus)))
                                .addGap(15, 15, 15))
        );
        jPanel3Layout.setVerticalGroup(
                jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel3Layout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jScrollPane4, javax.swing.GroupLayout.DEFAULT_SIZE, 464, Short.MAX_VALUE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                                        .addComponent(jLabelVon)
                                        .addComponent(jTextFieldVon, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                        .addComponent(jLabelNach)
                                        .addComponent(jTextFieldNach, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                        .addComponent(jButtonUp)
                                        .addComponent(jButtonDown)
                                        .addComponent(jButtonPlus)
                                        .addComponent(jButtonMinus))
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                                        .addComponent(jScrollPane3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                        .addComponent(jLabelAlert))
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jButtonReset)
                                .addContainerGap())
        );

        jPanel3Layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonDown, jButtonMinus, jButtonPlus, jButtonUp, jLabelNach, jLabelVon, jTextFieldNach, jTextFieldVon});

        jCheckBoxAscii.setText("nur ASCII-Zeichen erlauben");

        jPanel4.setBorder(javax.swing.BorderFactory.createEtchedBorder());

        jScrollPane1.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));

        jTextArea1.setEditable(false);
        jTextArea1.setBackground(javax.swing.UIManager.getDefaults().getColor("Label.background"));
        jTextArea1.setColumns(20);
        jTextArea1.setRows(3);
        jTextArea1.setText("Es werden alle Zeichen \"über 127\" ersetzt. \nAuch Umlaute wie \"ö -> oe\" werden ersetzt.\nWenn die Ersetzungstabelle aktiv ist, wird sie vorher abgearbeitet.");
        jTextArea1.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));
        jScrollPane1.setViewportView(jTextArea1);

        javax.swing.GroupLayout jPanel4Layout = new javax.swing.GroupLayout(jPanel4);
        jPanel4.setLayout(jPanel4Layout);
        jPanel4Layout.setHorizontalGroup(
                jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanel4Layout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jScrollPane1)
                                .addContainerGap())
        );
        jPanel4Layout.setVerticalGroup(
                jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanel4Layout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jScrollPane1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addContainerGap(13, Short.MAX_VALUE))
        );

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
                jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanel2Layout.createSequentialGroup()
                                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                        .addGroup(jPanel2Layout.createSequentialGroup()
                                                .addGap(29, 29, 29)
                                                .addComponent(jPanel4, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                                        .addGroup(jPanel2Layout.createSequentialGroup()
                                                .addContainerGap()
                                                .addComponent(jCheckBoxAscii)
                                                .addGap(0, 0, Short.MAX_VALUE))
                                        .addGroup(jPanel2Layout.createSequentialGroup()
                                                .addContainerGap()
                                                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                                        .addGroup(jPanel2Layout.createSequentialGroup()
                                                                .addGap(21, 21, 21)
                                                                .addComponent(jPanel3, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                                                        .addGroup(jPanel2Layout.createSequentialGroup()
                                                                .addComponent(jCheckBoxTable)
                                                                .addGap(0, 0, Short.MAX_VALUE)))))
                                .addContainerGap())
        );
        jPanel2Layout.setVerticalGroup(
                jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanel2Layout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jCheckBoxTable)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jPanel3, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                .addGap(18, 18, 18)
                                .addComponent(jCheckBoxAscii)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jPanel4, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addContainerGap())
        );

        jTabbedPane1.addTab("Eigene Einstellungen", jPanel2);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
                layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jTabbedPane1)
                                .addContainerGap())
        );
        layout.setVerticalGroup(
                layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(layout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jTabbedPane1)
                                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonDown;
    private javax.swing.JButton jButtonMinus;
    private javax.swing.JButton jButtonPlus;
    private javax.swing.JButton jButtonReset;
    private javax.swing.JButton jButtonUp;
    private javax.swing.JCheckBox jCheckBoxAscii;
    private javax.swing.JCheckBox jCheckBoxTable;
    private javax.swing.JLabel jLabelAlert;
    private javax.swing.JLabel jLabelNach;
    private javax.swing.JLabel jLabelVon;
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

}
