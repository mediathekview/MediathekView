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

import java.awt.Component;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import mediathek.daten.DDaten;
import mediathek.daten.Daten;
import mediathek.daten.DatenBlacklist;
import mediathek.daten.ListeBlacklist;
import mediathek.gui.PanelVorlage;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.Konstanten;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.TModel;

public class PanelBlacklist extends PanelVorlage {

    public boolean ok = false;
    public String ziel;
    private String[] sender;
    private String[][] themenPerSender;

    public PanelBlacklist(DDaten d, Component parentComponent) {
        super(d, parentComponent);
        initComponents();
        init();
    }

    private void init() {
        jTableSender.addMouseListener(new BeobMausTabelle());
        jList1.addMouseListener(new BeobMausListe());
        jButtonTabelleLoeschen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                ddaten.listeBlacklist.clear();
            }
        });
        jButtonListeLoeschen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.system[Konstanten.SYSTEM_BLACKLIST_TITEL_NR] = "";
                listeLaden();
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_BLACKLIST_GEAENDERT, PanelBlacklist.class.getSimpleName());
            }
        });
        jButtonPlus.addActionListener(new BeobAddListe());
        jTextFieldTitel.addActionListener(new BeobAddListe());
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_BLACKLIST_GEAENDERT, PanelBlacklist.class.getSimpleName()) {
            @Override
            public void ping() {
                tabelleLaden();
            }
        });
        jCheckBoxZukunftNichtAnzeigen.setSelected(Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_BLACKLIST_ZUKUNFT_NICHT_ANZEIGEN_NR]));
        jCheckBoxZukunftNichtAnzeigen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.system[Konstanten.SYSTEM_BLACKLIST_ZUKUNFT_NICHT_ANZEIGEN_NR] = Boolean.toString(jCheckBoxZukunftNichtAnzeigen.isSelected());
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_BLACKLIST_GEAENDERT, PanelBlacklist.class.getSimpleName());
            }
        });

        jButtonHinzufuegen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                String se = jComboBoxSender.getSelectedItem().toString();
                String th = jComboBoxThema.getSelectedItem().toString();
                if (!se.equals("") || !th.equals("")) {
                    ddaten.listeBlacklist.add(new DatenBlacklist(se, th));
                }
            }
        });
        jComboBoxSender.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                comboThemaLaden();
            }
        });
        initCombo();
        comboThemaLaden();
        tabelleLaden();
        listeLaden();
    }

    private void comboThemaLaden() {
        String filterSender = jComboBoxSender.getSelectedItem().toString();
        if (filterSender.equals("")) {
            jComboBoxThema.setModel(new javax.swing.DefaultComboBoxModel(getThemen("")));
        } else {
            jComboBoxThema.setModel(new javax.swing.DefaultComboBoxModel(getThemen(filterSender)));
        }

    }

    private String[] getThemen(String ssender) {
        for (int i = 1; i < themenPerSender.length; ++i) {
            if (sender[i].equals(ssender)) {
                return themenPerSender[i];
            }
        }
        //return alleThemen;
        return themenPerSender[0];
    }

    private void initCombo() {
        // der erste Sender ist ""
        sender = GuiFunktionen.addLeerListe(DDaten.filmeLaden.getSenderNamen());
        jComboBoxSender.setModel(new javax.swing.DefaultComboBoxModel(sender));
        //für den Sender "" sind alle Themen im themenPerSender[0]
        themenPerSender = new String[sender.length][];
        for (int i = 0; i < sender.length; ++i) {
            themenPerSender[i] = DDaten.listeFilmeNachBlackList.getModelOfFieldThema(sender[i]);
        }
    }

    private void tabelleLaden() {
        jTableSender.setModel(new TModel(ddaten.listeBlacklist.getObjectData(), DatenBlacklist.BLACKLIST_COLUMN_NAMES_ANZEIGE));
    }

    private void listeLaden() {
        jList1.setModel(new DefaultComboBoxModel(ListeBlacklist.getBlacklistTitel()));
    }

    private boolean bereitsInListe(String s) {
        for (String st : ListeBlacklist.getBlacklistTitel()) {
            if (st.equalsIgnoreCase(s)) {
                return true;
            }
        }
        return false;
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jPanel2 = new javax.swing.JPanel();
        label1 = new java.awt.Label();
        jTabbedPane1 = new javax.swing.JTabbedPane();
        jPanel1 = new javax.swing.JPanel();
        javax.swing.JScrollPane jScrollPane1 = new javax.swing.JScrollPane();
        jTableSender = new javax.swing.JTable();
        jButtonTabelleLoeschen = new javax.swing.JButton();
        jLabel1 = new javax.swing.JLabel();
        jPanel4 = new javax.swing.JPanel();
        jLabel5 = new javax.swing.JLabel();
        jComboBoxSender = new javax.swing.JComboBox();
        jLabel6 = new javax.swing.JLabel();
        jComboBoxThema = new javax.swing.JComboBox();
        jButtonHinzufuegen = new javax.swing.JButton();
        jLabel7 = new javax.swing.JLabel();
        jPanel3 = new javax.swing.JPanel();
        jPanel5 = new javax.swing.JPanel();
        jLabel3 = new javax.swing.JLabel();
        jCheckBoxZukunftNichtAnzeigen = new javax.swing.JCheckBox();
        jPanel6 = new javax.swing.JPanel();
        jLabel2 = new javax.swing.JLabel();
        jLabel4 = new javax.swing.JLabel();
        jScrollPane2 = new javax.swing.JScrollPane();
        jList1 = new javax.swing.JList();
        jTextFieldTitel = new javax.swing.JTextField();
        jButtonListeLoeschen = new javax.swing.JButton();
        jButtonPlus = new javax.swing.JButton();

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 100, Short.MAX_VALUE)
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 100, Short.MAX_VALUE)
        );

        label1.setText("label1");

        jTableSender.setAutoCreateRowSorter(true);
        jScrollPane1.setViewportView(jTableSender);

        jButtonTabelleLoeschen.setText("Liste löschen");

        jLabel1.setText("Sender / Sender-Thema in der Filmliste löschen");

        jPanel4.setBorder(javax.swing.BorderFactory.createEtchedBorder());

        jLabel5.setText("Sender:");

        jComboBoxSender.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));

        jLabel6.setText("Thema:");

        jComboBoxThema.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));

        jButtonHinzufuegen.setText("hinzufügen");

        jLabel7.setText("Sender oder Thema oder Sender/Thema:");

        javax.swing.GroupLayout jPanel4Layout = new javax.swing.GroupLayout(jPanel4);
        jPanel4.setLayout(jPanel4Layout);
        jPanel4Layout.setHorizontalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel4Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel4Layout.createSequentialGroup()
                        .addGap(0, 0, Short.MAX_VALUE)
                        .addComponent(jButtonHinzufuegen))
                    .addGroup(jPanel4Layout.createSequentialGroup()
                        .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabel5)
                            .addComponent(jLabel6))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jComboBoxThema, 0, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(jComboBoxSender, 0, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)))
                    .addGroup(jPanel4Layout.createSequentialGroup()
                        .addComponent(jLabel7)
                        .addGap(0, 0, Short.MAX_VALUE)))
                .addContainerGap())
        );
        jPanel4Layout.setVerticalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel4Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabel7)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel5)
                    .addComponent(jComboBoxSender, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel6)
                    .addComponent(jComboBoxThema, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jButtonHinzufuegen)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPane1, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 490, Short.MAX_VALUE)
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addComponent(jLabel1)
                        .addGap(0, 0, Short.MAX_VALUE))
                    .addComponent(jPanel4, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel1Layout.createSequentialGroup()
                        .addGap(0, 0, Short.MAX_VALUE)
                        .addComponent(jButtonTabelleLoeschen)))
                .addContainerGap())
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabel1)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane1, javax.swing.GroupLayout.PREFERRED_SIZE, 173, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jPanel4, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jButtonTabelleLoeschen)
                .addContainerGap(127, Short.MAX_VALUE))
        );

        jTabbedPane1.addTab("Sender-Thema", jPanel1);

        jPanel5.setBorder(javax.swing.BorderFactory.createEtchedBorder());

        jLabel3.setText("Filme deren Datum in der Zukunft liegt, sind meist nur Trailer");

        jCheckBoxZukunftNichtAnzeigen.setText("Filme mit Datum in der Zukunft nicht anzeigen");

        javax.swing.GroupLayout jPanel5Layout = new javax.swing.GroupLayout(jPanel5);
        jPanel5.setLayout(jPanel5Layout);
        jPanel5Layout.setHorizontalGroup(
            jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel5Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jLabel3)
                    .addComponent(jCheckBoxZukunftNichtAnzeigen))
                .addContainerGap(40, Short.MAX_VALUE))
        );
        jPanel5Layout.setVerticalGroup(
            jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel5Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabel3)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jCheckBoxZukunftNichtAnzeigen)
                .addContainerGap(42, Short.MAX_VALUE))
        );

        jPanel6.setBorder(javax.swing.BorderFactory.createEtchedBorder());

        jLabel2.setText("Titel die den Begriff enthalten in der Filmliste löschen");

        jLabel4.setText("Groß- und Kleinschreibung wird nicht unterschieden");

        jScrollPane2.setViewportView(jList1);

        jButtonListeLoeschen.setText("Liste löschen");

        jButtonPlus.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/edit_add_16.png"))); // NOI18N

        javax.swing.GroupLayout jPanel6Layout = new javax.swing.GroupLayout(jPanel6);
        jPanel6.setLayout(jPanel6Layout);
        jPanel6Layout.setHorizontalGroup(
            jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel6Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPane2)
                    .addGroup(jPanel6Layout.createSequentialGroup()
                        .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabel2)
                            .addComponent(jLabel4))
                        .addGap(0, 0, Short.MAX_VALUE))
                    .addComponent(jButtonListeLoeschen, javax.swing.GroupLayout.Alignment.TRAILING)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel6Layout.createSequentialGroup()
                        .addComponent(jTextFieldTitel)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(jButtonPlus)))
                .addContainerGap())
        );
        jPanel6Layout.setVerticalGroup(
            jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel6Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabel2)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jLabel4)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane2, javax.swing.GroupLayout.PREFERRED_SIZE, 180, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jTextFieldTitel, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jButtonPlus))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jButtonListeLoeschen)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel6Layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonPlus, jTextFieldTitel});

        javax.swing.GroupLayout jPanel3Layout = new javax.swing.GroupLayout(jPanel3);
        jPanel3.setLayout(jPanel3Layout);
        jPanel3Layout.setHorizontalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanel6, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel5, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        jPanel3Layout.setVerticalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel6, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jPanel5, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(81, Short.MAX_VALUE))
        );

        jTabbedPane1.addTab("Titel-Zeit", jPanel3);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
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
    private javax.swing.JButton jButtonHinzufuegen;
    private javax.swing.JButton jButtonListeLoeschen;
    private javax.swing.JButton jButtonPlus;
    private javax.swing.JButton jButtonTabelleLoeschen;
    private javax.swing.JCheckBox jCheckBoxZukunftNichtAnzeigen;
    private javax.swing.JComboBox jComboBoxSender;
    private javax.swing.JComboBox jComboBoxThema;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JList jList1;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JPanel jPanel6;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JTabbedPane jTabbedPane1;
    private javax.swing.JTable jTableSender;
    private javax.swing.JTextField jTextFieldTitel;
    private java.awt.Label label1;
    // End of variables declaration//GEN-END:variables

    private class BeobAddListe implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            String b = jTextFieldTitel.getText().trim().toLowerCase();
            if (!b.equals("") && !bereitsInListe(b)) {
                if (Daten.system[Konstanten.SYSTEM_BLACKLIST_TITEL_NR].equals("")) {
                    Daten.system[Konstanten.SYSTEM_BLACKLIST_TITEL_NR] = b;
                } else {
                    Daten.system[Konstanten.SYSTEM_BLACKLIST_TITEL_NR] += ";" + b;
                }
                listeLaden();
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_BLACKLIST_GEAENDERT, PanelBlacklist.class.getSimpleName());
            }
        }
    }

    private class BeobMausTabelle extends MouseAdapter {

        //rechhte Maustaste in der Tabelle
        BeobLoeschen beobLoeschen = new BeobLoeschen();
        private Point p;

        @Override
        public void mousePressed(MouseEvent arg0) {
            if (arg0.isPopupTrigger()) {
                showMenu(arg0);
            }
        }

        @Override
        public void mouseReleased(MouseEvent arg0) {
            if (arg0.isPopupTrigger()) {
                showMenu(arg0);
            }
        }

        private void showMenu(MouseEvent evt) {
            int nr;
            p = evt.getPoint();
            nr = jTableSender.rowAtPoint(p);
            if (nr >= 0) {
                jTableSender.setRowSelectionInterval(nr, nr);
            }
            JPopupMenu jPopupMenu = new JPopupMenu();
            //löschen
            JMenuItem item = new JMenuItem("Zeile löschen");
            item.addActionListener(beobLoeschen);
            jPopupMenu.add(item);
            //anzeigen
            jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
        }

        private class BeobLoeschen implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                int selectedTableRow = jTableSender.getSelectedRow();
                if (selectedTableRow >= 0) {
                    int del = jTableSender.convertRowIndexToModel(selectedTableRow);
                    ddaten.listeBlacklist.remove(del);
                }
            }
        }
    }

    private class BeobMausListe extends MouseAdapter {

        //rechhte Maustaste in der Tabelle
        BeobLoeschen beobLoeschen = new BeobLoeschen();
        private Point p;

        @Override
        public void mousePressed(MouseEvent arg0) {
            if (arg0.isPopupTrigger()) {
                showMenu(arg0);
            }
        }

        @Override
        public void mouseReleased(MouseEvent arg0) {
            if (arg0.isPopupTrigger()) {
                showMenu(arg0);
            }
        }

        private void showMenu(MouseEvent evt) {
            jList1.setSelectedIndex(jList1.locationToIndex(evt.getPoint()));
            JPopupMenu jPopupMenu = new JPopupMenu();
            //löschen
            JMenuItem item = new JMenuItem("Zeile löschen");
            item.addActionListener(beobLoeschen);
            jPopupMenu.add(item);
            //anzeigen
            jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
        }

        private class BeobLoeschen implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                int selectedRow = jList1.getSelectedIndex();
                if (selectedRow >= 0) {
                    ((DefaultComboBoxModel) jList1.getModel()).removeElementAt(selectedRow);
                    Daten.system[Konstanten.SYSTEM_BLACKLIST_TITEL_NR] = "";
                    if (((DefaultComboBoxModel) jList1.getModel()).getSize() > 0) {
                        Daten.system[Konstanten.SYSTEM_BLACKLIST_TITEL_NR] = ((DefaultComboBoxModel) jList1.getModel()).getElementAt(0).toString();
                        for (int i = 1; i < ((DefaultComboBoxModel) jList1.getModel()).getSize(); ++i) {
                            Daten.system[Konstanten.SYSTEM_BLACKLIST_TITEL_NR] += ";" + ((DefaultComboBoxModel) jList1.getModel()).getElementAt(i).toString();
                        }
                    }
                    ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_BLACKLIST_GEAENDERT, PanelBlacklist.class.getSimpleName());
                }
            }
        }
    }
}
